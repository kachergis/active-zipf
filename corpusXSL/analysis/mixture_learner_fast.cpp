// C++ port of learn_corpus_mixture() (mixture_learner.R) for vocabulary sizes
// that are prohibitively slow in pure R (M >= 5,000).
//
// Same model: guess-test learner with three episode types (passive, joint-
// attention, child-initiated) whose mixing proportions follow a linear ramp
// over T_ramp episodes. JA episodes use a C=1 scene (only the true referent),
// so any wrong guess is immediately disconfirmed and the correct referent
// proposed in the same step. CI episodes target an unknown word by frequency
// (the standard "unknown" active policy). JA caregiver policy is an integer:
//   0 = frequency  -- target ~ probs over all words
//   1 = gap        -- target ~ probs over believed-unknown words
//   2 = heard      -- target ~ probs over heard-but-believed-unknown words
//
// Developmental schedule: each proportion is a linear ramp
//   pi_X(t) = pi_X_start + (pi_X_end - pi_X_start) * min(1, t / T_ramp)
// pi_ci(t) = max(0, 1 - pi_p(t) - pi_jt(t)) is derived.
//
// Uses R's RNG (unif_rand()); set.seed() controls it. Distractor sampling uses
// a Fenwick tree (O(log M)) -- the main bottleneck in pure R at large M.
//
// Compile from R: Rcpp::sourceCpp("mixture_learner_fast.cpp")

#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

// O(log M) weighted sampler (without-replacement via temporary zero/restore).
struct Fenwick {
  int n;
  std::vector<double> t, w;
  Fenwick() : n(0) {}
  explicit Fenwick(int n_) : n(n_), t(n_ + 1, 0.0), w(n_, 0.0) {}
  void set(int i, double v) {
    double d = v - w[i]; w[i] = v;
    for (int j = i + 1; j <= n; j += j & -j) t[j] += d;
  }
  double total() const {
    double s = 0; for (int j = n; j > 0; j -= j & -j) s += t[j]; return s;
  }
  int sample() const {
    double u = unif_rand() * total();
    int pos = 0, logn = 1;
    while ((logn << 1) <= n) logn <<= 1;
    for (int step = logn; step > 0; step >>= 1) {
      int nx = pos + step;
      if (nx <= n && t[nx] < u) { pos = nx; u -= t[nx]; }
    }
    if (pos >= n) pos = n - 1;
    while (w[pos] <= 0 && pos > 0) pos--;
    return pos;
  }
};

// [[Rcpp::export]]
NumericVector mixture_fast(
    int C, int M, NumericVector probs,
    double pi_p_start,  double pi_p_end,
    double pi_jt_start, double pi_jt_end,
    double T_ramp,
    int jt_policy,   // 0=frequency, 1=gap, 2=heard
    int lock_k,
    double epsilon      = 0.01,
    double max_episodes = 1e6) {

  // --- state ---
  std::vector<int>  current_guess(M, -1);  // -1 = no guess
  std::vector<int>  claimed_by(M, -1);     // -1 = unclaimed
  std::vector<int>  confirm_count(M, 0);
  std::vector<bool> word_known(M, false);
  // believed[w]: true when confirm_count[w] >= lock_k (or word_known if lock_k=0)
  // Active and JA-gap/heard policies avoid believed words.
  std::vector<bool> believed(M, false);
  std::vector<int>  times_targeted(M, 0);
  int n_heard_unk = 0;  // size of targ_heard's support

  // Fenwick trees
  Fenwick targ_all(M);   // all words ~ probs (passive target, JA frequency)
  Fenwick targ_unk(M);   // believed-unknown words ~ probs (CI, JA gap)
  Fenwick targ_heard(M); // heard-but-believed-unknown words ~ probs (JA heard)
  Fenwick dist(M);       // distractor sampling (all words ~ probs)
  for (int i = 0; i < M; i++) {
    targ_all.set(i, probs[i]);
    targ_unk.set(i, probs[i]);
    dist.set(i, probs[i]);
  }

  double n_learned = 0;
  const double total_needed = M * (1.0 - epsilon);
  NumericVector dec(9, 0.0);
  double episodes = 0;
  bool censored = false;

  std::vector<int>    ctx(C);        // scene referent indices (passive/CI)
  std::vector<double> saved_w(C);    // saved Fenwick weights for restore
  std::vector<int>    unclaimed_buf; // scratch for propose step

  // --- helpers ---
  auto mark_believed = [&](int w) {
    if (believed[w]) return;
    believed[w] = true;
    targ_unk.set(w, 0.0);
    if (targ_heard.w[w] > 0) { targ_heard.set(w, 0.0); n_heard_unk--; }
  };

  auto unmark_believed = [&](int w) {
    if (!believed[w] || word_known[w]) return;
    believed[w] = false;
    targ_unk.set(w, probs[w]);
    if (times_targeted[w] > 0) { targ_heard.set(w, probs[w]); n_heard_unk++; }
  };

  auto mark_learned_fn = [&](int w) {
    word_known[w] = true;
    mark_believed(w);
    n_learned++;
    for (int d = 0; d < 9; d++)
      if (n_learned >= M * 0.1 * (d + 1) && dec[d] == 0.0) dec[d] = episodes;
  };

  // --- main loop ---
  while (n_learned < total_needed) {
    if (episodes >= max_episodes) { censored = true; break; }
    if (((long long)episodes & 0xFFFF) == 0) Rcpp::checkUserInterrupt();

    // Mixing proportions at time t
    double r   = (T_ramp > 0) ? std::min(1.0, episodes / T_ramp) : 1.0;
    double pp  = pi_p_start  + (pi_p_end  - pi_p_start)  * r;
    double pjt = pi_jt_start + (pi_jt_end - pi_jt_start) * r;
    double pci = std::max(0.0, 1.0 - pp - pjt);

    double u = unif_rand();
    int etype = (u < pp) ? 0 : (u < pp + pjt) ? 1 : 2;  // 0=passive, 1=JA, 2=CI

    // --- target selection ---
    int tgt;
    bool ja = false;

    if (etype == 0) {
      // Passive: target ~ probs over all words
      tgt = targ_all.sample();

    } else if (etype == 1) {
      // Joint attention: caregiver policy
      ja = true;
      if (jt_policy == 0) {
        // frequency: all words
        tgt = targ_all.sample();
      } else if (jt_policy == 1) {
        // gap: believed-unknown words; fall back to passive if none
        if (targ_unk.total() <= 0) { tgt = targ_all.sample(); ja = false; }
        else tgt = targ_unk.sample();
      } else {
        // heard: heard-but-believed-unknown; fall back to passive if none
        if (n_heard_unk == 0) { tgt = targ_all.sample(); ja = false; }
        else tgt = targ_heard.sample();
      }

    } else {
      // Child-initiated: unknown word ~ probs; fall back to passive if all believed known
      if (targ_unk.total() <= 0) tgt = targ_all.sample();
      else tgt = targ_unk.sample();
    }

    // Track first hearing
    times_targeted[tgt]++;
    if (times_targeted[tgt] == 1 && !believed[tgt]) {
      targ_heard.set(tgt, probs[tgt]);
      n_heard_unk++;
    }

    // --- build scene ---
    // JA: scene = {tgt} only; passive/CI: tgt + C-1 distractors
    if (!ja) {
      saved_w[0] = dist.w[tgt]; dist.set(tgt, 0.0); ctx[0] = tgt;
      for (int k = 1; k < C; k++) {
        int d = dist.sample();
        ctx[k] = d; saved_w[k] = dist.w[d]; dist.set(d, 0.0);
      }
      for (int k = C - 1; k >= 0; k--) dist.set(ctx[k], saved_w[k]);
    }

    // --- guess-test update ---
    int g = current_guess[tgt];

    // Disconfirmation
    if (g != -1) {
      bool disconfirmed;
      if (ja) {
        disconfirmed = (g != tgt);  // JA scene = {tgt}: any wrong guess evicted
      } else {
        disconfirmed = true;
        for (int k = 0; k < C; k++) if (ctx[k] == g) { disconfirmed = false; break; }
      }
      if (disconfirmed) {
        claimed_by[g] = -1;
        current_guess[tgt] = -1;
        confirm_count[tgt] = 0;
        g = -1;
        unmark_believed(tgt);  // restore to active pools if believed but not truly known
      }
    }

    // Propose or confirm
    if (g == -1) {
      if (ja) {
        // JA scene = {tgt}: propose tgt iff unclaimed
        if (claimed_by[tgt] == -1) {
          current_guess[tgt] = tgt;
          claimed_by[tgt] = tgt;
        }
      } else {
        // Find unclaimed referents in ctx and pick one uniformly at random
        unclaimed_buf.clear();
        for (int k = 0; k < C; k++)
          if (claimed_by[ctx[k]] == -1) unclaimed_buf.push_back(ctx[k]);
        if (!unclaimed_buf.empty()) {
          int sz = (int)unclaimed_buf.size();
          int idx = (sz == 1) ? 0 : (int)(unif_rand() * sz);
          if (idx >= sz) idx = sz - 1;
          int ng = unclaimed_buf[idx];
          current_guess[tgt] = ng;
          claimed_by[ng] = tgt;
        }
      }
    } else {
      confirm_count[tgt]++;
      if (lock_k > 0 && !believed[tgt] && confirm_count[tgt] >= lock_k)
        mark_believed(tgt);
    }

    // Check learned
    if (!word_known[tgt] && current_guess[tgt] == tgt &&
        (lock_k == 0 || confirm_count[tgt] >= lock_k))
      mark_learned_fn(tgt);

    episodes++;
  }

  NumericVector out(11);
  for (int d = 0; d < 9; d++) out[d] = dec[d];
  out[9] = episodes;
  out[10] = censored ? 1.0 : 0.0;
  return out;
}
