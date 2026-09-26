// Exact C++ port of learn_corpus_eliminative() (learners.R) for the cases the
// R version cannot finish at C=100: random or familiar context, with or
// without cross-word mutual exclusivity, and the full target-selection rule of
// learners.R's choose_target() (active_prob mixtures, the "unknown" and
// "goldilocks" policies, and bounded choice sets choice_k). Same model, same sampling rule, same
// learning criterion -- only the bookkeeping differs:
//   - weighted draws use a Fenwick tree (O(log M)) instead of R's sample(),
//     whose weighted sampling without replacement is O(M*C) per episode;
//   - distractors are drawn sequentially without replacement (exactly what
//     R's sample(..., prob=) does), by temporarily zeroing each chosen item;
//   - each word's candidate set is stored as a list rather than an M-length
//     row of hyp, with colcount[r] = colSums(hyp)[r] maintained incrementally
//     for the familiar-context weights 1/(colSums(hyp)+1).
// Uses R's RNG (unif_rand), so set.seed() controls it; draws are not
// value-for-value identical to the R version, but identically distributed.
//
// Compile from R with Rcpp::sourceCpp("learners_fast.cpp").
#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

struct Fenwick {
  int n; std::vector<double> t, w;
  Fenwick(int n_) : n(n_), t(n_ + 1, 0.0), w(n_, 0.0) {}
  void set(int i, double v) {  // 0-based
    double d = v - w[i]; w[i] = v;
    for (int j = i + 1; j <= n; j += j & -j) t[j] += d;
  }
  double total() const { double s = 0; for (int j = n; j > 0; j -= j & -j) s += t[j]; return s; }
  int sample() {  // index with probability w[i]/total
    double u = unif_rand() * total();
    int pos = 0, logn = 1; while ((logn << 1) <= n) logn <<= 1;
    for (int step = logn; step > 0; step >>= 1) {
      int nx = pos + step;
      if (nx <= n && t[nx] < u) { pos = nx; u -= t[nx]; }
    }
    if (pos >= n) pos = n - 1;
    while (w[pos] <= 0 && pos > 0) pos--;  // guard against float edge cases
    return pos;
  }
};

static NumericVector elim_core(int C, int M, NumericVector probs, bool active,
                        bool fam_context, double epsilon,
                        double max_episodes, bool mutual_exclusivity,
                        double active_prob, int policy, int choice_k,
                        double gold_target, double gold_sigma, bool pool_uniform,
                        bool fam_seen_only, int trace_every, std::vector<double> *trace) {
  // Target selection mirrors choose_target() in learners.R:
  //   active_prob < 0 -> use `active` (0 or 1), as in the original model;
  //     otherwise each episode is active with probability active_prob (a
  //     uniform draw is consumed only when 0 < active_prob < 1, as in R).
  //   passive episode: target ~ probs over all words.
  //   active episode: pool = all words (choice_k = 0) or choice_k words drawn
  //     without replacement ~ probs; among the pool's unknown words choose
  //     ~ probs (policy 0, "unknown") or ~ probs * exp(-(times_targeted -
  //     gold_target)^2 / (2 gold_sigma^2)) (policy 1, "goldilocks"); if the
  //     pool has no unknown word, a passive draw within the pool.
  //   pool_uniform (diagnostic only; not in learners.R): draw the choice_k
  //     pool uniformly without replacement instead of ~ probs.
  // fam_seen_only (diagnostic only; not in learners.R): familiar-context
  //   weights use the number of SEEN words still holding referent r as a
  //   candidate, 1/(colcount[r] - n_unseen + 1), instead of colSums(hyp).
  //   An unseen word's row is all ones, so in the original weighting every
  //   unseen word adds the same +1 to every referent's count, diluting the
  //   familiarity signal while many words are unseen; this option removes
  //   that dilution (non-ME only).
  // mutual_exclusivity (as in learners.R): when word w is learned, referent w is
  // removed from every still-unknown word's candidate set (hyp[others, w] <- 0),
  // and any word left with a single candidate is learned in the same episode,
  // cascading via a queue. Because referent indices equal word indices and a
  // word's own referent is never removed, an unknown word's ME exclusions are
  // exactly the currently-known words; an unseen word's candidate set is
  // therefore "all referents except known ones", never stored explicitly.
  const double total_needed = M * (1 - epsilon);
  std::vector<char> known(M, 0), seen(M, 0), inctx(M, 0);
  std::vector<std::vector<int>> cand(M);
  std::vector<int> colcount(M, M);  // colSums(hyp): hyp starts all ones
  const double AP = active_prob < 0 ? (active ? 1.0 : 0.0) : active_prob;
  Fenwick targ_all(M), targ_unk(M), dist(M);  // targ_unk: known words zeroed
  std::vector<int> times_targeted(M, 0);
  int n_unseen = M;
  auto fam_weight = [&](int r) {
    return probs[r] / (colcount[r] - (fam_seen_only ? n_unseen : 0) + 1.0);
  };
  for (int i = 0; i < M; i++) {
    targ_all.set(i, probs[i]);
    targ_unk.set(i, probs[i]);
    dist.set(i, fam_context ? fam_weight(i) : probs[i]);
  }
  auto refresh_dist = [&](int r) {
    if (fam_context) dist.set(r, fam_weight(r));
  };
  // tracing: competitor survival = share of the target's remaining competitors
  // (candidates other than its true referent) that reappear as distractors,
  // i.e. fail to be eliminated, on episodes where the target was already seen
  double tr_surv = 0, tr_n = 0;
  NumericVector dec(9, 0.0);
  double n_learned = 0, episodes = 0;
  bool censored = false;
  std::vector<int> ctx(C), keep;
  std::vector<double> saved(C);
  auto mark_learned = [&](int w) {
    known[w] = 1; n_learned++;
    targ_unk.set(w, 0.0);
    for (int d = 0; d < 9; d++)
      if (n_learned >= M * 0.1 * (d + 1) && dec[d] == 0) dec[d] = episodes;
  };

  // weighted draw from an explicit candidate list (R's sample(x, 1, prob = w))
  auto pick = [&](const std::vector<int> &x, const std::vector<double> &wt) {
    if (x.size() == 1) return x[0];
    double tot = 0; for (double v : wt) tot += v;
    double u = unif_rand() * tot;
    for (size_t i = 0; i < x.size(); i++) { u -= wt[i]; if (u < 0) return x[i]; }
    return x.back();
  };
  std::vector<int> pool, unk; std::vector<double> pw, pool_saved;
  auto choose_target = [&]() -> int {
    bool do_active = AP >= 1 ? true : (AP <= 0 ? false : unif_rand() < AP);
    if (!do_active) return targ_all.sample();
    if (choice_k <= 0 || choice_k >= M) {
      if (policy == 0) return targ_unk.sample();
      unk.clear(); pw.clear();
      for (int i = 0; i < M; i++) if (!known[i]) {
        double dd = times_targeted[i] - gold_target;
        unk.push_back(i); pw.push_back(exp(-(dd * dd) / (2 * gold_sigma * gold_sigma)) * probs[i]);
      }
      double tot = 0; for (double v : pw) tot += v;
      if (tot <= 0) for (size_t i = 0; i < unk.size(); i++) pw[i] = probs[unk[i]];
      return pick(unk, pw);
    }
    // bounded choice set: choice_k words without replacement ~ probs
    pool.clear(); pool_saved.clear();
    if (pool_uniform) {
      // partial Fisher-Yates over 0..M-1
      std::vector<int> idx(M); for (int i = 0; i < M; i++) idx[i] = i;
      for (int k = 0; k < choice_k; k++) {
        int j = k + (int)(unif_rand() * (M - k)); if (j >= M) j = M - 1;
        std::swap(idx[k], idx[j]); pool.push_back(idx[k]);
      }
    } else {
      for (int k = 0; k < choice_k; k++) {
        int r = targ_all.sample(); pool.push_back(r); pool_saved.push_back(targ_all.w[r]); targ_all.set(r, 0.0);
      }
      for (int k = choice_k - 1; k >= 0; k--) targ_all.set(pool[k], pool_saved[k]);
    }
    unk.clear(); pw.clear();
    for (int r : pool) if (!known[r]) {
      unk.push_back(r);
      if (policy == 1) { double dd = times_targeted[r] - gold_target;
        pw.push_back(exp(-(dd * dd) / (2 * gold_sigma * gold_sigma)) * probs[r]); }
      else pw.push_back(probs[r]);
    }
    if (unk.empty()) {
      pw.clear(); for (int r : pool) pw.push_back(probs[r]);
      return pick(pool, pw);
    }
    if (policy == 1) { double tot = 0; for (double v : pw) tot += v;
      if (tot <= 0) for (size_t i = 0; i < unk.size(); i++) pw[i] = probs[unk[i]]; }
    return pick(unk, pw);
  };

  while (n_learned < total_needed) {
    if (episodes >= max_episodes) { censored = true; break; }
    int target = choose_target();
    times_targeted[target]++;
    // distractors: C-1 draws without replacement from non-targets, weights dist
    saved[0] = dist.w[target]; dist.set(target, 0.0); ctx[0] = target;
    for (int k = 1; k < C; k++) {
      int r = dist.sample();
      ctx[k] = r; saved[k] = dist.w[r]; dist.set(r, 0.0);
    }
    for (int k = C - 1; k >= 0; k--) dist.set(ctx[k], saved[k]);  // restore
    for (int k = 0; k < C; k++) inctx[ctx[k]] = 1;
    if (trace && seen[target] && cand[target].size() > 1) {
      int hits = 0;
      for (int r : cand[target]) if (r != target && inctx[r]) hits++;
      tr_surv += (double)hits / (cand[target].size() - 1); tr_n++;
    }
    // eliminate target's candidates absent from this context
    if (!seen[target]) {
      seen[target] = 1; n_unseen--;
      if (!mutual_exclusivity) {
        for (int r = 0; r < M; r++) if (!inctx[r]) { colcount[r]--; refresh_dist(r); }
        cand[target].assign(ctx.begin(), ctx.end());
        if (fam_context && fam_seen_only) for (int r = 0; r < M; r++) refresh_dist(r);
      } else {
        // row already has zeros at known referents (an unseen word is unknown)
        for (int r = 0; r < M; r++) if (!inctx[r] && !known[r]) { colcount[r]--; refresh_dist(r); }
        cand[target].clear();
        for (int k = 0; k < C; k++) if (!known[ctx[k]] || ctx[k] == target) cand[target].push_back(ctx[k]);
      }
    } else {
      keep.clear();
      for (int r : cand[target]) {
        if (inctx[r]) keep.push_back(r); else { colcount[r]--; refresh_dist(r); }
      }
      cand[target].swap(keep);
    }
    for (int k = 0; k < C; k++) inctx[ctx[k]] = 0;
    if (cand[target].size() == 1 && !known[target]) {
      if (!mutual_exclusivity) {
        mark_learned(target);
      } else {
        std::vector<int> queue(1, target);
        size_t qi = 0;
        while (qi < queue.size()) {
          int w = queue[qi++];
          if (known[w]) continue;
          mark_learned(w);
          // hyp[others, w] <- 0 for all still-unknown words
          int n_unseen_unknown = 0;
          for (int v = 0; v < M; v++) {
            if (known[v]) continue;
            if (!seen[v]) {
              n_unseen_unknown++;
              if (M - n_learned == 1) queue.push_back(v);  // all but itself excluded
            } else {
              std::vector<int> &cv = cand[v];
              for (size_t i = 0; i < cv.size(); i++) if (cv[i] == w) {
                cv[i] = cv.back(); cv.pop_back(); colcount[w]--;
                if (cv.size() == 1) queue.push_back(v);
                break;
              }
            }
          }
          colcount[w] -= n_unseen_unknown; refresh_dist(w);
        }
      }
    }
    episodes++;
    if (trace && trace_every > 0 && ((long long)episodes % trace_every) == 0) {
      trace->push_back(episodes); trace->push_back(n_unseen); trace->push_back(n_learned);
      trace->push_back(tr_n > 0 ? tr_surv / tr_n : NA_REAL);
      tr_surv = 0; tr_n = 0;
    }
    if (((long long)episodes & 0xFFFF) == 0) Rcpp::checkUserInterrupt();
  }
  NumericVector out(11);
  for (int d = 0; d < 9; d++) out[d] = dec[d];
  out[9] = episodes; out[10] = censored ? 1 : 0;
  return out;
}

// [[Rcpp::export]]
NumericVector elim_fast(int C, int M, NumericVector probs, bool active,
                        bool fam_context, double epsilon = 0.01,
                        double max_episodes = 1e18, bool mutual_exclusivity = false,
                        double active_prob = -1, int policy = 0, int choice_k = 0,
                        double gold_target = 2, double gold_sigma = 2, bool pool_uniform = false,
                        bool fam_seen_only = false) {
  return elim_core(C, M, probs, active, fam_context, epsilon, max_episodes, mutual_exclusivity,
                   active_prob, policy, choice_k, gold_target, gold_sigma, pool_uniform,
                   fam_seen_only, 0, nullptr);
}

// Same simulation, additionally returning a trajectory sampled every
// trace_every episodes: n_unseen (words never yet targeted), n_learned, and
// the mean competitor-survival rate over the preceding window (see above).
// [[Rcpp::export]]
List elim_trace(int C, int M, NumericVector probs, bool fam_context, int trace_every,
                double max_episodes = 1e18, double active_prob = 1, int policy = 0,
                int choice_k = 0, bool pool_uniform = false, bool fam_seen_only = false) {
  std::vector<double> tr;
  NumericVector res = elim_core(C, M, probs, false, fam_context, 0.01, max_episodes, false,
                                active_prob, policy, choice_k, 2, 2, pool_uniform,
                                fam_seen_only, trace_every, &tr);
  int n = tr.size() / 4;
  NumericVector ep(n), un(n), le(n), sv(n);
  for (int i = 0; i < n; i++) { ep[i] = tr[4*i]; un[i] = tr[4*i+1]; le[i] = tr[4*i+2]; sv[i] = tr[4*i+3]; }
  return List::create(Named("result") = res,
                      Named("trace") = DataFrame::create(Named("episode") = ep, Named("n_unseen") = un,
                                                         Named("n_learned") = le, Named("competitor_survival") = sv));
}
