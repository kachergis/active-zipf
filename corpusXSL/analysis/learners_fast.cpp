// Exact C++ port of learn_corpus_eliminative() (learners.R) for the cases the
// R version cannot finish at C=100: constant active_prob in {0,1}, random or
// familiar context, with or without cross-word mutual exclusivity. Same model, same sampling rule, same
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

// [[Rcpp::export]]
NumericVector elim_fast(int C, int M, NumericVector probs, bool active,
                        bool fam_context, double epsilon = 0.01,
                        double max_episodes = 1e18, bool mutual_exclusivity = false) {
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
  Fenwick targ(M), dist(M);
  for (int i = 0; i < M; i++) {
    targ.set(i, probs[i]);
    dist.set(i, fam_context ? probs[i] / (M + 1.0) : probs[i]);
  }
  auto refresh_dist = [&](int r) {
    if (fam_context) dist.set(r, probs[r] / (colcount[r] + 1.0));
  };
  NumericVector dec(9, 0.0);
  double n_learned = 0, episodes = 0;
  bool censored = false;
  std::vector<int> ctx(C), keep;
  std::vector<double> saved(C);
  auto mark_learned = [&](int w) {
    known[w] = 1; n_learned++;
    if (active) targ.set(w, 0.0);
    for (int d = 0; d < 9; d++)
      if (n_learned >= M * 0.1 * (d + 1) && dec[d] == 0) dec[d] = episodes;
  };

  while (n_learned < total_needed) {
    if (episodes >= max_episodes) { censored = true; break; }
    // target: passive ~ probs over all words; active ~ probs over unknown words
    // (targ has known words zeroed when active)
    int target = targ.sample();
    // distractors: C-1 draws without replacement from non-targets, weights dist
    saved[0] = dist.w[target]; dist.set(target, 0.0); ctx[0] = target;
    for (int k = 1; k < C; k++) {
      int r = dist.sample();
      ctx[k] = r; saved[k] = dist.w[r]; dist.set(r, 0.0);
    }
    for (int k = C - 1; k >= 0; k--) dist.set(ctx[k], saved[k]);  // restore
    for (int k = 0; k < C; k++) inctx[ctx[k]] = 1;
    // eliminate target's candidates absent from this context
    if (!seen[target]) {
      seen[target] = 1;
      if (!mutual_exclusivity) {
        for (int r = 0; r < M; r++) if (!inctx[r]) { colcount[r]--; refresh_dist(r); }
        cand[target].assign(ctx.begin(), ctx.end());
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
    if (((long long)episodes & 0xFFFF) == 0) Rcpp::checkUserInterrupt();
  }
  NumericVector out(11);
  for (int d = 0; d < 9; d++) out[d] = dec[d];
  out[9] = episodes; out[10] = censored ? 1 : 0;
  return out;
}
