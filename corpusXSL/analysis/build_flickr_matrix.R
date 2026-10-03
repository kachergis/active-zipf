# Idealized active-vs-passive scene selection, Flickr8k analogue of
# build_sun_matrix.R's SUN database analysis (see "Active learning: Choosing
# situations" in the paper). Builds the N (words) x M (photos) conditional
# probability matrix P from 8k-flickr/8k-flickr_sentences_fixed.csv (8,108
# photos, 5 independent human captions each), then computes the same passive
# (uniform q) vs. optimal active (q-hat) min-probability comparison already
# run against SUN.
#
# DATA NOTE: an earlier, separate extraction (8k-flickr/extract_flickr.py)
# had a data-integrity bug: its lexicon was counted from cleaned/lemmatized
# tokens, but its photo-by-word matrix was populated by looking up the
# ORIGINAL uncleaned tokens in that same lexicon, so any word whose surface
# form differed from its lemma (plurals, possessives, ...) silently failed
# the lookup and got dropped from the matrix -- undercounting frequent words
# (e.g. "dog": 8,111 in the old matrix vs. 10,284 in the lexicon) and leaving
# at least one word ("ha") with zero recorded occurrences despite a positive
# lexicon count, which breaks min(Pq) for any q. Re-extracted with
# extract_flickr_fixed.py, which uses one consistent cleaned token list
# throughout; verified 0 colsum/lexicon mismatches and 0 all-zero word
# columns before use here. That script also skips ever writing a dense
# photo x word matrix (8,108 x 4,438, >99% sparse) to disk -- a plain-text
# version of it is ~900MB for no benefit -- so we build the sparse matrix
# directly from the per-caption word lists below instead.

library(Matrix)
library(lpSolve)

setwd("/Users/gkacherg/Documents/GitHub/active-zipf/8k-flickr")
lex <- read.delim("8k-flickr_lexicon_fixed.csv", header = FALSE, col.names = c("word", "count"), quote = "")
lex_filt <- lex[lex$count > 1, ]  # matches extract_flickr_fixed.py's filter (words seen only once excluded)
word_index <- setNames(seq_len(nrow(lex_filt)), lex_filt$word)

sent <- read.delim("8k-flickr_sentences_fixed.csv", header = FALSE, col.names = c("photo", "words"),
                    quote = "", colClasses = c("integer", "character"))
words_per_caption <- strsplit(sent$words, " ", fixed = TRUE)
n_words <- lengths(words_per_caption)
photo_idx <- rep(sent$photo, n_words)
word_str <- unlist(words_per_caption, use.names = FALSE)
keep <- word_str %in% names(word_index)  # drop hapax (count==1) words, matching the >1 filter
photo_idx <- photo_idx[keep]
word_idx <- word_index[word_str[keep]]

ph_wf <- sparseMatrix(i = photo_idx, j = word_idx, x = 1,
                       dims = c(max(sent$photo), nrow(lex_filt)), dimnames = list(NULL, lex_filt$word))
ph_wf <- as.matrix(ph_wf)

zero_photos <- which(rowSums(ph_wf) == 0)
cat("Dropping", length(zero_photos), "all-zero photo row(s) out of", nrow(ph_wf), "\n")
if (length(zero_photos) > 0) ph_wf <- ph_wf[-zero_photos, ]

N <- ncol(ph_wf)  # words
M <- nrow(ph_wf)  # photos
cat("N (words):", N, " M (photos):", M, "\n")

# P: N x M, each column (photo) normalized to a probability distribution over
# the words used to describe it -- exactly SUN's object-given-scene setup,
# with "photo" playing the role of "scene" and "word" the role of "object".
P <- t(as.matrix(ph_wf))
col_sums <- colSums(P)
stopifnot(all(col_sums > 0))
P <- sweep(P, 2, col_sums, "/")

saveRDS(P, "/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/flickr_P_matrix.rds")

cat("\n=== Passive baseline (uniform q) ===\n")
q_unif <- rep(1 / M, M)
marg_pass <- as.vector(P %*% q_unif)
min_p_pass <- min(marg_pass)
cat("min(P q_unif):", min_p_pass, "\n")
cat("argmin word:", rownames(P)[which.min(marg_pass)], "\n")

cat("\n=== Optimal active learner (q-hat) via LP ===\n")
# maximize t  s.t.  P q >= t*1,  sum(q)=1,  q>=0,  t free
# Sparse constraint specification (lpSolve's dense.const=FALSE triplet form):
# avoids materializing an N x (M+1) dense matrix (N=4438, M~8107 here, vs.
# SUN's N=3458, M=1111 -- meaningfully bigger on the M side).
nzP <- which(P != 0, arr.ind = TRUE)
# rows 1..N: sum_j P[i,j] q_j - t >= 0
main_rows <- cbind(nzP[, "row"], nzP[, "col"], P[nzP])
t_col <- cbind(seq_len(N), M + 1, -1)  # -t term on every row
# row N+1: sum q = 1
sum_row <- cbind(N + 1, seq_len(M), 1)
dense.const <- rbind(main_rows, t_col, sum_row)

nvar <- M + 1
obj <- c(rep(0, M), 1)  # maximize t
con_dir <- c(rep(">=", N), "=")
con_rhs <- c(rep(0, N), 1)

t0 <- Sys.time()
sol <- lp("max", obj, dense.const = dense.const, const.dir = con_dir, const.rhs = con_rhs)
cat("LP status:", sol$status, " (0 = optimal)\n")
cat("LP solve time:", as.numeric(Sys.time() - t0, units = "secs"), "s\n")

q_hat <- sol$solution[1:M]
t_opt <- sol$solution[M + 1]
marg_act <- as.vector(P %*% q_hat)
cat("t (LP optimum, = min(P q_hat)):", t_opt, "\n")
cat("min(P q_hat) recomputed:", min(marg_act), "\n")

R <- t_opt / min_p_pass
cat("\n=== Speedup ratio R = min(P q_hat) / min(P q_unif) ===\n")
cat("R =", R, "\n")

save(P, q_unif, q_hat, marg_pass, marg_act, min_p_pass, t_opt, R,
     file = "/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/flickr_active_results.RData")

cat("\nDone.\n")
