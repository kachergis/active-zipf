# Suggested edits: checklist

Status of the critical review of `main_revised.tex` (paper 1) and `paper2/draft.tex` (paper 2), on branch `shorten-and-reframe`. The original review, with its full reasoning and line references to the old version, is in git history (commit `ad1617b`).

Paper 1 is now about 4,900 words of text (35 pages, down from about 15,800 words and 76 pages). Paper 2 has a full draft of about 10,400 words (51 pages) plus an 8-page Supplementary Material. Last reviewed 2026-11-16; the new suggestions from that review are in the final section.

Legend: `[x]` done · `[ ]` open · `[~]` partly done, or decided against

---

## Top priorities

- [x] **Reframe the central claim.** Changed from "passive learning is implausibly slow" to a relative one: Zipf costs about H_W ≈ ln W, and active control recovers much of it. Abstract, Introduction, and Discussion all rewritten.
- [x] **Move context selection to paper 2.** The C=100 analyses, the "C, not C/M" result, and the mutual-exclusivity cascade are now paper 2 Study 1. The Flickr8k passive vs. active result moved the other way, into paper 1.
- [x] **Cut paper 1 to under 8,000 words.** It is now about 4,800.
- [x] **Lead with the 50–90% checkpoint speedups.** 99% is presented as the upper end.
- [x] **One symbol per concept.** W is vocabulary, C is context size, S is the number of scenes, in both papers.
- [x] **Remove duplicated text.** The familiarity-dilution passage is now only in paper 2.

## The story (§1)

- [x] Zipf penalty ≈ H_W (Eq. 6, Table 2); Blythe et al.'s factor of 11.58 explained as H_60000.
- [x] Pre-literacy (about 10,000 words) is the primary scope; 60,000 appears once, for comparison.
- [x] Static vs. adaptive active control is the organizing distinction, and is used to answer the CHILDES circularity point (Reviewer 2 #7).
- [x] Headline numbers are consistent across the paper. The "same random samples" and "orders of magnitude" wording is fixed.

## Moving material between papers (§2)

- [x] Paper 2 study order: which lever, how much and over what, when, at what cost.
- [x] Goldilocks result reported as an effect of familiarity dilution (paper 2 Study 2).
- [x] Baselines: the ramp study uses its own consistent set of runs, and the table caption points out the separate set used in Study 2.
- [x] Paper 2 literature added: Charnov; Hills et al.; Gureckis & Markant; Kretch et al.; Iverson; Walle & Campos; Tauber et al.; Sullivan et al.
- [ ] Paper 2: cite the curiosity literature (Oudeyer:2016 is already in the bib).
- [~] Paper 1 no longer cites unpublished paper 2 results. Regressed: it now points to the companion paper three times (C=100 footnote, guess-test blocking, word-driven learner). See N3 and N5 below.

## Paper 1, section by section (§4)

- [x] Introduction merged into one overview with three contributions. Novelty claims softened, with the classic unequal-probability coupon collector results cited (Flajolet et al. 1992; Boneh & Hofri 1997).
- [x] "Methods" renamed "Framework"; old Fig. 1 dropped.
- [x] Blythe reconciliation moved to a footnote; duplicate equations, unused O, and garbled sentences and captions removed.
- [x] Corpus estimates collected in a T₊ table.
- [x] **CHILDES exponent by word class (Reviewer 1).** Maximum-likelihood a is 1.08 for all words, 1.00 for content words, and 0.91 for function words. The tail is steeper (log-log slope 1.6–1.7 over ranks ≥ 10), which explains the large empirical penalty. Code: `corpusXSL/analysis/fit_childes_exponent.R`.
- [x] Morphology mentioned (Discussion).
- [x] SUN described; reconstruction-provenance sentence cut; Jensen argument and online-learning intuition moved to Appendix B.
- [x] **Online figure recolored** to a color-blind-safe palette: passive black, optimal blue, online orange. Caption updated; the original EPS is kept in `paper/original_figures/`.
- [x] Incremental section: ranked-frequency learner and guess-test bimodality cut; tables merged; mean-vs-median reduced to one sentence; defensive phrasing removed.
- [x] Discussion rewritten (about 3,250 words → about 1,100).

## Correctness (§5)

- [x] Appendix A notes that Eq. 3 treats words as independent (exact in the Poissonized limit).
- [x] C vs. C/M contradiction resolved (the material is now in paper 2, stated once).
- [x] The twofold online result is explained by the structure of P.
- [x] Section cross-references by name removed.

## Figures and tables (§7)

- [x] 9 figures → 7; 7 tables → 5; checkpoint figure and mutual-exclusivity table moved or cut.
- [~] Merge the CHILDES frequency figure into Fig. 3 as a panel. Optional; not done.

## Leftovers and copy edits (§8)

- [x] Stale comments, `\GK{}` macros, unused preamble macros, and the large commented-out proof block removed.
- [x] Abstract trimmed (about 280 → about 190 words); keywords updated.
- [ ] **Acknowledgments and funding.** The empty section was removed. Authors should add funding (the JSPS KAKENHI grant line was commented out in the old version).

## 2021 reviewer points (§9)

- [x] Editor: rejecting passive learning needs strong evidence → claim made relative.
- [x] Editor: 2–3k-word target → the penalty is 7.5x even at 1,000 words.
- [x] R1: morphology; R1: Zipf slope by word class; R1: Tauber et al. (2013).
- [x] R2 #7: CHILDES already reflects active shaping.
- [x] R3 #5: what SUN is; R3 #7: 235x vs. 2x.
- [x] **R2 #2: tightness of the bound.** Across 0 < a ≤ 1.5, ε from 0.01 to 0.99, and W of 10k or 60k, T₊ exceeds the exact root by 11–43% (11% for CHILDES). Code: `corpusXSL/analysis/bound_tightness.R`.

## Simulations (overnight run, `run_overnight_fill.R`)

- [x] Passive learners at a=1.5: eliminative 6.4×10⁷ episodes (216x the active learner), guess-test 253,259 (6.7x).
- [x] Forgetting, half-life 200, passive: median 12.6M episodes to 99% (467x).
- [~] Three confirmations, half-life 200, passive: still does not reach 90% within 2×10⁷ episodes.
- [x] Ramps: exact matched-autonomy comparison (forward 4–13% faster, reverse up to 2.3x slower).
- [x] Flickr8k crossover located between move_cost 1 and 2.

## Still open

- [ ] Co-author review of the reframed claim and the cuts (Shohei Hidaka, Takuma Torii), since this changes the paper's central argument.
- [ ] Acknowledgments and funding (paper 1 and paper 2).
- [x] Paper 2: Studies 2–4 extended to the guess-test learner (`run_guesstest_paper2.R`, `flickr_guesstest.R`). New findings: partial control can beat full control; Goldilocks targeting helps; active learning on captions plateaus near 98%.
- [x] Paper 2: guess-test mechanisms diagnosed (`diagnose_guesstest.R`, `diag_gt/`). Familiar context at C=100: wrong guesses confirm themselves when known words are never rehearsed. Choice sets: frequency counted twice (P ∝ p^1.6–1.9) starves the rare words that block frequent ones.
- [x] Uniform vs. frequency-weighted active targeting (`run_uniform_active.R`, `uniform_active/`). Eliminative: no difference with random context (paper 1 robustness note added); about 2x faster with familiar context at C=100 (both policies reported in paper 2, Study 1). Guess-test: paper 1 now notes that uniform targeting roughly doubles its active speedup (4.9x → 9.9x).
- [ ] Paper 2: curiosity citation (Oudeyer).
- [ ] Optional: merge the CHILDES frequency figure into Fig. 3.
- [ ] Optional: a data-driven estimate of the ramp timescale from egocentric video (future work, out of scope).
- [~] Push the branch and open a PR when ready. The branch is pushed and up to date with `origin/shorten-and-reframe`; no PR yet.


---

## Second review (2026-11-16): new suggestions

Both papers compile with no unresolved references. Since the checklist above was written, paper 2 gained the machine-learning framing, the word-driven learner, the observable (lock-in) criterion analyses, 200-replication reruns with bootstrap intervals, an overview figure, and the Supplementary Material. Everything marked done above is still done, with the one regression noted. The items below are new.

### Across both papers

- [ ] **N1. Make the observable learning criterion the primary guess-test model.** The default guess-test learner counts a word as learned, and stops pursuing it, the moment its guess is correct, which the learner cannot know. Paper 2 says so, yet reports this version "for comparability with Paper 1" and treats lock-in as robustness. Several headline results change under lock-in: no fully active learner finishes at C=100; uniform and Goldilocks targeting stall far more often (132 and 71 of 200 runs, against 15); the benefit of becoming active late disappears. A reviewer will say the oracle version is not a possible learner. Swap them: lock-in in the main text, the oracle version in the supplement. Paper 1 already has the lock-in numbers (4.4x; Table 5, first row).
- [ ] **N2. Paper 1's uniform-targeting sentence is out of date and too strong** (`main_revised.tex` L252). It cites 3,833 episodes from six replications; paper 2 now has 200 (3,760, against 8,007 for frequency-weighted). More important, paper 2 shows the gain does not survive the observable criterion cleanly: uniform targeting halves median time but leaves two-thirds of runs stalled. "The 4.9-fold figure understates what active targeting can achieve" needs that qualification, or the sentence should go.
- [ ] **N3. Paper 1 should state the word-driven result, since it bounds the central claim.** The 19-fold advantage assumes the learner can get any object it cannot name named on demand. A learner that can pursue only words it has already heard gains 3.4-fold (paper 2, Table 6). Paper 1 mentions this in the last sentence of a Discussion paragraph, with no number. Put the number in the Results or the "Why the estimates range" paragraph, and name the assumption in the Framework.
- [ ] **N4. Use one active policy on Flickr8k.** The eliminative learner targets the rarest unlearned learnable word, which presumes it knows word rarity and which words are learnable; the guess-test learner targets by frequency because rarest-first deadlocks it. The two are then compared in one study (paper 2, Study 4), and the eliminative row sits beside frequency-weighted synthetic rows in paper 1's Table 4. Rerun the eliminative Flickr8k conditions with frequency-weighted targeting, or report both.
- [ ] **N5. Reconcile the two taxonomies.** Paper 1 organizes by static vs. adaptive control; paper 2 by allocation among problems vs. choice of query. Paper 1's own result, that the active advantage grows like H_W, is exactly paper 2's "allocation" bound. One sentence in each paper relating the two would also let paper 1 cite the idea without leaning on unpublished results.
- [ ] **N6. Decide how the papers cite each other.** Each is "in preparation" in the other. If they will be submitted together, say so in both; if not, paper 1 must stand alone (see the regression above).

### Paper 1

- [ ] **N7. Title.** "Active learning expedites cross-situational word learning--even under Zipfian distributions" understates the finding. Active selection helps 3.2-fold under uniform input and 19- to 28-fold under Zipfian input: it helps *especially* under Zipfian distributions.
- [ ] **N8. Say in the Learners section that guess-test "learned" is not observable** (see N1). It is currently explained only in the Forgetting section.
- [ ] **N9. Explain the 3.2-fold active speedup under uniform input** (Table 3) in one sentence: even with equal frequencies a passive learner wastes episodes on known words, the coupon-collector tail.
- [ ] **N10. Fig. 2's note on forgetting** gives the gap at 99% (4.4x to 467x), the most extreme figure in the paper. To stay consistent with leading on 50--90%, add or substitute the 90% values (3.3x to 164x).

### Paper 2

- [ ] **N11. Length.** At about 10,400 words plus supplement it is twice paper 1 and approaching the length of the old manuscript. Study 2 has six subsections and carries five manipulations. Options: promote object- vs. word-driven pursuit to its own study (it is one of the two or three most consequential results and is easy to miss); move the long paragraph on why the familiar-context advantage shrinks with W (Study 1, about 300 words) and the 15-row blend table (Study 3) to the supplement.
- [ ] **N12. Abstract** is about 250 words with four numbered findings and several sub-findings. Cut to about 180: the two levers, the sampling-bias result, object- vs. word-driven pursuit, and one sentence for timing and cost together.
- [ ] **N13. Study 3 is now a small effect.** Becoming active late gains at most 12--14%, and nothing under the observable criterion. Consider shortening it to the two robust results (losing control early is costly; a passive start protects against stalling) and folding it into Study 2 as a subsection.
- [ ] **N14. Study 4 needs uncertainty estimates.** The eliminative linger vs. move-on differences are 1--7% from 30 replications with no intervals, and the crossover claim rests on 1% and 3% differences. Add bootstrap intervals as in Studies 2 and 3, or more replications.
- [ ] **N15. Report medians and proportion finished in Table 5** (guess-test). The text says C=10 times are skewed and gives medians that tell a different story from the tabled means (e.g. choice set of 50, familiar: mean 27,861, median 6,232).
- [ ] **N16. Check the machine-learning mapping with someone from that field.** Calling object-driven target selection a membership query is loose: a membership query constructs an arbitrary instance, whereas this learner chooses only the target, not the context. "Has no comparable bound" for context selection is also a strong claim resting on one learner at two context sizes.
- [ ] **N17. Mechanism tests rest on a few instrumented runs** (acknowledged in Limitations). The self-confirming-error account is the paper's most novel claim; six runs per intervention is thin. Rerun the two interventions at 30 replications.
- [ ] **N18. Small things.** Table 1: the Passive/Random cell at C=100 shows "1x" beside ">5e8". `W=1000` vs. `1{,}000` formatting. Fig. 3 includes a guess-test panel that Study 1 does not discuss. Two stray blank lines before Study 4. Curiosity literature (Oudeyer) and acknowledgments still missing.
