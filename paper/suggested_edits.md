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
- [x] Paper 1 no longer depends on unpublished paper 2 results: it points to the companion twice, and states its own numbers in both places (see N6).

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

- [x] **N1. The observable (lock-in) criterion is now the primary guess-test model in paper 2.** General method, Studies 1--4, abstract, and Discussion report lock-in results (medians, with stalled-run counts). The original criterion moved to a new supplement section, with the old tables. Paper 1 now says the default criterion is unobservable and gives the 4.4x lock-in figure next to 4.9x. Not done: making lock-in primary in paper 1 as well, which would need new guess-test runs across exponents and checkpoints.
- [x] **N2. Paper 1's uniform-targeting sentence** now cites the 200-replication figures (3,760 vs. 8,007) and says the gain is fragile: two-thirds of uniform runs stall under the observable criterion.
- [x] **N3. Paper 1 states the word-driven result** (3.4x rather than 19x) in the abstract, Framework, Results, and Discussion.
- [x] **N4. One active policy on Flickr8k.** The eliminative learner was rerun with frequency-weighted targeting (`run_flickr_freq.R`, 100 replications): 3.2x, 8.9x, and 20x at 50/90/99%, and the same crossover between move_cost 1 and 2. Both papers updated; rarest-first is mentioned as a variant. Paper 1's headline range is now 2--9x.
- [x] **N5. The two taxonomies are related in both papers.** Paper 1's Discussion has a new paragraph ("What kind of active learning this is"): static and adaptive control both allocate episodes among words, which is why the advantage tracks H_W. Paper 2's Introduction says static vs. adaptive is a distinction within the target-selection lever.
- [x] **N6. How the papers cite each other.** Paper 1 now stands alone: it names the companion (Kachergis, in preparation) twice, as a pointer for the C=100 case and for the stall analysis, and gives its own numbers and replication counts for every result it states. Paper 2 cites Paper 1 as Kachergis, Torii, & Hidaka (in preparation) and says it can be read without it. Revisit if the two are submitted together.

### Paper 1

- [x] **N7. Title** changed to "Active learning expedites cross-situational word learning, especially under Zipfian distributions". Check with co-authors.
- [x] **N8. The Learners section now says** that the guess-test learner cannot observe when a guess is correct, and that the observable criterion is used from the Forgetting section on.
- [x] **N9. The 3.2-fold speedup under uniform input** is explained in one sentence after the exponent results (the coupon-collector tail).
- [x] **N10. Fig. 2's forgetting note** now gives the 90% values (3.3x to 164x), and the Forgetting text gives 90% before 99%.

### Paper 2

- [x] **N11. Structure.** Object- vs. word-driven pursuit is now Study 3; "Starting passive" is Study 4 and the Flickr8k study is Study 5 (Introduction, overview figure, Discussion, and supplement renumbered). The analysis of why the familiar-context advantage shrinks with W moved to the supplement, leaving a three-sentence summary in Study 1; the blend table moved earlier (N13). The paper is still 49 pages.
- [x] **N12. Abstract** cut from about 250 to about 200 words, without the numbered findings: the two levers, the sampling-bias result, word-driven pursuit, the passive start, and the cost of switching scenes.
- [x] **N13. Study 3 is now "Starting passive".** The main text reports forward ramps only, in one table, with three positive results (a passive start costs little; late control is used at least as well as early control; it protects the guess-test learner against stalls) and one paragraph on losing control. The blend table, the ramp figure, reverse ramps, and the matched-autonomy method moved to a new supplement section.
- [x] **N14. Study 4 uncertainty estimates.** Both learners now have 100 replications per cell and bootstrap intervals. Guess-test learner: lingering is 5% faster than moving on at 90% when switching is free (CI 0.2--9%) and 23--38% faster at costs of 1--10; active runs still level off at 95.0% (move-on) and 96.2--96.4% (linger). One of 100 passive runs ended just short of 99%.
- [x] **N15. Medians and stalled-run counts** are now in the main guess-test table (done with N1).
- [ ] **N16. Check the machine-learning mapping with someone from that field.** Calling object-driven target selection a membership query is loose: a membership query constructs an arbitrary instance, whereas this learner chooses only the target, not the context. "Has no comparable bound" for context selection is also a strong claim resting on one learner at two context sizes.
- [x] **N17. Mechanism tests at 30 replications** (`diag_gt/run_N17.R`, `summarize_n14_n17.R`). The self-confirming-error account holds: 23 of 30 fully active runs did not finish, each with all 11--22 remaining words holding wrong guesses on one another's referents; both interventions let all 30 runs finish. Uniform choice within a frequency-weighted set of 100 is 18 times faster (CI 17--20). Still at 3--6 runs: the blocked-word shares, word-driven fixation, and the lock-in stall diagnostics.
- [~] **N18. Small things.** Done: Table 1's C=100 reference cell now shows a dash and the caption says speedups there are relative to the cap; `1{,}000` formatting; Study 1 now refers to the guess-test panel of the exponent figure; stray blank lines removed; curiosity literature cited in the Introduction (Oudeyer et al., 2016; Gottlieb & Oudeyer, 2018, both already in the bibliography). Not done: acknowledgments and funding, which need the authors' details.

- [x] **Authorship (2026-11-16).** Paper 1: Kachergis, Torii, Hidaka. Paper 2 and its supplement: Kachergis alone, for now. Affiliations kept as they were; check that they are current.
