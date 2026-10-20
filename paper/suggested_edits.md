# Suggested edits: checklist

Status of the critical review of `main_revised.tex` (paper 1) and `paper2/draft.tex` (paper 2), on branch `shorten-and-reframe`. The original review, with its full reasoning and line references to the old version, is in git history (commit `ad1617b`).

Paper 1 is now about 4,800 words of text (33 pages, down from about 15,800 words and 76 pages). Paper 2 has a full draft (35 pages).

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
- [x] Paper 1 no longer cites unpublished paper 2 results.

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
- [ ] Paper 2: extend Studies 2–4 to the guess-test learner (new simulations).
- [ ] Paper 2: curiosity citation (Oudeyer).
- [ ] Optional: merge the CHILDES frequency figure into Fig. 3.
- [ ] Optional: a data-driven estimate of the ramp timescale from egocentric video (future work, out of scope).
- [ ] Push the branch and open a PR when ready.
