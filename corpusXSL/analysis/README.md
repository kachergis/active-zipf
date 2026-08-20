# corpusXSL/analysis

## To run a longer, apples-to-apples verification of all three models

```bash
cd corpusXSL/analysis
Rscript run_verification_grid.R          # or: nohup Rscript run_verification_grid.R &
Rscript summarize_verification_grid.R    # once you have enough data (can be run anytime, including mid-run)
```

See the comment header at the top of `run_verification_grid.R` for the config options
(per-cell wall-clock budget, per-replication cap, which models/M/C/exponents to include,
how to size the run, how to resume). `summarize_verification_grid.R` prints a per-cell
mean/median/censoring-rate table and a side-by-side comparison against the point
estimates currently in the paper.

## To explore the three new active-selection extensions (partial autonomy, Goldilocks, bounded choice)

```bash
cd corpusXSL/analysis
Rscript run_active_modes_grid.R      # fixes M=1000, C=10, a=1 (the paper's "headline" case) and sweeps
                                      # active_prob / active_policy / choice_k across all three models
Rscript summarize_active_modes.R     # three figures in ../../paper/: active_prob_sweep.pdf,
                                      # goldilocks_policy.pdf, choice_k_sweep.pdf
```

Edit the `SWEEP_*` constants at the top of `run_active_modes_grid.R` to widen the sweep or
change `C`/`A`/`FAM_CONTEXT` to check whether a finding also holds at C=100 or other exponents.
Same wall-clock-budgeted, resumable design as `run_verification_grid.R`.

## Core library

- **`learners.R`** — the three learning mechanisms (`learn_corpus_eliminative`,
  `learn_corpus_guesstest`, `learn_corpus_rankedfreq`) plus `zipf_probs()` and the
  simulation-running helpers. Everything else sources this file. All three learners
  share one interface: `(C, M, a, uniform, active, fam_context, epsilon, max_episodes,
  max_seconds, active_prob, active_policy, choice_k, goldilocks_target,
  goldilocks_sigma)`, return `c(dec1..dec9, episodes, censored)`.
  - `repeat_sim(...)` — old, fixed-reps API (used by the one-off scripts below).
  - `run_cell_budgeted(...)` — new wall-clock-budgeted API (used by
    `run_verification_grid.R`). Runs replications in parallel batches until a
    per-cell time budget is spent; any replication that individually exceeds
    `rep_max_seconds` is marked `censored=1` and should be excluded from
    mean/median summaries (but its rate is itself informative — see the caution
    about the ranked-frequency model below).
  - `choose_target(...)` — the shared active/passive target-selection rule used
    by all three learners internally, generalized beyond the original binary
    active/passive choice into three composable dimensions (all backward-
    compatible: defaults exactly reproduce the original behavior):
    - `active_prob` (0-1): probability the learner gets to choose actively
      this episode, rather than falling back to passive/random. Models partial
      autonomy — a caregiver who doesn't always follow the child's lead.
    - `active_policy` (`"unknown"` default, or `"goldilocks"`): `"unknown"`
      prefers any not-yet-known word; `"goldilocks"` prefers not-yet-known
      words with an *intermediate* amount of prior exposure (tracked via a new
      per-word `times_targeted` counter), operationalizing the Goldilocks
      effect (Kidd, Piantadosi, & Aslin, 2012) already cited in the paper as
      motivation but never previously implemented as the actual selection rule.
    - `choice_k` (default `Inf`): restricts active choice to a random K-sized
      window of the vocabulary per episode rather than the full M, modeling a
      learner who can only choose among what's actually around them right now.

## One-off scripts already run (results committed, safe to leave alone)

These produced the numbers and figures currently in the paper. They use the older
fixed-reps API and are individually tuned (different rep counts, some hand-picked
cell exclusions) rather than run under one consistent budget — that inconsistency
is exactly what `run_verification_grid.R` replaces.

| Script | Produces | Notes |
|---|---|---|
| `build_sun_matrix.R` | `sun_P_matrix.rds`, `sun_active_results.RData` | Reconstructs the real SUN object-by-scene matrix from `corpusXSL/SUNdb/sundb_fulldata.txt`; deterministic, not a simulation, no need to rerun. |
| `run_full_grid.R` | `full_grid_results.rds` | Eliminative model, C=10 full exponent grid (complete) + C=100 partial (run was cut short — see `old_eliminative_C100.csv` for the C=100 numbers actually used in the paper, from an even older, differently-parametrized run). |
| `run_guesstest_grid.R` | `guesstest_results.rds` | Guess-test, C=10 full grid; C=100 numbers used in the paper came from this script's console log (`run_guesstest_grid.log`), not from saved raw data, because the run was killed on a bimodal cell before its final save. |
| `run_rankedfreq_grid.R` | `rankedfreq_results.rds` | Ranked-frequency, C=10 full grid. See caution below. |
| `fig_estimate.R`, `fig_optimalQ.R`, `fig_freq.R` | `paper/fig_*_R.pdf` | Pure recreations of the original (Python) figures in R — no simulation, safe to rerun anytime, will reproduce identically. |
| `make_figures.R`, `make_final_figures.R` | `paper/*.pdf` | Plotting only, consumes the `.rds` files above. |

## A caution about the ranked-frequency model

Its "learned" criterion counts a **tie** as success, and a word's very first exposure
as an *active* target always produces a tie (everything in the situation increments
together from the same starting count). So under active target selection this model
learns every word in exactly one exposure, deterministically — episodes-to-99% comes
out to exactly `M * 0.99` with **zero variance**, regardless of Zipf exponent. This is
a real, verified property of the original model's criterion (not a bug in this
reimplementation, and not a bug in your run if you see it reproduce again) — see the
paper's "Robustness across Zipf exponent and learning mechanism" section. Its passive
condition is the informative one for this model.

## CDI data

`cdi_vocab_by_age.csv` — expected productive vocabulary by age (months), computed from
`corpusXSL/ENitem_data_prop_producing_{OxfordCDI,WS}.csv`. Only covers ages 12–30
months (the instruments' range); does not speak to the paper's age-3/age-5 estimates.
