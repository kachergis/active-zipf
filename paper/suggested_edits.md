# Suggested edits: `main_revised.tex` (with notes on `paper2/draft.tex`)

Written as a critical reviewer would read it, with the main goal of **shortening and simplifying the story**. Line numbers refer to `paper/main_revised.tex` as of commit `fffcf0c`.

**Current size:** about 15,800 words of main text (per texcount), 76 manuscript pages, 7 tables, 9 figures, and 5 appendices. The Discussion alone runs about 3,250 words. Most of this growth came from adding new analyses in response to reviewers, each with its own defensive framing. The paper now reads as a revision log more than as an argument.

**Status (branch `shorten-and-reframe`):** Top priorities 1–6 are done in `main_revised.tex`, `paper2/draft.tex` (new Study 1), and `fig_active_processes.tex`. Paper 1 is now about 4,500 words of text (33 pages, down from 76). Many of the line-level items in §4–§8 were handled as part of the rewrite, but they have not been checked off one by one. Line numbers below refer to the old version.

---

## 0. Top priorities

1. **Change the central claim from "passive learning is implausibly slow" to "Zipf imposes a large, predictable penalty that active selection recovers."** The 2021 editor rejected the paper mainly because "rejecting a hypothesis (that passive learning is sufficient) … requires a large weight of evidence." The abstract (L106) still makes that claim. The relative claim is better supported, does not depend on the target vocabulary size, and is simpler to state (§1.1).
2. **Move context (distractor) selection, the C=100 analyses, and the deployment questions to paper 2.** Paper 1 then covers one lever, which word is targeted, tested three ways: analytically, on real scene data, and with incremental learners. Paper 2 gets the shared mechanism its draft says it lacks (§2).
3. **Cut paper 1 to about 8,000 words.** A section-by-section budget is in §3. The biggest savings come from the Discussion (summaries that repeat the Results), the incremental-results subsections, and a three-part Introduction overview that says the same thing three times.
4. **Make the headline numbers match the paper's own argument.** The checkpoint section (L610, L725) says the 50–90% speedups "are the better guide to what active selection buys a typical learner." The abstract and Discussion still lead with the 99% figures (19x, 28x, 235x) (§1.4).
5. **Use each symbol for one thing only.** `M` currently means context size (L280), number of scenes (L426), and vocabulary size (L521), and Fig. 1 uses `C/M` with yet another meaning (§6).
6. **Remove material duplicated between the two papers.** The familiarity-dilution paragraph (L738) repeats paper 2's Study 3 almost verbatim, numbers included. That is a duplicate-publication risk.

---

## 1. The story

### 1.1 Lead with the relative claim; the Zipf penalty is roughly the harmonic number

Under fast-mapping with a = 1, min p = 1/(W·H_W). The Zipf-to-uniform ratio of T₊ is therefore close to **H_W ≈ ln W + 0.58**:

| W | T₊ Zipf (ε=.01) | T₊ uniform | Ratio | Zipf episodes/hr (5 yrs × 8 h) |
|---|---|---|---|---|
| 1,000 | 8.6×10⁴ | 1.2×10⁴ | 7.5 | 6 |
| 3,000 | 3.3×10⁵ | 3.8×10⁴ | 8.6 | 22 |
| 10,000 | 1.35×10⁶ | 1.4×10⁵ | 9.8 | 93 |
| 60,000 | 1.08×10⁷ | 9.4×10⁵ | 11.6 | (206 over 18 yrs) |

(Computed from Eq. 2–3. These match the paper's own values at 10k and 60k.)

Why this simplifies things:
- **It answers the editor's 2,000–3,000-word objection directly.** The relative penalty grows only logarithmically with W, so it is 7–12x whether the target is 1,000 or 60,000 words. The repeated hedges about the 60,000-word target (L145–147, L729–731) can then shrink to one sentence.
- **It connects two parts of the paper that are currently separate.** L568 already notes that the eliminative active advantage at C=10 grows "roughly in proportion to the harmonic number H_M (7.5, 9.8, 11.6)." Together that is one story: *Zipf costs about a factor of H_W because the learner waits on the rarest word; active targeting removes the wait.* State it once in the fast-mapping section and refer back to it.
- **The Mollica comparison (L362) proves too much.** At about 1.5 effective instances per hour, the paper's own text concedes that even Blythe's uniform case (about 18/hr) is too fast. Absolute plausibility therefore does not separate Zipf from uniform. Keep the Mollica point as a short caveat about what an "episode" is, but do not build the argument on it. Reviewer 3 (#3) said the reasonable/unreasonable judgment is subjective. The relative framing avoids that problem.

### 1.2 Make pre-literacy the primary scope

Every incremental simulation already uses M = 1,000 or 10,000 (L521, L562). Only the fast-mapping headline and the Introduction still lead with 60k over 18 years. Make W ≈ 10,000 by age 5 the main framing. Report the 60k/18-year figure once, in the paragraph that reconciles with Blythe et al. That removes the "we retain 60,000 for comparability, but…" hedge, which appears at least three times (L145–147, L730, L731).

### 1.3 Organize the paper around static vs. adaptive active control

The paper has two different kinds of "active" without naming the difference:
- **Static** (the max-min section): a fixed mixture q̂ over scenes reshapes the *marginal* word distribution Pq.
- **Adaptive** (the incremental section): the choice depends on the learner's current state, i.e., which words are not yet known.

Naming the distinction (i) organizes the paper into two parts, (ii) explains why the speedups differ so much (Reviewer 3, #7, on 235x vs. 2x), and (iii) answers Reviewer 2's #7, which is still unaddressed: if children shape their input, then CHILDES frequencies already include any *static* shaping. Applying a passive sampler to CHILDES is then partly circular, but only for the static kind of control. Adaptive control can speed learning even when the pooled marginal stays Zipfian. One paragraph on this in the Discussion turns a reviewer objection into a point in the paper's favor.

### 1.4 Headline numbers

- The abstract (L108) says the advantage is "growing severalfold between a functional and a fully complete vocabulary," while the Discussion (L711) leads with 19x/29x/28x/33x, all at 99%. By the paper's own argument (L608–610, L725), the 50–90% figures (eliminative 4–9x; guess-test 1.7–3x) are the representative ones. Lead with those, and present 99% as an upper end that grows with H_W.
- The abstract (L108) says "active learners … substantially faster than passive learners **exposed to the same random samples**." Active learners do not receive the same samples. Suggested wording: "…than passive learners facing the same word and referent distributions."
- "Passive learning slows by orders of magnitude" (L109, L692 caption) overstates the forgetting results. At t½ = 1,000 passive learning slows 14.6x at 99%, and at t½ = 200 it slows more than 36x at 99% and 28x at 50%. "More than an order of magnitude" is accurate.

### 1.5 A proposed one-paragraph story (for the Introduction's last paragraph or the abstract)

> When a fast-mapping learner must wait for the rarest word, a Zipfian vocabulary takes about H_W ≈ ln W times as many episodes as a uniform one. That is 7–12x for vocabularies of 1,000–60,000 words, and the penalty is larger still for real corpora and scene statistics. Learners who influence which words they hear can recover much of this cost. Statically, a fixed preference over scenes raises the probability of the rarest word: 6.7–235x on two real datasets, and the preference can be learned online. Adaptively, an incremental learner targeting not-yet-known words gains 2–9x by the time it knows 50–90% of its vocabulary, up to about 19x by 99%. The gain grows under forgetting, because targeting what is unknown also works as rehearsal.

That covers the paper. Anything that does not support one of these sentences belongs in paper 2, the supplement, or nowhere.

---

## 2. Reshuffling between paper 1 and paper 2

### Recommendation: give paper 1 the target lever and paper 2 the other levers

| Material | Now in | Move to | Why |
|---|---|---|---|
| Fast-mapping bound, Zipf exponent, CHILDES, Google Books | P1 | **P1** (compressed) | Core. |
| Static max-min q̂, SUN, Flickr8k marginals, online learner | P1 | **P1** (compressed) | Core: static active control. |
| Eliminative + guess-test, **active target selection, random context, C=10**; M = 1k/10k/60k; exponent sweep | P1 | **P1** | Core: adaptive active control. |
| Completion checkpoints | P1 | **P1** (half the length) | Supports §1.4; also motivates P2's ramp. |
| Forgetting / rehearsal | P1 | **P1** | Directly answers the idealization critique. |
| **Familiar-context selection; C=100; the "C, not C/M" finding; why the familiar-context advantage shrinks with M (L568)** | P1 | **P2** | A separate lever with its own mechanism. It needs the Vogt framing, the mutual-exclusivity appendix, and the dilution caveat. It is the "what else is in view" half of Fig. 2B, which P2's Study 3 already depends on. |
| Mutual exclusivity (App. E): **C=100 rescue, and active selection inert during the cascade** | P1 App. E | **P2** | Only matters once C=100 is in play. Keep the C=10 result (3–4x help for both; Zipf penalty 13x→4.8x) as two sentences in P1's Discussion for Reviewer 1 (independence) and Hendrickson & Perfors. |
| Familiarity-dilution paragraph (L738) | P1 Disc. + P2 Study 3 | **P2 only** | Currently duplicated nearly word for word. |
| Guess-test bimodality at C=100 active/familiar (L639, n = 5) | P1 | **Cut**, or a P2 footnote | Based on five diagnostic runs; distracting. |
| Ranked-frequency learner (L641) | P1 | **Cut** (one-line footnote at most) | A degenerate result, a lesson about a learning criterion rather than a finding. Removing it also fixes the "three mechanisms" claim (L213). |
| **Flickr8k incremental: passive vs. active move-on (23x at 99%, `move_cost`=0)** | P2 Study 1 | **P1** | This is P1's missing real-data test of *adaptive* targeting, and it bridges the 235x/2x gap Reviewer 3 asked about. Keep linger/`move_cost` in P2. |
| Linger vs. move on, `move_cost` crossover | P2 | P2 (shorten, or drop) | The weakest P2 result: at most a 7% difference, and close to arithmetic once `move_cost` is charged per switch. |
| Ramp, partial autonomy, Goldilocks, bounded choice | P2 | P2 | Keep. |

**Benefits for paper 2.** The draft admits its "connection is more thematic than mechanistic" (draft L60). Moving context selection there gives it a mechanistic core: *which levers of active control matter, and when* (what is targeted, what accompanies it, how often, when in development, among which options, at what cost). C=100 and familiar context become its Study 1, Study 3's dilution mechanism follows directly, and the mutual-exclusivity cascade (where active targeting is inert) is a natural "levers that do not compose" result alongside guess-test's bimodality.

**Benefits for paper 1.** It no longer needs the Vogt distractor-composition paragraph (L171–174) beyond one sentence, the C/M discussion, Appendix E, the dilution caveat, half of Table 2, the C=100 block of Table 3, or the right panel of Fig. 7. About 2,500 words go, along with the paper's most confusing material.

**Cost.** Paper 1 loses a dramatic result: passive *and* active-target learners never finish at C=100 under Zipf. Fig. 2B's "What else is in view?" box must be removed or noted as "see companion paper." If you would rather keep context selection in paper 1, use the fallback below.

### Fallback: keep context selection in paper 1, but in one subsection

- Give it one subsection of about 600 words: Table 2 only (M = 1,000; C = 10 and 100), one paragraph on why random context fails at C=100 (the exponential-clock footnote is good; keep it), and one sentence that the pattern holds at 10k and 60k.
- Move L568 (the 400-word explanation of why the familiar-context advantage shrinks with M) and the C-vs-C/M analysis to the supplement.
- Keep App. E at C=10 plus a two-sentence C=100 result.

### Ordering paper 2's studies (whichever option you choose)

- Constant blends (`active_prob` 0.1–0.75) are reported twice, in Table `tab-ramp` and as the partial-autonomy rows of `tab-modes`, with slightly different numbers. Report them once, in the "how much autonomy" study, and have the ramp study refer back to them.
- Suggested order: (1) which lever: target vs. context, C=100 (moved from P1); (2) how much and over what: autonomy, Goldilocks, choice sets, dilution; (3) when: the ramp; (4) at what cost: linger vs. move on (short, or cut).
- The Goldilocks effect (0.56x) almost disappears once dilution is removed (0.93x; draft L231). Report it as a consequence of dilution, not as support for Goldilocks.
- Baselines drift across files: active, C=10, random is 4,695 (P1), 4,659 (P2 ramp), and 4,692 (P2 Study 3); passive is 89,454 vs. 91,963. Use one baseline per paper, or state that they come from independent replicate sets.
- Literature that is still missing (the draft's TODO): marginal value theorem and patch leaving (Charnov 1976); self-directed learning (Gureckis & Markant 2012; `Markant`/`Settles:2012` are already in the bib); the Goldilocks effect and curiosity (Kidd et al. 2012; Oudeyer); motor development and visual experience (Kretch, Franchak & Adolph 2014; Franchak); Tauber et al. (2013), which Reviewer 1 raised, on suboptimal self-selection.
- Paper 1 (L723) cites unpublished paper 2 results ("matches least-known targeting in most settings and improves on it in some"). Until paper 2 is out, replace this with one neutral sentence ("a companion paper examines Goldilocks-style policies").

---

## 3. Proposed structure and word budget for paper 1

| Section | Now (words) | Target | Main changes |
|---|---|---|---|
| Introduction | ~2,330 | ~1,200 | One overview instead of three ("present study" L186–207, five-item contributions list L209–216, Roadmap L258–259). Vogt paragraph cut to two sentences. Keep the vocabulary benchmark table (it answers the editor and Reviewer 1). Contributions: three bullets (bound + H_W penalty; static active control on real data; adaptive active control in incremental learners, including under forgetting). |
| Framework (now "Methods") | ~1,050 | ~400 | Keep the shared ingredients and running example. Drop the Roadmap. Drop the vocabulary-sizes table (`tab-sizes-used`), which exists only because there are seven sizes; after the reshuffle there are four (1k, 10k, 60k, plus the corpora), which fit in one sentence. |
| Passive fast-mapping | ~2,190 | ~1,200 | Blythe reconciliation (L306) to a footnote. Add the H_W result. Merge the CHILDES/Google Books/filtering prose (L394–404) into a small table of T₊ by source. Mollica paragraph to about 120 words. |
| Static active control | ~1,930 | ~1,100 | Cut the reconstruction-provenance sentence (L452). Add 2–3 sentences on what SUN *is* (Reviewer 3 #5 asked for this, and it is still missing). Keep Flickr8k. Online learner to 1 paragraph, Appendix D cut to 3 sentences or removed. |
| Adaptive active control (incremental) | ~4,290 | ~2,300 | Two learners, target selection only (or the fallback), scaling in M, exponent sweep, real-data Flickr check (from P2), checkpoints (half), forgetting (about two-thirds). |
| Discussion | ~3,250 | ~1,300 | See §4.6. |
| **Total** | **~15,000** | **~7,500** | |

---

## 4. Cuts and compressions by section

### 4.1 Introduction (L118–216)
- **L145–147.** The scoping paragraph largely repeats the Discussion's "Third" point (L731). Keep one version: a short one here and a pointer from the Discussion, or the reverse.
- **L152–159.** The heuristics and hypothesis-testing vs. associative paragraphs are general background. Reviewer 3 (#1) said the cross-situational framing misled them about what the paper models. Cut to about 3 sentences that lead to the models the paper actually tests.
- **L164–174.** The Blythe and Vogt paragraphs are good responses to Reviewer 2, but after the reshuffle the long Vogt explanation (L173–174) can be 1–2 sentences. Fix "(i.e., ∼60,000) words" (L166) and the unclear "(11.58 for 60,000 words, so 10.9 million)" (L168).
- **L179–183.** Active learning: add the ML active learning (Settles 2012) and self-directed learning (Gureckis & Markant 2012) references, which are already in the bib but never cited. A reviewer will ask about the ML literature.
- **L186–216.** Three overlapping overviews. Merge into one paragraph plus three contribution bullets.
- **Contribution 3 (L213)** says "three incremental learning mechanisms," but the third, ranked-frequency, is degenerate (§2). **Contribution 5 (L215)** is already a summary of results; shorten it to a clause.
- **Novelty claims.** "We derive a closed-form bound … for an arbitrary word frequency distribution" (L211) and "a first closed-form analytical bound" (L701). The coupon collector problem with unequal probabilities is classic (Flajolet, Gardy & Thimonier 1992; Boneh & Hofri 1997; the cited Blom et al. 1994 also covers it). The bound via min p is new mainly in how it is applied. Soften to "a simple bound, governed by the rarest word, that applies to any distribution," and cite the classical results. "Has not to our knowledge previously been subjected to theoretical analysis" (L705) also invites pushback given the ML and self-directed-learning literatures; soften it.

### 4.2 Framework / "Methods" (L220–267)
- The section is called "Methods" but contains no methods. Rename it "Framework" or merge it into the Introduction.
- Merge the running example (L234) into Fig. 2. The figure already uses the five toys.
- **Fig. 1 vs. Fig. 2.** Two overview figures. Fig. 2 (`fig_active_processes`) is clearer, maps onto the results, and uses consistent notation. Drop Fig. 1, or keep only its passive uniform/Zipf contrast as a small panel in Fig. 2A. Fig. 1 also uses `C/M` with M as the number of referents, which conflicts with the text.

### 4.3 Passive fast-mapping (L269–413)
- **L275.** "W words and O referents": O is never used again. Cut it.
- **L280.** "1 < M < W referents": M here is the context size, which fast-mapping ignores anyway. Drop it, or use C.
- **L306.** Move the 940k reconciliation to a footnote: "Blythe et al.'s 940,000 is the ε = 0.01 quantile; Eq. 2 at ε = 0.01 gives 936,000."
- **L331 and L792.** The "set ε = 1/2" sentence appears twice, in the main text and in Appendix A.
- **L345.** A good plain-language explanation of why min p matters. Keep it, and put the H_W result immediately after it.
- **L349.** Garbled: "as any distribution min p of W words holds min p ≤ min q."
- **L360.** The tightness check (T = 8.97×10⁶ vs. T₊ = 1.08×10⁷) answers Reviewer 2 #2. Keep it.
- **L378 vs. L381.** "Super-exponential" vs. "exponentially." What ∂²log T₊/∂a² ≥ 0 shows is that log T₊ is convex in a. Pick one term and use it consistently.
- **L388–389.** The caption is garbled ("ε = [.99, .01]; dotted lines, ε = 0.5; solid line") and has a duplicate `\label{fig}`.
- **L393.** Leftover comment: "Shohei, maybe we should just refer to your other paper's estimate…"
- **L394–404.** Replace the three paragraphs with one paragraph and a T₊ table: CHILDES (three filters), Google Books, SUN-passive, idealized Zipf, uniform. Each currently comes with its own caveat paragraph.
- **Reviewer 1 (Fig. 3) and Reviewer 1 (p. 9)** noted that function words and content words follow different Zipf slopes, and asked which exponent CHILDES actually has. This is still unaddressed. Report the fitted exponent for content words in one sentence.
- **Morphology** (Reviewer 1 #4) is never mentioned. One sentence will do: it reduces the effective W, and because the penalty grows only like log W, the relative conclusions do not change.

### 4.4 Static active control (L416–497)
- **L420–421.** Typos: "how about meanings," "set of of objects."
- **L423.** "She" for the learner; elsewhere the paper uses "they."
- **L433–447.** The footnote, Jensen argument, and rate R are fine. Move the Jensen inequality to the appendix and keep the verbal statement in the text.
- **L451.** Change `\cite{SUN2010}` to `\citep`, and fix "active leaning."
- **L452.** "This reconstruction reproduces … to four significant figures, giving us confidence that it faithfully reconstructs the original analysis" is internal provenance. Cut it. Add what Reviewer 3 asked for instead: what SUN contains (images, annotation, what a "scene category" is) and how P is built.
- **L458–461.** Interesting but confusing: the uniform-scene marginal is 3,990x harder than an idealized Zipf, and even the optimal learner remains 17x slower than idealized-Zipf passive. One sentence is enough. In the static/adaptive framing, the point is "real scene statistics are more skewed than Zipf, and static control recovers much but not all of the gap."
- **L463.** The Flickr8k explanation (category pooling vs. single photos) is good. Trim it to about 80 words.
- **L478.** Grammar: "which has the elements in each column are…"
- **L488 and Appendix D.** Appendix D gives intuition for a gradient method that "has no guarantee to converge," which is not the method actually simulated (iterated LP on P̂). Cut it, or reduce it to three sentences in the main text.
- **Fig. `fig-online`.** Old .eps with a red/green contrast. Recolor it for color-blind readers to match the grey/orange scheme of the other figures.

### 4.5 Adaptive active control / incremental (L499–694)
- **L502.** The opening paragraph (218 words) spends most of its length restating an objection. Two sentences will do: what is relaxed (instant learning) and what is kept (independence across words).
- **L505.** Drop "(A third … is examined under Robustness…)" along with the ranked-frequency learner.
- **L509.** The guess-test description has a long parenthetical about Blythe et al. 2016. Move that to a footnote.
- **L516.** The familiar-context bullet (about 200 words, three citations) moves to P2, or is shortened in the fallback.
- **L523 footnote.** Remove "An earlier version of this analysis … superseded" and the code paths. Put the code in a Data/Code Availability statement. Validation detail (|z| ≤ 1.03) can go to the supplement.
- **L526.** "Not previously appreciated" is a claim reviewers tend to challenge. Cut it.
- **Tables 2 and 3 overlap.** The M = 1,000 column of `tab-m10k` repeats `tab-incremental`, and `fig-incremental-sim` shows the same data again. Keep one table (Table 3 with the M columns) and either the figure or Table 2, not both.
- **L568.** About 400 words on why the familiar-context advantage shrinks with M, including per-frequency-band exposure counts and a C/M side sweep with 5 replications. This is the densest paragraph in the paper. Move it to P2 or the supplement, and keep only the last sentence's point: under random context the advantage grows with H_M.
- **L602–606, checkpoints.** The mean-vs-median question needs one sentence (the ratio is 0.99–1.09 throughout). Cut the paragraph that explains why the distribution is symmetric.
- **L610.** Keep it, but it is nearly repeated in L725. Keep one copy.
- **L637–641, robustness.** Keep the exponent sweep and the core guess-test comparison. Cut the guess-test bimodality (n = 5) and the ranked-frequency learner (§2).
- **L651.** "How large the local context is *relative to the vocabulary*" contradicts the Scaling section's conclusion (L566) that absolute C, not C/M, decides feasibility. The same contradiction appears at L709, L712 ("keeps learning at C/M = 0.1"), and L713. If context selection stays, rephrase all of these.
- **L656.** "A skeptical reader might reasonably worry…" Make it neutral: "All learners so far retain tentative guesses indefinitely. Here we add forgetting."
- **L658.** The rationale for lock-in is necessary but long. Move the explanation of the instant-lock learner to the footnote; it is partly there already.
- **L662.** Keep the half-life calibration.
- **L664.** Long. The k = 3 robustness check can be one sentence ("requiring three confirmations gives the same pattern, if anything stronger").
- **L666.** The stalled-runs caveat is honest and worth keeping. It is also a nice bridge to P2 (some passive input is needed to correct confident errors). Keep it short.
- **L668.** A rhetorical summary. Cut it; the Discussion covers it.
- **Note for authors (not necessarily for the text).** A reviewer could say the forgetting result follows from the design: if the half-life is shorter than a rare word's passive revisit gap (about 7,485 episodes), passive learning has to stall. Consider saying so plainly, and presenting the simulation as quantifying the size of the effect rather than as a test whose outcome was uncertain ("We predicted…", L660).

### 4.6 Discussion (L696–746): about 3,250 words → about 1,300

The Discussion currently (a) re-summarizes every result with numbers (L699–715, about 650 words), (b) lists five assumptions, each of which re-summarizes a Results section (L726–736), and (c) contains three paragraphs on paper 2 material (L723, L738, L740). Suggested structure:

1. **What we showed** (1 paragraph, no new numbers): the H_W penalty, and the static and adaptive recovery.
2. **Why the estimates span 2x–235x** (1 paragraph): static vs. adaptive control, real P vs. synthetic P, completion target. This answers Reviewer 3 #7 at the conceptual level.
3. **What the CHILDES result does and does not show** (1 paragraph): Reviewer 2 #7 circularity, and why the relative claim is the safe one.
4. **Simplifying assumptions** (1 paragraph, one sentence each): mutual-exclusivity lexicon/synonymy, any-W-of-many (L729 can be cut to about 2 sentences), pre-literacy scope, independence across words. Then 2–3 sentences on the mutual-exclusivity result at C=10 and Hendrickson & Perfors / Wolters, a compressed L734–736.
5. **Do children do this?** (1 paragraph): Goldilocks/Kidd, Ruggeri, Bazhydai (null), Yu & Smith, Suanda. Mention Tauber et al. 2013 (Reviewer 1).
6. **Companion paper** (1 sentence).

Specific cuts:
- **L700.** The Blythe 2016 and Reisenauer framing belongs in the Introduction, or can be cut.
- **L719–721.** Stale `\GK{}` text: "future work can use the same analytic techniques…" The paper now does this. Delete it.
- **L732 (Fourth).** Repeats the forgetting section in full. Cut to one clause.
- **L738.** Duplicate of P2 Study 3. Remove it.
- **L740.** Shorten to one sentence.

---

## 5. Consistency and correctness

- **Appendix A, Eq. A1 (L771).** "The left hand side is the probability that every word is present at least once in the T episodes." For multinomial sampling this is an approximation: it treats "word i seen" as independent across words. Those events are negatively associated, so the product slightly *overstates* P(all seen) and slightly understates T. For large W the error is negligible (it is the Poissonized approximation), but Reviewer 2 (Blythe) is exactly the kind of reader who would notice. Say "approximately (treating words as independent; exact in the Poissonized limit)."
- **C vs. C/M.** See L651, L709, L712, L713 vs. L566 (§4.5).
- **Headline vs. checkpoint argument.** See §1.4.
- **L486, L710.** The online learner's passive baseline "requires twice as many samples." It is then cited as "about 2x in our synthetic matrix." This is the number Reviewer 3 found puzzling. With the static/adaptive framing and a one-line explanation (the columns of the synthetic P are random permutations of similar Zipfs, so q has little leverage), it stops being a puzzle.
- **Mutual-exclusivity numbers (L736 vs. App. E).** These are consistent (13x → 4.8x). If App. E moves to P2, keep only this C=10 statement in P1.
- **Section cross-references by name** ("see the Incremental active learning section") appear about 13 times. Most can be deleted. Where one is needed, number the sections, or use `\ref` with a class option that numbers them.

---

## 6. Notation (one symbol, one meaning)

| Concept | Currently | Suggest |
|---|---|---|
| Vocabulary size (words = referents) | W (fast-map), N (active), M (incremental, tables), O (L275) | **W** throughout |
| Context size (referents in view) | M (L280), C (incremental), C in Fig. 1 | **C** |
| Number of scenes/situations | M (active section, online sim) | **S** (or K) |
| Scene-choice distribution | q, q̂ | keep |
| Completion tail probability | ε (fine), but the Fig. 3 caption is garbled | keep, fix the caption |

The incremental tables then read "W = 1,000; C = 10," and the online simulation reads "W = 1,000, S = 100."

---

## 7. Figures and tables

Now: 7 tables and 9 figures. Suggested: about 4 tables and 6 figures.
- **Keep:** Fig. 2 (`fig_active_processes`, updated after any reshuffle), Fig. 3 (T vs. a, caption fixed), Fig. 5 (`fig_optimalQ`), Fig. 6 (online, recolored), the checkpoint figure **or** table (not both), Fig. 9 (decay).
- **Merge or cut:** Fig. 1 (old overview, notation conflict); Fig. 4 (CHILDES frequency) can be a panel of Fig. 3; `fig-incremental-sim` duplicates Tables 2 and 3; the C=100 panel of `fig-exponent` moves to P2.
- **Tables:** keep the vocabulary benchmarks; add a new T₊-by-source table; one incremental table; the decay table. Cut `tab-sizes-used` and `tab-checkpoint` if the figure is kept (or the reverse). `tab-mutex` moves to P2 or is cut.

---

## 8. Leftovers and copy edits

- Remove the stale comments: L13–14 and L95–96 (word counts from 2018), L102, L98, L195, L218, L281–290, L307, L393, L442, L474, L716, L748–749, and the very large commented-out proof block (L941–1240). Remove the `\GK{}`/`\SH{}` macros and their uses (L153–159, L719–721, L743–746).
- The preamble contains macros for a different paper (`\learninggame`, `\Encode`, `\TransBlock`, …). Remove them.
- Remove the empty `\section*{Acknowledgment}` (L753), or fill it in (the JSPS grant is commented out at L754).
- L1467 of the log: the apa6 class ignores `\bibliographystyle`. This is harmless but can be deleted.
- Keywords: "Linear programming" and "Fast mapping" are less central now. Consider "cross-situational learning; active learning; Zipf's law; coupon collector; vocabulary development."
- Abstract: about 280 words. A version of about 180 words built from §1.5 would match journal norms.
- Phrases to cut throughout: "worth flagging up front," "it is worth being precise," "a skeptical reader," "genuinely," "we think," "if anything," "directly, rather than by argument." Together these make the paper sound defensive.

---

## 9. Reviewer points from 2021 that are still unaddressed or only partly addressed

| Reviewer point | Status in `main_revised` | Suggested fix |
|---|---|---|
| Editor: rejecting passive learning needs very strong evidence | Abstract L106 still says "implausibly large" | §1.1 reframing |
| Editor: if the real target is 2–3k words, "the mystery … disappears" | The 10k pre-literacy figure is given, but 60k is still primary | §1.1 table (7.5x at 1k); §1.2 |
| R1: morphology lowers effective W | Not mentioned | One sentence (§4.3) |
| R1: Zipf slope differs for function vs. content words; why a = 1? | The exponent sweep exists; the CHILDES slope is never reported | Report the fitted exponent (§4.3) |
| R1: adults choose suboptimal study schedules (Tauber et al. 2013) | Not cited | Discussion §4.6 item 5 |
| R2 #7: CHILDES already reflects any active shaping | Not addressed | §1.3 |
| R3 #5: what is SUN? | Still not described | §4.4 |
| R3 #7: 235x vs. 2x | Partly (L710) | Static/adaptive framing, plus the Flickr incremental result from P2 |
| R2 #2: tightness of the bound at other ε and a | Only at a = 1, ε = .01 (L360) | One sentence or a supplementary figure comparing T₊ to the exact root over the range of Fig. 3 |
