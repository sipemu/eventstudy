# Design Brief — Redesign EventStudy pkgdown site to match fdars-r

**Quick task:** 260904-x88 · **Status:** locked decisions, ready to plan/execute
**Read this in full before planning or editing.** The current site (Phase 11) was
REJECTED by the user as "far far away" from the intended look. This brief is the
authoritative target. Do not guess the design — replicate fdars-r as specified.

## Reference (the target — fetch these live)

- Repo: https://github.com/sipemu/fdars-r  ·  Live site: https://sipemu.github.io/fdars-r/
- Fetch the two authoritative source files directly (you have `gh`):
  ```bash
  gh api repos/sipemu/fdars-r/contents/pkgdown/extra.css --jq .content | base64 -d   # the gallery stylesheet — PORT THIS
  gh api repos/sipemu/fdars-r/contents/_pkgdown.yml       --jq .content | base64 -d   # the config structure to mirror
  ```

## Locked decisions (do NOT revisit)

1. **Redesign first** (this task); Phase 12 CI/CD + release comes AFTER and is out of scope here.
2. **Full fidelity** — match fdars-r including **bespoke per-vignette SVG card artwork**.
3. Match fdars-r's *approach*, adapted to EventStudy's 18 vignettes and existing 74 exports.

## What makes fdars-r look right (replicate all of these)

1. **Plain Bootstrap 5, NO bslib theming.** `_pkgdown.yml` template is just `bootstrap: 5`.
   Remove the Phase-11 bslib block (custom primary `#1C4E80`, Inter/JetBrains `google` fonts).
   Delete `pkgdown/extra.scss`; all visual identity moves to a hand-written `pkgdown/extra.css`.
2. **Sphinx-gallery card grid** — port fdars-r's `pkgdown/extra.css` verbatim, renaming the
   class prefix `fdars-` → `es-` (e.g. `.fdars-gallery` → `.es-gallery`, `.fdars-tag-learn`
   → `.es-tag-*`, `.fdars-section-heading` → `.es-section-heading`). Keep the 3-col responsive
   grid (2-col ≤991px, 1-col ≤575px), bordered rounded cards, hover box-shadow, thumbnail
   `object-fit:contain; height:100px`, title + method-tag badges, and the wider home content col.
3. **Color-coded thematic sections** (heading underline + matching tag badge colors). Reuse
   fdars-r's palette but map to EventStudy categories (see mapping below).
4. **Curated gallery hub page(s)** built from the card grid — the primary way users browse
   vignettes, replacing the Phase-11 navbar dropdown. `home: {sidebar: false}` + wide home col.
5. **Minimal navbar**: left = `[get-started, reference, articles]` (articles → the gallery hub),
   right = `[search, github]`. Drop the multi-group Articles dropdown.
6. **Reference** stays grouped (the Phase-11 10-group reference already covers all 74 exports
   once + internal section — KEEP that grouping; only restyle, don't regroup). Read the current
   `_pkgdown.yml` reference block and preserve its coverage.

## EventStudy vignette → category → color mapping (18 vignettes, 7 groups)

Mirror fdars-r's color-coded sections. Suggested palette (reuse fdars-r hex where sensible):

| Category (section heading) | Color | Vignettes (card slug = vignette name) |
|---|---|---|
| Core Workflow | `#0d6efd` blue | introduction, result-extraction, diagnostics-validation, inference-robustness |
| Return Models | `#198754` green | factor-models-bhar, time-varying-models, custom-models, volume-volatility-event-study |
| Test Statistics | `#fd7e14` orange | custom-test-statistics |
| Advanced Designs | `#6f42c1` purple | panel-event-study, modern-did-estimators, intraday-event-study, synthetic-control |
| Cross-Sectional & Simulation | `#dc3545` red | cross-sectional-analysis, simulation-power-analysis |
| AI Advisor | `#0dcaf0` teal | ai-advisor |
| Data & Reporting | `#6c757d` gray | data-download, automated-reports |

(Planner may refine titles/ordering, but every one of the 18 vignettes must appear exactly once
in the gallery, color-coded by its category. `introduction` doubles as the "Get Started" navbar item.)

## Bespoke SVG cards (the big deliverable — full fidelity)

Create **one card SVG per vignette** at `man/figures/card-<vignette-slug>.svg` (18 files). Match
fdars-r's card style: schematic/diagrammatic (NOT photorealistic), consistent template, each
depicting the vignette's concept with a simple motif. Constraints:
- `viewBox="0 0 360 200"`, no external fonts (use generic `sans-serif`), no raster images, self-contained.
- A category-colored accent (top bar or motif stroke) using that category's hex from the table.
- A small conceptual glyph per topic (e.g. event-window timeline for `introduction`/`result-extraction`;
  factor axes/regression line for `factor-models-bhar`; staggered cohorts for `panel-event-study`;
  candlestick/intraday ticks for `intraday-event-study`; donor-pool curves for `synthetic-control`;
  chat/advisor bubble for `ai-advisor`; bar/CI whiskers for `custom-test-statistics`; etc.).
- Keep them clean and legible at 100px height. Consistency across the 18 matters more than per-card artistry.
- Reference fdars-r's `man/figures/card-*.svg` for the visual register (fetch a couple as examples:
  `gh api repos/sipemu/fdars-r/contents/man/figures/card-introduction.svg --jq .content | base64 -d`).

Wire each card into the gallery hub as `.es-gallery-item` → thumbnail `<img>` = `card-<slug>.svg`,
title = vignette title, tags = category label.

## Scope guard — IN scope vs OUT of scope

IN (this task): `_pkgdown.yml` (rewrite), `pkgdown/extra.css` (new, ported), delete
`pkgdown/extra.scss`, the gallery hub page(s), `man/figures/card-*.svg` (18 new). A warning-clean
local `pkgdown::build_site(preview = FALSE)`.

OUT (Phase 12 — do NOT touch): CI workflows, `DESCRIPTION` `URL`/version, README badges,
`.Rbuildignore`, `NEWS.md`, network-vignette gating, `R CMD check`. Prefer a gallery mechanism
that needs NONE of these (e.g. a pkgdown-built article under `vignettes/articles/` OR the home
page — but do not add `.Rbuildignore` entries here; if a chosen mechanism would require Phase-12
files, pick a different mechanism). Keep `url:` as the already-set GitHub Pages URL.

## Definition of done

- `pkgdown::build_site(preview = FALSE)` exits 0 with zero missing-topic/orphaned-reference warnings
  (verify: `Rscript -e 'pkgdown::build_site(preview = FALSE)' 2>&1 | grep -viE '^#' | grep -iE 'missing|orphan|warning'; test $? -eq 1`).
- Site is plain BS5 + `es-` gallery CSS; navbar minimal; gallery hub shows all 18 vignettes as
  color-coded cards with bespoke `card-<slug>.svg` thumbnails.
- No Phase-12 files touched. `docs/` build output need not be committed.
