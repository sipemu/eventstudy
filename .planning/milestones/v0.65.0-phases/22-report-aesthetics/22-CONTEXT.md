# Phase 22: Report Aesthetics - Context

**Gathered:** 2026-09-08
**Status:** Ready for planning
**Mode:** Smart discuss (autonomous / YOLO — all recommended answers accepted)

<domain>
## Phase Boundary

Make `es_report()` output publication-grade across **all four formats**
(HTML / PDF / Word / Markdown) without changing any statistical result, any
prose text, or the render control flow. This phase touches only the
**presentation layer** of the existing report machinery
(`inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd`,
`R/report.R`, and a new `inst/rmarkdown/report.css`):

**In scope (VIZ-04, VIZ-05, VIZ-06, VIZ-07, CRAN-05):**
- **Styled tables** — route every `knitr::kable()` call in `skeleton.Rmd`
  through a single internal table-rendering helper that emits `tinytable`
  output when the package is available and degrades to `knitr::kable()`
  otherwise (`requireNamespace("tinytable")`-guarded), styled consistently
  across HTML/PDF/Word/Markdown.
- **Figure captions** — a `fig.cap` on every plot chunk in `skeleton.Rmd`.
- **Per-format figure sizing** — replace the single global `fig.width = 10`
  / `fig.height = 6` with per-format sizing so PDF/Word figures fit page
  margins, using `ragg` for anti-aliased raster output where a raster device
  is used (all `requireNamespace("ragg")`-guarded).
- **HTML typography/table CSS** — a new `inst/rmarkdown/report.css` injected
  **only** on the HTML branch (via `html_document(css = ...)` in
  `.build_output_format("html")`), brand-aligned to the Phase-20 palette
  (#2563eb primary, #0f172a fg, #ffffff bg, Inter/JetBrains Mono where
  webfont-safe).

**Out of scope / untouchable (LOCKED invariants):**
- The `knitr::is_html_output()` static/interactive plot switch (skeleton.Rmd
  L229, L253, L342) — must NOT move (v0.64.0 invariant).
- `.sanitise_prose()` (report.R) and `.validate_grounding()` — left UNTOUCHED.
- `JOINT_HYPOTHESIS_CAVEAT` wording and the narrative/grounding path.
- No new Imports; `tinytable`/`patchwork`/`ragg` stay Suggests (added Phase 21),
  every use `requireNamespace()`-guarded with a working fallback.
- PDF output must contain NO `<script>` tags; all four formats must still
  render; behavior on valid inputs unchanged beyond aesthetics.

</domain>

<decisions>
## Implementation Decisions

### Table Styling & Fallback (VIZ-04, CRAN-05)
- **Single rendering helper:** introduce one internal `@noRd` helper (e.g.
  `.report_table(df, caption, ...)`) that ALL table sites in `skeleton.Rmd`
  call, replacing the ~9 scattered `knitr::kable()` calls. The helper is the
  only place that branches on `requireNamespace("tinytable")`. Keeps the
  fallback logic in one guarded location rather than duplicated per chunk.
- **tinytable when present, kable fallback when absent:** when
  `requireNamespace("tinytable", quietly = TRUE)` is TRUE, render via
  `tinytable::tt()` styled and format-dispatched (tinytable auto-detects the
  knitr output format, covering all four); otherwise fall back to the exact
  current `knitr::kable(...)` call (same `col.names`, `caption`, `digits`) so
  behavior is byte-compatible when tinytable is not installed.
- **Consistent styling across formats:** the helper applies a small, uniform
  tinytable style — brand-aligned header emphasis (bold header row, subtle
  #2563eb accent on the header rule where the format supports it), right-
  aligned numeric columns, left-aligned text columns, and consistent
  numeric formatting (preserve existing `digits = 4` convention;
  keep the current `sprintf` pre-formatting for the hand-built value tables).
  Styling is expressed through tinytable's format-agnostic API so HTML/PDF
  (LaTeX)/Word/GFM each get the closest supported rendering, not HTML-only
  CSS. (Brand CSS colour is layered on top for HTML only via report.css.)
- **Caption parity:** captions currently passed to `kable(caption=)` are
  passed through the helper to `tinytable`'s caption arg so every styled
  table keeps its existing caption in all four formats.

### Figure Captions (VIZ-05)
- **Source of caption:** captions live on the chunk as a `fig.cap` chunk
  option (the knitr-idiomatic path that numbers/labels figures per output
  format), not baked into the plot title. Each of the ~3 plot chunks (AR/CAR
  plot, panel event-study plot, residual-sigma distribution) gains a concise,
  descriptive `fig.cap`.
- **Wording convention:** short declarative captions describing WHAT the
  figure shows (e.g. "Abnormal and cumulative abnormal returns around the
  event date.", "Distribution of estimation-window residual sigma across
  events.", "Event-time coefficients with confidence bands."). Sentence case,
  terminal period, no figure-number prose (knitr/pandoc supplies numbering).
- **Numbering:** rely on the output format's native figure numbering
  (pandoc/LaTeX auto-number where supported); do NOT hand-number in the
  caption text. Captions are plain descriptive strings — grounding-neutral,
  so they never trip `.validate_grounding()` (they contain no numeric
  results).
- **Interaction with the is_html_output switch:** `fig.cap` is a chunk option
  and is orthogonal to the interactive/static switch — it is set on the chunk
  header and does NOT touch the `knitr::is_html_output()` body logic. For HTML
  interactive plotly output where chunk `fig.cap` is not rendered by plotly,
  the caption degrades gracefully (title/label already present); the static
  (PDF/Word/MD) path — where captions matter most for print — always shows it.

### Per-Format Figure Sizing & ragg (VIZ-05, CRAN-05)
- **Replace the global size:** remove the single `fig.width = 10,
  fig.height = 6` from the setup chunk's `opts_chunk$set()` and set width/
  height/dpi **per format**, computed from `knitr::opts_knit$get("rmarkdown.pandoc.to")`
  (or the equivalent format detection already available in the template),
  applied once in the setup chunk so it flows to every plot chunk.
- **Recommended per-format sizes** (fit page margins; final values at
  planner/Claude discretion within these bounds):
  HTML — wider, screen-oriented (≈ 10 × 6 in, dpi ≈ 96, keeps today's look);
  PDF — fit LaTeX text width (≈ 6.5 × 4 in, dpi ≈ 300);
  Word — fit default page width (≈ 6 × 3.7 in, dpi ≈ 300);
  Markdown/GFM — moderate raster (≈ 8 × 5 in, dpi ≈ 150).
- **ragg device selection:** when `requireNamespace("ragg", quietly = TRUE)`
  is TRUE, set the raster device for print/raster output to `ragg::agg_png`
  (via `dev = "ragg_png"` chunk option / knitr's registered ragg device) for
  anti-aliased raster figures; when ragg is absent, fall through to knitr's
  default device (no error, slightly less crisp raster). The vector PDF path
  (LaTeX) is unaffected — ragg governs raster (PNG) output for HTML/Word/MD.
- **No structural plot change:** sizing/dpi/device are chunk-level rendering
  options only; the plot objects, `theme_eventstudy()`, `es_colours`, geoms,
  and the `is_html_output()` switch are untouched. (Also fold the one
  leftover `fill = "steelblue"` in the sigma-histogram chunk onto
  `es_colours` for palette consistency — aesthetics only.)

### HTML-Only report.css (VIZ-06, VIZ-07)
- **Location & injection:** new `inst/rmarkdown/report.css` shipped in the
  package; injected ONLY on the HTML branch by adding `css = <path>` to the
  `rmarkdown::html_document(...)` call in `.build_output_format("html")`
  (report.R L453). The PDF/Word/MD branches never reference it, so the PDF
  stays script/style-free and contains no `<script>` tags. `is_html_output()`
  in the template is untouched — CSS injection happens in the R format
  builder, not in the template body.
- **Scope of the CSS:** typography (Inter for body / JetBrains Mono for code,
  with robust web-safe fallbacks so it renders even offline), brand-aligned
  heading colour (#0f172a fg, #2563eb accent on links/section rules), and
  table styling that matches the tinytable look (header row emphasis, subtle
  row separation, right-aligned numerics) so HTML tables read as branded. It
  layers ON TOP of the existing `theme = "flatly"` bslib theme rather than
  replacing it.
- **Brand-palette alignment:** the CSS uses the Phase-20 LOCKED tokens —
  primary #2563eb, bg #ffffff, fg #0f172a — and the Phase-21 `es_colours`
  anchor, so the HTML report, the pkgdown site, and the R plots share one
  visual language across the eventstudy.de ecosystem.
- **Offline/CRAN safety:** fonts are declared with system/web-safe fallback
  stacks (no mandatory external webfont fetch that would break offline
  render); the CSS is a static asset (no `<script>`), keeping `R CMD check`
  clean and adding no new findings.

### Regression Test (Success Criterion 4)
- Add a regression test that renders (or asserts on the rendered artifacts of)
  the four formats and locks: (a) the PDF output contains NO `<script>` tags;
  (b) the `knitr::is_html_output()` switch still selects interactive (plotly)
  for HTML and static (ggplot) for non-HTML; (c) tables render via the helper
  with the kable fallback when tinytable is force-absent. Guarded/skipped
  gracefully when a toolchain (LaTeX/pandoc/tinytable/ragg) is unavailable on
  the CI/CRAN machine (`skip_if_not_installed`), consistent with existing
  report tests.

### Claude's Discretion
- Exact helper name/signature (`.report_table`), the precise tinytable style
  calls, the exact per-format inch/dpi numbers within the recommended bounds,
  the precise CSS rule set and font fallback stack, and the exact regression-
  test assertions/skip guards — all at Claude's discretion within the locked
  constraints above.

</decisions>

<code_context>
## Existing Code Insights

### Reusable Assets
- `es_colours` + `theme_eventstudy()` exist and are exported (Phase 21,
  `R/theme.R`) — the figures already carry the brand palette; the sigma
  histogram's leftover `fill = "steelblue"` (skeleton.Rmd L338) is the one
  hardcoded colour left to fold onto `es_colours`.
- `tinytable`, `patchwork`, `ragg` were added to `DESCRIPTION` Suggests in
  Phase 21 specifically for THIS phase to consume — all uses must be
  `requireNamespace()`-guarded (matching the rugarch/openxlsx/did guard
  pattern used across the package).
- The four-format switch is centralised in `.build_output_format(fmt)`
  (`R/report.R` L448) → `html_document`/`pdf_document`/`word_document`/
  `md_document(variant="gfm")`; `ext_map` at L222. This is the single place
  to inject the HTML-only `css =` argument.
- Per-format render loop is `R/report.R` L326-383; per-format `fig_path`
  isolation (`params$fig_path`) already exists (CR-02) and shows the
  established pattern for pushing per-format options through template params.

### Established Patterns
- Tables: ~9 `knitr::kable()` call sites in `skeleton.Rmd`
  (L130, L158, L218, L247, L277, L305, L328, L379) all pass `caption` and
  most pass `digits`/`col.names` — these become calls to one guarded helper.
- Plot chunks: 3 plot-producing chunks (results AR/CAR L226-240, panel
  L250-264, diagnostics sigma L332-354) each already carry the LOCKED
  `knitr::is_html_output()` interactive/static branch — captions and per-
  format sizing wrap these WITHOUT moving the switch.
- Setup chunk (`skeleton.Rmd` L26-61) is where `opts_chunk$set()` global
  options live and where the per-format sizing/device logic must go
  (single point of control, flows to all chunks).
- `.sanitise_prose()` (report.R L336) and the grounding guard run in the
  format loop — presentation changes stay outside this prose path.

### Integration Points
- `inst/rmarkdown/report.css` (new static asset) → referenced by
  `.build_output_format("html")` `css =` arg → shipped in the built package.
- `.report_table()` helper (new `@noRd`, likely in `R/report.R`) → called by
  every table chunk in `skeleton.Rmd` (template calls into the loaded
  `EventStudy` namespace, already `library(EventStudy)` in setup).
- `skeleton.Rmd` setup chunk → per-format `fig.width/height/dpi/dev` from
  format detection → flows to all plot chunks.
- `tests/testthat/` → new regression test for four-format render / no-script
  PDF / is_html_output invariant, guarded by `skip_if_not_installed`.

</code_context>

<specifics>
## Specific Ideas

- One guarded table helper is the single seam for tinytable-vs-kable — do NOT
  scatter `requireNamespace("tinytable")` across nine chunks.
- Inject CSS via the R format builder (`css =` in `html_document`), NOT via a
  template body branch — keeps the `is_html_output()` switch untouched and
  guarantees PDF/Word/MD never see the stylesheet (no `<script>`/`<style>` in
  print output).
- Captions via `fig.cap` chunk options (knitr-idiomatic, per-format
  numbering), not baked into plot titles — and grounding-neutral (no numeric
  results in caption text) so `.validate_grounding()` is never touched.
- Anchor all HTML/table colour on the Phase-20 LOCKED tokens
  (#2563eb / #0f172a / #ffffff) and the Phase-21 `es_colours` so report,
  site, and plots share one brand.

</specifics>

<deferred>
## Deferred Ideas

- `patchwork` figure composition (multi-panel report figures) — the Suggest
  was landed in Phase 21 but multi-panel composition is not required by the
  VIZ-04..07 criteria; leave for a future phase if demand appears.
- Rich native Word styling via `officedown` (RPTX-03, already deferred in the
  v0.64.0 roadmap) — out of scope; Word parity here is via the tinytable
  fallback path only.
- Bundling/embedding the Inter / JetBrains Mono webfonts into the HTML report
  for guaranteed offline typography — this phase uses fallback font stacks;
  font embedding is a heavier docs/site concern (Phase 24 / pkgdown).

</deferred>
</content>
</invoke>
