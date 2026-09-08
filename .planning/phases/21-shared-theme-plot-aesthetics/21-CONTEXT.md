# Phase 21: Shared Theme & Plot Aesthetics - Context

**Gathered:** 2026-09-08
**Status:** Ready for planning
**Mode:** Smart discuss (autonomous)

<domain>
## Phase Boundary

Establish a single, exported, colorblind-safe visual language — a new
`R/theme.R` exporting `theme_eventstudy()` (a ggplot2 theme) and an
Okabe-Ito–anchored `es_colours` palette — and route it through **every**
EventStudy plot: static ggplot2 helpers (`.plot_single_event`,
`.plot_multi_event`, `plot_diagnostics`) and interactive plotly
(`plot_stocks()`). All scattered hardcoded colours (`steelblue`, `red`,
`grey40`, `blue`, `"grey"`) are replaced by named palette references, and the
plotly visuals are restyled to the same palette for cross-plot consistency.

**In scope (VIZ-01, VIZ-02, VIZ-03, CRAN-01):**
- `R/theme.R`: `es_colours` (Okabe-Ito qualitative palette, brand blue #2563eb as
  primary anchor) + `theme_eventstudy()`, both `@export`ed and roxygen-documented,
  surfaced in the pkgdown reference index.
- Recolour/re-theme the three ggplot2 helpers with `theme_eventstudy()` + palette
  refs; remove all hardcoded colour literals in `R/plotting.R`.
- Restyle `plot_stocks()` plotly traces/hover/legend to `es_colours` — **colours
  only, structure (subplot, trace types, shapes) untouched.**
- `DESCRIPTION`: add ONLY `tinytable`, `patchwork`, `ragg` to Suggests, each
  `requireNamespace()`-guarded; no new Imports. (Phase 21 lands these Suggests once
  for the milestone; actual `tinytable`/`ragg`/`patchwork` USE lands in Phase 22.)

**Out of scope:** report table/figure aesthetics (Phase 22), API/message polish
(Phase 23), docs/site cross-linking (Phase 24), the pkgdown bslib palette (Phase 20,
locked). No change to plot STRUCTURE (geoms/layers/traces), no change to any
statistical behavior, no change to plot function signatures or return classes.

</domain>

<decisions>
## Implementation Decisions

### es_colours Palette Definition
- **Scheme:** Okabe-Ito colorblind-safe **qualitative** palette (the 8-colour CUD
  set) exposed as a named character vector `es_colours`, with the brand primary
  blue **#2563eb** (locked in Phase 20) set as the leading/primary series colour so
  the R plots read as part of the eventstudy.de ecosystem. Okabe-Ito's own blue
  (#0072B2) is retained in the qualitative set for multi-series contrast; the brand
  blue is the deliberate first/primary swatch.
- **Named roles for common plot elements** (so helpers reference roles, not raw
  hex): `primary` (brand blue #2563eb — main line/point series, replaces
  `steelblue`), `event` (a vermillion/orange from Okabe-Ito for the t=0 event line,
  replaces `red`), `reference`/`grid` (a neutral grey for zero-lines & gridlines,
  replaces `grey40`), `ci_band` (primary at low alpha for confidence ribbons,
  replaces `steelblue`+alpha), plus a qualitative vector for group/multi-series
  colouring (CAAR-by-group). CI band alpha kept ≈0.2 as today.
- **Discrete count:** full 8-colour Okabe-Ito qualitative set available for
  categorical/group series; helpers use `scale_colour_manual`/`scale_fill_manual`
  drawing from `es_colours` (recycling gracefully beyond 8 groups).
- **NA / negative handling:** no special negative-value colour scheme this phase
  (event-study AR/CAR plots are single-series lines, not diverging heatmaps);
  palette is qualitative only. NA/missing series fall through to a neutral grey.

### theme_eventstudy() Scope
- **Return type:** a standard ggplot2 theme object (built on `theme_minimal()` /
  `theme_bw()` base) so it composes with `+` like any ggplot theme; parameterised
  by `base_size` (default 11) and `base_family` (default "" = base R device font).
- **Typography:** default to the base R device font (`base_family = ""`) for CRAN /
  offline safety — do NOT hard-require the brand Inter/JetBrains Mono webfonts (they
  are a pkgdown/site concern, not available on arbitrary R devices). `base_family`
  is a parameter so a user CAN pass "Inter" if installed. This keeps rendering
  deterministic across CI and CRAN check machines.
- **Panel/grid styling:** minimal, publication-grade — light/subtle major
  gridlines in the neutral `es_colours` grey, no or muted minor gridlines, clean
  panel background (white/transparent), title left- or centre-aligned consistent
  with the current `hjust = 0.5` centre-title convention already in the helpers.
- **Legend:** bottom legend placement for multi-series/group plots (matches the
  plotly `orientation = "h"` horizontal legend already used in `plot_stocks`),
  keeping single-series plots legend-free.

### ggplot2 ↔ plotly Parity
- **Mechanism:** `theme_eventstudy()` is ggplot-only; parity for plotly is achieved
  by having the plotly path pull the **same `es_colours`** values (trace line
  colours, the event-date `vline` colour, hover/legend styling) so both render
  families share one palette. No attempt to convert the ggplot theme into a plotly
  layout wholesale.
- **Optional helper:** provide a small internal styling helper (e.g.
  `.style_plotly(p)` / a layout-defaults list) that applies `es_colours`-derived
  colours + consistent font/legend to a plotly object, so `plot_stocks()` and any
  future plotly plot share one styling code path. Internal (`@noRd`), not exported —
  only `theme_eventstudy()` and `es_colours` are exported (VIZ-01).
- **plot_stocks structural intactness:** the `subplot`, `add_trace`, `type/mode`,
  and `shapes`/`vline` structure of `plot_stocks()` stays exactly as-is; only the
  colour/font/legend styling values change (the `vline` default `color = "grey"`
  and per-trace colours now come from `es_colours`).

### Backwards-Compat / Opt-In Stance
- **Applied by default:** the ggplot helpers apply `theme_eventstudy()` +
  `es_colours` by default (no new opt-in flag) — a plot produced by
  `plot_event_study()` / `plot_diagnostics()` looks branded out of the box. This is
  an **aesthetics-only** change: geoms, layers, aes mappings, facets, labels, titles
  and return classes are unchanged, so the existing plot-STRUCTURE tests
  (`expect_s3_class(p, "gg")`, `p$labels$title` assertions) stay green. The suite
  asserts no pixel colours today, so no colour-assertion test can regress.
- **No signature changes:** no new parameters added to `plot_event_study`,
  `plot_diagnostics`, or `plot_stocks`; behavior on valid inputs is identical apart
  from colour/theme. (A user wanting the old look can still add their own theme on
  top since a ggplot object is returned.)
- **New tests:** add light structural tests that `theme_eventstudy()` returns a
  `theme`/`gg` object and `es_colours` is a non-empty named character vector of
  valid hex codes — without asserting exact per-geom colours (to avoid brittleness
  and keep future palette tweaks cheap).

### Claude's Discretion
- Exact assignment of the 8 Okabe-Ito hex values to named roles beyond the locked
  `primary = #2563eb`; whether the neutral grey is one of Okabe-Ito's greys or a
  brand-derived slate (#0f172a-family at reduced weight); precise `base_size`,
  gridline alpha, and legend key sizing; the exact name/signature of the internal
  plotly styling helper. All at Claude's discretion within the constraints above.

</decisions>

<code_context>
## Existing Code Insights

### Reusable Assets
- `R/plotting.R` is the single home for all plot code: `plot_stocks()` (plotly,
  L17), `plot_event_study()` dispatcher (L107), `.plot_single_event()` (L126,
  ggplot), `.plot_multi_event()` (L190, ggplot), `plot_diagnostics()` (L282,
  ggplot). No `R/theme.R` exists yet — this phase creates it.
- Locked brand token from Phase 20: primary **#2563eb**, bg #ffffff, fg #0f172a,
  Inter / Plus Jakarta Sans / JetBrains Mono typography, eventstudy.de ecosystem
  framing. Phase 21's palette must anchor on #2563eb for cross-tool consistency.
- `ggplot2` and `plotly` are already hard deps (DESCRIPTION Imports) — no new
  Imports needed; the palette/theme use existing deps.

### Established Patterns
- Hardcoded colour literals to replace, all in `R/plotting.R`:
  `steelblue` (line/point/fill/CI — L178-181, 257-263, 314, 321, 328, 345-346),
  `red` (event vline + qq/resid lines — L182, 264, 315, 322),
  `grey40` (zero hline — L181, 264, 343), `blue` (acf CI band — L344),
  `"grey"` (plotly `vline` default — L34). These map onto the named `es_colours`
  roles (`primary`/`event`/`reference`/`ci_band`).
- Existing centre-title convention: `theme(plot.title = element_text(hjust = 0.5))`
  appears in both single- and multi-event helpers — fold into `theme_eventstudy()`.
- Internal helpers use leading-dot snake_case + `@noRd` (project convention) — the
  plotly styling helper follows this.
- Suggests are `requireNamespace()`-guarded across the package (rugarch, openxlsx,
  did, etc.) — the same guard pattern applies to the new `tinytable`/`patchwork`/`ragg`
  additions.

### Integration Points
- New `R/theme.R` (exports `theme_eventstudy`, `es_colours`) → roxygen `@export` →
  NAMESPACE regenerated → pkgdown reference index (add to `_pkgdown.yml` reference
  group so they appear).
- `R/plotting.R` ggplot helpers `+ theme_eventstudy()` and `scale_*_manual`/colour
  refs; `plot_stocks()` plotly path pulls `es_colours` + optional `.style_plotly`.
- `DESCRIPTION` Suggests gains `tinytable`, `patchwork`, `ragg`.
- `tests/testthat/test_plotting.R` asserts only structure (`expect_s3_class(p, "gg")`,
  `p$labels$title`, `expect_no_error`, error messages) — aesthetics-only change keeps
  it green; add non-brittle theme/palette existence tests.

</code_context>

<specifics>
## Specific Ideas

- Anchor `es_colours` primary on the Phase-20-locked brand blue **#2563eb** so the
  static R plots visually match the eventstudy.de site and the WASM app.
- Use the canonical **Okabe-Ito** colorblind-safe set for the qualitative/group
  palette (the de-facto standard for accessible categorical colour in scientific
  plots) rather than inventing a bespoke scheme.
- Keep `theme_eventstudy()` a real, composable ggplot2 theme object (usable by
  package users on their own plots), not a private side-effecting styler.
- One palette, two render families: the plotly path deliberately reuses the SAME
  `es_colours` values as ggplot for cross-plot consistency.

</specifics>

<deferred>
## Deferred Ideas

- Actual `tinytable` table styling, `ragg` per-format figure sizing, and
  `patchwork` figure composition — Suggests are ADDED here but USED in Phase 22
  (Report Aesthetics).
- A diverging/sequential colour scale for future heatmap-style plots — not needed
  for the current line/point event-study visuals.
- Exposing `base_family = "Inter"` as a package default (requires the webfont to be
  present on the render device) — left as a user-supplied parameter, not a default.

</deferred>
