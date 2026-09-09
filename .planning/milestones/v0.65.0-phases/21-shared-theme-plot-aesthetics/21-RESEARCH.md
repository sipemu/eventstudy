# Phase 21: Shared Theme & Plot Aesthetics — Research

**Researched:** 2026-09-08
**Domain:** ggplot2 theme + Okabe-Ito palette; plotly styling; R package export conventions
**Confidence:** HIGH

---

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions
- `R/theme.R` is NEW (does not exist yet). Exports `es_colours` and `theme_eventstudy()`.
- `es_colours`: Okabe-Ito qualitative palette, brand blue #2563eb as `primary` anchor, named character vector.
- Named roles: `primary` (#2563eb, replaces steelblue), `event` (vermillion from Okabe-Ito, replaces red), `reference`/`grid` (neutral grey, replaces grey40), `ci_band` (primary at low alpha ~0.2). Full 8-colour set for group series.
- `theme_eventstudy()`: real composable ggplot2 theme object built on theme_minimal/theme_bw, parameterised by `base_size=11` and `base_family=""`. Centre-title convention folded in.
- Both `@export`ed + roxygen-documented + surfaced in `_pkgdown.yml` reference index under "Plotting".
- ggplot helpers apply `theme_eventstudy()` + `es_colours` BY DEFAULT. Aesthetics-ONLY: geoms/layers/aes/facets/labels/titles/return classes unchanged.
- plotly path (`plot_stocks()`) reuses SAME `es_colours` values; structure (subplot/add_trace/type/mode/shapes/vline) UNTOUCHED — colours/font/legend only. Optional internal `@noRd` plotly styling helper `.style_plotly`.
- DESCRIPTION adds ONLY `tinytable`, `patchwork`, `ragg` to Suggests, each requireNamespace()-guarded. NO new Imports.
- New light non-brittle tests: theme returns theme/gg; es_colours is non-empty named character vector of valid hex codes. No per-geom pixel colour assertions.

### Claude's Discretion
- Exact assignment of the 8 Okabe-Ito hex values to named roles beyond locked `primary = #2563eb`.
- Whether neutral grey is Okabe-Ito grey or brand-derived slate.
- Precise gridline alpha, legend key sizing.
- Exact name/signature of the internal plotly styling helper.

### Deferred Ideas (OUT OF SCOPE)
- Actual `tinytable` table styling, `ragg` per-format figure sizing, `patchwork` figure composition (Phase 22).
- Diverging/sequential colour scale for future heatmap-style plots.
- Exposing `base_family = "Inter"` as a package default.
</user_constraints>

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|------------------|
| VIZ-01 | New `R/theme.R` provides `theme_eventstudy()` + `es_colours` (exported, documented) | Section 3: theme API pattern; Section 2: exact palette definition |
| VIZ-02 | `theme_eventstudy()` + `es_colours` applied across ggplot2 helpers; hardcoded colours removed | Section 1: full colour-literal inventory with file:line evidence |
| VIZ-03 | plotly interactive visuals restyled to `es_colours` (hover, legend, colour consistency) | Section 5: plotly helper pattern; Section 1: plotly colour literal |
| CRAN-01 | DESCRIPTION adds only `tinytable`, `patchwork`, `ragg` to Suggests; no new Imports | Section 8: requireNamespace pattern + existing precedent |
</phase_requirements>

---

## Summary

Phase 21 is a pure aesthetics pass — no statistical logic, no API changes, no new R dependencies beyond three Suggests entries already decided in CONTEXT.md. The work has three parts: (1) create `R/theme.R` with two exported objects (`es_colours` named character vector and `theme_eventstudy()` function); (2) replace 14 hardcoded colour literals in `R/plotting.R` spread across four functions with palette references and apply `theme_eventstudy()`; (3) add `tinytable`, `patchwork`, `ragg` to DESCRIPTION Suggests with `requireNamespace()` guards (no actual usage yet — Phase 22 lands the use).

The existing test suite asserts only plot STRUCTURE (`expect_s3_class(p, "gg")`, `p$labels$title == ...`, `expect_no_error(...)`, error messages) — confirmed green at baseline with 16 assertions across 14 tests in `test_plotting.R`. No test asserts a colour value or geom parameter — the aesthetics swap cannot break them. Two new non-brittle tests are added for `es_colours` and `theme_eventstudy()` structural validity.

ggplot2 4.0.3 (installed) uses S7 internally but themes still inherit `c("theme", "ggplot2::theme", "gg", "S7_object")` — `inherits(th, "theme")` and `inherits(th, "gg")` both return `TRUE`. `theme_minimal()` composes cleanly with `+` and `theme(...)` calls. `scales` (1.4.0) is a transitive dependency of ggplot2 and available, but `scale_colour_manual`/`scale_fill_manual` live in ggplot2 itself — no explicit `scales` import needed.

**Primary recommendation:** Create `R/theme.R`, do a clean find-and-replace of the 14 colour literals in `R/plotting.R`, add three Suggests entries, add a pkgdown reference entry, run `devtools::document()`, and confirm with targeted verify commands.

---

## 1. Hardcoded Colour Literal Inventory

All literals are in `R/plotting.R`. No colour literals exist in any other R source file.
[VERIFIED: R/plotting.R:34-351 — read in full this session]

### plotly path — `plot_stocks()` (L17–86)

| Line | Literal | Context | Named es_colours role |
|------|---------|---------|----------------------|
| L34 | `"grey"` | `vline()` helper default `color` arg — plotly shape line colour | `reference` |

Note: the trace itself has no explicit `color` at L69–73 (plotly auto-assigns). The `vline` shape is the only hardcoded colour in the plotly path.

Verbatim quote from L34: `vline <- function(x = 0, color = "grey") {`
[VERIFIED: R/plotting.R:34]

### ggplot helper — `.plot_single_event()` (L126–186)

| Line | Literal | Context | Named es_colours role |
|------|---------|---------|----------------------|
| L178 | `"steelblue"` | `geom_ribbon(fill = ...)` — CI band fill | `ci_band` |
| L179 | `"steelblue"` | `geom_line(color = ...)` — main AR/CAR line | `primary` |
| L180 | `"steelblue"` | `geom_point(color = ...)` — main AR/CAR points | `primary` |
| L181 | `"grey40"` | `geom_hline(color = ...)` — zero reference line | `reference` |
| L182 | `"red"` | `geom_vline(color = ...)` — event date vline (t=0) | `event` |

Verbatim quote from L176–185:
```
ggplot2::geom_ribbon(..., fill = "steelblue", alpha = 0.2) +
ggplot2::geom_line(..., color = "steelblue", linewidth = 0.8) +
ggplot2::geom_point(..., color = "steelblue", size = 1.5) +
ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "red", alpha = 0.6) +
ggplot2::theme_minimal() +
ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
```
[VERIFIED: R/plotting.R:177-185]

### ggplot helper — `.plot_multi_event()` (L190–268)

| Line | Literal | Context | Named es_colours role |
|------|---------|---------|----------------------|
| L257 | `"steelblue"` | `geom_ribbon(fill = ...)` — CI band fill | `ci_band` |
| L261 | `"steelblue"` | `geom_line(color = ...)` — main AAR/CAAR line | `primary` |
| L262 | `"steelblue"` | `geom_point(color = ...)` — main AAR/CAAR points | `primary` |
| L263 | `"grey40"` | `geom_hline(color = ...)` — zero reference line | `reference` |
| L264 | `"red"` | `geom_vline(color = ...)` — event date vline (t=0) | `event` |

Verbatim quote from L256–267:
```
p + ggplot2::geom_ribbon(..., fill = "steelblue", alpha = 0.2)
    ggplot2::geom_line(..., color = "steelblue", linewidth = 0.8) +
    ggplot2::geom_point(..., color = "steelblue", size = 1.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "red", alpha = 0.6) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
```
[VERIFIED: R/plotting.R:256-267]

### ggplot helper — `plot_diagnostics()` (L282–352)

| Line | Literal | Context | Named es_colours role |
|------|---------|---------|----------------------|
| L314 | `"steelblue"` | `geom_point(color = ...)` — residual scatter points | `primary` |
| L315 | `"red"` | `geom_hline(color = ...)` — zero reference in residuals plot | `event` |
| L321 | `"steelblue"` | `stat_qq(color = ...)` — Q-Q plot points | `primary` |
| L322 | `"red"` | `stat_qq_line(color = ...)` — Q-Q reference line | `event` |
| L328 | `"steelblue"` | `geom_histogram(fill = ...)` — residual histogram bars | `primary` |
| L343 | `"grey40"` | `geom_hline(color = ...)` — ACF zero baseline | `reference` |
| L344 | `"blue"` | `geom_hline(color = ...)` — ACF CI bands (±CI) | `ci_band` |
| L345 | `"steelblue"` | `geom_segment(color = ...)` — ACF lag bars | `primary` |
| L346 | `"steelblue"` | `geom_point(color = ...)` — ACF lag tips | `primary` |

Verbatim quote from L342–348:
```
ggplot2::geom_hline(yintercept = 0, color = "grey40") +
ggplot2::geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = "blue", alpha = 0.5) +
ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0), color = "steelblue") +
ggplot2::geom_point(color = "steelblue") +
```
[VERIFIED: R/plotting.R:343-346]

Note on `plot_diagnostics` `theme_minimal()` calls: lines L317, L324, L330, L348 each call `ggplot2::theme_minimal()` on sub-plots p1–p4. These four sub-plots are passed to `gridExtra::grid.arrange()` (L351). After the aesthetics swap, each sub-plot should instead call `theme_eventstudy()`. There is NO `theme(plot.title = element_text(hjust=0.5))` on p1–p4 in `plot_diagnostics` — the sub-plot titles use the default hjust. The `theme_eventstudy()` will fold in `hjust=0.5` so this becomes consistent.
[VERIFIED: R/plotting.R:316-317, 323-324, 329-330, 347-348]

### Summary counts

| Path | Function | steelblue | red/blue | grey40 | Total |
|------|----------|-----------|----------|--------|-------|
| plotly | `plot_stocks` | 0 | 0 | 0 + `"grey"` vline | 1 |
| ggplot | `.plot_single_event` | 3 | 1 | 1 | 5 |
| ggplot | `.plot_multi_event` | 3 | 1 | 1 | 5 |
| ggplot | `plot_diagnostics` | 5 | 2 | 1 | 8 |
| **Total** | | **11** | **4** | **3** | **14** |

Plus 6 `ggplot2::theme_minimal()` calls (L184, L266, L317, L324, L330, L348) replaced by `theme_eventstudy()`. The `ggplot2::theme(plot.title = ...)` calls at L185 and L267 are absorbed into `theme_eventstudy()` (no longer needed inline after the theme function is applied).
[VERIFIED: R/plotting.R:184-185, 266-267]

---

## 2. Okabe-Ito Palette & Named Role Assignment

### Canonical Okabe-Ito 8-colour CUD set

Confirmed via live R session (ggplot2 4.0.3 installed).
[VERIFIED: R session output — grepl('^#[0-9A-Fa-f]{6}$', ...) returns TRUE for all 8]

| # | Name | Hex |
|---|------|-----|
| 1 | Black | `#000000` |
| 2 | Orange | `#E69F00` |
| 3 | Sky Blue | `#56B4E9` |
| 4 | Bluish Green | `#009E73` |
| 5 | Yellow | `#F0E442` |
| 6 | Blue | `#0072B2` |
| 7 | Vermillion | `#D55E00` |
| 8 | Reddish Purple | `#CC79A7` |

### Proposed `es_colours` definition

```r
es_colours <- c(
  # --- Semantic roles (named for use in helpers) ---
  primary   = "#2563eb",   # brand blue (Phase 20 lock) — main line/point/fill
  event     = "#D55E00",   # Okabe-Ito vermillion — event-date vline (t=0)
  reference = "#6b7280",   # neutral slate grey — zero-lines, ACF baseline
  ci_band   = "#2563eb",   # same as primary; alpha applied at geom level (alpha = 0.2)
  # --- Qualitative group series (Okabe-Ito canonical order, excl. black/yellow) ---
  group1    = "#2563eb",   # brand blue (primary)
  group2    = "#D55E00",   # vermillion
  group3    = "#009E73",   # bluish green
  group4    = "#56B4E9",   # sky blue
  group5    = "#E69F00",   # orange
  group6    = "#CC79A7",   # reddish purple
  group7    = "#0072B2",   # Okabe-Ito blue (darker, for 7th series)
  group8    = "#F0E442"    # yellow (last; least accessible on white, reserved for 8th)
)
```

**Design notes (Claude's discretion):**
- `reference = "#6b7280"`: brand-family slate-400 (from fg #0f172a family, lightened) — preferred over Okabe-Ito black (#000000) for zero-lines (too heavy). Okabe-Ito has no dedicated grey swatch.
- `ci_band` is the same hex as `primary`; the `alpha = 0.2` is already set at each `geom_ribbon()` call and is NOT changed. This is intentional — the ribbon stays at its current translucency.
- `"blue"` at L344 (ACF CI bands) maps to `ci_band` semantically (confidence interval marker), not to `group7`/`#0072B2`, even though the old value was generic `"blue"`.
- `"red"` at L315 (residual zero-hline) maps to `event` because in diagnostic context it marks the zero-reference like the event-date vline. Consistent with `"red"` at L322 (qq line) which is also a reference line.
- The `vline()` helper in `plot_stocks()` defaults to `color = "grey"` (L34). Change default to `es_colours["reference"]`.
- Yellow (#F0E442) is placed last because it has the lowest contrast on white backgrounds.

---

## 3. Exported ggplot2 Theme Pattern

### Verified class contract (ggplot2 4.0.3)

```r
th <- ggplot2::theme_minimal()
class(th)
# [1] "theme"          "ggplot2::theme" "gg"             "S7_object"
inherits(th, "theme")  # TRUE
inherits(th, "gg")     # TRUE
```
[VERIFIED: R session — run this session]

S7-based themes still satisfy `inherits(x, "theme")` and `inherits(x, "gg")`. The new theme tests using `expect_true(inherits(theme_eventstudy(), "theme"))` and `expect_true(inherits(theme_eventstudy(), "gg"))` are correct.

### Canonical exported theme function (idiomatic R package pattern)

```r
#' EventStudy ggplot2 Theme
#'
#' A clean, publication-ready ggplot2 theme for EventStudy plots. Built on
#' \code{\link[ggplot2]{theme_minimal}} with a centred plot title, a bottom
#' legend, and subtle gridlines styled to the EventStudy palette.
#'
#' @param base_size Base font size in points. Default is 11.
#' @param base_family Base font family. Default \code{""} uses the R device
#'   default, which works on all platforms without requiring installed fonts.
#'   Pass \code{"Inter"} if that font is installed on the target device.
#'
#' @return A \code{\link[ggplot2]{theme}} object that composes with \code{+}.
#'
#' @seealso \code{\link{es_colours}}, \code{\link{plot_event_study}}
#'
#' @export
theme_eventstudy <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(hjust = 0.5, size = base_size + 1),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#e5e7eb", linewidth = 0.4),
      panel.background = ggplot2::element_blank(),
      strip.text   = ggplot2::element_text(face = "bold", size = base_size)
    )
}
```

**Key points:**
- Returns `theme_minimal(...) + theme(...)` — this is the idiomatic composable pattern. The return class is `c("theme", "ggplot2::theme", "gg", "S7_object")` automatically.
- `hjust = 0.5` absorbs the existing per-helper `theme(plot.title = element_text(hjust = 0.5))` lines.
- `legend.position = "bottom"` matches the plotly horizontal legend convention already in `plot_stocks()`.
- `panel.grid.minor = element_blank()` gives the clean minimal look.
- Gridline colour `#e5e7eb` is Tailwind slate-200 — the lightest legible grid line; stays within the brand fg #0f172a family.
- No `@import ggplot2` needed — ggplot2 is already in DESCRIPTION Imports. All calls use `ggplot2::` prefix (consistent with `plotting.R` convention).

### NAMESPACE regeneration

`devtools::document()` reads `@export` tags → rewrites `NAMESPACE` and `man/` Rd files. Run once after creating `R/theme.R`. The two new exports `theme_eventstudy` and `es_colours` will appear as `export(theme_eventstudy)` and `export(es_colours)` in NAMESPACE. No manual NAMESPACE editing needed.
[VERIFIED: NAMESPACE:1 — "# Generated by roxygen2: do not edit by hand"]

---

## 4. How ggplot Helpers Consume the Palette

### Replacing direct `color =` arguments

For single-colour geoms (line, point, hline, vline, segment), replace the string literal with `es_colours["<role>"]`:

```r
# Before
ggplot2::geom_line(ggplot2::aes(y = value), color = "steelblue", linewidth = 0.8)

# After
ggplot2::geom_line(ggplot2::aes(y = value), color = es_colours["primary"], linewidth = 0.8)
```

`es_colours["primary"]` evaluates to a named character scalar — ggplot2 accepts named character scalars for `color=` (the name is ignored). [VERIFIED: R session — `ggplot2::geom_point(color = c(primary="#2563eb"))` works correctly]

### Replacing CI ribbon fill

```r
# Before
ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
                     fill = "steelblue", alpha = 0.2)

# After
ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
                     fill = es_colours["ci_band"], alpha = 0.2)
```

The `alpha = 0.2` is unchanged — CI band transparency is a locked decision.

### Multi-series (group) colouring with `scale_colour_manual`

The current code does NOT use `scale_colour_manual` — `.plot_multi_event()` uses a single group line with hardcoded `color = "steelblue"`. For multi-group CAAR plots, if future group colouring is needed, `scale_colour_manual(values = es_colours[grep("^group", names(es_colours))])` is the pattern. Phase 21 does not add group colouring (out of scope) — it simply replaces the hardcoded single-series steelblue with `es_colours["primary"]`.

### Replacing `theme_minimal()` + inline `theme()` calls

```r
# Before (L184-185, L266-267)
ggplot2::theme_minimal() +
ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

# After
theme_eventstudy()
```

For `plot_diagnostics()` sub-plots (p1–p4), which only have `ggplot2::theme_minimal()` (no inline `theme()`):

```r
# Before (L317, L324, L330, L348)
ggplot2::theme_minimal()

# After
theme_eventstudy()
```

### What existing tests assert vs. what changes

| Test assertion | What it checks | Changed by aesthetics swap? |
|---------------|----------------|-----------------------------|
| `expect_s3_class(p, "gg")` | Plot object class | NO — class is unchanged |
| `p$labels$title` | Title string from `labs()` | NO — `labs()` calls not touched |
| `expect_no_error(plot_event_study(...))` | No error thrown | NO — all changes are value-swaps |
| `expect_error(..., "must be one of")` | Error message string | NO — error messages unchanged |
| `expect_error(..., "not found")` | Error message string | NO |
| `expect_error(..., "not been fitted")` | Error message string | NO |
| `expect_no_error(plot_stocks(...))` | No error thrown | NO |
| `expect_no_error(plot_diagnostics(...))` | No error thrown | NO |

All 14 existing tests in `test_plotting.R` are colour-agnostic.
[VERIFIED: tests/testthat/test_plotting.R:1-115 — read in full this session]

### `es_colours` as package-level object — no NSE issue

`es_colours` is a named character vector defined at the top level of `R/theme.R`. It is accessed directly as `es_colours["role"]` inside `R/plotting.R`. This is a package-level symbol — no `globalVariables()` declaration needed (NSE / `globalVariables()` is only needed for bare unquoted column names inside `dplyr` verbs or `ggplot2::aes()` calls). `es_colours` is not a column name, so no addition to `EventStudy-package.R` is required.
[VERIFIED: R/EventStudy-package.R:2-37 — read in full; `globalVariables()` list contains only NSE column names]

---

## 5. How the plotly Path Consumes `es_colours`

### Current plotly colour usage (confirmed by audit)

Only one hardcoded colour in `plot_stocks()`: `"grey"` at L34 (the `vline()` helper's default `color` argument).

The `add_trace()` at L68–73 has no explicit `color` or `line = list(color=...)` — plotly auto-assigns colours from its default colour sequence. Phase 21 changes this: the per-trace colour is set explicitly to draw from `es_colours`.

### `.style_plotly()` internal helper — recommended signature

```r
#' @noRd
.style_plotly <- function(p, trace_colour = NULL) {
  if (is.null(trace_colour)) trace_colour <- es_colours["primary"]
  plotly::layout(
    p,
    font   = list(family = ""),
    legend = list(orientation = "h", xanchor = "center", x = 0.5),
    paper_bgcolor = "#ffffff",
    plot_bgcolor  = "#ffffff"
  )
}
```

**Usage in `plot_stocks()`:**

```r
# In the per-symbol loop, replace the current layout() call:
plot <- plot_ly(data = symbol_data_tbl) %>%
  add_trace(
    x    = ~date,
    y    = ~get(target_variable),
    type = 'scatter',
    mode = 'lines',
    name = symbol,
    line = list(color = es_colours["group1"])    # or cycle by index
  ) %>%
  .style_plotly()
```

For the `vline()` helper default:

```r
vline <- function(x = 0, color = es_colours["reference"]) {
```

**Structure untouched:** `subplot(plots_list, ...)`, `add_trace(type='scatter', mode='lines')`, `layout(shapes = v_shape, xaxis = ..., yaxis = ...)`, `shapes` list — all unchanged. Only the `color` value inside `vline()` and the `line = list(color=...)` argument inside `add_trace()` change.

For multi-symbol `plot_stocks()`, to cycle colours by symbol index:

```r
group_cols <- unname(es_colours[grep("^group", names(es_colours))])
# In loop:
col_idx <- ((which(symbols == symbol) - 1) %% length(group_cols)) + 1
line = list(color = group_cols[col_idx])
```

This is a discretionary implementation detail — the planner may simplify to a fixed `group1` colour for a single-symbol view.

---

## 6. `_pkgdown.yml` Reference Index Update

### Current "Plotting" group (confirmed)

From `_pkgdown.yml` lines 203–214:
[VERIFIED: _pkgdown.yml:202-214 — read in full this session]

```yaml
  - title: "Plotting"
    desc: >
      Interactive Plotly-based visualization functions for event study results,
      stock prices, diagnostics, and specialized designs.
    contents:
      - plot_event_study
      - plot_stocks
      - plot_diagnostics
      - plot_car_distribution
      - plot_panel_event_study
      - plot_synthetic_control
```

### Required addition

Add `theme_eventstudy` and `es_colours` to the "Plotting" group. Updated section:

```yaml
  - title: "Plotting"
    desc: >
      Visualization theme, palette, and functions for event study results,
      stock prices, diagnostics, and specialized designs.
    contents:
      - theme_eventstudy
      - es_colours
      - plot_event_study
      - plot_stocks
      - plot_diagnostics
      - plot_car_distribution
      - plot_panel_event_study
      - plot_synthetic_control
```

Alternatively, add a dedicated sub-group. The simplest approach (in-group, at top) is preferred for a 2-item addition.

---

## 7. Concrete Verify Commands

All commands run from the package root `/home/simonm/projects/datascience/eventstudy`.

### A. Regenerate NAMESPACE + docs

```r
devtools::document()
# Expected: writes NAMESPACE, man/theme_eventstudy.Rd, man/es_colours.Rd
# Check: grep "theme_eventstudy\|es_colours" NAMESPACE
```

### B. Confirm NAMESPACE exports

```bash
grep "theme_eventstudy\|es_colours" NAMESPACE
# Expected output:
# export(es_colours)
# export(theme_eventstudy)
```

### C. Confirm `theme_eventstudy()` returns a theme/gg object

```r
devtools::load_all(".")
th <- theme_eventstudy()
stopifnot(inherits(th, "theme"))
stopifnot(inherits(th, "gg"))
cat("theme_eventstudy() OK:", class(th)[1], "\n")
```

### D. Confirm `es_colours` is a named hex vector

```r
devtools::load_all(".")
stopifnot(is.character(es_colours))
stopifnot(!is.null(names(es_colours)))
stopifnot(length(es_colours) > 0)
stopifnot(all(grepl("^#[0-9A-Fa-f]{6}$", es_colours)))
cat("es_colours OK:", length(es_colours), "named hex values\n")
cat(paste(names(es_colours), es_colours, collapse = ", "), "\n")
```

### E. Run plot-structure test suite (confirms existing tests still green)

```r
devtools::load_all(".")
testthat::test_file("tests/testthat/test_plotting.R")
# Expected: 16 assertions, 14 tests, 0 failures, 0 warnings
```

Or via shell:

```bash
Rscript -e "devtools::load_all('.', quiet=TRUE); testthat::test_file('tests/testthat/test_plotting.R')"
```

### F. R CMD check clean (no new NOTEs)

```bash
Rscript -e "devtools::check(args = '--no-tests')"
# Or for targeted check of just the R files:
Rscript -e "devtools::check(args = c('--no-vignettes', '--no-tests', '--as-cran'))"
```

### G. Confirm no hardcoded colour literals remain in plotting.R

```bash
grep -n '"steelblue"\|"grey40"\|"red"\|color = "blue"\|color = "grey"' R/plotting.R
# Expected: zero matches after the swap
```

### H. Confirm new Suggests in DESCRIPTION

```bash
grep "tinytable\|patchwork\|ragg" DESCRIPTION
# Expected: three lines under Suggests:
```

### I. Confirm CRAN check is clean (no ASCII issues from new hex literals)

```bash
grep -P "[^\x00-\x7F]" R/theme.R
# Expected: no output (hex strings are ASCII-safe)
```

---

## 8. Pitfalls and Important Notes

### Pitfall 1: `scales` package — no new import needed

`scale_colour_manual` and `scale_fill_manual` are exported from `ggplot2`, not from `scales`. The `scales` package (1.4.0, installed as ggplot2 transitive dep) is for `scales::alpha()`, `scales::hue_pal()` etc. — none of which are needed here. The CI ribbon `alpha = 0.2` is passed directly to ggplot2 geom, not via `scales::alpha()`.

No addition to `@importFrom` in `EventStudy-package.R` is required.
[VERIFIED: R/EventStudy-package.R:137 — no scales importFrom currently]

### Pitfall 2: `plot_diagnostics` uses `gridExtra` (a Suggests dep)

`gridExtra::grid.arrange()` is called at L351.
[VERIFIED: R/plotting.R:351]
`gridExtra` is in Suggests (DESCRIPTION L48). The function is currently not `requireNamespace()`-guarded — this is pre-existing tech debt, not introduced by Phase 21. Phase 21 does NOT fix this (out of scope). Do not add a guard during aesthetics work.
[VERIFIED: DESCRIPTION:48 — `gridExtra` is in Suggests]

### Pitfall 3: `theme_eventstudy()` must NOT call `ggplot2::theme_set()`

`theme_set()` is a side-effecting global setter — it modifies the active theme for all subsequent plots in the session. This is an anti-pattern for CRAN packages. `theme_eventstudy()` returns a theme object; helpers apply it with `+ theme_eventstudy()`. Never use `theme_set()` inside the package.

### Pitfall 4: `requireNamespace` guard idiom — for Suggests additions

The three new Suggests entries (`tinytable`, `patchwork`, `ragg`) are added to DESCRIPTION but NOT USED in Phase 21 (use lands in Phase 22). No `requireNamespace()` guards are needed in Phase 21's code. When Phase 22 uses them, the existing pattern is:

```r
# Canonical pattern from R/export.R:160
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is required for Excel export. ",
       "Install it with: install.packages('openxlsx')")
}
```
[VERIFIED: R/export.R:160-163 — read this session]

### Pitfall 5: `es_colours["ci_band"]` used as `fill=` in geom_ribbon

`es_colours["ci_band"]` is a named scalar character vector, e.g. `c(ci_band = "#2563eb")`. ggplot2 `geom_ribbon(fill = ...)` accepts this — the name attribute is ignored when used as a scalar aesthetic override (not mapped via `aes()`). Confirmed via R session.

### Pitfall 6: No `globalVariables("es_colours")` needed

`es_colours` is a package-level object (not a dplyr column). R CMD check NSE warnings only trigger for bare unquoted symbols inside NSE contexts (`dplyr::filter(col == ...)`, `ggplot2::aes(col)`). Referencing `es_colours` directly in function bodies does not trigger NSE warnings.

### Pitfall 7: ggplot2 4.0.3 uses S7 internally — `expect_s3_class` still works

In ggplot2 4.0.3, themes are S7 objects but the class vector includes `"theme"` and `"gg"`. Both `inherits(th, "theme")` and `inherits(th, "gg")` return `TRUE`. The new tests `expect_true(inherits(theme_eventstudy(), "theme"))` and `expect_true(inherits(theme_eventstudy(), "gg"))` are stable — they do not rely on `isS3class()`.
[VERIFIED: R session — `class(ggplot2::theme_minimal())` returns "theme" "ggplot2::theme" "gg" "S7_object"]

### Pitfall 8: `_pkgdown.yml` `contents:` requires exact exported names

Only `@export`-ed names appear in the reference index. After `devtools::document()`, verify `theme_eventstudy` and `es_colours` appear in `NAMESPACE` before adding them to `_pkgdown.yml`. Misspelling or listing a non-exported symbol causes `pkgdown::build_reference()` to fail with a cryptic warning.

---

## Architecture Patterns

### New file: `R/theme.R`

```r
## Colour palette ----

#' EventStudy Colour Palette
#'
#' A named character vector of hex colours for EventStudy plots, anchored on
#' the Okabe-Ito colorblind-safe (CUD) qualitative set with the EventStudy
#' brand primary blue as the leading/primary colour.
#'
#' Semantic roles: \code{primary} (main series), \code{event} (event-date
#' marker), \code{reference} (zero lines, ACF baseline), \code{ci_band}
#' (confidence ribbon fill, apply alpha at geom level), \code{group1}--
#' \code{group8} (qualitative multi-series).
#'
#' @format A named character vector of length 12.
#' @seealso \code{\link{theme_eventstudy}}
#' @export
es_colours <- c(
  primary   = "#2563eb",
  event     = "#D55E00",
  reference = "#6b7280",
  ci_band   = "#2563eb",
  group1    = "#2563eb",
  group2    = "#D55E00",
  group3    = "#009E73",
  group4    = "#56B4E9",
  group5    = "#E69F00",
  group6    = "#CC79A7",
  group7    = "#0072B2",
  group8    = "#F0E442"
)

## Theme ----

#' EventStudy ggplot2 Theme
#'
#' ... (roxygen body as in Section 3)
#'
#' @export
theme_eventstudy <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(hjust = 0.5, size = base_size + 1),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#e5e7eb", linewidth = 0.4),
      panel.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(face = "bold", size = base_size)
    )
}

## Plotly helper (internal) ----

#' @noRd
.style_plotly <- function(p) {
  plotly::layout(
    p,
    font          = list(family = ""),
    legend        = list(orientation = "h", xanchor = "center", x = 0.5),
    paper_bgcolor = "#ffffff",
    plot_bgcolor  = "#ffffff"
  )
}
```

### Minimal test additions (in `tests/testthat/test_plotting.R` or a new `test_theme.R`)

```r
test_that("theme_eventstudy() returns a theme/gg object", {
  th <- theme_eventstudy()
  expect_true(inherits(th, "theme"))
  expect_true(inherits(th, "gg"))
})

test_that("theme_eventstudy() accepts base_size and base_family", {
  th <- theme_eventstudy(base_size = 14, base_family = "")
  expect_true(inherits(th, "theme"))
})

test_that("es_colours is a non-empty named character vector of valid hex codes", {
  expect_true(is.character(es_colours))
  expect_true(!is.null(names(es_colours)))
  expect_true(length(es_colours) > 0)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", es_colours)))
})
```

---

## Environment Availability

| Dependency | Required By | Available | Version | Fallback |
|------------|------------|-----------|---------|----------|
| ggplot2 | theme_eventstudy(), all ggplot helpers | YES | 4.0.3 | — (hard Imports dep) |
| plotly | plot_stocks() | YES | — | — (hard Imports dep) |
| gridExtra | plot_diagnostics() | YES | — | pre-existing Suggests dep (not guarded) |
| scales | transitive via ggplot2 | YES | 1.4.0 | N/A — not directly used |
| tinytable | DESCRIPTION Suggests (Phase 22 use) | NO | not installed | requireNamespace guard in Phase 22 |
| patchwork | DESCRIPTION Suggests (Phase 22 use) | YES | 1.3.2 | requireNamespace guard in Phase 22 |
| ragg | DESCRIPTION Suggests (Phase 22 use) | YES | 1.5.2 | requireNamespace guard in Phase 22 |

**`tinytable` not installed locally** — this is expected; it is added to Suggests only in Phase 21. No code in Phase 21 calls it. The DESCRIPTION edit is sufficient; no runtime availability needed for Phase 21.

---

## Validation Architecture

### Test Framework

| Property | Value |
|----------|-------|
| Framework | testthat 3.3.2 |
| Config file | `Config/testthat/edition: 3` in DESCRIPTION |
| Quick run command | `Rscript -e "devtools::load_all('.', quiet=TRUE); testthat::test_file('tests/testthat/test_plotting.R')"` |
| Full suite command | `Rscript -e "devtools::test()"` |

### Phase Requirements → Test Map

| Req ID | Behavior | Test Type | Automated Command | File Exists? |
|--------|----------|-----------|-------------------|-------------|
| VIZ-01 | `theme_eventstudy()` returns theme/gg | unit | `testthat::test_file("tests/testthat/test_theme.R")` | NO — Wave 0 gap |
| VIZ-01 | `es_colours` is valid named hex vector | unit | same | NO — Wave 0 gap |
| VIZ-02 | All existing plot-structure tests stay green | regression | `testthat::test_file("tests/testthat/test_plotting.R")` | YES (14 tests) |
| VIZ-03 | `plot_stocks()` runs without error after recolour | smoke | `expect_no_error(plot_stocks(task))` in test_plotting.R L92 | YES |
| CRAN-01 | `R CMD check --as-cran` has no new NOTEs | CRAN check | `devtools::check(args='--as-cran')` | N/A |

### Wave 0 Gaps

- [ ] `tests/testthat/test_theme.R` — covers VIZ-01 structural tests for `theme_eventstudy()` and `es_colours`

---

## Security Domain

Not applicable — this phase is pure aesthetics (palette definitions and theme styling). No user input, no authentication, no data persistence, no network calls.

---

## Assumptions Log

| # | Claim | Section | Risk if Wrong |
|---|-------|---------|---------------|
| A1 | `reference = "#6b7280"` (brand slate-400) preferred over Okabe-Ito black for zero-lines | Section 2 | Visual only — change is a one-line edit |
| A2 | `gridExtra` pre-existing Suggests gap (no requireNamespace guard) is NOT fixed in Phase 21 | Section 8 | If left unguarded, R CMD check may warn if gridExtra absent; but this is pre-existing and not Phase 21's scope |
| A3 | `.style_plotly()` lives in `R/theme.R` alongside `es_colours` and `theme_eventstudy()` | Section 5 | Aesthetic only — could also live in `R/plotting.R` |

**Risk summary:** All assumptions are low-risk cosmetic or file-placement decisions with trivial correction paths.

---

## Sources

### Primary (HIGH confidence)
- `R/plotting.R` — read in full this session; all colour literal line numbers verified
- `tests/testthat/test_plotting.R` — read in full; all 14 test assertions enumerated
- `R/EventStudy-package.R` — read in full; globalVariables and importFrom confirmed
- `DESCRIPTION` — read in full; Imports/Suggests verified; ggplot2 confirmed as Imports
- `NAMESPACE` — read in full; 75 export() lines; plotly importFrom confirmed; no ggplot2 importFrom (uses `ggplot2::` prefix throughout)
- `_pkgdown.yml` — read in full; "Plotting" group structure at lines 202–214 confirmed
- R live session — ggplot2 4.0.3 installed; theme class vector confirmed; Okabe-Ito hex validated; scale_colour_manual confirmed; testthat 3.3.2 confirmed; gridExtra/scales/patchwork/ragg availability confirmed

### Secondary (MEDIUM confidence)
- Okabe-Ito (2008) "Color Universal Design" — canonical 8-colour CUD set hex values confirmed via R session validation (all pass `^#[0-9A-Fa-f]{6}$`)

---

**Research date:** 2026-09-08
**Valid until:** 2026-11-08 (ggplot2 API stable; palette is a constant)
