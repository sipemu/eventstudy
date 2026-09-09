---
phase: quick
plan: 260909-jls
type: execute
wave: 1
depends_on: []
files_modified:
  - _pkgdown.yml
  - pkgdown/extra.css
  - vignettes/introduction.Rmd
autonomous: false
requirements: [DOCS-SITE-FIX-01, DOCS-SITE-FIX-02]
estimate:
  tokens: 45000
  raw_tokens: 30000
  tasks: 3
  confidence: med
must_haves:
  truths:
    - "Rebuilt docs/articles/introduction.html has NO <img class=\"logo\"> in the page-header above the H1"
    - "Rebuilt docs/articles/introduction.html shows executed code outputs (#> lines / rendered tables)"
    - "The navbar brand remains the text title \"EventStudy\" (pkgdown BS5 has no navbar logo-image slot)"
    - "pkgdown::build_site() (or build_articles + build_home) completes without error"
  artifacts:
    - _pkgdown.yml
    - pkgdown/extra.css
    - vignettes/introduction.Rmd
  key_links:
    - "pkgdown/extra.css .page-header .logo{display:none} rule suppresses the header logo image site-wide"
    - "introduction.Rmd data(dieselgate) replaces tidyquant::tq_get network download so chunks evaluate offline"
---

<objective>
Fix two pkgdown documentation-site complaints, grounded on live inspection of installed pkgdown 2.2.0 (not on the DOCS-SITE-FIXES.md hypothesis, which was partly wrong):

1. Remove the large logo image that pkgdown injects into every article/content page-header (right above the H1). The user dislikes this placement.
2. Make `vignettes/introduction.Rmd` show executed code outputs instead of input-only code.

Purpose: The live gh-pages site shows an ugly body logo and an output-less Getting Started vignette. Fix both correctly and verify locally before the CI redeploy.

Output: Edited `_pkgdown.yml`, `pkgdown/extra.css`, `vignettes/introduction.Rmd`, plus a freshly rebuilt `docs/` verified locally.

IMPORTANT — planner findings from live pkgdown 2.2.0 source inspection (these OVERRIDE the DOCS-SITE-FIXES.md guesses):

- The body logo is NOT caused by `logo` in `navbar.structure.left`. In pkgdown 2.2.0 `navbar_link_components()` does `intersect(structure.left, names(components))`; `logo` is not a component name, so it is silently dropped and does nothing. Likewise the `navbar.logo:` block in `_pkgdown.yml` is never read by any code path — it is inert. Removing both is harmless cleanup but by itself does NOT remove the body logo.
- The body logo comes from `data_template()`: `if (has_logo(pkg)) out$logo <- ...`, and `has_logo()` is TRUE whenever `man/figures/logo.png` exists (via `find_logo()`). The BS5 template `content-article.html` line 8 then renders `{{#logo}}<img ... class="logo">{{/logo}}` into `<div class="page-header">` on every content page. It is decoupled from the navbar entirely.
- pkgdown BS5 has NO navbar logo-image slot. `navbar.html` renders `<a class="navbar-brand">{{title}}</a>` — always the text title. So "use a compact navbar brand logo" is NOT achievable via config; do not chase it. The navbar brand stays the text "EventStudy".
- Therefore the correct, CRAN-safe way to remove the header logo image while keeping `man/figures/logo.png` for README/favicon is a CSS rule in `pkgdown/extra.css` (pkgdown-only, not shipped in the tarball): `.page-header .logo { display: none; }`.
- For issue 2: the `introduction.Rmd` example is built on `tidyquant::tq_get()` (live Yahoo network download) and `DT::datatable()` (DT is NOT installed here). A blind global `eval = TRUE` would FAIL the build. The package bundles a frozen `dieselgate` dataset (`data/dieselgate.rda`) whose `firm`/`index`/`request` tibbles have exactly the schema the vignette hand-builds. The sibling vignette `ai-advisor.Rmd` already demonstrates the offline pattern (eval on by default, `data(dieselgate)`, gate only truly non-runnable chunks). Mirror that pattern.
</objective>

<execution_context>
@~/.claude/gsd-core/workflows/execute-plan.md
@~/.claude/gsd-core/templates/summary.md
</execution_context>

<context>
@.planning/DOCS-SITE-FIXES.md
@_pkgdown.yml
@vignettes/introduction.Rmd
@vignettes/ai-advisor.Rmd
@pkgdown/extra.css

Constraints (from CLAUDE.md): docs/site-only change. No R/ source behavior change, DESCRIPTION unchanged, man/ untouched. The vignette edit is knitr chunk-option + data-source changes only. No new CRAN check findings.
</context>

<tasks>

<task type="tracer">
  <name>Task 1: Suppress the page-header logo image and remove the inert navbar logo config</name>
  <files>pkgdown/extra.css, _pkgdown.yml</files>
  <precondition>Installed pkgdown is 2.2.0 (confirmed at planning time). If a different major/minor is installed at execution time, re-verify that the body logo is still driven by has_logo()/`content-article.html` `{{#logo}}` before relying on the CSS approach.</precondition>
  <action>Fix the body-logo placement the correct way for pkgdown 2.2.0.

(A) In pkgdown/extra.css, append a new rule at the end of the file that hides the auto-injected header logo image on all content pages. Target the image pkgdown places inside the page-header: the selector must scope to `.page-header .logo` so it hides only the header image and does not touch the README logo, favicon, or Open Graph card. Add a short comment above the rule explaining WHY (pkgdown auto-injects man/figures/logo.png into every content page-header via has_logo(); the user wants it gone but the file must stay for README/favicon). Keep the file ASCII-only.

(B) In _pkgdown.yml, remove the two inert entries that mislead future readers (both are no-ops in pkgdown 2.2.0, confirmed by source inspection — this is cleanup, not the fix): (1) delete the top-level `logo:` block under `navbar:` (the `image:`/`href:`/`alt:` keys, currently lines ~37-40), and (2) remove the leading `logo,` token from `navbar.structure.left` so it reads `left: [get-started, reference, articles, methods, gallery, examples]`. Do NOT alter any other navbar keys, components, the `title:` field, or the `opengraph` block. The navbar brand intentionally remains the text title "EventStudy".</action>
  <verify>
    <automated>grep -Eq '\.page-header[[:space:]]+\.logo' pkgdown/extra.css && ! grep -Eq '^\s*left:\s*\[\s*logo,' _pkgdown.yml && ! grep -Eq '^\s{2}logo:\s*$' _pkgdown.yml && echo PASS</automated>
  </verify>
  <done>pkgdown/extra.css contains a `.page-header .logo { display: none; }` rule (with explanatory comment); _pkgdown.yml no longer has the `navbar.logo` block nor `logo` in `structure.left`; no other yaml keys changed.</done>
</task>

<task type="auto">
  <name>Task 2: Make introduction.Rmd evaluate against bundled dieselgate data (outputs visible, offline-safe)</name>
  <files>vignettes/introduction.Rmd</files>
  <action>Turn on code outputs for the Getting Started vignette without breaking the offline pkgdown/CRAN build. Mirror the established offline pattern from vignettes/ai-advisor.Rmd.

(1) Setup chunk (lines 12-18): change `eval = FALSE` to `eval = TRUE` in `knitr::opts_chunk$set(...)`, keeping `collapse = TRUE` and `comment = "#>"`. This makes evaluation the default; individually gate only the chunks that genuinely cannot run offline.

(2) Replace the network-download initialization chunk (the `{r, warning=FALSE, message=FALSE}` chunk at lines ~59-91 that calls `library(tidyquant)`, `tidyquant::tq_get(...)`, and builds `request_tbl`/`firm_tbl`/`index_tbl`) with an offline chunk that loads the bundled dataset instead. The bundled `dieselgate` list (`data/dieselgate.rda`) already provides `dieselgate$firm` and `dieselgate$index` (both with columns `symbol`, `date`, `adjusted`) and a 9-column `dieselgate$request` (`event_id, firm_symbol, index_symbol, event_date, group, event_window_start, event_window_end, shift_estimation_window, estimation_window_length`) — exactly the objects the rest of the vignette consumes. Rewrite the chunk to: `library(EventStudy)`, `data(dieselgate)`, then bind `firm_tbl <- dieselgate$firm`, `index_tbl <- dieselgate$index`, `request_tbl <- dieselgate$request`. Drop the `tidyquant`/`readr`/`DT` library-load lines from this chunk. Keep a one-line prose/code comment noting the frozen bundled dataset replaces the live download so the vignette builds offline. Do NOT keep any `tq_get` call.

(3) The Quick Start chunk (lines ~30-45) references `firm_tbl`, `index_tbl`, `request_tbl` BEFORE the initialization chunk defines them (chunk order: quick-start is above the init chunk). With `eval = TRUE` this will error (objects not yet defined). Fix the ordering dependency by adding `eval = FALSE` to ONLY the `quick-start` chunk (it is a teaser/preview shown before the data is loaded), OR move the data-loading so quick-start can run — prefer the minimal fix: set the `quick-start` chunk to `eval = FALSE` and leave a brief note in prose that it is a preview of the full pipeline unpacked below. (Choosing eval=FALSE for quick-start avoids reordering the narrative and keeps the diff small; documented here per Claude's discretion.)

(4) Replace `DT::datatable(...)` calls (chunks at lines ~99-101, ~144-153) — DT is not a declared dependency and is not installed in the build environment, so these will error under eval=TRUE. Replace each `DT::datatable(x)` with a plain print of a small slice suitable for a static HTML article: use `head(x)` (or `x` where the object is already small, e.g. a single request/AR/CAR tibble). Do NOT add DT to any chunk's library loads. Ensure no remaining chunk calls `DT::` or `tidyquant::`.

(5) Leave all remaining pipeline chunks (define param set, EventStudyTask$new, prepare_event_study, fit_model, calculate_statistics, `$aar_caar_tbl`, `$CSectT`, etc.) at the default `eval = TRUE`; they operate purely on the now-loaded `firm_tbl`/`index_tbl`/`request_tbl` and are fully offline-runnable.

Do NOT change the vignette's YAML front-matter, title, or narrative structure beyond the minimal prose note required by steps (2)-(3).</action>
  <verify>
    <automated>grep -Eq 'eval = TRUE' vignettes/introduction.Rmd && ! grep -q 'tq_get' vignettes/introduction.Rmd && ! grep -q 'DT::datatable' vignettes/introduction.Rmd && grep -q 'data(dieselgate)' vignettes/introduction.Rmd && echo PASS</automated>
  </verify>
  <done>Setup chunk sets `eval = TRUE`; the init chunk loads `data(dieselgate)` and binds `firm_tbl`/`index_tbl`/`request_tbl` from it; no `tq_get` or `DT::datatable` calls remain; the quick-start teaser chunk is `eval = FALSE`; all pipeline chunks run against the bundled data.</done>
  <reversibility rating="reversible">Vignette source edit; fully revertable via git.</reversibility>
</task>

<task type="checkpoint:human-verify" gate="blocking-human">
  <name>Task 3: Rebuild the site and human-verify logo placement + code outputs</name>
  <files>docs/ (regenerated)</files>
  <action>Rebuild the pkgdown site and confirm both fixes on the rendered HTML. The full build may be slow (32 articles); the introduction vignette now runs offline against bundled data, so no network is required.

Build command (run from the package root):
`Rscript -e 'pkgdown::build_site(preview = FALSE)'`

If a full build is too slow or fails on an unrelated article, fall back to the targeted rebuild of the two affected surfaces:
`Rscript -e 'pkgdown::build_articles(); pkgdown::build_home()'`

Then run these grounded checks on the rebuilt output:

  # (a) No header logo image injected above the H1 in the introduction article:
  grep -c 'class="logo"' docs/articles/introduction.html    # expect 0 in page-header context
  # (b) Executed code outputs present (collapsed #> comment lines rendered):
  grep -c '#&gt;' docs/articles/introduction.html           # expect > 0
  # (c) extra.css rule made it into the built site assets:
  grep -R 'page-header .logo' docs/ | head

Note: because the logo suppression is via extra.css (`.page-header .logo{display:none}`), the `<img class="logo">` MAY still be present in the HTML source but hidden by CSS — that is acceptable and expected. The human-verify step must open `docs/articles/introduction.html` in a browser and visually confirm: (1) NO large logo image renders above the H1 on the article, and (2) code chunks show executed outputs (`#>` results / printed tibbles). Also spot-check one other content page (e.g. `docs/reference/index.html` or another article) to confirm the header logo is gone site-wide and the navbar still shows the text brand "EventStudy".

Present to the operator: the build outcome (success / which fallback used), the three grep counts, and confirmation of the visual check. Do NOT mark done until the operator confirms the visual result.</action>
  <verify>
    <human-check>Operator opens rebuilt docs/articles/introduction.html: (1) no logo image above the H1, (2) code outputs visible; and confirms navbar brand is text "EventStudy" with logo gone on a second content page.</human-check>
  </verify>
  <done>Operator confirms: header logo image no longer visible on article/content pages, introduction.html shows executed code outputs, navbar brand renders as text "EventStudy", build completed without error.</done>
</task>

</tasks>

<threat_model>
## Trust Boundaries

| Boundary | Description |
|----------|-------------|
| build-time network | introduction.Rmd previously called tidyquant::tq_get (live Yahoo download) at build time — a network dependency inside a CRAN/pkgdown build |

## STRIDE Threat Register

| Threat ID | Category | Component | Severity | Disposition | Mitigation Plan |
|-----------|----------|-----------|----------|-------------|-----------------|
| T-quick-01 | Denial of Service | introduction.Rmd build (network tq_get) | medium | mitigate | Replace live download with bundled frozen `dieselgate` dataset so the vignette builds offline and deterministically (Task 2) |
| T-quick-02 | Tampering | _pkgdown.yml edits | low | mitigate | Scope edits to navbar.logo block + structure.left token only; leave all other keys untouched; grep gate asserts no stray changes (Task 1 verify) |
| T-quick-03 | Information Disclosure | pkgdown/extra.css scope | low | accept | CSS selector scoped to `.page-header .logo`; hides only the header image, not README/favicon/OG assets — reviewed at planning time |

No package installs are introduced by this plan (no npm/pip/cargo/R Suggests additions); package-legitimacy gate not applicable.
</threat_model>

<verification>
- Task 1 grep gate: extra.css has `.page-header .logo` rule; _pkgdown.yml has neither `navbar.logo` block nor `logo` in structure.left.
- Task 2 grep gate: `eval = TRUE` default set; no `tq_get`; no `DT::datatable`; `data(dieselgate)` present.
- Task 3 human-verify: rebuilt introduction.html visually shows no header logo and shows code outputs; navbar brand is text "EventStudy".
- CRAN safety: DESCRIPTION unchanged, no R/ or man/ edits, no new Suggests/Imports; pkgdown/extra.css is not shipped in the tarball.
</verification>

<success_criteria>
- The auto-injected page-header logo image no longer renders on article/content pages (hidden via extra.css), while man/figures/logo.png stays available for README/favicon.
- The navbar brand renders as the text title "EventStudy" (unchanged; pkgdown BS5 has no navbar logo-image slot).
- vignettes/introduction.Rmd renders with executed code outputs, built fully offline from the bundled dieselgate dataset.
- pkgdown site rebuilds without error; operator visually confirms both fixes.
- No CRAN check findings introduced (docs/site + vignette-source only).
</success_criteria>

<output>
Create `.planning/quick/260909-jls-fix-pkgdown-site-remove-logo-from-navbar/260909-jls-SUMMARY.md` when done.
</output>
