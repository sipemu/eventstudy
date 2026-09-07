---
resolves_phase: 12
source: 11-REVIEW.md
priority: low
audit_acknowledged:
  milestone: v0.64.0
  at: 2026-09-07
---

# Phase 12 carryforward from Phase 11 code review

Two advisory findings from the Phase 11 pkgdown code review (0 critical). Neither
affects the current build (pkgdown 2.2.0), but both are cleanly resolved as part of
Phase 12's DESCRIPTION / release-integrity work:

- **WR-01** — `_pkgdown.yml` sets `font-size-base` and `headings-font-weight` under
  `template.bslib`. pkgdown < 2.0.7 silently drops these (no error, just wrong
  size/weight). Fix: pin `pkgdown (>= 2.0.7)` in DESCRIPTION `Suggests:` (LINK/BUILD
  scope), or move the two vars into `pkgdown/extra.scss` as SCSS `!default` overrides.
- **IN-01** — `pkgdown/extra.scss` has a dead `.r-function` selector (downlit emits
  `.fu`, already present/correct). Remove the dead selector during Phase 12 polish.
