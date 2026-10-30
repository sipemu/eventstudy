---
quick_id: 261030-taz
mode: quick
---

# Fix CRAN policy violation: report rendering writes into the installed package library

CRAN (K. Hornik, 2026-10-28; deadline 2026-11-18): Debian checks ERROR because the
user library is remounted read-only during checks. `generate_report()` renders the
installed `skeleton.Rmd` in place, so knitr/rmarkdown write intermediates
(`skeleton.knit.md`, `.log`, ...) next to it -> `file(con, "w"): cannot open the connection`
in test_report_advice.R:67/113 and test_report_narrative.R:146.

## Task 1 -- stage the template in tempdir
- files: R/report.R
- action: copy skeleton.Rmd into a per-call `tempfile()` directory, render from that
  copy (intermediates + knit root there), `unlink()` it via `on.exit()`.
- verify: report tests green; installed skeleton dir unchanged after render.

## Task 2 -- regression test
- files: tests/testthat/test_report_readonly_template.R
- action: assert generate_report() adds no files to the template directory
  (runs on CRAN; html only, needs rmarkdown + pandoc).

## Task 3 -- release metadata
- files: DESCRIPTION, NEWS.md, cran-comments.md, inst/.../skeleton/*.log (untracked, delete)
- action: bump to 0.66.1, NEWS entry, cran-comments update section.
