---
quick_id: 261030-taz
status: complete
commit: becbf74
---

# Summary

- Root cause: `generate_report()` rendered `system.file(.../skeleton.Rmd)` in place; knitr wrote
  intermediates into the installed library (read-only on CRAN Debian checks).
- Fix: template copied to a per-call `tempfile()` dir and rendered there (`R/report.R`).
- Regression test: `tests/testthat/test_report_readonly_template.R` (not skipped on CRAN).
- Verified: installed into chmod a-w library, tests without NOT_CRAN -> 0.66.0 reproduces the
  3 CRAN failures; fixed build 0 failures / 2597 pass. `R CMD check --as-cran`: Status OK.
- Version 0.66.1, NEWS + cran-comments updated; stray skeleton/*.log removed;
  `tests/testthat/_problems` build-ignored.
