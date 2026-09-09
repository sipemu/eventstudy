# Phase 24 Deferred Items

- **Test-artifact hygiene:** The report-template test (`test_report_narrative_asm.R` /
  `es_report` rendering path) writes transient `file*.log` files into
  `inst/rmarkdown/templates/event_study_report/skeleton/`. These are not gitignored,
  so a test run leaves untracked `.log` files in the working tree. Discovered during
  the 24-02 regression run; removed manually before finishing. Out of scope for the
  docs-only 24-02 plan (no R/*.R or config changes permitted). Fix candidate: add
  `inst/rmarkdown/templates/**/skeleton/*.log` (or the rmarkdown intermediates dir)
  to `.gitignore`, or point the template render at a tempdir.
  status: acknowledged
