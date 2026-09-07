# EventStudy — Living Retrospective

Milestone retrospectives, most recent first. Cross-milestone trends at the end.

## Milestone: v0.64.0 — Automated AI Reporting

**Shipped:** 2026-09-07
**Phases:** 4 (17, 18, 19, 19.1) | **Plans:** 9 | **Tasks:** 11

### What Was Built
One-call `es_report()` orchestrator (deep-clone non-mutation → `es_diagnostics()` →
grounded section-by-section narrative → multi-format render) plus additive
`run_event_study(report = TRUE)`. HTML/PDF/Word/Markdown output from a fixed 6-section
template, offline-first (rule-based engine when no provider), with the free-text prose
grounding guard wired into the report path so a fabricated numeric literal is dropped to
offline and never rendered.

### What Worked
- **Tracer-first decomposition** kept each phase shippable: Phase 17 landed the core-value
  grounding/offline gate first, so later renderer/orchestrator work built on a verified base.
- **The milestone audit caught the one real hole.** GROUND-01/02/03 (prose scanner built in
  Phase 17 but never wired into the Phase 18 assembler) passed per-phase unit checks yet was
  a dead seam end-to-end. The 3-source cross-reference + integration checker surfaced it.
- **Small, focused gap-closure phase (19.1)** fixed it in ~15 LOC + one report-path regression
  test rather than reopening Phase 18.

### What Was Inefficient
- **Phase 19 shipped without a canonical VERIFICATION.md** (its verification was the
  human-approved CRAN release gate recorded only in the SUMMARY), which blocked milestone
  close until the report was back-filled. A blocking-human gate should also emit the
  verification artifact.
- **Gap discovered at milestone audit, not at Phase 18 execution.** A "does this new guard
  actually run on the production path?" integration check during the phase would have caught
  the dead seam earlier.

### Patterns Established
- **Grounding guards must be verified on the production call path, not just in isolation** —
  a unit-tested guard with zero production callers is a dead seam.
- **Human release-gate sign-off is a verification** — record it in the canonical
  VERIFICATION.md, not only in the SUMMARY.

### Key Lessons
- End-to-end "is it wired?" checks belong inside the phase that builds the seam, not only at
  milestone audit.
- Additive-parameter pattern (`isTRUE(report)` guard, `narrative = NULL` seam) reliably keeps
  the default path byte-identical — a repeatable way to extend without regressing valid-input behavior.

### Cost Observations
- Model mix: predominantly Opus (orchestration + gap analysis), Sonnet executors, Haiku
  integration checker.
- Notable: the audit → gap-closure → re-verify loop was cheap relative to the correctness it bought.

## Cross-Milestone Trends

| Milestone | Shipped | Phases | Plans | Theme |
|-----------|---------|--------|-------|-------|
| v0.50.0 | 2026-09-02 | 4 | — | Robustness hardening |
| v0.60.0 | 2026-09-04 | 4 | 10 | Grounded AI advisor |
| v0.61.0 | 2026-09-04 | — | — | Advisor vignette |
| v0.62.0 | 2026-09-06 | — | — | Docs site + CI/CD |
| v0.63.0 | 2026-09-06 | 4 | 4 | Docs depth |
| v0.64.0 | 2026-09-07 | 4 | 9 | Automated AI reporting |

**Recurring theme:** the "never silently wrong" core value keeps extending outward — from
statistical results (v0.50.0) to AI advice (v0.60.0) to the rendered report's prose (v0.64.0).
The recurring failure mode is *guards that exist but aren't wired on the live path*; the
mitigation that keeps working is adversarial/integration verification before close.
