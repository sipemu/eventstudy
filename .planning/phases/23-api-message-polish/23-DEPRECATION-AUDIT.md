# 23 Deprecation Audit — VERIFIED NO-OP (API-06 / CRAN-06)

**Date:** 2026-09-09
**Plan:** 23-03 (Wave 3)
**Conclusion:** No deprecation lifecycle work is required. API-06/CRAN-06 are
satisfied as a documented **no-op**: no `lifecycle` dependency is added, no
deprecation shim is introduced, and no public function or argument was renamed
or aliased in the v0.62–v0.64 milestones.

## Method

Scan the entire `R/` source tree for any deprecation machinery, lifecycle
imports, or documentation aliases that would indicate a renamed/aliased public
surface requiring a compatibility shim.

## Evidence (fresh grep, this task)

```
$ grep -rnE '\.Deprecated|deprecated|@aliases|lifecycle::' R/
(no matches)
```

```
$ grep -niE 'cli|lifecycle' DESCRIPTION
(no cli/lifecycle in DESCRIPTION)
```

```
$ grep -rn 'eventstudy_warning_deprecated' R/
(none)
```

## Findings

1. **No `.Deprecated()` calls** anywhere in `R/` — no function signals its own
   deprecation at runtime.
2. **No `@aliases` roxygen tags** — no public function is documented under an
   old-name alias, so no rename shim exists or is needed.
3. **No `lifecycle::` usage and no `lifecycle` in DESCRIPTION** — the package
   does not depend on the `lifecycle` framework and this plan does NOT add it.
4. **`eventstudy_warning_deprecated` does not exist in the codebase.** It is a
   design placeholder mentioned only in planning artifacts; no classed
   deprecation warning condition is emitted anywhere. It remains a placeholder
   only — no shim is wired.
5. **No renamed/aliased public function or argument surfaced in v0.62–v0.64.**
   The argument-naming and classed-conditions work in Waves 1–2 (23-01, 23-02)
   standardised argument names and error classes but introduced no public
   rename that would require a back-compat alias.

## Disposition

- **API-06:** Deprecation status is now recorded with grep-verified evidence.
  No lifecycle dependency, no shim. Requirement closed as a documented no-op.
- **CRAN-06:** No `lifecycle` Suggests/Imports added; no new dependency; no new
  `R CMD check` surface introduced by deprecation machinery (because there is
  none). The only CRAN-06 code change in this plan is the unrelated
  `gridExtra::grid.arrange` requireNamespace guard (Task 2).

If a genuine public rename ever surfaces in a future milestone, revisit this
audit and add a proper `lifecycle`-based deprecation path at that time — not
before.
