# EventStudy Deprecation Policy

**Policy: warn, never silently break.**

Deprecated argument names and function names in EventStudy continue to
work and emit exactly one deprecation warning per call, pointing the
user to the replacement. Deprecated surfaces are never removed in a
patch or minor release; removal (the "defunct" stage) is reserved for a
future major version and will be announced in NEWS.md.

**Mechanism:** Every renamed public argument has an explicit old-name
shim that calls `.deprecate_arg()` and then forwards the supplied value
under the new name. The shim fires before any computation, so the old
name is fully-equivalent to the new name for all valid inputs.

**Warning source:** Deprecation warnings always use base
[`.Deprecated()`](https://rdrr.io/r/base/Deprecated.html) so they work
without any additional package dependency. When the optional lifecycle
package is installed (Suggests), the warning is also routed through
[`lifecycle::deprecate_warn()`](https://lifecycle.r-lib.org/reference/deprecate_soft.html)
for richer IDE integration; lifecycle is never a hard `Imports`
dependency.

**NEWS discipline:** Every deprecation and rename is documented in
`NEWS.md` under the release heading in which it was introduced.

## See also

[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md),
[`plot_stocks`](https://sipemu.github.io/eventstudy/reference/plot_stocks.md)
