## Update 0.70 (addressing CRAN check ERRORs, request of 2026-10-28)

This update fixes the test ERRORs on the Debian check flavors
(r-devel-linux-x86_64-debian-clang/gcc, r-patched-linux, r-release-linux).
`generate_report()` rendered the bundled R Markdown template in place, so
knitr wrote its intermediate files into the installed package library, which is
now read-only during checks. The template is now copied to a temporary
directory (`tempfile()`, removed on exit) and rendered from there; no package
code writes outside the R session's temporary directory.

Verified by installing the package into a read-only library and running the
test suite without `NOT_CRAN`: 0.66.0 reproduces the three failures reported
on CRAN; 0.70 passes with 0 failures. A new test
(`test_report_readonly_template.R`, not skipped on CRAN) asserts that the
installed template directory is unchanged after rendering.

## R CMD check results

Local -- Linux (Manjaro), R 4.6.1, `R CMD check --as-cran` on the built
source tarball:

```
Status: OK
```

## Examples policy

Examples that are slow or data-dependent but otherwise safe (no network, no
credentials, `tempdir()`-only writes) use `\donttest{}`. Only the two
LLM-provider examples (`AnthropicProvider`, `OpenAICompatProvider`), which
require a network connection and API credentials, use `\dontrun{}`.
