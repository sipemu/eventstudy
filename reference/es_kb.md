# Access the EventStudy Grounding Knowledge Base

Returns `EVENTSTUDY_KB`, the package-level pure-R list of rule records
that maps diagnostic conditions to grounded methodological
recommendations. Each rule carries a structured academic citation and
can be evaluated against an
[`es_diagnostics`](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md)
object.

## Usage

``` r
es_kb()
```

## Value

A named list of rule records, each with fields: `id`, `category`,
`condition`, `recommendation`, `citation` (list of
`author`/`year`/`key`/`venue`), and `severity`.

## Details

The KB is the correctness-critical layer consumed by
[`recommend_stat`](https://sipemu.github.io/eventstudy/reference/recommend_stat.md)
and
[`flag_robustness`](https://sipemu.github.io/eventstudy/reference/flag_robustness.md),
and exported here so that Phase 7 can inject its contents into an LLM
system prompt without accessing an internal package object.

**KB-04 note:** This function delivers the *structure* only — exported
and serializable, ready for Phase 7 system-prompt injection. The actual
prompt-injection behavior is a Phase 7 deliverable and is deliberately
out of scope here.

## Note

Each rule's `condition` field is an R function closure and is therefore
**not** JSON-serializable. Any consumer that serializes the KB (e.g.
Phase 7 system-prompt injection) must drop the `condition` field from
each rule record before encoding to JSON.

## See also

[`recommend_stat`](https://sipemu.github.io/eventstudy/reference/recommend_stat.md),
[`flag_robustness`](https://sipemu.github.io/eventstudy/reference/flag_robustness.md),
[`es_diagnostics`](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md)

## Examples

``` r
kb <- es_kb()
length(kb)             # number of rules
#> [1] 8
kb[[1]]$id             # id of first rule
#> [1] "KB-NORM-PATELL"
kb[[1]]$citation       # citation list
#> $author
#> [1] "Patell, J.M."
#> 
#> $year
#> [1] 1976
#> 
#> $key
#> [1] "Patell1976"
#> 
#> $venue
#> [1] "Journal of Accounting Research, 14(2), 246-276"
#> 
```
