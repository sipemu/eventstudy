# Construct a Grounded AI Advisor provider

Convenience factory that resolves the backend by 3-tier precedence
(explicit `type` argument -\> `EVENTSTUDY_ADVISOR_PROVIDER` env selector
-\> default `"custom"`) and constructs the matching provider object. All
three backends are wired: the in-process `"custom"` hook and the HTTP
`"openai"` (OpenAI-compatible) and `"anthropic"` (native Anthropic
Messages) providers.

## Usage

``` r
provider(type = NULL, fn = NULL, model = NULL, base_url = NULL, ...)
```

## Arguments

- type:

  Provider type, one of `"custom"`, `"openai"`, `"anthropic"`. When
  `NULL` (the default) it is resolved from the
  `EVENTSTUDY_ADVISOR_PROVIDER` env var, falling back to `"custom"`.

- fn:

  For `type = "custom"`, the user function
  `function(prompt, schema, ...)` returning a list or character.
  Required for the custom branch.

- model:

  Optional character model identifier (resolved from
  `EVENTSTUDY_ADVISOR_MODEL` when `NULL`).

- base_url:

  Optional character base URL for HTTP providers (resolved from
  `EVENTSTUDY_ADVISOR_BASE_URL` when `NULL`).

- ...:

  Additional arguments forwarded to the provider constructor.

## Value

A `ProviderBase` subclass instance.

## Details

The `"custom"` branch needs no network and neither `httr2` nor
`jsonlite`. Keys are never read here; the HTTP providers resolve them at
call time inside their own `complete()`.

## Examples

``` r
# Custom provider runs in-process, no network:
p <- provider("custom", fn = function(prompt, schema) "advice text")
p$complete("Summarise")$text
#> [1] "advice text"

if (FALSE) { # \dontrun{
# HTTP providers make live calls; never run in examples or on CRAN:
p <- provider("openai", model = "gpt-4o")
p$complete("Summarise these diagnostics")

a <- provider("anthropic", model = "claude-opus-4-8")
a$complete("Summarise these diagnostics")
} # }
```
