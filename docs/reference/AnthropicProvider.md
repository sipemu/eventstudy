# AnthropicProvider

A provider that issues `POST {base_url}/v1/messages` against the native
Anthropic Messages API. The request carries the mandatory `max_tokens`
field and the `anthropic-version` header; when a `schema` is supplied it
adds a tool-use `input_schema` block (`tools` + `tool_choice`) to obtain
structured output, and the response extractor prefers that tool call's
structured `input` while falling back to the first text block — so the
provider never crashes when the model replies with plain text instead of
a tool call.

The API key is resolved at CALL time from `ANTHROPIC_API_KEY` (never at
construction, never stored on the object) and attached with
`req_headers_redacted("x-api-key" = key)`, which redacts the key in
every print/error path. Note `req_auth_bearer_token` does NOT redact an
`x-api-key` header — the redacted-headers helper is mandatory here. A
missing key, transport/timeout failure, non-2xx status, malformed body,
or missing completion text each degrades to exactly one
[`warning()`](https://rdrr.io/r/base/warning.html) plus an
\`es_provider_response\` with `text = NA_character_` — it never crashes
the session and never returns a fabricated completion.

`httr2` is required only for this provider and is guarded by
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) at the top
of `complete()`, so the package installs and `R CMD check`s cleanly with
`httr2` absent.

## Super class

[`ProviderBase`](https://sipemu.github.io/eventstudy/reference/ProviderBase.md)
-\> `AnthropicProvider`

## Public fields

- `max_tokens`:

  Integer max completion tokens (Anthropic REQUIRES this).

## Methods

### Public methods

- [`AnthropicProvider$new()`](#method-AnthropicProvider-initialize)

- [`AnthropicProvider$complete()`](#method-AnthropicProvider-complete)

- [`AnthropicProvider$clone()`](#method-AnthropicProvider-clone)

------------------------------------------------------------------------

### `AnthropicProvider$new()`

Construct an AnthropicProvider. Stores non-secret config only; the API
key is resolved at call time inside `complete()`.

#### Usage

    AnthropicProvider$new(
      model,
      base_url = "https://api.anthropic.com",
      max_tokens = 1024L,
      ...
    )

#### Arguments

- `model`:

  Character model identifier, e.g. "claude-opus-4-8".

- `base_url`:

  Character base URL. Defaults to the Anthropic cloud endpoint.

- `max_tokens`:

  Integer maximum number of tokens to generate. The Anthropic Messages
  API REQUIRES this field; defaults to 1024.

- `...`:

  Ignored; accepted for forward-compatibility.

------------------------------------------------------------------------

### `AnthropicProvider$complete()`

Complete a prompt via `POST {base_url}/v1/messages`. Resolves the key at
call time; on a missing key or any HTTP/parse failure returns one
warning + `NA`. Structured output is OPTIONAL: when `schema` is supplied
a tool-use `input_schema` block is added and the extractor prefers the
tool call's `input`, falling back to the first text block.

#### Usage

    AnthropicProvider$complete(prompt, schema = NULL, ...)

#### Arguments

- `prompt`:

  Character prompt.

- `schema`:

  Optional structured-output schema (list) or `NULL`.

- `...`:

  Ignored; accepted for forward-compatibility.

#### Returns

An \`es_provider_response\` (see `ProviderBase$complete`).

------------------------------------------------------------------------

### `AnthropicProvider$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AnthropicProvider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Reads ANTHROPIC_API_KEY from the environment at call time:
p <- AnthropicProvider$new(model = "claude-opus-4-8")
p$complete("Summarise these event-study diagnostics")

# Structured output via a tool-use input_schema:
schema <- list(type = "object",
               properties = list(advice = list(type = "string")))
p$complete("Recommend a test statistic", schema = schema)
} # }
```
