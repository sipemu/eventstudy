# OpenAICompatProvider

A provider that issues `POST {base_url}/chat/completions` with an
OpenAI-shaped request body. Because the endpoint is selected purely by
`base_url` + `model`, this single class covers OpenAI itself AND every
OpenAI-compatible backend — Ollama, LM Studio, or any gateway — via a
`base_url` override, with no code change.

The API key is resolved at CALL time from `OPENAI_API_KEY` (never at
construction, never stored on the object) and attached with
`req_auth_bearer_token()`, which redacts the `Authorization` header in
every print/error path. A missing key, transport/timeout failure,
non-2xx status, malformed body, or missing completion text each degrades
to exactly one [`warning()`](https://rdrr.io/r/base/warning.html) plus
an \`es_provider_response\` with `text = NA_character_` — it never
crashes the session and never returns a fabricated completion.

`httr2` is required only for this provider and is guarded by
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) at the top
of `complete()`, so the package installs and `R CMD check`s cleanly with
`httr2` absent.

## Super class

[`ProviderBase`](https://sipemu.github.io/eventstudy/reference/ProviderBase.md)
-\> `OpenAICompatProvider`

## Methods

### Public methods

- [`OpenAICompatProvider$new()`](#method-OpenAICompatProvider-initialize)

- [`OpenAICompatProvider$complete()`](#method-OpenAICompatProvider-complete)

- [`OpenAICompatProvider$clone()`](#method-OpenAICompatProvider-clone)

------------------------------------------------------------------------

### `OpenAICompatProvider$new()`

Construct an OpenAICompatProvider. Stores non-secret config only; the
API key is resolved at call time inside `complete()`.

#### Usage

    OpenAICompatProvider$new(model, base_url = "https://api.openai.com/v1", ...)

#### Arguments

- `model`:

  Character model identifier, e.g. "gpt-4o" or a local model name like
  "llama3".

- `base_url`:

  Character base URL. Defaults to the OpenAI cloud endpoint; override
  for OpenAI-compatible servers (Ollama, LM Studio, gateways).

- `...`:

  Ignored; accepted for forward-compatibility.

------------------------------------------------------------------------

### `OpenAICompatProvider$complete()`

Complete a prompt via `POST {base_url}/chat/completions`. Resolves the
key at call time; on a missing key or any HTTP/parse failure returns one
warning + `NA`. Structured output is OPTIONAL: when `schema` is supplied
a `response_format` json_schema is added to the body, but omitting it
uses the plain-text path (which local models that lack `response_format`
support still handle).

#### Usage

    OpenAICompatProvider$complete(prompt, schema = NULL, ...)

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

### `OpenAICompatProvider$clone()`

The objects of this class are cloneable with this method.

#### Usage

    OpenAICompatProvider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# OpenAI (reads OPENAI_API_KEY from the environment at call time):
p <- OpenAICompatProvider$new(model = "gpt-4o")
p$complete("Summarise these event-study diagnostics")

# Any OpenAI-compatible endpoint works via a base_url override — e.g. a local
# Ollama server (no cloud key needed by the server, but OPENAI_API_KEY is still
# read as the bearer token; set it to any non-empty value for local servers):
p_local <- OpenAICompatProvider$new(
  model = "llama3",
  base_url = "http://localhost:11434/v1"   # Ollama; LM Studio: :1234/v1
)
p_local$complete("Summarise these diagnostics")
} # }
```
