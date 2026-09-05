# CustomProvider

A provider that wraps a user-supplied
`function(prompt, schema, ...) -> list | character`. This is the offline
end-to-end seam and the escape hatch for backends the built-in HTTP
providers do not cover. It uses NO network and needs neither `httr2` nor
`jsonlite`.

The user function is run inside `tryCatch`: if it errors, the provider
degrades to exactly one
[`warning()`](https://rdrr.io/r/base/warning.html) plus an
\`es_provider_response\` with `text = NA_character_` — it never crashes
the session.

## Super class

[`ProviderBase`](https://sipemu.github.io/eventstudy/reference/ProviderBase.md)
-\> `CustomProvider`

## Methods

### Public methods

- [`CustomProvider$new()`](#method-CustomProvider-initialize)

- [`CustomProvider$complete()`](#method-CustomProvider-complete)

- [`CustomProvider$clone()`](#method-CustomProvider-clone)

------------------------------------------------------------------------

### `CustomProvider$new()`

Construct a CustomProvider.

#### Usage

    CustomProvider$new(fn, model = NULL, ...)

#### Arguments

- `fn`:

  A function of `(prompt, schema, ...)` returning a list or a character
  completion. Required.

- `model`:

  Optional character model identifier (informational only).

- `...`:

  Ignored; forwarded for forward-compatibility.

------------------------------------------------------------------------

### `CustomProvider$complete()`

Run the wrapped user function and wrap its result in an
\`es_provider_response\`. A throwing function degrades to one warning +
NA.

#### Usage

    CustomProvider$complete(prompt, schema = NULL, ...)

#### Arguments

- `prompt`:

  Character prompt passed to the user function.

- `schema`:

  Optional structured-output schema passed through to the user function.

- `...`:

  Additional arguments forwarded to the user function.

#### Returns

An \`es_provider_response\` (see `ProviderBase$complete`).

------------------------------------------------------------------------

### `CustomProvider$clone()`

The objects of this class are cloneable with this method.

#### Usage

    CustomProvider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# In-process, no network: the user function supplies the completion text.
p <- CustomProvider$new(function(prompt, schema) "canned advice")
res <- p$complete("Summarise these diagnostics")
res$text
#> [1] "canned advice"
res$source
#> [1] "custom"
```
