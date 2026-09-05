# ProviderBase

Abstract base class for Grounded AI Advisor providers. Defines the
uniform \`complete(prompt, schema, ...)\` contract, mirroring the
package's \`ModelBase\` / \`TestStatisticBase\` strategy-pattern idiom.
Concrete providers (\`CustomProvider\`, and the HTTP
\`OpenAICompatProvider\` / \`AnthropicProvider\` delivered in later
plans) inherit from this base and implement \`complete()\`.

\`ProviderBase\` itself is abstract: calling \`complete()\` on it
errors. It stores only non-secret configuration (\`model\`,
\`base_url\`); API keys are NEVER read at construction — they are
resolved at call time inside each concrete provider's \`complete()\`.

## Public fields

- `model`:

  Character model identifier (or NULL for provider default).

- `base_url`:

  Character base URL for HTTP providers (or NULL).

## Methods

### Public methods

- [`ProviderBase$new()`](#method-ProviderBase-initialize)

- [`ProviderBase$complete()`](#method-ProviderBase-complete)

- [`ProviderBase$clone()`](#method-ProviderBase-clone)

------------------------------------------------------------------------

### `ProviderBase$new()`

Construct a provider. Stores non-secret config only; never reads or
stores API keys (keys are resolved at call time).

#### Usage

    ProviderBase$new(model = NULL, base_url = NULL, ...)

#### Arguments

- `model`:

  Optional character model identifier.

- `base_url`:

  Optional character base URL (HTTP providers only).

- `...`:

  Ignored; accepted for subclass forward-compatibility.

------------------------------------------------------------------------

### `ProviderBase$complete()`

Complete a prompt. Abstract on the base class — concrete providers
override this.

#### Usage

    ProviderBase$complete(prompt, schema = NULL, ...)

#### Arguments

- `prompt`:

  Character prompt to send to the provider.

- `schema`:

  Optional structured-output schema (list) or NULL.

- `...`:

  Provider-specific arguments.

#### Returns

An \`es_provider_response\` S3 list with fields: `source` (character
provider label), `is_deterministic` (always `FALSE` for provider
output), `text` (character completion, or `NA_character_` on failure),
`error` (`NULL` on success, character reason on failure). These field
names match the Phase 5 `es_advice` shape so the Phase 7 wrapper slots
in trivially.

------------------------------------------------------------------------

### `ProviderBase$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ProviderBase$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
