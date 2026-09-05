# =============================================================================
# Provider seam for the Grounded AI Advisor (Phase 6)
# =============================================================================
#
# This file defines the uniform LLM-provider seam. It mirrors the package's
# existing ModelBase / TestStatisticBase strategy-pattern idiom (R/models.R):
# an abstract R6 base (`ProviderBase`) defines the uniform
# `complete(prompt, schema, ...)` contract, and concrete subclasses implement it.
#
# In this plan (06-1) only `CustomProvider` (an in-process user-function hook
# that needs NO network) is fully wired. The two HTTP providers
# (OpenAICompatProvider in 06-2, AnthropicProvider in 06-3) expand out from this
# proven skeleton. `httr2`/`jsonlite` live in Suggests and are only touched by
# those later HTTP providers; CustomProvider and the resolution helpers need
# neither, so this whole plan runs with them uninstalled.
#
# Failure discipline mirrors .handle_degenerate() in R/contract.R: every
# provider failure routes through the SINGLE point `.provider_failure()`, which
# emits EXACTLY ONE warning(msg, call. = FALSE) and returns an
# `es_provider_response` carrying text = NA_character_ — it never lets an
# uncaught error escape and never returns a plausible-looking wrong completion.

# ---------------------------------------------------------------------------
# es_provider_response builders (the single success / failure plumbing)
# ---------------------------------------------------------------------------

#' Build a successful es_provider_response
#'
#' The field names (`source`, `is_deterministic`, `text`, `error`) are LOCKED to
#' match the Phase 5 `es_advice` shape (see R/advise_offline.R:221-229) so that
#' Phase 7's `es_advise()` wrapper slots the online path next to the offline path
#' without reshaping. The provider path is never deterministic
#' (`is_deterministic = FALSE`) and returns raw completion `text` for Phase 7 to
#' ground-check.
#'
#' @param source_label Character provider label, e.g. "custom", "openai".
#' @param text Character completion text.
#' @return An `es_provider_response` S3 object.
#' @noRd
.provider_success <- function(source_label, text) {
  structure(
    list(
      source           = source_label,
      is_deterministic = FALSE,
      text             = text,
      error            = NULL
    ),
    class = "es_provider_response"
  )
}

#' Build a failing es_provider_response (the SINGLE warning-emitting point)
#'
#' Every provider failure path routes through here so that exactly ONE warning is
#' emitted per failure (mirroring `.handle_degenerate()` in R/contract.R). Never
#' call `warning()` for a provider failure anywhere else. The returned object
#' carries `text = NA_character_` — never a fabricated completion — preserving the
#' package's "never silently wrong" core value at the LLM layer.
#'
#' Secrets are NEVER placed into `reason`, the warning, or the returned object.
#'
#' @param source_label Character provider label.
#' @param reason Character, short human-readable failure reason (no secrets).
#' @return An `es_provider_response` S3 object with `text = NA_character_`.
#' @noRd
.provider_failure <- function(source_label, reason) {
  warning(
    sprintf("Advisor provider '%s' failed: %s. Returning NA.",
            source_label, reason),
    call. = FALSE
  )
  structure(
    list(
      source           = source_label,
      is_deterministic = FALSE,
      text             = NA_character_,
      error            = reason
    ),
    class = "es_provider_response"
  )
}

# ---------------------------------------------------------------------------
# Call-time backend + key resolution (arg -> env -> default)
# ---------------------------------------------------------------------------

#' Resolve provider / model / base_url by 3-tier precedence
#'
#' Precedence for each field is: explicit argument -> environment selector
#' (\code{EVENTSTUDY_ADVISOR_PROVIDER} / \code{_MODEL} / \code{_BASE_URL}) ->
#' documented default. An unset or empty (\code{""}) env var is treated as unset.
#' The default provider is \code{"custom"} (the no-network provider), so the
#' package resolves to a working provider with no configuration.
#'
#' These environment variables are SELECTORS (which backend/model/URL) — distinct
#' from the API-key SECRETS resolved by \code{.resolve_api_key()}. Both are read,
#' never written.
#'
#' @param provider Optional character provider type; \code{NULL} to resolve.
#' @param model Optional character model identifier; \code{NULL} to resolve.
#' @param base_url Optional character base URL; \code{NULL} to resolve.
#' @return A named list with elements \code{provider}, \code{model},
#'   \code{base_url} (the latter two may be \code{NULL} when neither arg nor env
#'   supplies them).
#' @noRd
.resolve_provider_config <- function(provider = NULL, model = NULL,
                                     base_url = NULL) {
  # "" (unset env default) collapses to NULL so the next tier applies.
  env_or_null <- function(name) {
    val <- Sys.getenv(name, unset = "")
    if (identical(val, "")) NULL else val
  }

  provider <- provider %||% env_or_null("EVENTSTUDY_ADVISOR_PROVIDER") %||% "custom"
  model    <- model    %||% env_or_null("EVENTSTUDY_ADVISOR_MODEL")
  base_url <- base_url %||% env_or_null("EVENTSTUDY_ADVISOR_BASE_URL")

  list(provider = provider, model = model, base_url = base_url)
}

#' Resolve an API key from the environment at CALL time
#'
#' Reads the provider-conventional secret env var (\code{OPENAI_API_KEY} for
#' openai-compatible, \code{ANTHROPIC_API_KEY} for anthropic). Returns
#' \code{NA_character_} when unset — it NEVER stops here; a missing key surfaces
#' as one clear failure at call time via \code{.provider_failure()} inside the
#' HTTP providers' \code{complete()} (delivered in 06-2/06-3). Keys are read at
#' call time only, never stored on any object and never printed.
#'
#' @param provider Character provider type ("openai" or "anthropic"). Other
#'   values (e.g. "custom") need no key and return \code{NA_character_}.
#' @return The key string, or \code{NA_character_} when unset / not applicable.
#' @noRd
.resolve_api_key <- function(provider) {
  name <- switch(
    provider,
    openai    = "OPENAI_API_KEY",
    anthropic = "ANTHROPIC_API_KEY",
    NULL
  )
  if (is.null(name)) {
    return(NA_character_)
  }
  Sys.getenv(name, unset = NA_character_)
}

# ---------------------------------------------------------------------------
# Shared httr2 request/response plumbing (never-throw discipline)
# ---------------------------------------------------------------------------
#
# Both helpers reference `httr2` ONLY inside their bodies. They are called only
# from a concrete provider's complete(), which guards `requireNamespace("httr2")`
# at its top — so R CMD check stays clean with httr2 uninstalled.

#' Build and perform a thin httr2 request, trapping transport failures
#'
#' Assembles the request pipeline (url path append, JSON body, timeout, retry
#' WITHOUT transient/transport retry, and \code{req_error(is_error = ~FALSE)} so a
#' non-2xx status returns an inspectable response instead of throwing), attaches
#' auth (bearer -> \code{req_auth_bearer_token} which auto-redacts Authorization;
#' x-api-key -> \code{req_headers_redacted} so the key is redacted in every
#' print/error path), and performs it inside \code{tryCatch}. A
#' transport/timeout failure is returned as the condition object (never thrown)
#' so \code{.finish_response()} can degrade it uniformly.
#'
#' Shared by \code{OpenAICompatProvider} (bearer) and \code{AnthropicProvider}
#' (x-api-key, delivered in 06-3). References \code{httr2} only inside the body;
#' callers guard \code{requireNamespace("httr2")}.
#'
#' @param base_url Character base URL, e.g. "https://api.openai.com/v1".
#' @param path Character path appended to the base, e.g. "chat/completions".
#' @param body Named list request body (JSON-encoded via \code{req_body_json}).
#' @param key Character API key (attached redacted).
#' @param auth One of \code{"bearer"} or \code{"x-api-key"}.
#' @param extra_headers Named list of additional headers (e.g. the Anthropic
#'   \code{anthropic-version}). Empty by default.
#' @param timeout Numeric request timeout in seconds (default 30).
#' @param max_tries Integer max attempts (default 2); transport failures are NOT
#'   retried so a down endpoint fails fast to one warning.
#' @return An httr2 response object, OR a condition object on transport/timeout
#'   failure.
#' @noRd
.perform_request <- function(base_url, path, body, key,
                             auth = c("bearer", "x-api-key"),
                             extra_headers = list(),
                             timeout = 30, max_tries = 2) {
  auth <- match.arg(auth)

  # The ENTIRE request-building sequence is inside the tryCatch (not just
  # req_perform): req_body_json() calls check_installed("jsonlite") which THROWS
  # when jsonlite is absent, and any other build-time error must degrade too. Any
  # such error is returned as the condition object `e` (never thrown), which
  # .finish_response() maps to the standard one-warning + NA failure. Redaction
  # logic (req_auth_bearer_token / req_headers_redacted) is preserved exactly.
  tryCatch(
    {
      req <- httr2::request(base_url)
      req <- httr2::req_url_path_append(req, path)
      req <- httr2::req_body_json(req, body)
      req <- httr2::req_timeout(req, timeout)
      req <- httr2::req_retry(req, max_tries = max_tries, retry_on_failure = FALSE)
      # is_error = FALSE turns non-2xx into a normal return so .finish_response()
      # branches on resp_status() instead of httr2 throwing an httr2_http_* error.
      req <- httr2::req_error(req, is_error = function(resp) FALSE)

      if (identical(auth, "bearer")) {
        req <- httr2::req_auth_bearer_token(req, key)
      } else {
        req <- httr2::req_headers_redacted(req, "x-api-key" = key)
      }
      if (length(extra_headers)) {
        req <- do.call(httr2::req_headers, c(list(req), extra_headers))
      }

      httr2::req_perform(req)
    },
    error = function(e) e
  )
}

#' Branch on an httr2 response and safely extract completion text
#'
#' Consumes the return of \code{.perform_request()} and produces an
#' \code{es_provider_response} on every path: a condition (transport/timeout) ->
#' failure; a non-2xx status -> failure carrying ONLY \code{"HTTP <status>"} (no
#' body, no key); a malformed 200 body (\code{resp_body_json} trapped to
#' \code{NULL}) -> failure; an empty / missing completion text -> failure; and a
#' well-formed body -> success. Every failure routes through
#' \code{.provider_failure()} so EXACTLY ONE warning is emitted. References
#' \code{httr2} only inside the body; callers guard \code{requireNamespace}.
#'
#' @param resp An httr2 response object or a condition (from
#'   \code{.perform_request()}).
#' @param extract_fn A function \code{function(parsed) -> character} pulling the
#'   completion text out of the parsed JSON body.
#' @param source_label Character provider label used in the response + warning.
#' @return An \code{es_provider_response} S3 object.
#' @noRd
.finish_response <- function(resp, extract_fn, source_label) {
  if (inherits(resp, "condition")) {
    return(.provider_failure(source_label, "request failed (network/timeout)"))
  }
  status <- httr2::resp_status(resp)
  if (status < 200L || status >= 300L) {
    # NEVER include the response body or key — only the status code.
    return(.provider_failure(source_label, paste0("HTTP ", status)))
  }
  parsed <- tryCatch(
    httr2::resp_body_json(resp),
    error = function(e) NULL
  )
  if (is.null(parsed)) {
    return(.provider_failure(source_label, "malformed response body"))
  }
  text <- tryCatch(extract_fn(parsed), error = function(e) NA_character_)
  if (length(text) != 1L || is.na(text) || !nzchar(text)) {
    return(.provider_failure(source_label, "no completion text in response"))
  }
  .provider_success(source_label, text)
}

# ---------------------------------------------------------------------------
# ProviderBase — abstract R6 contract
# ---------------------------------------------------------------------------

#' @title ProviderBase
#' @description Abstract base class for Grounded AI Advisor providers. Defines the
#'   uniform `complete(prompt, schema, ...)` contract, mirroring the package's
#'   `ModelBase` / `TestStatisticBase` strategy-pattern idiom. Concrete providers
#'   (`CustomProvider`, and the HTTP `OpenAICompatProvider` / `AnthropicProvider`
#'   delivered in later plans) inherit from this base and implement `complete()`.
#'
#'   `ProviderBase` itself is abstract: calling `complete()` on it errors. It
#'   stores only non-secret configuration (`model`, `base_url`); API keys are
#'   NEVER read at construction — they are resolved at call time inside each
#'   concrete provider's `complete()`.
#' @export
ProviderBase <- R6::R6Class(
  "ProviderBase",
  public = list(
    #' @field model Character model identifier (or NULL for provider default).
    model = NULL,
    #' @field base_url Character base URL for HTTP providers (or NULL).
    base_url = NULL,

    #' @description Construct a provider. Stores non-secret config only; never
    #'   reads or stores API keys (keys are resolved at call time).
    #' @param model Optional character model identifier.
    #' @param base_url Optional character base URL (HTTP providers only).
    #' @param ... Ignored; accepted for subclass forward-compatibility.
    initialize = function(model = NULL, base_url = NULL, ...) {
      self$model <- model
      self$base_url <- base_url
      invisible(self)
    },

    #' @description Complete a prompt. Abstract on the base class — concrete
    #'   providers override this.
    #' @param prompt Character prompt to send to the provider.
    #' @param schema Optional structured-output schema (list) or NULL.
    #' @param ... Provider-specific arguments.
    #' @return An `es_provider_response` S3 list with fields:
    #'   \code{source} (character provider label),
    #'   \code{is_deterministic} (always \code{FALSE} for provider output),
    #'   \code{text} (character completion, or \code{NA_character_} on failure),
    #'   \code{error} (\code{NULL} on success, character reason on failure).
    #'   These field names match the Phase 5 \code{es_advice} shape so the Phase 7
    #'   wrapper slots in trivially.
    complete = function(prompt, schema = NULL, ...) {
      stop("ProviderBase is abstract; use a concrete provider.", call. = FALSE)
    }
  )
)

# ---------------------------------------------------------------------------
# CustomProvider — in-process user-function hook (NO HTTP)
# ---------------------------------------------------------------------------

#' @title CustomProvider
#' @description A provider that wraps a user-supplied
#'   \code{function(prompt, schema, ...) -> list | character}. This is the
#'   offline end-to-end seam and the escape hatch for backends the built-in HTTP
#'   providers do not cover. It uses NO network and needs neither \code{httr2}
#'   nor \code{jsonlite}.
#'
#'   The user function is run inside \code{tryCatch}: if it errors, the provider
#'   degrades to exactly one \code{warning()} plus an `es_provider_response` with
#'   \code{text = NA_character_} — it never crashes the session.
#' @export
#' @examples
#' # In-process, no network: the user function supplies the completion text.
#' p <- CustomProvider$new(function(prompt, schema) "canned advice")
#' res <- p$complete("Summarise these diagnostics")
#' res$text
#' res$source
CustomProvider <- R6::R6Class(
  "CustomProvider",
  inherit = ProviderBase,
  public = list(
    #' @description Construct a CustomProvider.
    #' @param fn A function of \code{(prompt, schema, ...)} returning a list or a
    #'   character completion. Required.
    #' @param model Optional character model identifier (informational only).
    #' @param ... Ignored; forwarded for forward-compatibility.
    initialize = function(fn, model = NULL, ...) {
      stopifnot(is.function(fn))
      super$initialize(model = model, base_url = NULL)
      private$.fn <- fn
      invisible(self)
    },

    #' @description Run the wrapped user function and wrap its result in an
    #'   `es_provider_response`. A throwing function degrades to one warning + NA.
    #' @param prompt Character prompt passed to the user function.
    #' @param schema Optional structured-output schema passed through to the user
    #'   function.
    #' @param ... Additional arguments forwarded to the user function.
    #' @return An `es_provider_response` (see \code{ProviderBase$complete}).
    complete = function(prompt, schema = NULL, ...) {
      out <- tryCatch(
        private$.fn(prompt, schema, ...),
        error = function(e) e
      )
      if (inherits(out, "condition")) {
        return(.provider_failure("custom", "custom function errored"))
      }
      # A fn returning NULL / character(0) makes as.character(out) length-0, so
      # [[1L]] would throw "subscript out of bounds" — coerce safely and guard
      # empties/NA so an empty return degrades to one warning + NA, never crashes.
      txt <- tryCatch(as.character(out), error = function(e) character(0))
      if (length(txt) == 0L || is.na(txt[[1L]])) {
        return(.provider_failure("custom", "custom function returned no text"))
      }
      .provider_success("custom", txt[[1L]])
    }
  ),
  private = list(
    .fn = NULL
  )
)

# ---------------------------------------------------------------------------
# OpenAICompatProvider — OpenAI + any OpenAI-compatible endpoint (HTTP)
# ---------------------------------------------------------------------------

#' @title OpenAICompatProvider
#' @description A provider that issues \code{POST {base_url}/chat/completions}
#'   with an OpenAI-shaped request body. Because the endpoint is selected purely
#'   by \code{base_url} + \code{model}, this single class covers OpenAI itself
#'   AND every OpenAI-compatible backend — Ollama, LM Studio, or any gateway —
#'   via a \code{base_url} override, with no code change.
#'
#'   The API key is resolved at CALL time from \code{OPENAI_API_KEY} (never at
#'   construction, never stored on the object) and attached with
#'   \code{req_auth_bearer_token()}, which redacts the \code{Authorization}
#'   header in every print/error path. A missing key, transport/timeout failure,
#'   non-2xx status, malformed body, or missing completion text each degrades to
#'   exactly one \code{warning()} plus an `es_provider_response` with
#'   \code{text = NA_character_} — it never crashes the session and never returns
#'   a fabricated completion.
#'
#'   \code{httr2} is required only for this provider and is guarded by
#'   \code{requireNamespace()} at the top of \code{complete()}, so the package
#'   installs and \code{R CMD check}s cleanly with \code{httr2} absent.
#' @export
#' @examples
#' \dontrun{
#' # OpenAI (reads OPENAI_API_KEY from the environment at call time):
#' p <- OpenAICompatProvider$new(model = "gpt-4o")
#' p$complete("Summarise these event-study diagnostics")
#'
#' # Any OpenAI-compatible endpoint works via a base_url override — e.g. a local
#' # Ollama server (no cloud key needed by the server, but OPENAI_API_KEY is still
#' # read as the bearer token; set it to any non-empty value for local servers):
#' p_local <- OpenAICompatProvider$new(
#'   model = "llama3",
#'   base_url = "http://localhost:11434/v1"   # Ollama; LM Studio: :1234/v1
#' )
#' p_local$complete("Summarise these diagnostics")
#' }
OpenAICompatProvider <- R6::R6Class(
  "OpenAICompatProvider",
  inherit = ProviderBase,
  public = list(
    #' @description Construct an OpenAICompatProvider. Stores non-secret config
    #'   only; the API key is resolved at call time inside \code{complete()}.
    #' @param model Character model identifier, e.g. "gpt-4o" or a local model
    #'   name like "llama3".
    #' @param base_url Character base URL. Defaults to the OpenAI cloud endpoint;
    #'   override for OpenAI-compatible servers (Ollama, LM Studio, gateways).
    #' @param ... Ignored; accepted for forward-compatibility.
    initialize = function(model, base_url = "https://api.openai.com/v1", ...) {
      super$initialize(model = model, base_url = base_url)
      invisible(self)
    },

    #' @description Complete a prompt via \code{POST {base_url}/chat/completions}.
    #'   Resolves the key at call time; on a missing key or any HTTP/parse failure
    #'   returns one warning + \code{NA}. Structured output is OPTIONAL: when
    #'   \code{schema} is supplied a \code{response_format} json_schema is added to
    #'   the body, but omitting it uses the plain-text path (which local models
    #'   that lack \code{response_format} support still handle).
    #' @param prompt Character prompt.
    #' @param schema Optional structured-output schema (list) or \code{NULL}.
    #' @param ... Ignored; accepted for forward-compatibility.
    #' @return An `es_provider_response` (see \code{ProviderBase$complete}).
    complete = function(prompt, schema = NULL, ...) {
      if (!requireNamespace("httr2", quietly = TRUE)) {
        stop("Package 'httr2' is required for the OpenAI-compatible provider. ",
             "Install it with: install.packages('httr2')")
      }
      # jsonlite is Suggests-only and req_body_json() calls check_installed(),
      # which THROWS when it is absent. Degrade to one warning + NA (never an
      # uncaught crash) via the single failure funnel, mirroring the httr2 guard.
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        return(.provider_failure(
          "openai",
          "package 'jsonlite' is required for HTTP providers (install.packages('jsonlite'))"
        ))
      }
      key <- .resolve_api_key("openai")
      # Treat unset (NA) AND set-but-empty ("") identically as "no key".
      if (is.na(key) || !nzchar(key)) {
        return(.provider_failure("openai", "no API key (set OPENAI_API_KEY)"))
      }

      body <- list(
        model = self$model,
        messages = list(list(role = "user", content = prompt))
      )
      if (!is.null(schema)) {
        # OPTIONAL: local models (Ollama/LM Studio) may not support this; the
        # plain-text path above still works when schema is omitted.
        body$response_format <- list(
          type = "json_schema",
          json_schema = list(name = "advice", schema = schema)
        )
      }

      resp <- .perform_request(self$base_url, "chat/completions", body, key,
                               auth = "bearer")
      .finish_response(resp, function(p) p$choices[[1]]$message$content, "openai")
    }
  )
)

# ---------------------------------------------------------------------------
# AnthropicProvider — native Anthropic Messages API (HTTP)
# ---------------------------------------------------------------------------

#' Render a tool_use block's structured input to a single character string
#'
#' Given the parsed Anthropic response, prefer the FIRST content block whose
#' \code{type == "tool_use"} and render its \code{$input} to a character string
#' (via \code{jsonlite::toJSON} when available, else a \code{paste} of its
#' fields). When no tool_use block is present, fall back to the FIRST content
#' block's \code{$text} — so the provider never crashes when the model answers
#' with plain text instead of a tool call. This whole function is wrapped by
#' \code{.finish_response}'s \code{tryCatch}, so any unexpected shape degrades to
#' the standard one-warning + NA path rather than throwing.
#'
#' @param parsed Parsed Anthropic response body (a list with \code{$content}).
#' @return A single character string, or \code{NA_character_} when nothing usable.
#' @noRd
.extract_anthropic_text <- function(parsed) {
  content <- parsed$content
  if (length(content) == 0L) {
    return(NA_character_)
  }

  # Prefer the first tool_use block's structured input.
  for (block in content) {
    if (identical(block$type, "tool_use") && !is.null(block$input)) {
      input <- block$input
      if (requireNamespace("jsonlite", quietly = TRUE)) {
        return(as.character(
          jsonlite::toJSON(input, auto_unbox = TRUE, null = "null")
        ))
      }
      # jsonlite absent: render key=value pairs deterministically.
      return(paste(
        vapply(
          seq_along(input),
          function(i) paste0(names(input)[[i]], "=", as.character(input[[i]])[[1L]]),
          character(1L)
        ),
        collapse = ", "
      ))
    }
  }

  # Fallback: the first block's text (plain-text answer despite a schema).
  txt <- content[[1L]]$text
  if (is.null(txt)) NA_character_ else as.character(txt)[[1L]]
}

#' @title AnthropicProvider
#' @description A provider that issues \code{POST {base_url}/v1/messages} against
#'   the native Anthropic Messages API. The request carries the mandatory
#'   \code{max_tokens} field and the \code{anthropic-version} header; when a
#'   \code{schema} is supplied it adds a tool-use \code{input_schema} block
#'   (\code{tools} + \code{tool_choice}) to obtain structured output, and the
#'   response extractor prefers that tool call's structured \code{input} while
#'   falling back to the first text block — so the provider never crashes when the
#'   model replies with plain text instead of a tool call.
#'
#'   The API key is resolved at CALL time from \code{ANTHROPIC_API_KEY} (never at
#'   construction, never stored on the object) and attached with
#'   \code{req_headers_redacted("x-api-key" = key)}, which redacts the key in
#'   every print/error path. Note \code{req_auth_bearer_token} does NOT redact an
#'   \code{x-api-key} header — the redacted-headers helper is mandatory here. A
#'   missing key, transport/timeout failure, non-2xx status, malformed body, or
#'   missing completion text each degrades to exactly one \code{warning()} plus an
#'   `es_provider_response` with \code{text = NA_character_} — it never crashes the
#'   session and never returns a fabricated completion.
#'
#'   \code{httr2} is required only for this provider and is guarded by
#'   \code{requireNamespace()} at the top of \code{complete()}, so the package
#'   installs and \code{R CMD check}s cleanly with \code{httr2} absent.
#' @export
#' @examples
#' \dontrun{
#' # Reads ANTHROPIC_API_KEY from the environment at call time:
#' p <- AnthropicProvider$new(model = "claude-opus-4-8")
#' p$complete("Summarise these event-study diagnostics")
#'
#' # Structured output via a tool-use input_schema:
#' schema <- list(type = "object",
#'                properties = list(advice = list(type = "string")))
#' p$complete("Recommend a test statistic", schema = schema)
#' }
AnthropicProvider <- R6::R6Class(
  "AnthropicProvider",
  inherit = ProviderBase,
  public = list(
    #' @field max_tokens Integer max completion tokens (Anthropic REQUIRES this).
    max_tokens = NULL,

    #' @description Construct an AnthropicProvider. Stores non-secret config only;
    #'   the API key is resolved at call time inside \code{complete()}.
    #' @param model Character model identifier, e.g. "claude-opus-4-8".
    #' @param base_url Character base URL. Defaults to the Anthropic cloud
    #'   endpoint.
    #' @param max_tokens Integer maximum number of tokens to generate. The
    #'   Anthropic Messages API REQUIRES this field; defaults to 1024.
    #' @param ... Ignored; accepted for forward-compatibility.
    initialize = function(model, base_url = "https://api.anthropic.com",
                          max_tokens = 1024L, ...) {
      super$initialize(model = model, base_url = base_url)
      self$max_tokens <- max_tokens
      invisible(self)
    },

    #' @description Complete a prompt via \code{POST {base_url}/v1/messages}.
    #'   Resolves the key at call time; on a missing key or any HTTP/parse failure
    #'   returns one warning + \code{NA}. Structured output is OPTIONAL: when
    #'   \code{schema} is supplied a tool-use \code{input_schema} block is added and
    #'   the extractor prefers the tool call's \code{input}, falling back to the
    #'   first text block.
    #' @param prompt Character prompt.
    #' @param schema Optional structured-output schema (list) or \code{NULL}.
    #' @param ... Ignored; accepted for forward-compatibility.
    #' @return An `es_provider_response` (see \code{ProviderBase$complete}).
    complete = function(prompt, schema = NULL, ...) {
      if (!requireNamespace("httr2", quietly = TRUE)) {
        stop("Package 'httr2' is required for the Anthropic provider. ",
             "Install it with: install.packages('httr2')")
      }
      # jsonlite is Suggests-only and req_body_json() calls check_installed(),
      # which THROWS when it is absent. Degrade to one warning + NA (never an
      # uncaught crash) via the single failure funnel, mirroring the httr2 guard.
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        return(.provider_failure(
          "anthropic",
          "package 'jsonlite' is required for HTTP providers (install.packages('jsonlite'))"
        ))
      }
      key <- .resolve_api_key("anthropic")
      # Treat unset (NA) AND set-but-empty ("") identically as "no key".
      if (is.na(key) || !nzchar(key)) {
        return(.provider_failure("anthropic", "no API key (set ANTHROPIC_API_KEY)"))
      }

      body <- list(
        model = self$model,
        max_tokens = self$max_tokens,
        messages = list(list(role = "user", content = prompt))
      )
      if (!is.null(schema)) {
        # Tool-use structured-output path: define a single "advice" tool whose
        # input_schema is the caller's schema and force the model to call it.
        body$tools <- list(list(
          name = "advice",
          description = "Return the structured advice payload.",
          input_schema = schema
        ))
        body$tool_choice <- list(type = "tool", name = "advice")
      }

      resp <- .perform_request(
        self$base_url, "v1/messages", body, key,
        auth = "x-api-key",
        extra_headers = list("anthropic-version" = "2023-06-01")
      )
      .finish_response(resp, .extract_anthropic_text, "anthropic")
    }
  )
)

# ---------------------------------------------------------------------------
# provider() — convenience factory (all three branches wired)
# ---------------------------------------------------------------------------

#' Construct a Grounded AI Advisor provider
#'
#' Convenience factory that resolves the backend by 3-tier precedence
#' (explicit \code{type} argument -> \code{EVENTSTUDY_ADVISOR_PROVIDER} env
#' selector -> default \code{"custom"}) and constructs the matching provider
#' object. All three backends are wired: the in-process \code{"custom"} hook and
#' the HTTP \code{"openai"} (OpenAI-compatible) and \code{"anthropic"} (native
#' Anthropic Messages) providers.
#'
#' The \code{"custom"} branch needs no network and neither \code{httr2} nor
#' \code{jsonlite}. Keys are never read here; the HTTP providers resolve them at
#' call time inside their own \code{complete()}.
#'
#' @param type Provider type, one of \code{"custom"}, \code{"openai"},
#'   \code{"anthropic"}. When \code{NULL} (the default) it is resolved from the
#'   \code{EVENTSTUDY_ADVISOR_PROVIDER} env var, falling back to \code{"custom"}.
#' @param fn For \code{type = "custom"}, the user function
#'   \code{function(prompt, schema, ...)} returning a list or character. Required
#'   for the custom branch.
#' @param model Optional character model identifier (resolved from
#'   \code{EVENTSTUDY_ADVISOR_MODEL} when \code{NULL}).
#' @param base_url Optional character base URL for HTTP providers (resolved from
#'   \code{EVENTSTUDY_ADVISOR_BASE_URL} when \code{NULL}).
#' @param ... Additional arguments forwarded to the provider constructor.
#' @return A \code{ProviderBase} subclass instance.
#' @export
#' @examples
#' # Custom provider runs in-process, no network:
#' p <- provider("custom", fn = function(prompt, schema) "advice text")
#' p$complete("Summarise")$text
#'
#' \dontrun{
#' # HTTP providers make live calls; never run in examples or on CRAN:
#' p <- provider("openai", model = "gpt-4o")
#' p$complete("Summarise these diagnostics")
#'
#' a <- provider("anthropic", model = "claude-opus-4-8")
#' a$complete("Summarise these diagnostics")
#' }
provider <- function(type = NULL, fn = NULL, model = NULL, base_url = NULL, ...) {
  cfg <- .resolve_provider_config(provider = type, model = model,
                                  base_url = base_url)
  type <- match.arg(cfg$provider, c("custom", "openai", "anthropic"))

  if (identical(type, "custom")) {
    return(CustomProvider$new(fn = fn, model = cfg$model, ...))
  }

  # OpenAICompatProvider now exists (06-2); construct it directly. base_url is
  # passed only when resolved, so the class default (OpenAI cloud) applies when
  # neither arg nor env supplies one.
  if (identical(type, "openai")) {
    args <- list(model = cfg$model)
    if (!is.null(cfg$base_url)) args$base_url <- cfg$base_url
    return(do.call(OpenAICompatProvider$new, c(args, list(...))))
  }

  # AnthropicProvider (06-3): all three branches are now live. base_url is passed
  # only when resolved, so the class default (Anthropic cloud) applies when
  # neither arg nor env supplies one.
  args <- list(model = cfg$model)
  if (!is.null(cfg$base_url)) args$base_url <- cfg$base_url
  do.call(AnthropicProvider$new, c(args, list(...)))
}
