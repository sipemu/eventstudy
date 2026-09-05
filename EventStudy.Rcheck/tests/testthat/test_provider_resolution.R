# Tests for call-time backend + key resolution.
#
# Precedence is explicit arg -> EVENTSTUDY_ADVISOR_* env selector -> default.
# API keys are read from the environment at CALL time only (never at
# construction) and .resolve_api_key returns NA_character_ (never stops) when a
# key is unset. withr::local_envvar scopes env changes to each test.

test_that(".resolve_provider_config returns documented defaults with no env set", {
  withr::local_envvar(
    EVENTSTUDY_ADVISOR_PROVIDER = "",
    EVENTSTUDY_ADVISOR_MODEL = "",
    EVENTSTUDY_ADVISOR_BASE_URL = ""
  )
  cfg <- EventStudy:::.resolve_provider_config()
  expect_identical(cfg$provider, "custom")
  expect_null(cfg$model)
  expect_null(cfg$base_url)
})

test_that("env selector fills provider/model/base_url when arg is NULL", {
  withr::local_envvar(
    EVENTSTUDY_ADVISOR_PROVIDER = "anthropic",
    EVENTSTUDY_ADVISOR_MODEL = "claude-opus-4-8",
    EVENTSTUDY_ADVISOR_BASE_URL = "https://example.test/v1"
  )
  cfg <- EventStudy:::.resolve_provider_config()
  expect_identical(cfg$provider, "anthropic")
  expect_identical(cfg$model, "claude-opus-4-8")
  expect_identical(cfg$base_url, "https://example.test/v1")
})

test_that("explicit argument overrides the env selector", {
  withr::local_envvar(
    EVENTSTUDY_ADVISOR_PROVIDER = "anthropic",
    EVENTSTUDY_ADVISOR_MODEL = "claude-opus-4-8",
    EVENTSTUDY_ADVISOR_BASE_URL = "https://env.test/v1"
  )
  cfg <- EventStudy:::.resolve_provider_config(
    provider = "openai",
    model = "gpt-4o",
    base_url = "https://arg.test/v1"
  )
  expect_identical(cfg$provider, "openai")
  expect_identical(cfg$model, "gpt-4o")
  expect_identical(cfg$base_url, "https://arg.test/v1")
})

test_that(".resolve_api_key reads the conventional env var per provider", {
  withr::local_envvar(
    OPENAI_API_KEY = DUMMY_OPENAI_KEY,
    ANTHROPIC_API_KEY = DUMMY_ANTHROPIC_KEY
  )
  expect_identical(EventStudy:::.resolve_api_key("openai"), DUMMY_OPENAI_KEY)
  expect_identical(EventStudy:::.resolve_api_key("anthropic"), DUMMY_ANTHROPIC_KEY)
})

test_that(".resolve_api_key returns NA_character_ when unset and never stops", {
  withr::local_envvar(OPENAI_API_KEY = "", ANTHROPIC_API_KEY = "")
  # Sys.getenv treats "" as set-to-empty; emulate truly-unset via NA fallback by
  # unsetting entirely.
  withr::local_envvar(OPENAI_API_KEY = NA, ANTHROPIC_API_KEY = NA)
  expect_true(is.na(EventStudy:::.resolve_api_key("openai")))
  expect_true(is.na(EventStudy:::.resolve_api_key("anthropic")))
  expect_type(EventStudy:::.resolve_api_key("openai"), "character")
  # An unknown/keyless provider (e.g. "custom") also returns NA, never stops.
  expect_true(is.na(EventStudy:::.resolve_api_key("custom")))
})

test_that("constructing a provider with no key set raises no error (call-time only)", {
  withr::local_envvar(OPENAI_API_KEY = NA, ANTHROPIC_API_KEY = NA)
  expect_no_error(CustomProvider$new(function(prompt, schema) "x"))
})

# --- provider() factory -----------------------------------------------------

test_that("provider('custom') dispatches CustomProvider with no httr2", {
  p <- provider("custom", fn = function(prompt, schema) "x")
  expect_true(inherits(p, "CustomProvider"))
  expect_identical(p$complete("hi")$text, "x")
})

test_that("provider() with no type resolves to the default (custom) working provider", {
  withr::local_envvar(EVENTSTUDY_ADVISOR_PROVIDER = "")
  p <- provider(fn = function(prompt, schema) "default-branch")
  expect_true(inherits(p, "CustomProvider"))
  expect_identical(p$complete("hi")$text, "default-branch")
})

test_that("provider('openai') now constructs the OpenAICompatProvider (delivered in 06-2)", {
  withr::local_envvar(EVENTSTUDY_ADVISOR_PROVIDER = "")
  p <- provider("openai", model = "gpt-4o")
  expect_true(inherits(p, "OpenAICompatProvider"))
})

test_that("provider('anthropic') now constructs the AnthropicProvider (delivered in 06-3)", {
  withr::local_envvar(EVENTSTUDY_ADVISOR_PROVIDER = "")
  p <- provider("anthropic", model = "claude-opus-4-8")
  expect_true(inherits(p, "AnthropicProvider"))
})

test_that("provider() rejects an unknown type via match.arg", {
  withr::local_envvar(EVENTSTUDY_ADVISOR_PROVIDER = "")
  expect_error(provider("gpt-9000"))
})
