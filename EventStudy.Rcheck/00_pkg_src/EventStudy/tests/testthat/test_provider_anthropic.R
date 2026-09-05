# =============================================================================
# AnthropicProvider — POST /v1/messages, tool-use structured output + text
# fallback, and the provider() factory (Phase 6-3)
# =============================================================================
#
# Every test here is OFFLINE: httr2 responses are driven through
# httr2::local_mocked_responses() with a dummy key set via withr. No network call
# is made and no real key is read, so the suite is CRAN-safe and passes with
# httr2/jsonlite present (all references are requireNamespace-guarded in the
# provider code itself).

# ---------------------------------------------------------------------------
# Task 1: AnthropicProvider text path (no schema)
# ---------------------------------------------------------------------------

test_that("AnthropicProvider returns a text-block completion (no schema)", {
  skip_if_not_installed("httr2")
  withr::local_envvar(ANTHROPIC_API_KEY = DUMMY_ANTHROPIC_KEY)
  httr2::local_mocked_responses(
    mock_200(list(content = list(list(type = "text", text = "anthropic says hi"))))
  )
  res <- AnthropicProvider$new(model = "claude-opus-4-8")$complete("hi")
  expect_s3_class(res, "es_provider_response")
  expect_equal(res$source, "anthropic")
  expect_false(res$is_deterministic)
  expect_equal(res$text, "anthropic says hi")
  expect_null(res$error)
})

# ---------------------------------------------------------------------------
# Task 1: tool-use structured-output path (schema supplied, tool_use returned)
# ---------------------------------------------------------------------------

test_that("AnthropicProvider extracts the tool_use input when schema supplied", {
  skip_if_not_installed("httr2")
  withr::local_envvar(ANTHROPIC_API_KEY = DUMMY_ANTHROPIC_KEY)
  httr2::local_mocked_responses(
    mock_200(list(content = list(list(
      type  = "tool_use",
      input = list(advice = "use Kolari-Pynnonen")
    ))))
  )
  schema <- list(type = "object",
                 properties = list(advice = list(type = "string")))
  res <- AnthropicProvider$new(model = "claude-opus-4-8")$complete("hi", schema = schema)
  expect_equal(res$source, "anthropic")
  expect_false(is.na(res$text))
  # The structured advice must survive into the character text field.
  expect_match(res$text, "Kolari-Pynnonen")
  expect_null(res$error)
})

# ---------------------------------------------------------------------------
# Task 1: fallback when the model returns a text block DESPITE a schema
# ---------------------------------------------------------------------------

test_that("AnthropicProvider falls back to the text block when schema present but no tool_use", {
  skip_if_not_installed("httr2")
  withr::local_envvar(ANTHROPIC_API_KEY = DUMMY_ANTHROPIC_KEY)
  httr2::local_mocked_responses(
    mock_200(list(content = list(list(type = "text", text = "plain fallback text"))))
  )
  schema <- list(type = "object",
                 properties = list(advice = list(type = "string")))
  res <- AnthropicProvider$new(model = "claude-opus-4-8")$complete("hi", schema = schema)
  expect_equal(res$text, "plain fallback text")
  expect_null(res$error)
})

# ---------------------------------------------------------------------------
# Task 1: failure paths — never crash, exactly one warning + NA
# ---------------------------------------------------------------------------

test_that("AnthropicProvider with no key -> one warning + NA at call time", {
  skip_if_not_installed("httr2")
  withr::local_envvar(ANTHROPIC_API_KEY = NA)  # unset
  p <- AnthropicProvider$new(model = "claude-opus-4-8")  # no construction error
  expect_warning(res <- p$complete("hi"), "no API key")
  expect_true(is.na(res$text))
  expect_equal(res$source, "anthropic")
})

test_that("AnthropicProvider with an empty key -> one warning + NA", {
  skip_if_not_installed("httr2")
  withr::local_envvar(ANTHROPIC_API_KEY = "")  # set-but-empty
  expect_warning(
    res <- AnthropicProvider$new(model = "claude-opus-4-8")$complete("hi"),
    "no API key"
  )
  expect_true(is.na(res$text))
})

test_that("AnthropicProvider degrades a 401 to one warning + NA", {
  skip_if_not_installed("httr2")
  withr::local_envvar(ANTHROPIC_API_KEY = DUMMY_ANTHROPIC_KEY)
  httr2::local_mocked_responses(mock_status(401L))
  expect_warning(
    res <- AnthropicProvider$new(model = "claude-opus-4-8")$complete("hi"),
    "HTTP 401"
  )
  expect_true(is.na(res$text))
})

test_that("AnthropicProvider degrades a 500 to one warning + NA", {
  skip_if_not_installed("httr2")
  withr::local_envvar(ANTHROPIC_API_KEY = DUMMY_ANTHROPIC_KEY)
  httr2::local_mocked_responses(mock_status(500L))
  expect_warning(
    res <- AnthropicProvider$new(model = "claude-opus-4-8")$complete("hi"),
    "HTTP 500"
  )
  expect_true(is.na(res$text))
})

test_that("AnthropicProvider degrades a malformed body to one warning + NA", {
  skip_if_not_installed("httr2")
  withr::local_envvar(ANTHROPIC_API_KEY = DUMMY_ANTHROPIC_KEY)
  httr2::local_mocked_responses(mock_malformed())
  expect_warning(
    res <- AnthropicProvider$new(model = "claude-opus-4-8")$complete("hi"),
    "malformed"
  )
  expect_true(is.na(res$text))
})

test_that("AnthropicProvider degrades a transport/timeout to one warning + NA", {
  skip_if_not_installed("httr2")
  withr::local_envvar(ANTHROPIC_API_KEY = DUMMY_ANTHROPIC_KEY)
  httr2::local_mocked_responses(mock_timeout())
  expect_warning(
    res <- AnthropicProvider$new(model = "claude-opus-4-8")$complete("hi"),
    "network|timeout"
  )
  expect_true(is.na(res$text))
})

test_that("AnthropicProvider degrades a missing/empty content block to one warning + NA", {
  skip_if_not_installed("httr2")
  withr::local_envvar(ANTHROPIC_API_KEY = DUMMY_ANTHROPIC_KEY)
  # 200 OK but no usable text — extraction yields nothing.
  httr2::local_mocked_responses(mock_200(list(content = list())))
  expect_warning(
    res <- AnthropicProvider$new(model = "claude-opus-4-8")$complete("hi"),
    "no completion text"
  )
  expect_true(is.na(res$text))
})

# ---------------------------------------------------------------------------
# Task 3: provider() factory — all three branches live
# ---------------------------------------------------------------------------

test_that("provider() constructs an AnthropicProvider for type = 'anthropic'", {
  withr::local_envvar(EVENTSTUDY_ADVISOR_PROVIDER = NA)
  p <- provider("anthropic", model = "claude-opus-4-8")
  expect_s3_class(p, "AnthropicProvider")
  expect_s3_class(p, "ProviderBase")
})

test_that("provider() constructs an OpenAICompatProvider for type = 'openai'", {
  withr::local_envvar(EVENTSTUDY_ADVISOR_PROVIDER = NA)
  p <- provider("openai", model = "gpt-4o")
  expect_s3_class(p, "OpenAICompatProvider")
})

test_that("provider() constructs a CustomProvider for type = 'custom'", {
  withr::local_envvar(EVENTSTUDY_ADVISOR_PROVIDER = NA)
  p <- provider("custom", fn = function(prompt, schema) "x")
  expect_s3_class(p, "CustomProvider")
})

test_that("provider() honors EVENTSTUDY_ADVISOR_PROVIDER and an explicit type overrides it", {
  withr::local_envvar(EVENTSTUDY_ADVISOR_PROVIDER = "anthropic",
                      EVENTSTUDY_ADVISOR_MODEL = "claude-opus-4-8")
  # Env selects anthropic when type is NULL.
  expect_s3_class(provider(), "AnthropicProvider")
  # Explicit type overrides the env selector.
  expect_s3_class(provider("openai", model = "gpt-4o"), "OpenAICompatProvider")
})

test_that("provider() rejects a bogus type via match.arg", {
  withr::local_envvar(EVENTSTUDY_ADVISOR_PROVIDER = NA)
  expect_error(provider("bogus"))
})
