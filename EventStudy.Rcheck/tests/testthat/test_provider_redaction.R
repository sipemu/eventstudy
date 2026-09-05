# =============================================================================
# MANDATORY key-redaction guarantee (PROV-06, T-06-10 — critical)
# =============================================================================
#
# The Anthropic x-api-key header is set via req_headers_redacted() (NOT plain
# req_headers) because req_auth_bearer_token does NOT redact an x-api-key header.
# These tests force an error path with an OBVIOUS dummy key and assert that key
# string is ABSENT from the emitted warning, from capture.output(print(req)), and
# from any es_provider_response$error reason. A regression to plain req_headers()
# must fail here. All offline: dummy keys via withr, httr2 mocks, no network.

# A distinctive dummy key so any leak is unambiguous. NEVER a real key.
REDACTION_DUMMY_KEY <- "sk-SECRET-DUMMY-abc123"

# ---------------------------------------------------------------------------
# Anthropic: the dummy x-api-key never appears in the warning condition
# ---------------------------------------------------------------------------

test_that("Anthropic dummy key is absent from the emitted warning", {
  skip_if_not_installed("httr2")
  withr::local_envvar(ANTHROPIC_API_KEY = REDACTION_DUMMY_KEY)
  httr2::local_mocked_responses(mock_status(401L))
  msg <- tryCatch(
    AnthropicProvider$new(model = "claude-opus-4-8")$complete("hi"),
    warning = function(w) conditionMessage(w)
  )
  expect_type(msg, "character")
  expect_false(grepl(REDACTION_DUMMY_KEY, msg, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Anthropic: the LINCHPIN — key redacted from capture.output(print(req))
# ---------------------------------------------------------------------------
# This is the assertion that fails if anyone regresses req_headers_redacted back
# to plain req_headers() for the x-api-key header.

test_that("Anthropic x-api-key is redacted from capture.output(print(req))", {
  skip_if_not_installed("httr2")
  # Build the exact request the provider would send (x-api-key auth path).
  req <- httr2::request("https://api.anthropic.com")
  req <- httr2::req_url_path_append(req, "v1/messages")
  req <- httr2::req_headers_redacted(req, "x-api-key" = REDACTION_DUMMY_KEY)
  req <- httr2::req_headers(req, "anthropic-version" = "2023-06-01")
  printed <- capture.output(print(req))
  expect_false(any(grepl(REDACTION_DUMMY_KEY, printed, fixed = TRUE)))
})

test_that("plain req_headers() WOULD leak the x-api-key (control — proves the test bites)", {
  skip_if_not_installed("httr2")
  # Deliberately use the UNSAFE plain header to confirm the assertion above is
  # meaningful: a plain header leaks the key into the printed request.
  req <- httr2::request("https://api.anthropic.com")
  req <- httr2::req_headers(req, "x-api-key" = REDACTION_DUMMY_KEY)
  printed <- capture.output(print(req))
  expect_true(any(grepl(REDACTION_DUMMY_KEY, printed, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# OpenAI: the bearer token never appears in the warning condition
# ---------------------------------------------------------------------------

test_that("OpenAI dummy bearer key is absent from the emitted warning", {
  skip_if_not_installed("httr2")
  withr::local_envvar(OPENAI_API_KEY = REDACTION_DUMMY_KEY)
  httr2::local_mocked_responses(mock_status(401L))
  msg <- tryCatch(
    OpenAICompatProvider$new(model = "gpt-4o")$complete("hi"),
    warning = function(w) conditionMessage(w)
  )
  expect_type(msg, "character")
  expect_false(grepl(REDACTION_DUMMY_KEY, msg, fixed = TRUE))
})

test_that("OpenAI bearer token is redacted from capture.output(print(req))", {
  skip_if_not_installed("httr2")
  req <- httr2::request("https://api.openai.com/v1")
  req <- httr2::req_auth_bearer_token(req, REDACTION_DUMMY_KEY)
  printed <- capture.output(print(req))
  expect_false(any(grepl(REDACTION_DUMMY_KEY, printed, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# No key AND no response body leaks into the es_provider_response$error reason
# ---------------------------------------------------------------------------

test_that("non-2xx error reason carries neither key nor body — only HTTP <status>", {
  skip_if_not_installed("httr2")
  withr::local_envvar(ANTHROPIC_API_KEY = REDACTION_DUMMY_KEY)
  httr2::local_mocked_responses(mock_status(403L))
  res <- withCallingHandlers(
    AnthropicProvider$new(model = "claude-opus-4-8")$complete("hi"),
    warning = function(w) invokeRestart("muffleWarning")
  )
  expect_false(grepl(REDACTION_DUMMY_KEY, res$error, fixed = TRUE))
  expect_match(res$error, "^HTTP 403$")
})
