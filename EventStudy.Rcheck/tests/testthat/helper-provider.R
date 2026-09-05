# Shared fixtures for the provider suite.
#
# Automatically loaded by testthat before running tests. This file is EXTENDED by
# the HTTP-provider plans (06-2 OpenAI-compatible, 06-3 Anthropic) with
# httr2::with_mocked_responses mock-response builders (mock_200 / mock_status /
# mock_malformed / mock_timeout) and the mandatory key-redaction fixtures. In
# 06-1 it carries only the dummy-key / env-var constants used by the CustomProvider
# and resolution tests — none of which touch the network.

# A dummy API key used with withr::local_envvar in tests. It is intentionally
# obvious so a leak assertion (added with the HTTP providers) is unambiguous.
# NEVER a real key.
DUMMY_OPENAI_KEY    <- "sk-DUMMYKEY-openai-should-never-print"
DUMMY_ANTHROPIC_KEY <- "sk-ant-DUMMYKEY-should-never-print"

# ---------------------------------------------------------------------------
# Offline httr2 mock-response builders (the CRAN-safe HTTP harness)
# ---------------------------------------------------------------------------
#
# Each builder returns a `function(req) -> httr2_response` suitable for
# httr2::local_mocked_responses() / with_mocked_responses(). They construct real
# httr2 response objects so resp_status() / resp_body_json() behave exactly as in
# production — no network, no keys. Added in 06-2 (OpenAI); reused unchanged by
# 06-3 (Anthropic).

# A well-formed 200 whose JSON body is `body` (an R list).
mock_200 <- function(body) {
  function(req) httr2::response_json(status = 200L, body = body)
}

# A bare non-2xx response (e.g. 401, 404, 500, 503) with no body.
mock_status <- function(code) {
  function(req) httr2::response(status_code = code)
}

# A 200 OK whose body is invalid JSON — resp_body_json() must trap, not crash.
mock_malformed <- function() {
  function(req) httr2::response(status_code = 200L, body = charToRaw("{not json"))
}

# A transport/timeout failure: req_perform() raises an httr2_failure condition.
mock_timeout <- function() {
  function(req) {
    stop(structure(
      class = c("httr2_failure", "error", "condition"),
      list(message = "timed out")
    ))
  }
}
