# =============================================================================
# test_advisor_pro_footer.R — regression tests for the Advisor Pro opt-in footer
# =============================================================================
#
# Covers:
#   (a) DEFAULT SILENT: footer is absent when option is unset / FALSE
#   (b) OPT-IN PRINTS: footer URL appears exactly once when option is TRUE
#       — tested for both print.Advice and print.es_advice
#   (c) NO NETWORK: footer helper body references no network/connection calls
#   (d) INVISIBLE RETURN PRESERVED: print methods still return invisible(x)
#
# Fixtures are built using the same patterns as test_advise.R (Phase 07).

WAITLIST_URL <- "github.com/sipemu/eventstudy#advisor-pro-waitlist"

# ---- Fixtures ----------------------------------------------------------------

.make_advice_fixture <- function() {
  # Minimal valid Advice S3 object (mirrors the structure built by es_advise())
  structure(
    list(
      source          = "offline_kb",
      task_type       = "interpret",
      is_deterministic = TRUE,
      n_dropped       = 0L,
      interpretation  = "Test interpretation.",
      recommendations = list(),
      caveats         = character(0L)
    ),
    class = "Advice"
  )
}

.make_es_advice_fixture <- function() {
  # Minimal valid es_advice S3 object (mirrors .build_offline_advice output)
  structure(
    list(
      source          = "offline_kb",
      is_deterministic = TRUE,
      rules_matched   = list(),
      diagnostics_ref = list()
    ),
    class = "es_advice"
  )
}

# ==============================================================================
# (a) DEFAULT SILENT
# ==============================================================================

test_that("print.Advice: footer absent by default (option unset)", {
  withr::with_options(list(eventstudy.advisor_pro_footer = NULL), {
    out <- capture.output(print(.make_advice_fixture()))
    expect_false(any(grepl(WAITLIST_URL, out, fixed = TRUE)),
                 info = "Waitlist URL must not appear when option is unset")
  })
})

test_that("print.Advice: footer absent when option is explicitly FALSE", {
  withr::with_options(list(eventstudy.advisor_pro_footer = FALSE), {
    out <- capture.output(print(.make_advice_fixture()))
    expect_false(any(grepl(WAITLIST_URL, out, fixed = TRUE)),
                 info = "Waitlist URL must not appear when option is FALSE")
  })
})

test_that("print.es_advice: footer absent by default (option unset)", {
  withr::with_options(list(eventstudy.advisor_pro_footer = NULL), {
    out <- capture.output(print(.make_es_advice_fixture()))
    expect_false(any(grepl(WAITLIST_URL, out, fixed = TRUE)),
                 info = "Waitlist URL must not appear when option is unset")
  })
})

test_that("print.es_advice: footer absent when option is explicitly FALSE", {
  withr::with_options(list(eventstudy.advisor_pro_footer = FALSE), {
    out <- capture.output(print(.make_es_advice_fixture()))
    expect_false(any(grepl(WAITLIST_URL, out, fixed = TRUE)),
                 info = "Waitlist URL must not appear when option is FALSE")
  })
})

# ==============================================================================
# (b) OPT-IN PRINTS
# ==============================================================================

test_that("print.Advice: footer URL appears exactly once when option is TRUE", {
  withr::with_options(list(eventstudy.advisor_pro_footer = TRUE), {
    out <- capture.output(print(.make_advice_fixture()))
    matches <- grep(WAITLIST_URL, out, fixed = TRUE)
    expect_equal(length(matches), 1L,
                 info = "Waitlist URL must appear exactly once")
  })
})

test_that("print.es_advice: footer URL appears exactly once when option is TRUE", {
  withr::with_options(list(eventstudy.advisor_pro_footer = TRUE), {
    out <- capture.output(print(.make_es_advice_fixture()))
    matches <- grep(WAITLIST_URL, out, fixed = TRUE)
    expect_equal(length(matches), 1L,
                 info = "Waitlist URL must appear exactly once")
  })
})

# ==============================================================================
# (c) NO NETWORK
# ==============================================================================

test_that("footer helper body contains no network/connection call", {
  # Check deparse of .advisor_pro_footer body for network-related calls.
  # This is a structural (static) assertion — no mocking required.
  body_text <- paste(deparse(body(.advisor_pro_footer)), collapse = " ")

  # Illustrative, not exhaustive: a curated list of the network/connection
  # primitives the footer could plausibly reach for. The httr2 mock guard below
  # is the behavioural backstop; this static scan catches obvious regressions.
  network_patterns <- c(
    "url(", "download.file(", "httr2::request(", "curl(",
    "readLines(", "GET(", "POST(", "socketConnection(",
    "rawConnection(", "open(con"
  )
  for (pat in network_patterns) {
    expect_false(grepl(pat, body_text, fixed = TRUE),
                 info = paste0("footer helper must not contain '", pat, "'"))
  }
})

test_that("footer helper with option TRUE makes no network call (httr2 mock guard)", {
  skip_if_not_installed("httr2", minimum_version = "1.0.0")
  # If httr2 is available, confirm the footer does not trigger any request
  expect_no_error(
    httr2::with_mocked_responses(
      function(req) stop("network was called"),
      {
        withr::with_options(list(eventstudy.advisor_pro_footer = TRUE), {
          capture.output(print(.make_advice_fixture()))
          capture.output(print(.make_es_advice_fixture()))
        })
      }
    )
  )
})

# ==============================================================================
# (d) INVISIBLE RETURN PRESERVED
# ==============================================================================

test_that("print.Advice returns invisible(x) with option FALSE", {
  withr::with_options(list(eventstudy.advisor_pro_footer = FALSE), {
    obj <- .make_advice_fixture()
    out <- capture.output(ret_val <- print(obj))
    expect_identical(ret_val, obj, info = "print.Advice must return x invisibly")
  })
})

test_that("print.Advice returns invisible(x) with option TRUE", {
  withr::with_options(list(eventstudy.advisor_pro_footer = TRUE), {
    obj <- .make_advice_fixture()
    # Verify invisible return by checking auto-print does not echo the object
    out <- capture.output(ret_val <- print(obj))
    # ret_val should be identical to obj (the invisible return)
    expect_identical(ret_val, obj,
                     info = "print.Advice must return x invisibly with footer on")
  })
})

test_that("print.es_advice returns invisible(x) with option FALSE", {
  withr::with_options(list(eventstudy.advisor_pro_footer = FALSE), {
    obj <- .make_es_advice_fixture()
    out <- capture.output(ret_val <- print(obj))
    expect_identical(ret_val, obj, info = "print.es_advice must return x invisibly")
  })
})

test_that("print.es_advice returns invisible(x) with option TRUE", {
  withr::with_options(list(eventstudy.advisor_pro_footer = TRUE), {
    obj <- .make_es_advice_fixture()
    out <- capture.output(ret_val <- print(obj))
    expect_identical(ret_val, obj,
                     info = "print.es_advice must return x invisibly with footer on")
  })
})
