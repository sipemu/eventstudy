# =============================================================================
# test_prose_sanitiser.R -- Tests for the per-format prose sanitiser functions
#
# Coverage:
#   FORMAT-04: .sanitise_prose() per-format dispatch
#              .sanitise_universal() smart-quote/dash normalisation
#              .sanitise_for_pdf() LaTeX special escaping
#              .sanitise_for_word() XML entity escaping
#   Security: LaTeX injection (\input, \write18) rendered inert under pdf
#              Word field-code control sequences rendered inert under word
#
# Test input strings containing smart-quotes/dashes use \u Unicode escapes
# so the test source file is ASCII-clean (CRAN non-ASCII hygiene).
# No network, no LLM, no render -- pure string transformation tests.
# =============================================================================

# Helper: U+201C left double quote, U+201D right double quote,
#         U+2018 left single, U+2019 right single, U+2014 em dash, U+2013 en dash
LDQUOTE <- "\u201c"  # left  double quote U+201C
RDQUOTE <- "\u201d"  # right double quote U+201D
LSQUOTE <- "\u2018"  # left  single quote U+2018
RSQUOTE <- "\u2019"  # right single quote U+2019
EMDASH  <- "\u2014"  # em dash U+2014
ENDASH  <- "\u2013"  # en dash U+2013


# ---- .sanitise_universal() -------------------------------------------------

test_that(".sanitise_universal() converts em-dash to '--'", {
  input     <- paste0("result ", EMDASH, " significant")
  sanitised <- EventStudy:::.sanitise_universal(input)
  expect_identical(sanitised, "result -- significant")
})


test_that(".sanitise_universal() converts en-dash to '-'", {
  input     <- paste0("years ", ENDASH, " 2020")
  sanitised <- EventStudy:::.sanitise_universal(input)
  expect_identical(sanitised, "years - 2020")
})


test_that(".sanitise_universal() converts left double quote (U+201C) to straight \"", {
  input     <- paste0(LDQUOTE, "Hello", RDQUOTE)
  sanitised <- EventStudy:::.sanitise_universal(input)
  expect_identical(sanitised, "\"Hello\"")
})


test_that(".sanitise_universal() converts right double quote (U+201D) to straight \"", {
  input     <- paste0("say ", RDQUOTE, " he said")
  sanitised <- EventStudy:::.sanitise_universal(input)
  expect_identical(sanitised, "say \" he said")
})


test_that(".sanitise_universal() converts left single quote (U+2018) to straight '", {
  input     <- paste0(LSQUOTE, "it", RSQUOTE, "s fine")
  sanitised <- EventStudy:::.sanitise_universal(input)
  expect_identical(sanitised, "'it's fine")
})


test_that(".sanitise_universal() converts right single quote (U+2019) to straight '", {
  input     <- paste0("it", RSQUOTE, "s")
  sanitised <- EventStudy:::.sanitise_universal(input)
  expect_identical(sanitised, "it's")
})


test_that(".sanitise_universal() converts combined smart-quotes and em-dash", {
  # Mirrors the RESEARCH.md Spike 2 fixture
  # Input: <LDQUOTE>Hello<RDQUOTE> <EMDASH> it<RSQUOTE>s fine
  # Expected: "Hello" -- it's fine
  input     <- paste0(LDQUOTE, "Hello", RDQUOTE, " ", EMDASH, " it", RSQUOTE, "s fine")
  sanitised <- EventStudy:::.sanitise_universal(input)
  expect_identical(sanitised, "\"Hello\" -- it's fine")
})


test_that(".sanitise_universal() is a no-op on ASCII-only input", {
  input <- "plain ASCII text with no special chars"
  expect_identical(EventStudy:::.sanitise_universal(input), input)
})


# ---- .sanitise_for_pdf() ---------------------------------------------------

test_that(".sanitise_for_pdf() escapes LaTeX specials: % $ & # _ { }", {
  # Fixture containing LaTeX specials (excluding ~ and ^ which get replaced with
  # macro calls like \textasciitilde{} that intentionally contain '{}').
  # Backslash tested separately.
  input     <- "Results: 50% return & $100 profit #1 with {high} var_iance"
  sanitised <- EventStudy:::.sanitise_for_pdf(input)

  # After escaping, no unescaped special from {% $ & # _ { }} should remain.
  # The acceptance criterion regex: (?<!\\)[%$&#_{}]
  expect_false(
    grepl("(?<!\\\\)[%$&#_{}]", sanitised, perl = TRUE),
    info = paste("Unescaped LaTeX special found in:", sanitised)
  )
})


test_that(".sanitise_for_pdf() tilde and caret get macro replacements", {
  # ~ becomes \textasciitilde{} and ^ becomes \textasciicircum{}
  # These are valid LaTeX -- no unescaped ~ or ^ should appear as literals
  input_tilde <- "50~100"
  input_caret <- "x^2"
  expect_false(grepl("(?<!\\\\)~", EventStudy:::.sanitise_for_pdf(input_tilde), perl = TRUE))
  expect_false(grepl("(?<!\\\\)\\^", EventStudy:::.sanitise_for_pdf(input_caret), perl = TRUE))
})


test_that(".sanitise_for_pdf() handles backslash first to avoid double-escaping", {
  # An input with a backslash -- should become \textbackslash{}, NOT \\textbackslash{}
  input     <- "path\\to\\file"
  sanitised <- EventStudy:::.sanitise_for_pdf(input)
  expect_true(grepl("textbackslash", sanitised, fixed = TRUE),
    info = "Backslash should be replaced with \\textbackslash{}")
  # The replacement for backslash itself must not be double-escaped
  expect_false(grepl("\\\\\\\\textbackslash", sanitised, perl = TRUE),
    info = "Backslash must not be double-escaped")
})


test_that(".sanitise_for_pdf() escapes percent sign", {
  input     <- "50% return"
  sanitised <- EventStudy:::.sanitise_for_pdf(input)
  expect_true(grepl("\\%", sanitised, fixed = TRUE))
  expect_false(grepl("(?<!\\\\)%", sanitised, perl = TRUE))
})


test_that(".sanitise_for_pdf() escapes dollar sign", {
  input     <- "value is $100"
  sanitised <- EventStudy:::.sanitise_for_pdf(input)
  expect_true(grepl("\\$", sanitised, fixed = TRUE))
})


test_that(".sanitise_for_pdf() escapes ampersand", {
  input     <- "A & B"
  sanitised <- EventStudy:::.sanitise_for_pdf(input)
  expect_true(grepl("\\&", sanitised, fixed = TRUE))
  expect_false(grepl("(?<!\\\\)&", sanitised, perl = TRUE))
})


test_that(".sanitise_for_pdf() escapes underscore", {
  input     <- "var_name"
  sanitised <- EventStudy:::.sanitise_for_pdf(input)
  expect_true(grepl("\\_", sanitised, fixed = TRUE))
})


test_that(".sanitise_for_pdf() escapes tilde to \\textasciitilde{}", {
  input     <- "tilde ~ here"
  sanitised <- EventStudy:::.sanitise_for_pdf(input)
  expect_true(grepl("textasciitilde", sanitised, fixed = TRUE))
  expect_false(grepl("(?<!\\\\)~", sanitised, perl = TRUE))
})


test_that(".sanitise_for_pdf() escapes caret to \\textasciicircum{}", {
  input     <- "power^2"
  sanitised <- EventStudy:::.sanitise_for_pdf(input)
  expect_true(grepl("textasciicircum", sanitised, fixed = TRUE))
  expect_false(grepl("(?<!\\\\)\\^", sanitised, perl = TRUE))
})


# ---- LaTeX injection security test -----------------------------------------

test_that(".sanitise_for_pdf() neutralises LaTeX \\input injection", {
  # An LLM might try to inject \input{/etc/passwd}. After sanitisation
  # the leading backslash must be escaped so the LaTeX command is inert.
  input     <- "\\input{/etc/passwd}"
  sanitised <- EventStudy:::.sanitise_for_pdf(input)

  # The original \input should NOT appear literally in the output
  # (the backslash is replaced with \textbackslash{})
  expect_false(grepl("\\\\input\\{", sanitised, perl = TRUE),
    info = paste("LaTeX injection not neutralised. Got:", sanitised))

  # The output should contain textbackslash (backslash was escaped)
  expect_true(grepl("textbackslash", sanitised, fixed = TRUE),
    info = "Leading backslash must be replaced with \\textbackslash{}")
})


test_that(".sanitise_for_pdf() neutralises LaTeX \\write18 injection", {
  input     <- "\\write18{rm -rf /}"
  sanitised <- EventStudy:::.sanitise_for_pdf(input)

  # Should NOT have raw \write18 that LaTeX would execute
  expect_false(grepl("^\\\\write18", sanitised, perl = TRUE),
    info = paste("\\write18 injection not neutralised. Got:", sanitised))
  expect_true(grepl("textbackslash", sanitised, fixed = TRUE))
})


# ---- .sanitise_for_word() --------------------------------------------------

test_that(".sanitise_for_word() converts & to &amp;", {
  input     <- "A & B"
  sanitised <- EventStudy:::.sanitise_for_word(input)
  expect_true(grepl("&amp;", sanitised, fixed = TRUE))
  # No raw & should remain (the &amp; itself is the encoded form, no loose &)
  expect_false(grepl("&(?!amp;|lt;|gt;|quot;|#39;)", sanitised, perl = TRUE))
})


test_that(".sanitise_for_word() converts < to &lt;", {
  input     <- "value < 0.05"
  sanitised <- EventStudy:::.sanitise_for_word(input)
  expect_true(grepl("&lt;", sanitised, fixed = TRUE))
  expect_false(grepl("<", sanitised, fixed = TRUE))
})


test_that(".sanitise_for_word() converts > to &gt;", {
  input     <- "p > 0.10"
  sanitised <- EventStudy:::.sanitise_for_word(input)
  expect_true(grepl("&gt;", sanitised, fixed = TRUE))
  expect_false(grepl(">", sanitised, fixed = TRUE))
})


test_that(".sanitise_for_word() converts \" to &quot;", {
  input     <- 'say "hello"'
  sanitised <- EventStudy:::.sanitise_for_word(input)
  expect_true(grepl("&quot;", sanitised, fixed = TRUE))
})


test_that(".sanitise_for_word() converts ' to &#39;", {
  input     <- "it's"
  sanitised <- EventStudy:::.sanitise_for_word(input)
  expect_true(grepl("&#39;", sanitised, fixed = TRUE))
})


test_that(".sanitise_for_word() produces &amp;, &lt;, &gt; from & < >", {
  # Combined fixture
  input     <- "A & B < C > D"
  sanitised <- EventStudy:::.sanitise_for_word(input)
  expect_identical(sanitised, "A &amp; B &lt; C &gt; D")
})


test_that(".sanitise_for_word() does not double-encode ampersand", {
  # & -> &amp; must happen exactly once; not &amp;amp;
  input     <- "risk & return"
  sanitised <- EventStudy:::.sanitise_for_word(input)
  expect_true(grepl("&amp;", sanitised, fixed = TRUE))
  # Critically, &amp; must NOT become &amp;amp; (no double-encode)
  expect_false(grepl("&amp;amp;", sanitised, fixed = TRUE))
})


# ---- .sanitise_prose() dispatcher ------------------------------------------

test_that(".sanitise_prose(text, 'pdf') applies universal then pdf escaping", {
  # Input has smart quotes + percent
  input     <- paste0(LDQUOTE, "results", RDQUOTE, " with 50% return")
  sanitised <- EventStudy:::.sanitise_prose(input, "pdf")

  # Smart quote should be normalised to ASCII "
  expect_true(grepl("\"results\"", sanitised, fixed = TRUE),
    info = "Smart quotes should be normalised to ASCII")

  # Percent should be LaTeX-escaped
  expect_false(grepl("(?<!\\\\)%", sanitised, perl = TRUE),
    info = "Percent must be LaTeX-escaped")
})


test_that(".sanitise_prose(text, 'word') applies universal then XML entity escaping", {
  input     <- paste0(LDQUOTE, "value", RDQUOTE, " & test < 0.05")
  sanitised <- EventStudy:::.sanitise_prose(input, "word")

  # Smart quotes normalised to ASCII " then XML-escaped to &quot;
  expect_true(grepl("&quot;value&quot;", sanitised, fixed = TRUE),
    info = paste("Smart quotes should be normalized then XML-encoded. Got:", sanitised))
  # & escaped
  expect_true(grepl("&amp;", sanitised, fixed = TRUE))
  # < escaped
  expect_true(grepl("&lt;", sanitised, fixed = TRUE))
})


test_that(".sanitise_prose(text, 'html') applies universal normalisation only", {
  input     <- paste0(LDQUOTE, "Hello", RDQUOTE, " ", EMDASH, " world")
  sanitised <- EventStudy:::.sanitise_prose(input, "html")

  # Smart quotes normalised
  expect_true(grepl("\"Hello\"", sanitised, fixed = TRUE))
  # Em dash normalised
  expect_true(grepl("--", sanitised, fixed = TRUE))
  # No LaTeX escaping applied (% stays as %)
  input2     <- "50% return"
  sanitised2 <- EventStudy:::.sanitise_prose(input2, "html")
  expect_true(grepl("%", sanitised2, fixed = TRUE),
    info = "html mode must not apply LaTeX escaping")
})


test_that(".sanitise_prose(text, 'md') applies universal normalisation only", {
  input     <- paste0(EMDASH, " it", RSQUOTE, "s fine")
  sanitised <- EventStudy:::.sanitise_prose(input, "md")
  expect_identical(sanitised, "-- it's fine")
})


test_that(".sanitise_prose() passes through unchanged after universal for unknown format", {
  input     <- paste0(LDQUOTE, "test", RDQUOTE)
  sanitised <- EventStudy:::.sanitise_prose(input, "unknown_format")
  # After universal normalisation, smart quotes become ASCII "
  expect_identical(sanitised, "\"test\"")
})
