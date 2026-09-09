# Snapshot lock for the prose sanitiser (FORMAT-04, v0.64.0 invariants).
#
# Locks the LOCKED ampersand-first ordering and the exact escape output of
# each sanitiser variant so this phase (API & message polish) cannot perturb
# them. These functions are NOT edited in this phase -- snapshot only.
#
# One fixed ASCII input string exercising every escape class: ampersand,
# angle brackets, LaTeX specials, quotes, hash, dollar, percent, underscore,
# tilde, caret, backslash, braces. ASCII-only source (CRAN guardrail).

.prose_fixture <- paste0(
  "Returns rose 5% & fell <10> for A_B firms; ",
  "cost $3 #tag {x} ~approx ^power \\path \"quoted\" 'single'."
)

test_that(".sanitise_universal snapshot", {
  expect_snapshot(cat(EventStudy:::.sanitise_universal(.prose_fixture), sep = "\n"))
})

test_that(".sanitise_for_pdf snapshot", {
  expect_snapshot(cat(EventStudy:::.sanitise_for_pdf(.prose_fixture), sep = "\n"))
})

test_that(".sanitise_for_word snapshot", {
  expect_snapshot(cat(EventStudy:::.sanitise_for_word(.prose_fixture), sep = "\n"))
})

test_that(".sanitise_prose dispatcher snapshot (pdf + word)", {
  expect_snapshot(cat(EventStudy:::.sanitise_prose(.prose_fixture, "pdf"), sep = "\n"))
  expect_snapshot(cat(EventStudy:::.sanitise_prose(.prose_fixture, "word"), sep = "\n"))
})
