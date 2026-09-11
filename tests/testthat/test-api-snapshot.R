# Structural API snapshot — EventStudy public surface lock
#
# Implements APIS-03. Pins the full public signature surface STRUCTURALLY:
#   - sorted getNamespaceExports("EventStudy")
#   - per exported plain function: formals() as sorted arg-name -> deparsed
#     default pairs (deterministic order; no print() output)
#   - sorted list of R6ClassGenerator exports
#   - sorted S3method() registrations from NAMESPACE
#
# Install-gated: runs only against the installed package so that a
# load_all-vs-installed divergence also fails CI (CI wiring is Phase 29).
# An accidental signature break — renamed arg, added/removed export, changed
# default — will trip expect_snapshot_value() and require an explicit
# testthat::snapshot_accept() in the diff.
#
# Storage: testthat manages the value under tests/testthat/_snaps/ via
# expect_snapshot_value(style = "json2").

test_that("APIS-03: structural API surface snapshot (exports + formals + S3 methods)", {
  skip_if_not_installed("EventStudy")

  ns <- getNamespace("EventStudy")
  pkg_exports <- sort(getNamespaceExports("EventStudy"))

  # --- Plain functions: sorted arg-names -> deparsed defaults ---
  fns_surface <- list()
  r6_classes   <- character(0)
  other_exports <- list()

  for (sym in pkg_exports) {
    obj <- tryCatch(get(sym, envir = ns), error = function(e) NULL)
    if (is.null(obj)) next

    if (inherits(obj, "R6ClassGenerator")) {
      r6_classes <- c(r6_classes, sym)
    } else if (is.function(obj)) {
      f <- formals(obj)
      # Sort arg names so order of declaration does not affect the snapshot.
      sorted_args <- sort(names(f))
      fns_surface[[sym]] <- stats::setNames(
        lapply(sorted_args, function(a) deparse(f[[a]])),
        sorted_args
      )
    } else {
      # Non-function, non-R6 (e.g. es_colours character vector): record class only.
      other_exports[[sym]] <- class(obj)
    }
  }

  # --- S3 method registrations from the installed NAMESPACE file ---
  ns_lines <- readLines(system.file("NAMESPACE", package = "EventStudy"))
  s3_methods <- sort(grep("^S3method\\(", ns_lines, value = TRUE))

  # Assemble the full structural surface object.
  # All sub-lists are explicitly sorted so the snapshot never churns on
  # declaration-order changes.
  surface <- list(
    exports      = pkg_exports,        # already sorted above
    functions    = fns_surface,        # sorted by export name; each entry sorted by arg name
    r6_classes   = sort(r6_classes),   # sorted
    other_exports = other_exports,     # sorted by iteration order (pkg_exports is sorted)
    s3_methods   = s3_methods          # sorted
  )

  testthat::expect_snapshot_value(surface, style = "json2")
})
