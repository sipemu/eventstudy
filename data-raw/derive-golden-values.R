# derive-golden-values.R
#
# NON-SHIPPED derivation script for the Phase 26 golden-value regression net.
# This file lives in data-raw/ which is .Rbuildignore'd, so it is NEVER part of
# the built package tarball and estudy2/eventstudies are NEVER added to
# DESCRIPTION. It exists only to (re)derive and audit the reference constants
# that are pinned as literals in tests/testthat/test_golden_values.R.
#
# Two classes of golden values:
#   1. Published-first constants: taken from published tables / closed-form
#      algebra (MacKinlay 1997, Patell 1976, Boehmer/BMP 1991, Kolari-Pynnonen
#      2010, Fama-French). These need no external package and are the primary
#      source of truth.
#   2. Cross-implementation cross-checks: derived from estudy2 / eventstudies
#      where installed. These are BEST-EFFORT ONLY -- guarded by
#      requireNamespace() so the script degrades cleanly when the packages are
#      absent (they are CRAN-archived and not installed in CI).
#
# Reproducibility: run under `Rscript data-raw/derive-golden-values.R` with the
# package loaded via devtools::load_all(). The sessionInfo() dump below records
# the exact R / package versions used to derive the pinned constants.

message("== derive-golden-values.R ==")
message("Recording session for provenance:")
print(utils::sessionInfo())

if (requireNamespace("devtools", quietly = TRUE)) {
  suppressMessages(devtools::load_all(".", quiet = TRUE))
} else {
  message("devtools not available; load the package manually before sourcing.")
}

# Shared deterministic fixture (mirrors tests/testthat/helper-golden-data.R).
golden_market_model_fixture <- function() {
  est_index <- c(-0.02, -0.01, 0.00, 0.01, 0.02, 0.03)
  est_resid <- c(0.001, -0.001, 0.002, -0.002, 0.001, -0.001)
  est_firm  <- 0.005 + 1.5 * est_index + est_resid
  evt_index <- c(0.015, -0.005, 0.010)
  evt_firm  <- c(0.040, 0.000, 0.030)
  tibble::tibble(
    firm_returns      = c(est_firm, evt_firm),
    index_returns     = c(est_index, evt_index),
    estimation_window = c(rep(1, length(est_firm)), rep(0, length(evt_firm))),
    event_window      = c(rep(0, length(est_firm)), rep(1, length(evt_firm))),
    relative_index    = c(seq(-length(est_firm), -1), seq(0, length(evt_firm) - 1)),
    event_date        = c(rep(0, length(est_firm)), 1, rep(0, length(evt_firm) - 1))
  )
}

# --------------------------------------------------------------------------
# 1. Market Model AR/CAR t-test (MacKinlay 1997) -- closed form, no estudy2.
# --------------------------------------------------------------------------
message("\n-- Market Model AR/CAR t (MacKinlay 1997) --")
est_index <- c(-0.02, -0.01, 0.00, 0.01, 0.02, 0.03)
est_resid <- c(0.001, -0.001, 0.002, -0.002, 0.001, -0.001)
est_firm  <- 0.005 + 1.5 * est_index + est_resid
mm_fit <- lm(est_firm ~ est_index)
mm_sum <- summary(mm_fit)
alpha  <- unname(coef(mm_fit)[1])
beta   <- unname(coef(mm_fit)[2])
sigma  <- mm_sum$sigma                       # residual SE, df = m - 2 = 4
evt_index <- c(0.015, -0.005, 0.010)
evt_firm  <- c(0.040, 0.000, 0.030)
ar    <- evt_firm - (alpha + beta * evt_index)   # AR = R - (a + b Rm), MacKinlay eq.(5)
ar_t  <- ar / sigma                              # per-day AR t, constant-sigma
car   <- cumsum(ar)
Lvec  <- seq_along(ar)
car_t <- car / (sqrt(Lvec) * sigma)              # L-day CAR t, constant-sigma approx

cat(sprintf("alpha = %.15g\nbeta  = %.15g\nsigma = %.15g\ndf    = %d\n",
            alpha, beta, sigma, mm_fit$df.residual))
cat("ar_t  =", paste(sprintf("%.15g", ar_t), collapse = ", "), "\n")
cat("car_t =", paste(sprintf("%.15g", car_t), collapse = ", "), "\n")

# --------------------------------------------------------------------------
# 2. Optional estudy2 / eventstudies cross-checks (best-effort only).
# --------------------------------------------------------------------------
if (requireNamespace("estudy2", quietly = TRUE) ||
    requireNamespace("eventstudies", quietly = TRUE)) {
  message("\nestudy2/eventstudies detected -- cross-implementation checks would run here.")
  # (Cross-impl derivations are added per statistic in later tasks. They are
  #  cross-checks only; the pinned constants above/below are the source of truth.)
} else {
  message(paste0(
    "\nestudy2 / eventstudies not installed -- published/closed-form constants ",
    "are pinned directly in tests/testthat/test_golden_values.R. No estudy2 ",
    "dependency is introduced (this is expected in CI and on CRAN)."
  ))
}

message("\n== done ==")
