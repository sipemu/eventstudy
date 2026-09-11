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
# 1b. Market Adjusted / Comparison-Mean models -- closed form, no estudy2.
# --------------------------------------------------------------------------
message("\n-- Market Adjusted Model (MacKinlay 1997, restricted market model) --")
ma_est_firm  <- c(0.012, 0.008, 0.015, 0.006, 0.011, 0.009)
ma_est_index <- c(0.010, 0.010, 0.010, 0.010, 0.010, 0.010)
ma_resid <- ma_est_firm - ma_est_index          # AR = R - Rm on estimation window
ma_sigma <- sd(ma_resid)                         # df = m - 1 = 5
ma_evt_firm  <- c(0.030, -0.010, 0.020)
ma_evt_index <- c(0.012, -0.004, 0.008)
ma_ar <- ma_evt_firm - ma_evt_index
cat(sprintf("sigma = %.17g\ndf = %d\n", ma_sigma, length(ma_resid) - 1))
cat("ar =", paste(sprintf("%.17g", ma_ar), collapse = ", "), "\n")

message("\n-- Comparison Period Mean Model (Brown-Warner 1985) --")
cm_est <- c(0.010, 0.014, 0.008, 0.012, 0.006, 0.010)  # mean = 0.01 exactly
cm_evt <- c(0.030, -0.005, 0.018)
cm_mean  <- mean(cm_est)
cm_sigma <- sd(cm_est - cm_mean)                 # df = m - 1 = 5
cm_ar <- cm_evt - cm_mean
cat(sprintf("mean = %.17g\nsigma = %.17g\ndf = %d\n", cm_mean, cm_sigma, length(cm_est) - 1))
cat("ar =", paste(sprintf("%.17g", cm_ar), collapse = ", "), "\n")

# --------------------------------------------------------------------------
# 1c. Multi-factor models (Fama-French 1993/2015, Carhart 1997) -- closed form.
#     One fixed factor design; each model's formula selects its columns. AR =
#     excess_return - predict(lm), df = m - (k + 1).
# --------------------------------------------------------------------------
message("\n-- Multi-factor models (FF3/FF5/Carhart4) --")
fm_me  <- c(0.010, -0.005, 0.008, 0.002, -0.010, 0.006, 0.004, -0.003)
fm_smb <- c(0.002, 0.001, -0.003, 0.004, 0.000, 0.002, -0.001, 0.003)
fm_hml <- c(-0.001, 0.002, 0.001, -0.002, 0.003, 0.000, 0.002, -0.001)
fm_mom <- c(0.003, -0.002, 0.001, 0.002, -0.001, 0.004, 0.000, 0.001)
fm_rmw <- c(0.001, 0.000, 0.002, -0.001, 0.002, -0.001, 0.003, 0.000)
fm_cma <- c(-0.002, 0.001, 0.000, 0.002, -0.001, 0.001, -0.002, 0.003)
fm_resid <- c(0.0010, -0.0008, 0.0012, -0.0009, 0.0007, -0.0011, 0.0006, -0.0007)
fm_excess <- 0.0005 + 1.1 * fm_me + 0.5 * fm_smb - 0.3 * fm_hml +
  0.2 * fm_mom + 0.15 * fm_rmw - 0.1 * fm_cma + fm_resid
fm_est <- data.frame(excess_return = fm_excess, market_excess = fm_me,
                     smb = fm_smb, hml = fm_hml, mom = fm_mom,
                     rmw = fm_rmw, cma = fm_cma)
fm_evt <- data.frame(
  excess_return = c(0.020, -0.008, 0.014),
  market_excess = c(0.012, -0.004, 0.006),
  smb = c(0.001, -0.002, 0.003), hml = c(0.002, 0.001, -0.001),
  mom = c(0.001, 0.002, 0.000), rmw = c(0.002, -0.001, 0.001),
  cma = c(-0.001, 0.000, 0.002)
)
report_factor <- function(label, formula) {
  m <- lm(formula, data = fm_est)
  ar <- fm_evt$excess_return - predict(m, newdata = fm_evt)
  cat(sprintf("%s: sigma = %.17g, df = %d\n", label, summary(m)$sigma, m$df.residual))
  cat("  ar =", paste(sprintf("%.17g", ar), collapse = ", "), "\n")
}
report_factor("FF3", excess_return ~ market_excess + smb + hml)
report_factor("FF5", excess_return ~ market_excess + smb + hml + rmw + cma)
report_factor("Carhart4", excess_return ~ market_excess + smb + hml + mom)

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
