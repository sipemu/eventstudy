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
# 1d. Specialized / time-varying models -- closed form, no estudy2 (Task 3).
#     BHAR (Barber-Lyon 1997), Volume, Volatility, Rolling-Window. GARCH/DCC are
#     identity-pinned (skip-guarded on rugarch/rmgarch) so no fitted constant is
#     derived here -- their AR is exactly firm - (mu + beta * index) for any fit.
# --------------------------------------------------------------------------
message("\n-- BHAR Model + BHARTTest (Barber-Lyon 1997) --")
bhar_est_firm  <- c(0.020, 0.010, 0.030, 0.005, 0.015, 0.025)
bhar_est_index <- c(0.010, 0.012, 0.020, 0.008, 0.010, 0.015)
bhar_evt_firm  <- c(0.040, -0.010, 0.030)
bhar_evt_index <- c(0.015, -0.005, 0.010)
bhar_sigma <- sd(bhar_est_firm - bhar_est_index)            # df = m - 1 = 5
bhar_ar <- cumprod(1 + bhar_evt_firm) - cumprod(1 + bhar_evt_index)
bhar_se <- bhar_sigma * sqrt(seq_along(bhar_evt_firm))       # Lyon-Barber-Tsai sqrt(n)
bhar_t  <- bhar_ar / bhar_se
cat(sprintf("sigma = %.17g, df = %d\n", bhar_sigma, length(bhar_est_firm) - 1))
cat("ar =", paste(sprintf("%.17g", bhar_ar), collapse = ", "), "\n")
cat("se =", paste(sprintf("%.17g", bhar_se), collapse = ", "), "\n")
cat("t  =", paste(sprintf("%.17g", bhar_t),  collapse = ", "), "\n")

message("\n-- Volume Model (log-mean abnormal volume) --")
vol_est <- c(1000, 1200, 900, 1100, 1050, 950)
vol_evt <- c(2000, 800, 1500)
vol_expected <- mean(log(vol_est + 1))                       # log_transform = TRUE
vol_sigma <- sd(log(vol_est + 1) - vol_expected)            # df = m - 1 = 5
vol_ar <- log(vol_evt + 1) - vol_expected
cat(sprintf("expected = %.17g, sigma = %.17g, df = %d\n",
            vol_expected, vol_sigma, length(vol_est) - 1))
cat("ar =", paste(sprintf("%.17g", vol_ar), collapse = ", "), "\n")

message("\n-- Volatility Model (squared-return / variance ratio) --")
volat_est <- c(0.02, -0.01, 0.03, -0.02, 0.015, -0.005)
volat_evt <- c(0.05, -0.04, 0.01)
est_var <- var(volat_est)
volat_sigma <- sd(volat_est^2 / est_var - 1)               # df = m - 1 = 5
volat_ar <- volat_evt^2 / est_var - 1
cat(sprintf("est_var = %.17g, sigma = %.17g, df = %d\n",
            est_var, volat_sigma, length(volat_est) - 1))
cat("ar =", paste(sprintf("%.17g", volat_ar), collapse = ", "), "\n")

message("\n-- Rolling-Window Model (single-window OLS on m = 30 fixture) --")
# window_size default 60, min_obs default 30; ws = min(60, 30) = 30 -> single
# window = full-sample OLS; df = max(ws - 2, 1) = 28.
n_est <- 30L
rw_index <- seq(-0.03, 0.03, length.out = n_est)
rw_resid <- rep(c(0.001, -0.001, 0.0015, -0.0015, 0.0005, -0.0005), length.out = n_est)
rw_firm  <- 0.004 + 1.2 * rw_index + rw_resid
rw_fit <- lm(rw_firm ~ rw_index)
rw_alpha <- unname(coef(rw_fit)[1]); rw_beta <- unname(coef(rw_fit)[2])
# model uses denom = max(ws - 2, 1); lm's residual SE uses n - 2 = 28 -> identical
rw_sigma <- summary(rw_fit)$sigma
rw_evt_index <- c(0.02, -0.01, 0.015)
rw_evt_firm  <- c(0.050, 0.000, 0.040)
rw_ar <- rw_evt_firm - (rw_alpha + rw_beta * rw_evt_index)
cat(sprintf("alpha = %.17g, beta = %.17g, sigma = %.17g, df = %d\n",
            rw_alpha, rw_beta, rw_sigma, max(min(60L, n_est) - 2L, 1L)))
cat("ar =", paste(sprintf("%.17g", rw_ar), collapse = ", "), "\n")

message(paste0(
  "\nGARCH / DCC-GARCH: no fitted constant derived. Their abnormal return is the\n",
  "exact identity AR = firm - (mu + beta * index) for ANY fitted coefficients,\n",
  "so test_golden_values.R pins that identity guarded by\n",
  "skip_if_not_installed('rugarch') / ('rmgarch') -- avoiding a version-fragile pin."
))

# --------------------------------------------------------------------------
# 1e. Cross-sectional test statistics -- closed form, no estudy2 (Task 4).
#     Three Market-Model events (m = 8, L = 3, k = 2). CSectT (Brown-Warner
#     1985), Patell Z (Patell 1976), BMP (Boehmer-Musumeci-Poulsen 1991).
# --------------------------------------------------------------------------
message("\n-- Cross-sectional statistics (CSectT / Patell / BMP) --")
me_spec <- list(
  E1 = list(est_index = c(-0.02, -0.01, 0.00, 0.01, 0.02, 0.03, -0.015, 0.005),
            est_resid = c(0.001, -0.001, 0.002, -0.002, 0.0015, -0.0015, 0.0005, -0.0005),
            alpha = 0.004, beta = 1.2,
            evt_index = c(0.015, -0.005, 0.010), evt_firm = c(0.040, 0.000, 0.030)),
  E2 = list(est_index = c(-0.018, -0.008, 0.002, 0.012, 0.022, 0.028, -0.012, 0.008),
            est_resid = c(0.0012, -0.0008, 0.0018, -0.0016, 0.0010, -0.0012, 0.0006, -0.0010),
            alpha = 0.003, beta = 1.0,
            evt_index = c(0.012, -0.004, 0.009), evt_firm = c(0.030, -0.010, 0.020)),
  E3 = list(est_index = c(-0.025, -0.012, 0.001, 0.010, 0.020, 0.030, -0.010, 0.006),
            est_resid = c(0.0008, -0.0012, 0.0016, -0.0014, 0.0012, -0.0010, 0.0004, -0.0004),
            alpha = 0.005, beta = 1.4,
            evt_index = c(0.018, -0.006, 0.011), evt_firm = c(0.050, -0.005, 0.035))
)
me_k <- 2L
me_fit <- function(s) {
  est_firm <- s$alpha + s$beta * s$est_index + s$est_resid
  fit <- lm(est_firm ~ s$est_index)
  a <- unname(coef(fit)[1]); b <- unname(coef(fit)[2])
  sigma <- summary(fit)$sigma                 # df = m - 2 = 6
  ar <- s$evt_firm - (a + b * s$evt_index)
  m <- length(est_firm)
  mean_rm <- mean(s$est_index)
  ss_mkt <- sum((s$est_index - mean_rm)^2)
  fec <- sigma * sqrt(1 + 1 / m + (s$evt_index - mean_rm)^2 / ss_mkt)
  list(sigma = sigma, ar = ar, fec = fec, m = m)
}
me_fits <- lapply(me_spec, me_fit)
N <- length(me_fits)

# CSectT (Brown-Warner 1985)
ar_by_day <- sapply(me_fits, function(f) f$ar)          # day x event
aar   <- rowMeans(ar_by_day)
sd_ar <- apply(ar_by_day, 1, sd)
aar_t <- sqrt(N) * aar / sd_ar
car_by_day <- apply(ar_by_day, 2, cumsum)
caar   <- rowMeans(car_by_day)
sd_caar <- apply(car_by_day, 1, sd)
caar_t <- sqrt(N) * caar / sd_caar
cat("CSectT aar   =", paste(sprintf("%.17g", aar),   collapse = ", "), "\n")
cat("CSectT aar_t =", paste(sprintf("%.17g", aar_t), collapse = ", "), "\n")
cat("CSectT caar  =", paste(sprintf("%.17g", caar),  collapse = ", "), "\n")
cat("CSectT caar_t=", paste(sprintf("%.17g", caar_t),collapse = ", "), "\n")

# Patell (1976): SAR = AR / fec_sigma; Q_i = (m-k)/(m-k-2); Q_total = sqrt(sum Q_i)
sar_p <- sapply(seq_along(me_fits), function(j) me_fits[[j]]$ar / me_fits[[j]]$fec)
Q_i <- sapply(me_fits, function(f) (f$m - me_k) / (f$m - me_k - 2))
Q_total <- sqrt(sum(Q_i))
aar_z <- rowSums(sar_p) / Q_total
csar_p <- sapply(seq_along(me_fits), function(j) {
  s <- cumsum(sar_p[, j]); n <- seq_along(s); s / sqrt(n * Q_i[j])
})
caar_z <- (1 / sqrt(N)) * rowSums(csar_p)
cat("Patell Q_i   =", paste(sprintf("%.17g", Q_i), collapse = ", "), "\n")
cat("Patell aar_z =", paste(sprintf("%.17g", aar_z),  collapse = ", "), "\n")
cat("Patell caar_z=", paste(sprintf("%.17g", caar_z), collapse = ", "), "\n")

# BMP (1991): SAR = AR / model sigma; bmp_t = sqrt(N)*mean(SAR)/sd(SAR)
sar_b <- sapply(seq_along(me_fits), function(j) me_fits[[j]]$ar / me_fits[[j]]$sigma)
mean_sar <- rowMeans(sar_b)
bmp_t <- sqrt(N) * mean_sar / apply(sar_b, 1, sd)
csar_b <- apply(sar_b, 2, cumsum)
cbmp_t <- sqrt(N) * rowMeans(csar_b) / apply(csar_b, 1, sd)
cat("BMP mean_sar =", paste(sprintf("%.17g", mean_sar), collapse = ", "), "\n")
cat("BMP bmp_t    =", paste(sprintf("%.17g", bmp_t),    collapse = ", "), "\n")
cat("BMP cbmp_t   =", paste(sprintf("%.17g", cbmp_t),   collapse = ", "), "\n")

# --------------------------------------------------------------------------
# 1f. Nonparametric / correlation-robust statistics -- closed form (Task 5).
#     Same three-event fixture (m = 8, L = 3, k = 2, N = 3). SignTest,
#     GeneralizedSignTest (Cowan 1992), RankTest (Corrado 1989),
#     CalendarTimePortfolioTest, KolariPynnonenTest (Kolari-Pynnonen 2010).
#
#     NOTE (audit finding, Task 5): RankTest$rank_z and
#     CalendarTimePortfolioTest$caltime_t / ccaltime_t previously used base
#     ifelse() with a SCALAR condition (S_rank / ts_sd), so the per-day z/t
#     vector was silently collapsed to its first element and recycled across
#     every event day. Both were fixed in R/multi_event_test_statistics.R to
#     divide the vector directly (guarding the scalar denominator with a plain
#     if()). The constants derived below are the CORRECT per-day values that the
#     fixed code now returns, matched by the golden tests.
# --------------------------------------------------------------------------
message("\n-- Nonparametric / correlation-robust statistics (Task 5) --")
ks_spec <- me_spec           # reuse the three-event fixture from section 1e
ks_fit_ar <- function(s) {
  est_firm <- s$alpha + s$beta * s$est_index + s$est_resid
  fit <- lm(est_firm ~ s$est_index)
  a <- unname(coef(fit)[1]); b <- unname(coef(fit)[2])
  list(sigma = summary(fit)$sigma,
       ar    = s$evt_firm - (a + b * s$evt_index),
       est_ar = residuals(fit))     # estimation-window abnormal returns
}
ks_fits <- lapply(ks_spec, ks_fit_ar)
ar_mat  <- sapply(ks_fits, function(f) f$ar)         # day x event (L = 3)
Nk      <- ncol(ar_mat)
days_k  <- 0:(nrow(ar_mat) - 1)

# SignTest: n_pos = sum(AR > 0) (strict >, zeros counted as non-positive);
# z = (n_pos - 0.5 N) / (0.5 sqrt(N)); N >= 2 guard.
n_pos   <- apply(ar_mat, 1, function(x) sum(x > 0))
n_val   <- rep(Nk, nrow(ar_mat))
sign_z  <- (n_pos - 0.5 * n_val) / (0.5 * sqrt(n_val))
car_mat <- apply(ar_mat, 2, cumsum)
n_pos_c <- apply(car_mat, 1, function(x) sum(x > 0))
csign_z <- (n_pos_c - 0.5 * n_val) / (0.5 * sqrt(n_val))
cat("Sign sign_z  =", paste(sprintf("%.17g", sign_z),  collapse = ", "), "\n")
cat("Sign csign_z =", paste(sprintf("%.17g", csign_z), collapse = ", "), "\n")

# GeneralizedSignTest (Cowan 1992): p_hat = mean over firms of
# mean(estimation AR > 0); z = (n_pos - N p_hat) / sqrt(N p_hat (1 - p_hat)).
p_hat_firm <- sapply(ks_fits, function(f) mean(f$est_ar > 0))
p_hat      <- mean(p_hat_firm)
gsign_z    <- (n_pos   - n_val * p_hat) / sqrt(n_val * p_hat * (1 - p_hat))
cgsign_z   <- (n_pos_c - n_val * p_hat) / sqrt(n_val * p_hat * (1 - p_hat))
cat(sprintf("GSign p_hat = %.17g\n", p_hat))
cat("GSign gsign_z  =", paste(sprintf("%.17g", gsign_z),  collapse = ", "), "\n")
cat("GSign cgsign_z =", paste(sprintf("%.17g", cgsign_z), collapse = ", "), "\n")

# RankTest (Corrado 1989): rank AR within each firm over the COMBINED
# estimation+event window; centered rank K = rank / (T + 1) - 0.5; per relative
# day take the cross-firm mean centered rank; S_rank = sd of the per-day mean
# centered rank across ALL combined days; rank_z = mean_rank_day / S_rank.
crank_by_event <- lapply(ks_fits, function(f) {
  v <- c(f$est_ar, f$ar); Tt <- length(v)
  rank(v) / (Tt + 1) - 0.5
})
# combined-window relative indices: estimation days then event days
comb_rel <- c(seq(-8, -1), days_k)
crank_mat <- sapply(crank_by_event, identity)        # combined_day x event
mean_crank_day_all <- rowMeans(crank_mat)
S_rank <- sd(mean_crank_day_all)
mean_rank_evt <- rowMeans(tail(crank_mat, length(days_k)))
rank_z <- mean_rank_evt / S_rank
cat(sprintf("Rank S_rank = %.17g\n", S_rank))
cat("Rank mean_rank =", paste(sprintf("%.17g", mean_rank_evt), collapse = ", "), "\n")
cat("Rank rank_z    =", paste(sprintf("%.17g", rank_z),        collapse = ", "), "\n")

# CalendarTimePortfolioTest: equal-weight portfolio AAR per day; time-series
# t = AAR / sd(AAR) and CAAR / (sd(AAR) sqrt(L)); sd over the L portfolio days.
aar_ct  <- rowMeans(ar_mat)
caar_ct <- cumsum(aar_ct)
ts_sd   <- sd(aar_ct)
caltime_t  <- aar_ct  / ts_sd
ccaltime_t <- caar_ct / (ts_sd * sqrt(seq_along(aar_ct)))
cat(sprintf("CalTime ts_sd = %.17g\n", ts_sd))
cat("CalTime caltime_t  =", paste(sprintf("%.17g", caltime_t),  collapse = ", "), "\n")
cat("CalTime ccaltime_t =", paste(sprintf("%.17g", ccaltime_t), collapse = ", "), "\n")

# KolariPynnonenTest (Kolari-Pynnonen 2010): BMP scaled by
# kp_adj = sqrt((1 - r_bar) / (1 + (N - 1) r_bar)), r_bar = average pairwise
# correlation of estimation-window SARs (SAR = AR / model sigma).
sig_k    <- sapply(ks_fits, function(f) f$sigma)
sar_evt  <- sweep(ar_mat, 2, sig_k, "/")
mean_sar <- rowMeans(sar_evt)
bmp_t    <- sqrt(Nk) * mean_sar / apply(sar_evt, 1, sd)
csar_evt <- apply(sar_evt, 2, cumsum)
cbmp_t   <- sqrt(Nk) * rowMeans(csar_evt) / apply(csar_evt, 1, sd)
est_sar  <- sapply(ks_fits, function(f) f$est_ar / f$sigma)   # est_day x event
cor_m    <- cor(est_sar, use = "pairwise.complete.obs")
r_bar    <- (sum(cor_m) - Nk) / (Nk * (Nk - 1))
kp_adj   <- sqrt((1 - r_bar) / (1 + (Nk - 1) * r_bar))
kp_t     <- bmp_t  * kp_adj
ckp_t    <- cbmp_t * kp_adj
cat(sprintf("KP r_bar = %.17g, kp_adj = %.17g\n", r_bar, kp_adj))
cat("KP kp_t  =", paste(sprintf("%.17g", kp_t),  collapse = ", "), "\n")
cat("KP ckp_t =", paste(sprintf("%.17g", ckp_t), collapse = ", "), "\n")

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
