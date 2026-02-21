#' @title EventStudy: Event Study Analysis in R
#'
#' @description
#' Perform financial event study analysis in R. Implements the classical
#' event study methodology (MacKinlay 1997) with multiple return models,
#' parametric and non-parametric test statistics, and visualization tools.
#'
#' @section Return Models:
#' \itemize{
#'   \item \code{\link{MarketModel}} -- OLS market model (single factor)
#'   \item \code{\link{MarketAdjustedModel}} -- Market-adjusted returns
#'   \item \code{\link{ComparisonPeriodMeanAdjustedModel}} -- Mean-adjusted returns
#'   \item \code{\link{FamaFrench3FactorModel}} -- Fama-French 3-factor model
#'   \item \code{\link{FamaFrench5FactorModel}} -- Fama-French 5-factor model
#'   \item \code{\link{Carhart4FactorModel}} -- Carhart 4-factor model
#'   \item \code{\link{GARCHModel}} -- GARCH(1,1) model
#'   \item \code{\link{BHARModel}} -- Buy-and-hold abnormal returns
#'   \item \code{\link{VolumeModel}} -- Volume event study model
#'   \item \code{\link{VolatilityModel}} -- Volatility event study model
#' }
#'
#' @section Test Statistics:
#' \strong{Single-event:}
#' \itemize{
#'   \item \code{\link{ARTTest}} -- Abnormal return t-test
#'   \item \code{\link{CARTTest}} -- Cumulative abnormal return t-test
#'   \item \code{\link{BHARTTest}} -- BHAR t-test
#' }
#' \strong{Multi-event:}
#' \itemize{
#'   \item \code{\link{CSectTTest}} -- Cross-sectional t-test
#'   \item \code{\link{PatellZTest}} -- Patell standardized residual test
#'   \item \code{\link{SignTest}} -- Sign test
#'   \item \code{\link{GeneralizedSignTest}} -- Generalized sign test (Cowan 1992)
#'   \item \code{\link{RankTest}} -- Rank test (Corrado 1989)
#'   \item \code{\link{BMPTest}} -- Boehmer, Musumeci & Poulsen (1991) test
#'   \item \code{\link{CalendarTimePortfolioTest}} -- Calendar-time portfolio test
#' }
#'
#' @section Pipeline:
#' The standard workflow is:
#' \enumerate{
#'   \item Create an \code{\link{EventStudyTask}} with data
#'   \item Define a \code{\link{ParameterSet}} with model and test choices
#'   \item Run \code{\link{run_event_study}(task, parameter_set)} (or
#'     the individual steps: \code{\link{prepare_event_study}},
#'     \code{\link{fit_model}}, \code{\link{calculate_statistics}})
#'   \item Extract results with \code{get_ar()}, \code{get_car()},
#'     \code{get_aar()}, or \code{\link{tidy.EventStudyTask}}
#'   \item Export with \code{\link{export_results}}
#'   \item Visualize with \code{\link{plot_event_study}}
#' }
#'
#' @section Extensions:
#' \itemize{
#'   \item \code{\link{cross_sectional_regression}} -- Explain CARs with firm characteristics
#'   \item \code{\link{IntradayEventStudyTask}} -- Intraday event studies
#'   \item \code{\link{PanelEventStudyTask}} -- Panel DiD event studies
#' }
#'
#' @references
#' MacKinlay, A. C. (1997). Event Studies in Economics and Finance.
#' \emph{Journal of Economic Literature}, 35(1), 13--39.
#'
#' Fama, E. F. and French, K. R. (1993). Common risk factors in the
#' returns on stocks and bonds. \emph{Journal of Financial Economics},
#' 33(1), 3--56.
#'
#' Carhart, M. M. (1997). On persistence in mutual fund performance.
#' \emph{The Journal of Finance}, 52(1), 57--82.
#'
#' @import R6
#' @importFrom dplyr %>% mutate filter select n group_by summarise left_join
#'   rename bind_rows arrange transmute ungroup n_distinct coalesce any_of
#'   case_when
#' @importFrom tibble as_tibble
#' @importFrom rlang .data %||%
#' @importFrom distributional dist_student_t
#' @importFrom plotly plot_ly add_trace layout subplot
#' @importFrom stats sd lm na.omit shapiro.test Box.test acf qnorm pt pnorm
#'   predict poly setNames
#' @keywords internal
# Suppress R CMD check notes for NSE column names used in dplyr/ggplot2
utils::globalVariables(c(
  ".", "aar", "aar_t", "abnormal_returns", "car", "caar",
  "ci_lower", "ci_upper", "data", "estimate", "event_date",
  "event_id", "event_window", "estimation_window",
  "firm_price", "firm_returns", "firm_symbol",
  "group", "index", "index_price", "index_returns",
  "model", "relative_index", "relative_time", "request",
  "risk_free_rate", "se", "statistic", "statistics", "std.error",
  "t_stat", "tidy_data", "timestamp", "tmp_index", "value",
  "sigma", "sar", "csar", "n_pos_car", "n_valid", "sd_caar",
  "centered_rank", "mean_rank", "mean_sar", "sd_sar", "port_sd",
  "n_pos", "n_neg", "n_valid_events", "n_events",
  "total_obs", "ar_rank", "mean_centered_rank",
  "standardized_abnormal_returns",
  "car_window", "lag"
))
"_PACKAGE"
