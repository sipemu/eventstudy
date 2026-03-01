#' @noRd
.ai_system_prompt <- function() {
  paste0(
    "You are a financial econometrics expert writing sections of an event study report. ",
    "You write for an audience of finance researchers and practitioners who understand ",
    "statistical concepts but want clear, precise interpretation of results.\n\n",
    "Guidelines:\n",
    "- Be precise with numbers: report exact values from the data provided.\n",
    "- Use proper statistical language (e.g., 'statistically significant at the 5% level').\n",
    "- Distinguish between economic significance and statistical significance.\n",
    "- Reference specific test statistics and their values.\n",
    "- When discussing model assumptions, explain practical implications of violations.\n",
    "- Do not speculate beyond what the data shows.\n",
    "- Write in third person, academic style.\n",
    "- Use percentage points (pp) for abnormal returns, not percentages of percentages.\n",
    "- Format numbers consistently: 4 decimal places for returns, 2 for t-statistics."
  )
}


#' @noRd
.ai_prompt_executive_summary <- function(context) {
  paste0(
    "Write an executive summary (approximately 500 tokens) for an event study report.\n\n",
    "## Study Configuration\n",
    "- Study type: ", context$study_type, "\n",
    "- Number of events: ", context$n_events, "\n",
    "- Number of groups: ", context$n_groups, "\n",
    "- Groups: ", paste(context$groups, collapse = ", "), "\n",
    "- Return model: ", context$return_model, "\n",
    "- Event window: [", context$event_window[1], ", ", context$event_window[2], "]\n",
    "- Estimation window length: ", context$estimation_window_length, " days\n",
    "\n## Key Results\n",
    context$key_results_text, "\n",
    "\nWrite a concise executive summary covering: the study design, key CAAR findings, ",
    "statistical significance, and one-paragraph conclusion. Start directly with the summary."
  )
}


#' @noRd
.ai_prompt_result_interpretation <- function(context) {
  paste0(
    "Write a detailed interpretation (approximately 800 tokens) of the event study results.\n\n",
    "## AAR/CAAR Results by Group\n",
    context$aar_caar_text, "\n",
    "\n## Single Event Summary\n",
    context$single_event_text, "\n",
    "\nProvide a plain-language reading of the AAR and CAAR tables. For each group:\n",
    "1. Describe the pattern of abnormal returns around the event date.\n",
    "2. Identify the event window days with the largest effects.\n",
    "3. Discuss the direction and magnitude of the cumulative effect.\n",
    "4. Note any differences between groups if multiple groups exist.\n",
    "Start directly with the interpretation."
  )
}


#' @noRd
.ai_prompt_test_guidance <- function(context) {
  paste0(
    "Write test statistic guidance (approximately 400 tokens) for this event study.\n\n",
    "## Diagnostics Summary\n",
    context$diagnostics_text, "\n",
    "\n## Available Test Statistics\n",
    context$test_statistics_text, "\n",
    "\nBased on the diagnostic results, recommend which test statistics are most ",
    "appropriate for this study. Consider:\n",
    "1. Normality of residuals (Shapiro-Wilk results) - if violated, prefer non-parametric tests.\n",
    "2. Autocorrelation (Durbin-Watson, Ljung-Box) - if present, prefer robust tests like Kolari-Pynnonen.\n",
    "3. Cross-sectional correlation and event clustering - if present, prefer generalized sign or rank tests.\n",
    "4. Sample size - with few events, non-parametric tests may lack power.\n",
    "Explain why each recommendation is appropriate given the specific diagnostic values. ",
    "Start directly with the guidance."
  )
}


#' @noRd
.ai_prompt_model_validation <- function(context) {
  paste0(
    "Write a model validation assessment (approximately 600 tokens) for this event study.\n\n",
    "## Model Diagnostics\n",
    context$diagnostics_text, "\n",
    "\n## Pre-trend Test Results\n",
    context$pretrend_text, "\n",
    "\n## Model Fit Statistics\n",
    context$model_fit_text, "\n",
    "\nAssess the validity of the event study model by interpreting:\n",
    "1. **Shapiro-Wilk test**: Are residuals normally distributed? What p-values were observed?\n",
    "2. **Durbin-Watson statistic**: Is there first-order autocorrelation? Values near 2 indicate no autocorrelation.\n",
    "3. **Ljung-Box test**: Is there higher-order serial correlation in residuals?\n",
    "4. **R-squared**: How well does the model explain return variation? What is the range across events?\n",
    "5. **Pre-trend test**: Are there significant pre-event abnormal returns suggesting anticipation or misspecification?\n",
    "For each diagnostic, state whether the assumption is met and discuss practical implications. ",
    "Start directly with the validation assessment."
  )
}


#' @noRd
.ai_prompt_cross_sectional <- function(context) {
  paste0(
    "Write an interpretation (approximately 400 tokens) of the cross-sectional regression results.\n\n",
    "## Cross-Sectional Regression\n",
    context$cross_sectional_text, "\n",
    "\nInterpret the cross-sectional regression results:\n",
    "1. Which firm characteristics significantly explain variation in CARs?\n",
    "2. What is the direction and magnitude of each significant coefficient?\n",
    "3. How much of the cross-sectional variation is explained (R-squared)?\n",
    "4. Are the standard errors robust (HC1)? Discuss implications.\n",
    "5. Note any coefficients that are economically meaningful but statistically insignificant.\n",
    "Start directly with the interpretation."
  )
}
