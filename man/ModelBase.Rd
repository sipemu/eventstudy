% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{ModelBase}
\alias{ModelBase}
\title{ModelBase}
\description{
Base model class for event study. Each single event study will
get its own model initialization and fitting. Therefore, the input DataFrame
contains the data for a single Event Study. For custom models, except the
child modles of the market model, several statistics must be included, namely
sigma, degree_of_freedom, first_order_auto_correlation, residuals,
forecast_error_corrected_sigma, and forecast_error_corrected_sigma_car. Part
of these statistics are necessary for calculating the Event Study statistics.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{model_name}}{Name of the model.}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{statistics}}{Read-only field to get statistics.}

\item{\code{model}}{Read-only field to get the fitted model.}

\item{\code{is_fitted}}{Read-only field to check if the model is fitted.
Statistics object contains different model specific KPIs
that describes the fitted model.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ModelBase-fit}{\code{ModelBase$fit()}}
\item \href{#method-ModelBase-abnormal_returns}{\code{ModelBase$abnormal_returns()}}
\item \href{#method-ModelBase-clone}{\code{ModelBase$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelBase-fit"></a>}}
\if{latex}{\out{\hypertarget{method-ModelBase-fit}{}}}
\subsection{Method \code{fit()}}{
Fits the model with given data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelBase$fit(data_tbl)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data_tbl}}{A data frame or tibble containing the data to fit.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelBase-abnormal_returns"></a>}}
\if{latex}{\out{\hypertarget{method-ModelBase-abnormal_returns}{}}}
\subsection{Method \code{abnormal_returns()}}{
Calculate the abnormal returns with given data and fitted model.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelBase$abnormal_returns(data_tbl)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data_tbl}}{Data frame or tibble containing the data to calculate abnormal returns.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelBase-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ModelBase-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelBase$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
