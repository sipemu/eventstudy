% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{ComparisonPeriodMeanAdjustedModel}
\alias{ComparisonPeriodMeanAdjustedModel}
\title{Comparison Period Mean Adjusted Model}
\description{
Comparison Period Mean Adjusted Model

Comparison Period Mean Adjusted Model
}
\details{
The Comparison Period Mean Adjusted Model is another relatively simple
approach used in event studies to estimate the expected returns of a stock
and calculate its abnormal returns during an event window. This model is
based on the assumption that a stock’s expected return during the event
window is equal to its average return during a comparison period (typically
a pre-event period). This model is particularly useful when researchers want
to control for a stock’s historical performance and do not wish to rely on
market return data.
}
\section{Super class}{
\code{\link[EventStudy:ModelBase]{EventStudy::ModelBase}} -> \code{ComparisonPeriodMeanAdjustedModel}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{model_name}}{Name of the model.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ComparisonPeriodMeanAdjustedModel-fit}{\code{ComparisonPeriodMeanAdjustedModel$fit()}}
\item \href{#method-ComparisonPeriodMeanAdjustedModel-abnormal_returns}{\code{ComparisonPeriodMeanAdjustedModel$abnormal_returns()}}
\item \href{#method-ComparisonPeriodMeanAdjustedModel-clone}{\code{ComparisonPeriodMeanAdjustedModel$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ComparisonPeriodMeanAdjustedModel-fit"></a>}}
\if{latex}{\out{\hypertarget{method-ComparisonPeriodMeanAdjustedModel-fit}{}}}
\subsection{Method \code{fit()}}{
Fit the model with given data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ComparisonPeriodMeanAdjustedModel$fit(data_tbl)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data_tbl}}{Data frame or tibble containing the data to fit.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ComparisonPeriodMeanAdjustedModel-abnormal_returns"></a>}}
\if{latex}{\out{\hypertarget{method-ComparisonPeriodMeanAdjustedModel-abnormal_returns}{}}}
\subsection{Method \code{abnormal_returns()}}{
Calculate the abnormal returns with given data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ComparisonPeriodMeanAdjustedModel$abnormal_returns(data_tbl)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-ComparisonPeriodMeanAdjustedModel-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ComparisonPeriodMeanAdjustedModel-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ComparisonPeriodMeanAdjustedModel$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
