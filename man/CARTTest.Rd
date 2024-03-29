% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/single_event_test_statistics.R
\name{CARTTest}
\alias{CARTTest}
\title{Cumulative Abnormal Return T Statistic (CART)}
\description{
Cumulative Abnormal Return T Statistic (CART)

Cumulative Abnormal Return T Statistic (CART)
}
\details{
The CAR t-test is a statistical method used to determine whether the
cumulative abnormal return of a security over an event window is
significantly different from zero. This test helps researchers identify
whether the event of interest has a significant impact on the security’s
return over the entire event window, considering the cumulative effects of
the event.

See also \url{https://eventstudy.de/statistics/ar_car_statistics.html}
}
\section{Super class}{
\code{\link[EventStudy:TestStatisticBase]{EventStudy::TestStatisticBase}} -> \code{CARTTest}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{Short code of the test statistic.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CARTTest-compute}{\code{CARTTest$compute()}}
\item \href{#method-CARTTest-clone}{\code{CARTTest$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="EventStudy" data-topic="TestStatisticBase" data-id="initialize"><a href='../../EventStudy/html/TestStatisticBase.html#method-TestStatisticBase-initialize'><code>EventStudy::TestStatisticBase$initialize()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CARTTest-compute"></a>}}
\if{latex}{\out{\hypertarget{method-CARTTest-compute}{}}}
\subsection{Method \code{compute()}}{
Computes the test CAR test statistics for a single event.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CARTTest$compute(data_tbl, model)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data_tbl}}{The data for a single event with
calculated abnormal returns.}

\item{\code{model}}{The fitted model that includes the
necessary information for calculating the test
statistic.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CARTTest-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CARTTest-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CARTTest$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
