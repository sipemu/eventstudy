## Colour palette ----

#' EventStudy Colour Palette
#'
#' A named character vector of hex colours for EventStudy plots, anchored on
#' the Okabe-Ito colorblind-safe (Color Universal Design) qualitative set with
#' the EventStudy brand primary blue as the leading colour.
#'
#' Semantic roles: \code{primary} (main series line/point/fill), \code{event}
#' (event-date marker), \code{reference} (zero lines, ACF baseline),
#' \code{ci_band} (confidence ribbon fill; apply alpha at the geom level), and
#' \code{group1}--\code{group8} (qualitative multi-series palette in
#' Okabe-Ito order).
#'
#' @format A named character vector of length 12. Every value is a six-digit
#'   hex colour (\code{^#[0-9A-Fa-f]\{6\}$}).
#' @seealso \code{\link{theme_eventstudy}}, \code{\link{plot_event_study}}
#' @export
es_colours <- c(
  primary   = "#2563eb",
  event     = "#D55E00",
  reference = "#6b7280",
  ci_band   = "#2563eb",
  group1    = "#2563eb",
  group2    = "#D55E00",
  group3    = "#009E73",
  group4    = "#56B4E9",
  group5    = "#E69F00",
  group6    = "#CC79A7",
  group7    = "#0072B2",
  group8    = "#F0E442"
)

## Theme ----

#' EventStudy ggplot2 Theme
#'
#' A clean, publication-ready ggplot2 theme for EventStudy plots. Built on
#' \code{\link[ggplot2]{theme_minimal}} with a centred plot title, a bottom
#' legend, and subtle gridlines styled to the EventStudy palette.
#'
#' @param base_size Base font size in points. Default is 11.
#' @param base_family Base font family. Default \code{""} uses the R device
#'   default, which works on all platforms without requiring installed fonts.
#'   Pass a family name (e.g. \code{"Inter"}) if that font is installed on the
#'   target device.
#'
#' @return A \code{\link[ggplot2]{theme}} object that composes with \code{+}.
#'
#' @seealso \code{\link{es_colours}}, \code{\link{plot_event_study}}
#'
#' @export
theme_eventstudy <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(hjust = 0.5, size = base_size + 1),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#e5e7eb", linewidth = 0.4),
      panel.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(face = "bold", size = base_size)
    )
}

## Plotly styling (internal) ----

#' @noRd
.style_plotly <- function(p) {
  plotly::layout(
    p,
    font          = list(family = ""),
    legend        = list(orientation = "h", xanchor = "center", x = 0.5),
    paper_bgcolor = "#ffffff",
    plot_bgcolor  = "#ffffff"
  )
}
