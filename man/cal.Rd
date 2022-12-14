% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cal.R
\name{cal}
\alias{cal}
\title{Draw calibrate curve}
\usage{
cal(
  ...,
  newdata = NULL,
  model.names = NULL,
  boot = 10,
  facet = c("data", "model"),
  linewidth = 0.5,
  linecolor = NULL,
  xlab = "Predicted probability",
  ylab = "Actual probability",
  xbreaks = NULL,
  ybreaks = NULL,
  fontfamily = "serif",
  fontsize = 12,
  explain = TRUE,
  seed = 1234
)
}
\arguments{
\item{...}{one or more object of 'nmtask' or 'glm'.}

\item{newdata}{new data for verification.}

\item{model.names}{vector of model's names to use when plotting legends.}

\item{boot}{boot is an upper limit on the number of resamples for which information
is printed about which variables were selected in each model re-fit, default 1000.}

\item{facet}{of 'data' or 'model', specifying grouping variables for faceting
the plot into multiple panels, default 'data'.}

\item{linewidth}{line width, default 0.5.}

\item{linecolor}{line color.}

\item{xlab}{x axis label.}

\item{ylab}{y axis label.}

\item{xbreaks}{x axis breaks.}

\item{ybreaks}{y axis breaks.}

\item{fontfamily}{font family, sefault serif.}

\item{fontsize}{font size, default 12.}

\item{explain}{whether explain the figure, default TRUE.}

\item{seed}{a single value, interpreted as an integer, or NULL, default 1234.}
}
\value{
a ggplot object from the \link{ggplot2} package, or a patchwork object from \link{patchwork} package.
}
\description{
Calibration was quantified by comparing the actual observed progression with
the model-predicted progression, and the results were graphically evaluated
as calibration curves. Perfect calibration would be exhibited by a direct
alignment between the actual observation and nomogram prediction probability
along the 45 degree diagonal line.
}
\examples{
# Examples see in [nmtask] function.
}
