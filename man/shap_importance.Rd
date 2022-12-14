% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SHAP.R
\name{shap_importance}
\alias{shap_importance}
\title{SHAP importance plots}
\usage{
shap_importance(
  x,
  kind = c("beeswarm", "bar", "both", "no"),
  max.display = 15L,
  colors = "#fca50a",
  ...
)
}
\arguments{
\item{x}{An object from \link{shap} function.}

\item{kind}{Should a "beeswarm" plot (the default), a "bar" plot, or "both" be
shown? Set to "no" in order to suppress plotting. In that case, the sorted
SHAP feature importances of all variables are returned.}

\item{max.display}{Maximum number of features (with highest importance) should
be plotted? If there are more, the least important variables are collapsed:
their SHAP values are added and their min-max-scaled feature values are added
as well (and the resulting vector is min-max-scaled again). Set to Inf to
show all features. Has no effect if kind = "no".}

\item{colors}{Color used to fill the bars (only used if bars are shown).}

\item{...}{More arguments.}
}
\value{
A "ggplot" object representing an importance plot, or - if kind =
"no" - a named numeric vector of sorted SHAP feature importances.
}
\description{
This function provides two types of SHAP importance plots: a bar plot and a
beeswarm plot (sometimes called "SHAP summary plot"). The bar plot shows SHAP
feature importances, calculated as the average absolute SHAP value per feature.
The beeswarm plot displays SHAP values per feature, using min-max scaled
feature values on the color axis. Non-numeric features are transformed to
numeric by calling data.matrix() first. For both types of plots, the features
are sorted in decreasing order of importance. The two types of plots can also
be combined.
}
\examples{
# Examples see in [shap] function.
}
