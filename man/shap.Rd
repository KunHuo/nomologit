% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SHAP.R
\name{shap}
\alias{shap}
\title{Initialize "shap" Object}
\usage{
shap(task, newdata = NULL, nsim = 100)
}
\arguments{
\item{task}{an object of 'nmtask'.}

\item{newdata}{A matrix-like R object (e.g., a data frame or matrix) containing
ONLY the feature columns for the observation(s) of interest; that is, the
observation(s) you want to compute explanations for. Default is NULL which
will produce approximate Shapley values for all the rows in X (i.e., the
training data).}

\item{nsim}{The number of Monte Carlo repetitions to use for estimating each
Shapley value (only used when exact = FALSE). Default is 1. NOTE: To obtain
the most accurate results, nsim should be set as large as feasibly possible.}
}
\value{
An object with the following three elements:
\itemize{
\item S: A numeric matrix of SHAP values.
\item X: A data.frame containing the feature values corresponding to S.
\item baseline: Baseline value, representing the average prediction at the scale of the SHAP values.
}
}
\description{
Initialize "shap" Object
}
\examples{
\dontrun{
index <- sample(1:nrow(HCC), 12000)
train <- HCC[index, ]
test  <- HCC[-index, ]

# Develop a prediction model task.
# The prediction outcome is status,
# and the prediction variables are AJCC_T, AJCC_M, and AJCC_N.
tk1 <- nmtask(train.data = train,
              test.data  = test,
              outcome    = "status",
              predictors = c("AJCC_T", "AJCC_M", "AJCC_N"))
# View task
tk1

sp <- shap(tk1)

# SHAP force plot
shap_force(sp)

# SHAP waterfall plot
shap_waterfall(sp)

# SHAP importance plots
shap_shap_importance(sp)

# SHAP dependence plots
shap_dependence(sp, variable = "AJCC_M")
}
}
