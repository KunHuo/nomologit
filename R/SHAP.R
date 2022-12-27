shap_exec <- function(data, outcome, predictors, newdata = NULL, nsim = 100){
  data <- as.data.frame(data)

  if(is.null(newdata)){
    newdata <- data
  }else{
    newdata <- as.data.frame(newdata)
  }

  fit <- logistic(data = data,
                  outcome = outcome,
                  predictors = predictors,
                  method = "glm")
  sp <- fastshap::explain(
    fit,
    X = data[predictors],
    newdata = newdata[predictors],
    nsim = nsim,
    pred_wrapper = stats::predict
  )
  sv <- shapviz::shapviz(sp, X = data)
  sv
}


#' Initialize "shap" Object
#'
#' @param task an object of 'nmtask'.
#' @param newdata A matrix-like R object (e.g., a data frame or matrix) containing
#' ONLY the feature columns for the observation(s) of interest; that is, the
#' observation(s) you want to compute explanations for. Default is NULL which
#' will produce approximate Shapley values for all the rows in X (i.e., the
#' training data).
#' @param nsim The number of Monte Carlo repetitions to use for estimating each
#' Shapley value (only used when exact = FALSE). Default is 1. NOTE: To obtain
#' the most accurate results, nsim should be set as large as feasibly possible.
#'
#' @return
#' An object with the following three elements:
#' * S: A numeric matrix of SHAP values.
#' * X: A data.frame containing the feature values corresponding to S.
#' * baseline: Baseline value, representing the average prediction at the scale of the SHAP values.
#' @export
shap <- function(task, newdata = NULL, nsim = 100){
  train.data <- task$train.data

  if(is.null(newdata)){
    test.data  <- task$test.data
  }else{
    test.data  <- newdata
  }
  outcome    <- task$outcome
  predictors <- task$predictors

  out <- shap_exec(data = train.data,
       outcome = outcome,
       predictors = predictors,
       newdata = newdata,
       nsim = nsim)

  invisible(out)
}



#' SHAP force plot
#'
#' @description
#' Creates a force plot of SHAP values of one single observation. The value of
#' f(x) denotes the prediction on the SHAP scale, while E(f(x)) refers to the
#' baseline SHAP value.
#'
#' @param x An object from [shap] function.
#' @param row_id 	A single row number to plot.
#' @param colors A vector of exactly two fill colors: the first for positive
#' SHAP values, the other for negative ones.
#' @param ... More arguments.
#'
#' @seealso [shap_waterfall]
#'
#' @return An object of class "ggplot" representing a force plot.
#' @export
shap_force <- function(x, row_id = 1L, colors = c("#f7d13d", "#a52c60"), ...){

}


#' SHAP waterfall plot
#'
#' @description
#' Creates a waterfall plot of SHAP values of one single observation. The value
#' of f(x) denotes the prediction on the SHAP scale, while E(f(x)) refers to the
#' baseline SHAP value. The plot has to be read from bottom to top.
#'
#' @inheritParams shap_force
#'
#' @seealso [shap_force]
#'
#' @return An object of class "ggplot" representing a waterfall plot.
#' @export
shap_waterfall <- function(x, row_id = 1L, colors = c("#f7d13d", "#a52c60"), ...){

}


#' SHAP importance plots
#'
#' @description
#' This function provides two types of SHAP importance plots: a bar plot and a
#' beeswarm plot (sometimes called "SHAP summary plot"). The bar plot shows SHAP
#' feature importances, calculated as the average absolute SHAP value per feature.
#' The beeswarm plot displays SHAP values per feature, using min-max scaled
#' feature values on the color axis. Non-numeric features are transformed to
#' numeric by calling data.matrix() first. For both types of plots, the features
#' are sorted in decreasing order of importance. The two types of plots can also
#' be combined.
#'
#' @param x An object from [shap] function.
#' @param kind Should a "beeswarm" plot (the default), a "bar" plot, or "both" be
#' shown? Set to "no" in order to suppress plotting. In that case, the sorted
#' SHAP feature importances of all variables are returned.
#' @param colors Color used to fill the bars (only used if bars are shown).
#' @param ... More arguments.
#'
#' @return A "ggplot" object representing an importance plot, or - if kind =
#' "no" - a named numeric vector of sorted SHAP feature importances.
#'
#' @export
shap_impoertance <- function(x, kind = c("beeswarm", "bar", "both", "no"), colors = "#fca50a", ...){

}


#' SHAP dependence plots
#'
#' @description
#' Creates a scatter plot of the SHAP values of a feature against its feature
#' values. A second variable, color_var, can be selected to be used on the color
#' axis. In this way, one can get a sense of possible interaction effects. Set
#' color_var = "auto" to use a simple heuristic to select the color feature with
#' the strongest apparent interaction. With discrete v, horizontal jitter is
#' added by default.
#'
#' @param x An object from [shap] function.
#' @param colors Color used to fill the bars (only used if bars are shown).
#' @param ... More arguments.
#'
#' @return An object of class ggplot representing a dependence plot.
#'
#' @export
shap_dependence <- function(x, colors = "#3b528b", ...){

}
