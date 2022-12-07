#' Draw decision curves
#'
#' @param data data
#' @param outcome predict outcome.
#' @param predictors predictors.
#' @param newdata new data for verification.
#' @param B  Number of bootstrap replicates to use to calculate confidence intervals (default 1000).
#' @param xlab defaults to "Predicted x-units Survival" or to a suitable label for other models.
#' @param ylab defaults to "Fraction Surviving x-units" or to a suitable label for other models.
#' @param ... further arguments.
#'
#' @export
dca <- function(data, outcome = NULL, predictors = NULL, newdata = NULL, B = 1000, thresholds = seq(0, 1, by = 0.01), xlab = NULL, ylab = NULL, ...){
  UseMethod("dca")
}


#' @rdname dca
#' @export
dca.data.frame <- function(data, outcome = NULL, predictors = NULL, newdata = NULL, B = 1000, thresholds = seq(0, 1, by = 0.01), xlab = NULL, ylab = NULL, ...){

}


#' @rdname dca
#' @export
dca.nmtask <- function(data, outcome = NULL, predictors = NULL, newdata = NULL, B = 1000, thresholds = seq(0, 1, by = 0.01), xlab = NULL, ylab = NULL, ...){

}
