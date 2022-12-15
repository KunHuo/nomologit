#' Hosmer-Lemeshow test
#'
#' @param data data.
#' @param outcome predict outcome.
#' @param predictors predictors.
#' @param g number of bins to use to calculate quantiles.
#'
#' @export
hoslem_test <- function(data,  outcome = NULL, predictors = NULL, g = 10){
  frm <- paste(predictors, collapse = " + ")
  frm <- paste(outcome, frm, sep = " ~ ")
  frm <- stats::as.formula(frm)

  fit <- stats::glm(formula = frm, family = stats::binomial(), data = data)

  hoslem_test_exec(fit$y, stats::fitted(fit), g = g)
}


hoslem_test_exec <- function (x, y, g = 10) {
  yhat <- y
  y    <- x

  qq       <- unique(stats::quantile(yhat, probs = seq(0, 1, 1 / g)))
  cutyhat  <- cut(yhat, breaks = qq, include.lowest = TRUE)

  observed <- stats::xtabs(cbind(y0 = 1 - y, y1 = y) ~ cutyhat)
  expected <- stats::xtabs(cbind(yhat0 = 1 - yhat, yhat1 = yhat) ~ cutyhat)

  chisq   <- sum((observed - expected) ^ 2 / expected)
  pvalue  <- 1 - stats::pchisq(chisq, g - 2)

  sprintf("Hosmer and Lemeshow test: X-squared = %.3f, P value = %.3f", chisq, pvalue)
}
