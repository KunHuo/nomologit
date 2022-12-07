#' Hosmer-Lemeshow Test
#'
#' @param data data.
#' @param outcome predict outcome.
#' @param predictors predictors.
#' @param g number of bins to use to calculate quantiles.
#'
#' @export
hoslem_test <- function(data, outcome = NULL, predictors = NULL, g = 10){
  UseMethod("hoslem_test")
}


#' @rdname hoslem_test
#' @export
hoslem_test.data.frame <- function(data,  outcome = NULL, predictors = NULL, g = 10){
  frm <- paste(predictors, collapse = " + ")
  frm <- paste(outcome, frm, sep = " ~ ")
  frm <- stats::as.formula(frm)

  fit <- stats::glm(formula = frm, family = stats::binomial(), data = data)

  hoslem_test_exec(fit$y, stats::fitted(fit), g = g)
}


#' @rdname hoslem_test
#' @export
hoslem_test.nmtask <- function(data, outcome = NULL, predictors = NULL, g = 10){

  train.data <- data$train.data

  if(is.null(outcome)){
    outcome <- data$outcome
  }

  if(is.null(predictors)){
    predictors <- data$predictors
  }

  hoslem_test.data.frame(data = train.data,
                         outcome = outcome,
                         predictors = predictors,
                         g = g)
}


hoslem_test_exec <- function (x, y, g = 10) {
  DNAME <- paste(deparse(substitute(x)), deparse(substitute(y)), sep = ", ")
  METHOD <- "Hosmer and Lemeshow goodness of fit (GOF) test"
  yhat <- y
  y <- x
  qq <- unique(stats::quantile(yhat, probs = seq(0, 1, 1/g)))
  cutyhat <- cut(yhat, breaks = qq, include.lowest = TRUE)
  observed <- stats::xtabs(cbind(y0 = 1 - y, y1 = y) ~ cutyhat)
  expected <- stats::xtabs(cbind(yhat0 = 1 - yhat, yhat1 = yhat) ~ cutyhat)
  chisq <- sum((observed - expected)^2/expected)
  PVAL = 1 - stats::pchisq(chisq, g - 2)
  PARAMETER <- g - 2
  names(chisq) <- "X-squared"
  names(PARAMETER) <- "df"
  structure(list(statistic = chisq, parameter = PARAMETER,
                 p.value = PVAL, method = METHOD, data.name = DNAME, observed = observed,
                 expected = expected), class = "htest")
}
