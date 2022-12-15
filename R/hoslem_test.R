#' Hosmer-Lemeshow test
#'
#' @param data data.
#' @param outcome predict outcome.
#' @param predictors predictors.
#' @param g number of bins to use to calculate quantiles.
#'
#' @export
hoslem_test <- function(..., newdata = NULL, g = 10, digits = 3){

  tasks <- flatten_list(list(...))

  tasks <- lapply(tasks, \(tk){
    if("glm" %in% class(tk)){
      as_nmtask(tk)
    }else{
      tk
    }
  })

  exec <- function(tk){
    train.data <- tk$train.data
    outcome    <- tk$outcome
    predictors <- tk$predictors

    train.fit <- logistic(data = train.data,
                          outcome = outcome,
                          predictors = predictors,
                          method = "glm")

    train.res <- hoslem_test_exec(x = train.fit$y,
                                  y = stats::fitted(train.fit),
                                  g = g,
                                  digits = digits)
    if(is.null(newdata)){
      test.data  <- tk$test.data
    }else{
      test.data <- newdata
    }

    train.res
  }


  out <- lapply(tasks, exec)

  out
}


hoslem_test_exec <- function (x, y, g = 10, digits = 3) {
  yhat <- y
  y    <- x

  qq       <- unique(stats::quantile(yhat, probs = seq(0, 1, 1 / g)))
  cutyhat  <- cut(yhat, breaks = qq, include.lowest = TRUE)

  observed <- stats::xtabs(cbind(y0 = 1 - y, y1 = y) ~ cutyhat)
  expected <- stats::xtabs(cbind(yhat0 = 1 - yhat, yhat1 = yhat) ~ cutyhat)

  chisq   <- sum((observed - expected) ^ 2 / expected)
  pvalue  <- 1 - stats::pchisq(chisq, g - 2)

  data.frame(chisq  = format_statistic(chisq, digits),
             pvalue = format_pvalue(pvalue, 3))
}
