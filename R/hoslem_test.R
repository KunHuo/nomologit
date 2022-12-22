#' Hosmer-Lemeshow test
#'
#' @param ... one or more object of 'nmtask' or 'glm'.
#' @param newdata new data for verification.
#' @param digits digits for p value, default 3.
#' @param g number of bins to use to calculate quantiles. As far as I have seen,
#' there is little guidance as to how to choose the number of groups g.
#' Hosmer and Lemeshow’s conclusions from simulations were based on using g>p+1,
#' suggesting that if we have 10 covariates in the model, we should choose g>11,
#' although this doesn’t appear to be mentioned in text books or software packages.
#' @param filename filename, if you want to save to word.
#'
#' @details
#' The Hosmer-Lemeshow test is a statistical test for goodness of fit for logistic
#' regression models. This means that given our fitted model, the p-value can be
#' calculated as the right hand tail probability of the corresponding chi-squared
#' distribution using the calculated test statistic. If the p-value is small,
#' this is indicative of poor fit. It should be emphasized that a large p-value
#' does not mean the model fits well, since lack of evidence against a null
#' hypothesis is not equivalent to evidence in favour of the alternative hypothesis.
#' In particular, if our sample size is small, a high p-value from the test may
#' simply be a consequence of the test having lower power to detect mis-specification,
#' rather than being indicative of good fit.
#'
#' About number of groups:
#' Intuitively, using a small value of g ought to give less opportunity to detect
#' mis-specification. However, if we choose g to large, the numbers in each group
#' may be so small that it will be difficult to determine whether differences
#' between observed and expected are due to chance or indicative or model
#' mis-specification.
#'
#' A further problem, highlighted by many others (e.g. Paul Allison) is that,
#' for a given dataset, if one changes g, sometimes one obtains a quite different
#' p-value, such that with one choice of g we might conclude our model does not
#' fit well, yet with another we conclude there is no evidence of poor fit.
#' This is indeed a troubling aspect of the test.
#'
#' @references
#' Hosmer D W, Lemeshow S 2000. Applied Logistic Regression. New York, USA: John Wiley and Sons.
#'
#' @export
#'
#' @examples
#' tk1 <- nmtask(train.data = aps,
#'               outcome = "elope",
#'               predictors = c("age", "gender", "place3"))
#'
#' hl_test(tk1)
hl_test <- function(..., newdata = NULL, g = 10, digits = 3, filename = ""){

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

    train.res <- tibble::add_column(train.res, data = "Training set", .before = 1)

    if(is.null(newdata)){
      test.data  <- tk$test.data
    }else{
      test.data <- newdata
    }

    if(!is.null(test.data)){
      test.pred <- stats::predict(train.fit, test.data)
      test.fit  <- stats::glm(test.data[[outcome]] ~ test.pred, family = stats::binomial())
      test.res  <- hoslem_test_exec(x = test.fit$y,
                                    y = stats::fitted(test.fit),
                                    g = g,
                                    digits = digits)
      test.res <- tibble::add_column(test.res, data = "Validation set", .before = 1)
      rbind(train.res, test.res)
    }else{
      train.res
    }
  }

  out <- lapply(tasks, exec)

  if(is.null(names(tasks))){
    names(out) <- sprintf("Model %d", 1:length(tasks))
  }else{
    names(out) <- names(tasks)
  }

  if(length(tasks) == 1L){
    out <- list_rbind(out,
                      collapse.names = FALSE,
                      collapse.one.row = TRUE,
                      names.as.column = FALSE)
    names(out) <- c("Data set", "Chi-squared", "P value")
  }else{
    out <- list_rbind(out,
                      collapse.names = TRUE,
                      collapse.one.row = TRUE)
    names(out) <- c("Models", "Chi-squared", "P value")
  }

  out <- add_title(out, "Hosmer-Lemeshow goodness of fit test")
  out <- add_note(out, "P value > 0.05 indicating no evidence of poor fit.")
  class(out) <- c("hoslem", class(out))

  if(filename == ""){
    out
  }else{
    write_docx.data.frame(out, path = filename)
  }
}


#' Print hoslem
#'
#' @param x a object of hoslem
#' @param ... more.
#'
#' @keywords internal
#' @export
print.hoslem <- function(x, ...){
  print_booktabs(x, adj = c("l", "c"))
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
