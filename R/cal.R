#' Draw calibrate curve
#'
#' @param data data
#' @param outcome predict outcome.
#' @param predictors predictors.
#' @param newdata new data for verification.
#' @param B  B is an upper limit on the number of resamples for which information
#' is printed about which variables were selected in each model re-fit.
#' @param xlab defaults to "Predicted x-units Survival" or to a suitable label for other models.
#' @param ylab defaults to "Fraction Surviving x-units" or to a suitable label for other models.
#' @param subtitles set to FALSE to suppress subtitles in plot describing method and for lrm and
#' ols the mean absolute error and original sample size
#' @param ... further arguments.
#'
#' @export
cal <- function(data, outcome = NULL, predictors = NULL, newdata = NULL,
                B = 1000,  xlab = NULL, ylab = NULL, subtitles = FALSE, ...){
  UseMethod("cal")
}


#' @rdname cal
#' @export
cal.data.frame <- function(data, outcome = NULL, predictors = NULL, newdata = NULL,
                           B = 1000, xlab = NULL, ylab = NULL, subtitles = FALSE, ...){

  set.seed(1234)

  train  <- data[c(outcome, predictors)]
  dnames <- names(train)[-1][sapply(train[-1], \(x) {is.factor(x) | is.character(x)})]
  train  <- dummy.data.frame(train, varnames = dnames)

  model <- logistic(data = train, outcome = outcome, predictors = names(train)[-1])


  if(is.null(xlab)){
    xlab <- "Predicted probability"
  }

  if(is.null(ylab)){
    ylab <- "Actual probability"
  }

  if(!is.null(newdata)){
    test <- newdata[c(outcome, predictors)]
    test <- dummy.data.frame(test, varnames = dnames)

    pred_f_validation <- stats::predict(model, test)

    fit.vad <-
      rms::lrm(
        test[[outcome]] ~ pred_f_validation,
        data = test,
        x = T,
        y = T
      )

    plot(rms::calibrate(fit.vad, B = B),
         xlab = xlab,
         ylab = ylab,
         subtitles = subtitles)
  }else{
    rms::calibrate(model, B = B)
  }
}


#' @rdname cal
#' @export
cal.nmtask <- function(data, outcome = NULL, predictors = NULL, newdata = NULL,
                       B = 1000, xlab = NULL, ylab = NULL, subtitles = FALSE, ...){

  train.data <- data$train.data

  if(is.null(newdata)){
    newdata  <- data$test.data
  }

  if(is.null(outcome)){
    outcome <- data$outcome
  }

  if(is.null(predictors)){
    predictors <- data$predictors
  }

  cal.data.frame(data = train.data,
                 outcome = outcome,
                 predictors = predictors,
                 newdata = newdata,
                 B = B,
                 xlab = xlab,
                 ylab = ylab,
                 subtitles = subtitles, ...)
}
