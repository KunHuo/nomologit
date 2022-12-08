#' Draw calibrate curve
#'
#' @param data data
#' @param outcome predict outcome.
#' @param predictors predictors.
#' @param newdata new data for verification.
#' @param B  B is an upper limit on the number of resamples for which information
#' is printed about which variables were selected in each model re-fit.
#' @param ... further arguments.
#'
#' @export
cal <- function(data, outcome = NULL, predictors = NULL, newdata = NULL, B = 1000, ...){
  UseMethod("cal")
}


#' @rdname cal
#' @export
cal.data.frame <- function(data, outcome = NULL, predictors = NULL, newdata = NULL, B = 1000, ...){

  set.seed(1234)

  train  <- data[c(outcome, predictors)]
  dnames <- names(train)[-1][sapply(train[-1], \(x) {is.factor(x) | is.character(x)})]
  train  <- dummy.data.frame(train, varnames = dnames)

  model <- logistic(data = train, outcome = outcome, predictors = names(train)[-1])

  if(!is.null(newdata)){
    test <- newdata[c(outcome, predictors)]
    test <- dummy.data.frame(test, varnames = dnames)
    pred_f_validation <- stats::predict(model, test)
    fit.vad <- rms::lrm(test[[outcome]] ~ pred_f_validation, data = test, x = T, y = T)
    plot_cal(rms::calibrate(fit.vad, B = B))
  }else{
    plot_cal(rms::calibrate(model, B = B))
  }
}


#' @rdname cal
#' @export
cal.nmtask <- function(data, outcome = NULL, predictors = NULL, newdata = NULL, B = 1000, ...){

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
                 ylab = ylab, ...)
}


plot_cal <- function(cal){

  plotdata <- cal[, 1:3]
  plotdata <- as.data.frame(plotdata)
  plotdata$ideal <- plotdata$predy

  plotdata <- reshape_long(plotdata,
                           cols = c("ideal", "calibrated.orig", "calibrated.corrected"))

  plotdata$.name <- factor(plotdata$.name,
                           levels = c("ideal", "calibrated.orig", "calibrated.corrected"),
                           labels = c("Ideal", "Apparent", "Bias-corrected"))

  minaxis <- min(c(plotdata$predy, plotdata$.value))
  maxaxis <- max(c(plotdata$predy, plotdata$.value))
  axis <- pretty(c(minaxis, maxaxis), 5)

  ggplot2::ggplot(plotdata) +
    ggplot2::geom_abline(intercept = 0, color = "#374E55FF", linetype = 3)  +
    ggplot2::geom_line(ggplot2::aes_string(x = "predy", y = ".value", color = ".name", linetype = ".name")) +
    ggplot2::scale_color_manual(values = c("#374E55FF", "#00A1D5FF", "#DF8F44FF")) +
    ggplot2::scale_linetype_manual(values = c(3, 2, 1)) +
    gg_theme_sci(legend.key.size = 1.2) +
    gg_legend_position(c(1, 0)) +
    gg_delete_legend_title() +
    gg_xlab("Predicted probability") +
    gg_ylab("Actual probability") +
    ggplot2::scale_x_continuous(breaks = axis, limits = c(min(axis), max(axis)), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = axis, limits = c(min(axis), max(axis)), expand = c(0, 0))
}
