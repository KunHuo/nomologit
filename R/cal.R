#' Draw calibrate curve
#'
#' @description
#' Calibration was quantified by comparing the actual observed progression with
#' the model-predicted progression, and the results were graphically evaluated
#' as calibration curves. Perfect calibration would be exhibited by a direct
#' alignment between the actual observation and nomogram prediction probability
#' along the 45 degree diagonal line.
#'
#' @param data data
#' @param outcome predict outcome.
#' @param predictors predictors.
#' @param newdata new data for verification.
#' @param B  B is an upper limit on the number of resamples for which information
#' is printed about which variables were selected in each model re-fit, default 1000.
#' @param linesize line size, default 0.25.
#' @param linecolor line color, the length must be 3.
#' @param linelabel line label,the length must be 3.
#' @param xlab label for X axis.
#' @param ylab label for Y axis.
#' @param show.explain explain the figure, default TRUE.
#' @param ... further arguments.
#'
#' @export
cal <- function(data,
                outcome = NULL,
                predictors = NULL,
                newdata = NULL,
                B = 1000,
                linesize = 0.5,
                linecolor = NULL,
                linelabel = NULL,
                xlab = NULL,
                ylab = NULL,
                show.explain = TRUE,...){
  UseMethod("cal")
}


#' @rdname cal
#' @export
cal.data.frame <- function(data,
                           outcome = NULL,
                           predictors = NULL,
                           newdata = NULL,
                           B = 1000,
                           linesize = 0.5,
                           linecolor = NULL,
                           linelabel = NULL,
                           xlab = NULL,
                           ylab = NULL,
                           show.explain = TRUE, ...){

  train  <- data[c(outcome, predictors)]
  dnames <- names(train)[-1][sapply(train[-1], \(x) {is.factor(x) | is.character(x)})]
  train  <- dummy.data.frame(train, varnames = dnames)

  model <- logistic(data = train, outcome = outcome, predictors = names(train)[-1])

  A <- plot_cal(rms::calibrate(model, B = B),
                linesize = linesize,
                linecolor = linecolor,
                linelabel = linelabel,
                xlab = xlab,
                ylab = ylab, ...)

  explain <- sprintf("The gray line represents the ideal nomogram, the blue line represents the observed nomogram, and the orange line represents the corrected observed nomogram with %d bootstrap resamples. The predicted probability of risk by the nomogram is projected onto the x-axis, and the actual risk is projected onto the y-axis.", B)

  if(!is.null(newdata)){
    test <- newdata[c(outcome, predictors)]
    test <- dummy.data.frame(test, varnames = dnames)
    pred_f_validation <- stats::predict(model, test)
    fit.vad <- rms::lrm(test[[outcome]] ~ pred_f_validation, data = test, x = T, y = T)

    B <- plot_cal(rms::calibrate(fit.vad, B = B),
             linesize = linesize,
             linecolor = linecolor,
             linelabel = linelabel,
             xlab = xlab,
             ylab = ylab, ...)

    A <- A + gg_tags("A")
    B <- B + gg_tags("B")

    if(show.explain){
      cat("Figure: Calibration plots of the nomogram for training set (A) and validation set (B).\n")
      cat(explain)
    }

    patchwork::wrap_plots(A, B)

  }else{
    if(show.explain){
      cat("Figure: Calibration plots of the nomogram for training set.\n")
      cat(explain)
    }
    A
  }
}


#' @rdname cal
#' @export
cal.nmtask <- function(data,
                       outcome = NULL,
                       predictors = NULL,
                       newdata = NULL,
                       B = 1000,
                       linesize = 0.5,
                       linecolor = NULL,
                       linelabel = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       show.explain = TRUE, ...){

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
                 show.explain = show.explain, ...)
}


plot_cal <- function(cal, linesize = 0.5, linecolor = NULL, linelabel = NULL, xlab = NULL, ylab = NULL, ...){

  if(is.null(linecolor)){
    linecolor <- c("#374E55FF", "#00A1D5FF", "#DF8F44FF")
  }else{
    stopifnot(length(linecolor) == 3L)
  }

  if(is.null(linelabel)){
    linelabel <- c("Ideal", "Apparent", "Bias-corrected")
  }else{
    stopifnot(length(linelabel) == 3L)
  }

  if(is.null(xlab)){
    xlab <- "Predicted probability"
  }

  if(is.null(ylab)){
    ylab <- "Actual probability"
  }

  plotdata <- cal[, 1:3]
  plotdata <- as.data.frame(plotdata)
  plotdata$ideal <- plotdata$predy

  plotdata <- reshape_long(plotdata,
                           cols = c("ideal", "calibrated.orig", "calibrated.corrected"))
  plotdata <- rbind(plotdata[plotdata$.name != "ideal", ], plotdata[plotdata$.name == "ideal", ][1, ])

  plotdata$.name <- factor(plotdata$.name,
                           levels = c("ideal", "calibrated.orig", "calibrated.corrected"),
                           labels = linelabel)

  minaxis <- min(c(plotdata$predy, plotdata$.value))
  maxaxis <- max(c(plotdata$predy, plotdata$.value))
  axis <- pretty(c(minaxis, maxaxis), 5)

  ggplot2::ggplot(plotdata) +
    ggplot2::geom_abline(intercept = 0, color = linecolor[1], linetype = 3, size = linesize)  +
    ggplot2::geom_line(ggplot2::aes_string(x = "predy", y = ".value", color = ".name", linetype = ".name"), size = linesize) +
    ggplot2::scale_color_manual(values = linecolor) +
    ggplot2::scale_linetype_manual(values = c(3, 2, 1)) +
    gg_theme_sci(legend.key.size = 1.2, ...) +
    gg_legend_position(c(1, 0)) +
    gg_delete_legend_title() +
    gg_xlab(xlab) +
    gg_ylab(ylab) +
    ggplot2::scale_x_continuous(breaks = axis, limits = c(min(axis), max(axis)), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = axis, limits = c(min(axis), max(axis)), expand = c(0, 0))
}
