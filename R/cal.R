#' Draw calibrate curve
#'
#' @description
#' Calibration was quantified by comparing the actual observed progression with
#' the model-predicted progression, and the results were graphically evaluated
#' as calibration curves. Perfect calibration would be exhibited by a direct
#' alignment between the actual observation and nomogram prediction probability
#' along the 45 degree diagonal line.
#'
#' @param ... one or more object of 'nmtask' or 'glm'.
#' @param newdata new data for verification.
#' @param model.names vector of model's names to use when plotting legends.
#' @param boot boot is an upper limit on the number of resamples for which information
#' is printed about which variables were selected in each model re-fit, default 1000.
#' @param facet of 'data' or 'model', specifying grouping variables for faceting
#' the plot into multiple panels, default 'data'.
#' @param linewidth line width, default 0.5.
#' @param linecolor line color.
#' @param xlab x axis label.
#' @param ylab y axis label.
#' @param xbreaks x axis breaks.
#' @param ybreaks y axis breaks.
#' @param fontfamily font family, sefault serif.
#' @param fontsize font size, default 12.
#' @param explain whether explain the figure, default TRUE.
#' @param seed a single value, interpreted as an integer, or NULL, default 1234.
#'
#' @return a ggplot object from the [ggplot2] package, or a patchwork object from [patchwork] package.
#' @export
#'
#' @examples
#' head(aps)
#'
#' # Without validation
#' tk <- nmtask(train.data = aps,
#'              outcome = "elope",
#'              predictors = c("age", "gender"))
#' cal(tk)
#'
#' # With validation
#' index <- sample(1:nrow(aps), 300)
#' train <- aps[index, ]
#' test  <- aps[-index, ]
#'
#' tk1 <- nmtask(train.data = train,
#'               test.data = test,
#'               outcome = "elope",
#'               predictors = c("age", "gender"))
#' tk2 <- nmtask(train.data = train,
#'               test.data = test,
#'               outcome = "elope",
#'               predictors = c("age", "gender", "place3"))
#'
#' # facet by data, defalut.
#' cal(tk1, facet = "d")
#' # facet by models.
#' cal(tk1, facet = "m")
#' # plot two or more models
#' cal(tk1, tk2)
#' # rename models
#' cal("Nomogram 1" = tk1, "Nomogram 2" = tk2)
#'
#' # a list of predictors
#' tks <- nmtask(train.data = train,
#'               test.data = test,
#'               outcome = "elope",
#'               predictors = list("Nomogram 1" = c("age", "gender"),
#'                                 "Nomogram 2" = c("age", "gender", "place3")))
#' cal(tks)
#' cal(tks, facet = "m")
#'
#' # support glm
#' model1 <- glm(elope ~ age + gender + place3 + neuro,
#'               data = train,
#'               family = binomial())
#' cal(model1)
#' # with new data.
#' cal(model1, newdata = test)
cal <- function(...,
                newdata = NULL,
                model.names = NULL,
                boot = 10,
                facet = c("data", "model"),
                linewidth = 0.5,
                linecolor = NULL,
                xlab = "Predicted probability",
                ylab = "Actual probability",
                xbreaks = NULL,
                ybreaks = NULL,
                fontfamily = "serif",
                fontsize = 12,
                explain = TRUE,
                seed = 1234) {

  facet <- match.arg(facet)

  tasks <- list(...)
  tasks <- flatten_list(tasks)

  # supoort glm
  tasks <- lapply(tasks, \(tk){
    if("glm" %in% class(tk)){
      as_nmtask(tk)
    }else{
      tk
    }
  })

  # plot data
  plotdata <- lapply(tasks, \(tk){
    train.data <- tk$train.data

    if(is.null(newdata)){
      test.data  <- tk$test.data
    }else{
      test.data  <- newdata
    }

    outcome    <- tk$outcome
    predictors <- tk$predictors

    train.data  <- train.data[c(outcome, predictors)]
    dnames      <- names(train.data)[-1][sapply(train.data[-1], \(x) {is.factor(x) | is.character(x)})]
    train.data  <- dummy.data.frame(train.data, varnames = dnames)

    set.seed(seed)
    train.fit <- logistic(data = train.data, outcome = outcome, predictors = names(train.data)[-1])
    train.cal <- rms::calibrate(train.fit, B = boot)

    train.plotdata <- train.cal[, 1:3]
    train.plotdata <- as.data.frame(train.plotdata)
    train.plotdata$group <- "Training set"

    # test data
    if(!is.null(test.data)){
      test.data <- test.data[c(outcome, predictors)]
      test.data <- dummy.data.frame(test.data, varnames = dnames)
      test.pred <- stats::predict(train.fit, test.data)

      test.fit <- rms::lrm(test.data[[outcome]] ~ test.pred, data = test.data, x = T, y = T)
      set.seed(seed)
      test.cal <- rms::calibrate(test.fit, B = boot)

      test.plotdata <- test.cal[, 1:3]
      test.plotdata <- as.data.frame(test.plotdata)
      test.plotdata$group <- "Validation set"

      rbind(train.plotdata, test.plotdata)
    }else{
      train.plotdata
    }
  })

  # set names
  if(is.null(model.names)){
    if(is.null(names(tasks))){
      names(plotdata) <- sprintf("Nomogram %d", 1:length(tasks))
    }else{
      names(plotdata) <- names(tasks)
    }
  }else{
    stopifnot(length(model.names) == length(plotdata))
    names(plotdata) <- model.names
  }

  levels <- names(plotdata)

  plotdata <- list_rbind(plotdata, varname = "model")
  plotdata$model <- factor(plotdata$model, levels = levels)



  if(facet == "data"){
    plotdata <- split.data.frame(plotdata, plotdata$group)
    group <- "model"

    if(length(plotdata) == 1L){
      title <- "Figure: Calibration plots of the nomogram for training set"
    }else{
      title <- "Figure: Calibration plots of the nomogram for training set (A) and and validation set (B)"
    }
  }else if(facet == "model"){
    plotdata <- split.data.frame(plotdata, plotdata$model)
    group <- "group"

    if(length(plotdata) == 1L){
      title <- "Figure: Calibration plots for nomogram model"
    }else{
      title <- sprintf("%s (%s)", levels, LETTERS[1:length(levels)])
      title <- paste(title, collapse = ", ")
      title <- paste("Figure: Calibration plots for", title, sep = " ")
    }
  }

  plots <- lapply(plotdata, \(pdata) {
    plot_cal(
      pdata,
      linewidth = linewidth,
      linecolor = linecolor,
      xlab = xlab,
      ylab = ylab,
      xbreaks = xbreaks,
      ybreaks = ybreaks,
      fontfamily = fontfamily,
      fontsize = fontsize,
      group = group
    )
  })

  # set tags
  if(length(plots) >= 2L){
    plots <- Map(\(plot, tag){
      plot + gg_tags(tag)
    }, plots, LETTERS[1:length(plots)])
  }

  note <- paste("The gray line represents the ideal nomogram, other line(s)",
                "represents the corrected observed nomogram with %d bootstrap",
                "resamples. The predicted probability of risk by the nomogram",
                "is projected onto the x-axis, and the actual risk is projected",
                "onto the y-axis.", sep = " ")
  note <- sprintf(note, boot)

  if(explain){
    cat(title)
    cat("\n")
    cat(note)
  }

  patchwork::wrap_plots(plots)
}


plot_cal <- function(pdata, linewidth, linecolor, xlab, ylab, xbreaks, ybreaks, fontfamily, fontsize, group){

  minaxis <- min(c(pdata$predy, pdata$calibrated.corrected), na.rm = TRUE)
  maxaxis <- max(c(pdata$predy, pdata$calibrated.corrected), na.rm = TRUE)
  axis <- pretty(c(minaxis, maxaxis), 4)

  if(is.null(xbreaks)){
    xbreaks <- axis
  }

  if(is.null(ybreaks)){
    ybreaks <- axis
  }

  p <- ggplot2::ggplot(pdata) +
    ggplot2::geom_abline(intercept = 0, color = "#374E55FF", linetype = 2, linewidth = linewidth)  +
    ggplot2::geom_line(ggplot2::aes_string(x = "predy", y = "calibrated.corrected", color = group), linewidth = linewidth) +
    gg_theme_sci(legend.key.size = 1.2, font.family = fontfamily, font.size = fontsize) +
    gg_legend_position(c(1, 0)) +
    gg_delete_legend_title() +
    gg_xlab(xlab) +
    gg_ylab(ylab) +
    ggplot2::scale_x_continuous(breaks = xbreaks, limits = c(min(xbreaks), max(xbreaks)), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = ybreaks, limits = c(min(ybreaks), max(ybreaks)), expand = c(0, 0))

  if(!is.null(linecolor)){
    p <- p +
      ggplot2::scale_color_manual(values = linecolor)
  }

  p
}
