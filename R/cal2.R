cal2 <- function(...,
                 newdata = NULL,
                 B = 10,
                 facet = c("data", "model", "both"),
                 linewidth = 0.5,
                 linecolor = NULL,
                 xlab = "Predicted probability",
                 ylab = "Actual probability",
                 xbreaks = NULL,
                 ybreaks = NULL,
                 fontfamily = "serif",
                 fontsize = 12){

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

    train.fit <- logistic(data = train.data, outcome = outcome, predictors = names(train.data)[-1])
    train.cal <- rms::calibrate(train.fit, B = B)

    train.plotdata <- train.cal[, 1:3]
    train.plotdata <- as.data.frame(train.plotdata)
    train.plotdata$group <- "Training set"

    plotdata <- train.plotdata

    # test data
    if(!is.null(test.data)){
      test.data <- test.data[c(outcome, predictors)]
      test.data <- dummy.data.frame(test.data, varnames = dnames)
      test.pred <- stats::predict(train.fit, test.data)

      test.fit <- rms::lrm(test.data[[outcome]] ~ test.pred, data = test.data, x = T, y = T)
      test.cal <- rms::calibrate(test.fit, B = B)

      test.plotdata <- test.cal[, 1:3]
      test.plotdata <- as.data.frame(test.plotdata)
      test.plotdata$group <- "Validation set"

      plotdata <-  rbind(train.plotdata, test.plotdata)
    }
    plotdata
  })

  # set names
  if(is.null(names(tasks))){
    names(plotdata) <- sprintf("Model %d", 1:length(tasks))
  }else{
    names(plotdata) <- names(tasks)
  }

  levels <- names(plotdata)

  plotdata <- list_rbind(plotdata, varname = "model")
  plotdata$model <- factor(plotdata$model, levels = levels)

  if(facet == "data"){
    plotdata <- split.data.frame(plotdata, plotdata$group)
    group <- "model"
  }else if(facet == "model"){
    plotdata <- split.data.frame(plotdata, plotdata$model)
    group <- "group"
  }

  plots <- lapply(plotdata, \(pdata){
          plot_cal2(pdata,
                    linewidth = linewidth,
                    xlab = xlab,
                    ylab = ylab,
                    xbreaks =xbreaks,
                    ybreaks = ybreaks,
                    group = group)
  })

  # set tags
  if(length(plots) >= 2L){
    plots <- Map(\(plot, tag){
      plot + gg_tags(tag)
    }, plots, LETTERS[1:length(plots)])
  }

  patchwork::wrap_plots(plots)
}


plot_cal2 <- function(pdata, linewidth, xlab, ylab, xbreaks, ybreaks, group){

  minaxis <- min(c(pdata$predy, pdata$calibrated.corrected))
  maxaxis <- max(c(pdata$predy, pdata$calibrated.corrected))
  axis <- pretty(c(minaxis, maxaxis), 4)

  if(is.null(xbreaks)){
    xbreaks <- axis
  }

  if(is.null(ybreaks)){
    ybreaks <- axis
  }

  ggplot2::ggplot(pdata) +
    ggplot2::geom_abline(intercept = 0, color = "#374E55FF", linetype = 2, linewidth = linewidth)  +
    ggplot2::geom_line(ggplot2::aes_string(x = "predy", y = "calibrated.corrected", color = group), linewidth = linewidth) +
    gg_theme_sci(legend.key.size = 1.2) +
    gg_legend_position(c(1, 0)) +
    gg_delete_legend_title() +
    gg_xlab(xlab) +
    gg_ylab(ylab) +
    ggplot2::scale_x_continuous(breaks = xbreaks, limits = c(min(xbreaks), max(xbreaks)), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = ybreaks, limits = c(min(ybreaks), max(ybreaks)), expand = c(0, 0))
}
