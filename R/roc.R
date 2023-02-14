#' Draw ROC curves
#'
#' @inheritParams cal
#' @param probability Whether to calculate the probability of the best cutoff value.
#'
#' @inherit cal return
#' @export
#'
#' @examples
#' # Examples see in [nmtask] function.
roc <- function(...,
                newdata = NULL,
                model.names = NULL,
                boot = 10,
                facet = c("data", "model"),
                linewidth = 0.5,
                linecolor = NULL,
                xlab = "1 - Specificity",
                ylab = "Sensitivity",
                xbreaks = seq(0, 1, 0.2),
                ybreaks = seq(0, 1, 0.2),
                fontfamily = "serif",
                fontsize = 12,
                explain = TRUE,
                seed = 1234,
                probability = TRUE) {

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

    if(length(predictors) == 1L){
       if(!is.numeric(train.data[[predictors]])){
         probability <- TRUE
       }
    }else{
      probability <- TRUE
    }

    if(probability){
      train.fit <- logistic(data = train.data, outcome = outcome, predictors = predictors, method = "glm")
      train.roc <- suppressMessages(pROC::roc(response = train.data[[outcome]],
                                              predictor = train.fit$fitted.values,
                                              direction = "<"))
    }else{
      train.roc <- suppressMessages(pROC::roc(response = train.data[[outcome]],
                                              predictor = train.data[[predictors]],
                                              direction = "<"))
    }

    train.plotdata <- roc_plotdata(train.roc)
    train.plotdata$group <- "Training set"

    if(!is.null(test.data)){
      if(probability){
        test.pred <- stats::predict(train.fit, test.data)
        test.roc  <- suppressMessages(pROC::roc(response = test.data[[outcome]], predictor = test.pred, direction = "<"))
      }else{
        test.roc  <- suppressMessages(pROC::roc(response = test.data[[outcome]], predictor = test.data[[predictors]], direction = "<"))
      }
      test.plotdata <- roc_plotdata(test.roc)
      test.plotdata$group <- "Validation set"
      rbind(train.plotdata, test.plotdata)
    }else{
      train.plotdata
    }
  })

  # set names
  if(is.null(model.names)){
    if(is.null(names(tasks))){
      if(length(tasks) == 1L){
        names(plotdata) <- "Nomogram"
      }else{
        names(plotdata) <- sprintf("Nomogram %d", 1:length(tasks))
      }
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
      title <- "Figure: ROC curves of the nomogram in the training set"
    }else{
      title <- "Figure: ROC curves of the nomogram in the training set (A) and validation set (B)"
    }
  }else if(facet == "model"){
    plotdata <- split.data.frame(plotdata, plotdata$model)
    group <- "group"

    if(length(plotdata) == 1L){
      title <- "Figure: ROC curves for nomogram model"
    }else{
      title <- sprintf("%s (%s)", levels, LETTERS[1:length(levels)])
      title <- paste(title, collapse = ", ")
      title <- paste("Figure: ROC curves for", title, sep = " ")
    }
  }

  plots <- lapply(plotdata, \(pdata) {
    plot_roc(
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

  plots <- patchwork::wrap_plots(plots)

  attr(plots, "explain") <- explain
  plots <- add_title(plots, title)
  # plots <- add_note(plots, note)
  class(plots) <- c("nmplot", class(plots))
  plots
}



roc_plotdata <- function(x){
  df <- pROC::coords(x, "all", transpose = FALSE)
  df <- as.data.frame(df[rev(seq(nrow(df))), ])
  df$specificity <- 1 - df$specificity
  df
}


plot_roc <- function(pdata, linewidth, linecolor, xlab, ylab, xbreaks, ybreaks, fontfamily, fontsize, group){

  p <- ggplot2::ggplot(pdata) +
    ggplot2::geom_abline(intercept = 0, color = "#374E55FF", linetype = 2, linewidth = linewidth)  +
    ggplot2::geom_line(ggplot2::aes_string(x = "specificity", y = "sensitivity",  color = group), linewidth = linewidth) +
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


