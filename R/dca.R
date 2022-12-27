#' Draw DCA curves
#'
#' @inheritParams cal
#' @param thresholds Numeric vector of high risk thresholds to use when plotting
#' and calculating net benefit values.
#'
#' @inherit cal return
#' @export
#'
#' @examples
#' # Examples see in [nmtask] function.
dca <- function(...,
                newdata = NULL,
                model.names = NULL,
                boot = 10,
                thresholds = seq(0, 1, by = 0.01),
                facet = c("data"),
                linewidth = 0.5,
                linecolor = NULL,
                xlab = "Risk threshold",
                ylab = "Standardized net benefit",
                xbreaks = seq(0, 1, 0.2),
                ybreaks = seq(-0.2, 1, 0.2),
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

  plotdata <- lapply(tasks, \(tk){
    train.data <- tk$train.data

    if(is.null(newdata)){
      test.data  <- tk$test.data
    }else{
      test.data  <- newdata
    }

    outcome    <- tk$outcome
    predictors <- tk$predictors

    train.frm <- paste(predictors, collapse = " + ")
    train.frm <- paste(outcome, train.frm, sep = " ~ ")
    train.frm <- stats::as.formula(train.frm)
    set.seed(seed)
    train.dca <- dca_data(formula = train.frm, data = train.data, thresholds = thresholds, bootstraps = boot)

    train.plotdata <- train.dca$derived.data
    train.plotdata$group <- "Training set"

    if(!is.null(test.data)){

      train.fit <- logistic(data = train.data, outcome = outcome, predictors = predictors, method = "glm")
      test.pred <- stats::predict(train.fit, test.data)

      test.data$.pre <- test.pred
      test.frm <- paste(outcome, ".pre", sep = " ~ ")
      test.frm <- stats::as.formula(test.frm)
      set.seed(seed)
      test.dca <- dca_data(formula = test.frm, data = test.data, thresholds = thresholds, bootstraps = boot)

      test.plotdata <- test.dca$derived.data
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

  plotdata <- Map(function(d, name){
    d$model[!(d$model == "None" | d$model == "All")] <- name
    d
  }, plotdata, names(plotdata))


  plotdata <- list_rbind(plotdata, names.as.column = FALSE)
  plotdata[["model"]] <- factor(plotdata[["model"]], levels = c("None", "All", levels))

  plotdata <- split.data.frame(plotdata, plotdata[["group"]])

  plots <- lapply(plotdata, \(pdata){
    plot_dca(pdata,
              linewidth = linewidth,
              linecolor = linecolor,
              xlab = xlab,
              ylab = ylab,
              xbreaks = xbreaks,
              ybreaks = ybreaks,
              fontfamily = fontfamily,
              fontsize = fontsize,
              group = "model")
  })

  # set tags
  if(length(plots) >= 2L){
    plots <- Map(\(plot, tag){
      plot + gg_tags(tag)
    }, plots, LETTERS[1:length(plots)])
  }

  plots <- patchwork::wrap_plots(plots)

  attr(plots, "explain") <- explain
  plots <- add_title(plots, "title")
  plots <- add_note(plots, "note")
  class(plots) <- c("nmplot", class(plots))
  plots
}



plot_dca <- function(pdata, linewidth, linecolor, xlab, ylab, xbreaks, ybreaks, fontfamily, fontsize, group){

  p <- ggplot2::ggplot(pdata) +
    ggplot2::geom_line(ggplot2::aes_string(x = "thresholds", y = "sNB", color = group), linewidth = linewidth) +
    gg_theme_sci(legend.key.size = 1.2, font.family = fontfamily, font.size = fontsize) +
    gg_legend_position(c(1, 1)) +
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
