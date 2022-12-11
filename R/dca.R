#' Draw decision curves
#'
#' @param data data
#' @param outcome predict outcome.
#' @param predictors predictors.
#' @param newdata new data for verification.
#' @param B  Number of bootstrap replicates to use to calculate confidence intervals (default 1000).
#' @param thresholds thresholds.
#' @param xlab label for X axis.
#' @param ylab label for Y axis.
#' @param linesize line size, default 0.25.
#' @param linecolor line color, the length must be 3.
#' @param linelabel line label,the length must be 3.
#' @param ... further arguments.
#'
#' @export
#' @examples
#' head(aps)
#'
#' # Basic usage
#' dca(aps,
#'     outcome = "elope",
#'     predictors = c("age", "gender", "place3", "neuro"))
#'
#' # From a nmtask
#' tk <- nmtask(train.data = aps,
#'              outcome = "elope",
#'              predictors = c("age", "gender", "place3", "neuro"))
#' dca(tk)
#'
#' # With validation
#' index <- sample(1:nrow(aps), 300)
#' train <- aps[index, ]
#' test  <- aps[-index, ]
#'
#' dca(train,
#'     outcome = "elope",
#'     predictors = c("age", "gender", "place3", "neuro"),
#'     newdata = test)
#' # or
#' tk <- nmtask(train.data = train,
#'              test.data = test,
#'              outcome = "elope",
#'              predictors = c("age", "gender", "place3", "neuro"))
#' dca(tk)
dca <- function(data, outcome = NULL, predictors = NULL, newdata = NULL, B = 1000,
                thresholds = seq(0, 1, by = 0.01), linesize = 0.5,
                linecolor = NULL, linelabel = NULL, xlab = NULL, ylab = NULL, ...){
  UseMethod("dca")
}


#' @rdname dca
#' @export
dca.data.frame <- function(data, outcome = NULL, predictors = NULL, newdata = NULL,
                           B = 1000, thresholds = seq(0, 1, by = 0.01),
                           linesize = 0.5, linecolor = NULL, linelabel = NULL, xlab = NULL, ylab = NULL, ...){

  options( warn = -1)

  data[[outcome]] <- as.numeric(as.factor(data[[outcome]])) - 1
  frm <- paste(predictors, collapse = " + ")
  frm <- paste(outcome, frm, sep = " ~ ")
  frm <- stats::as.formula(frm)
  fitA <- rmda::decision_curve(formula = frm, data = data, thresholds = thresholds, bootstraps = B)
  dcaA <- plot_dca(fitA, linesize = linesize, linecolor = linecolor, xlab = xlab, ylab = ylab)

  if(is.null(newdata)){
    cat("Figure: Decision curves of the nomogram for training set.\n")
    dcaA
  }else{
    newdata[[outcome]] <- as.numeric(as.factor(newdata[[outcome]])) - 1
    fitg <- stats::glm(formula = frm, family = stats::binomial(), data = data)
    pre <- stats::predict(fitg, newdata = newdata, type = "response")
    newdata$.pre <- pre
    frmB <- paste(outcome, ".pre", sep = " ~ ")
    frmB <- stats::as.formula(frmB)
    fitB <- rmda::decision_curve(formula = frmB, data = newdata, thresholds = thresholds, bootstraps = B)

    dcaB <- plot_dca(fitB, linesize = linesize, linecolor = linecolor, xlab = xlab, ylab = ylab)

    dcaA <- dcaA + gg_tags("A")
    dcaB <- dcaB + gg_tags("B")

    cat("Figure: Decision curves of the nomogram for training set (A) and validation set (B).\n")
    suppressMessages(patchwork::wrap_plots(dcaA, dcaB))
  }

}


#' @rdname dca
#' @export
dca.nmtask <- function(data, outcome = NULL, predictors = NULL, newdata = NULL,
                       B = 1000, thresholds = seq(0, 1, by = 0.01),
                       linesize = 0.5, linecolor = NULL, linelabel = NULL,
                       xlab = NULL, ylab = NULL, ...){

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

  dca.data.frame(data = train.data,
                 outcome = outcome,
                 predictors = predictors,
                 newdata = newdata,
                 B = B,
                 thresholds = thresholds,
                 linesize = linesize,
                 linecolor = linecolor,
                 linelabel = linelabel,
                 xlab = xlab,
                 ylab = ylab,
                 ...)
}


#' @rdname dca
#' @export
dca.glm <- function(data, outcome = NULL, predictors = NULL, newdata = NULL,
                    B = 1000, thresholds = seq(0, 1, by = 0.01),
                    linesize = 0.5, linecolor = NULL, linelabel = NULL,
                    xlab = NULL, ylab = NULL, ...){

  if(data$family[[1]] == "binomial"){

    train.data <- data$data
    outcome <- all.vars(data$formula)[1]
    predictors <- all.vars(data$formula)[-1]

    dca.data.frame(data = train.data, outcome = outcome, predictors = predictors, newdata = newdata,
                   B = B, thresholds = thresholds,
                   linesize = linesize, linecolor = linecolor, linelabel = linelabel,
                   xlab = xlab, ylab = ylab, ...)
  }

}


plot_dca <- function(fit, linesize = 0.5, linecolor = NULL, linelabel = NULL, xlab = NULL, ylab = NULL, ...){

  if(is.null(linecolor)){
    linecolor <- c("#DF8F44FF", "#00A1D5FF", "#374E55FF")
  }else{
    stopifnot(length(linecolor) == 3L)
  }

  if(is.null(linelabel)){
    linelabel <- c("Nomogram", "All", "None")
  }else{
    stopifnot(length(linelabel) == 3L)
  }

  if(is.null(xlab)){
    xlab <- "Risk threshold"
  }

  if(is.null(ylab)){
    ylab <- "Standardized net benefit"
  }

  plotdata <- fit$derived.data
  plotdata$model[!(plotdata$model == "None" | plotdata$model == "All")] <- "Nomogram"
  plotdata$model <- factor(plotdata$model, levels = c("Nomogram", "All", "None"), labels = linelabel)

  ggplot2::ggplot(plotdata) +
    ggplot2::geom_line(ggplot2::aes_string(x = "thresholds", y = "sNB", color = "model", linetype = "model"), size = linesize) +
    gg_theme_sci(legend.key.size = 1.2, ...) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = seq(-0.2, 1, 0.2), limits = c(-0.2, 1), expand = c(0, 0)) +
    ggplot2::scale_color_manual(values = linecolor) +
    ggplot2::scale_linetype_manual(values = c(1, 2, 3)) +
    gg_legend_position(c(1, 1)) +
    gg_delete_legend_title() +
    gg_xlab(xlab) +
    gg_ylab(ylab)
}
