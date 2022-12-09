#' Draw decision curves
#'
#' @param data data
#' @param outcome predict outcome.
#' @param predictors predictors.
#' @param newdata new data for verification.
#' @param B  Number of bootstrap replicates to use to calculate confidence intervals (default 1000).
#' @param thresholds thresholds.
#' @param xlab defaults to "Predicted x-units Survival" or to a suitable label for other models.
#' @param ylab defaults to "Fraction Surviving x-units" or to a suitable label for other models.
#' @param ... further arguments.
#'
#' @export
dca <- function(data, outcome = NULL, predictors = NULL, newdata = NULL, B = 1000, thresholds = seq(0, 1, by = 0.01), xlab = NULL, ylab = NULL, ...){
  UseMethod("dca")
}


#' @rdname dca
#' @export
dca.data.frame <- function(data, outcome = NULL, predictors = NULL, newdata = NULL, B = 1000, thresholds = seq(0, 1, by = 0.01), xlab = NULL, ylab = NULL, ...){


  data[[outcome]] <- as.numeric(as.factor(data[[outcome]])) - 1

  frm <- paste(predictors, collapse = " + ")
  frm <- paste(outcome, frm, sep = " ~ ")
  frm <- stats::as.formula(frm)

  fit <- rmda::decision_curve(formula = frm, data = data, thresholds = thresholds, bootstraps = B)

  rmda::plot_decision_curve(fit, confidence.intervals = FALSE)

  plotdata <- fit$derived.data

  plotdata$model[!(plotdata$model == "None" | plotdata$model == "All")] <- "Nomogram"

  plotdata$model <- factor(plotdata$model, levels = c("Nomogram", "All", "None"))

  ggplot2::ggplot(plotdata) +
    ggplot2::geom_line(ggplot2::aes_string(x = "thresholds", y = "sNB", color = "model", linetype = "model")) +
    gg_theme_sci() +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = seq(-0.2, 1, 0.2), limits = c(-0.2, 1), expand = c(0, 0)) +
    ggplot2::scale_color_manual(values = c("#DF8F44FF", "#00A1D5FF", "#374E55FF")) +
    ggplot2::scale_linetype_manual(values = c(1, 1, 2)) +
    gg_legend_position(c(1, 1)) +
    gg_delete_legend_title() +
    gg_xlab("Risk threshold") +
    gg_ylab("Standardized net benefit")

}


#' @rdname dca
#' @export
dca.nmtask <- function(data, outcome = NULL, predictors = NULL, newdata = NULL, B = 1000, thresholds = seq(0, 1, by = 0.01), xlab = NULL, ylab = NULL, ...){

}
