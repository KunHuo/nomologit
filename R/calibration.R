calibration <- function (data, outcome, predictors, groups = 10) {

  data[[outcome]] <- as.numeric(as.factor(data[[outcome]])) - 1

  frm <- paste(predictors, collapse = " + ")
  frm <- paste(outcome, frm, sep = " ~ ")
  frm <- stats::as.formula(frm)

  fit <- stats::glm(formula = frm, data = data, family = stats::binomial())

  p <- fit$fitted.values
  y <- data[[outcome]]

  if (length(unique(y)) != 2) {
    stop(" The specified outcome is not a binary variable.\n")
  }
  else {
    matres <- matrix(NA, nrow = groups, ncol = 5)
    sor <- order(p)
    p <- p[sor]
    y <- y[sor]
    groep <- cut2(p, g = groups)
    total <- tapply(y, groep, length)
    predicted <- round(tapply(p, groep, sum), 2)
    observed <- tapply(y, groep, sum)
    meanpred <- round(tapply(p, groep, mean), 3)
    meanobs <- round(tapply(y, groep, mean), 3)
    matres <- cbind(total, meanpred, meanobs, predicted, observed)
    matres <- as.data.frame(matres)

    ggplot2::ggplot(matres) +
      ggplot2::geom_abline(intercept = 0, color = "#374E55FF", linetype = 3, linewidth = 0.5) +
      ggplot2::geom_point(ggplot2::aes_string(x = "meanpred", y = "meanobs")) +
      ggplot2::geom_smooth(ggplot2::aes_string(x = "meanpred", y = "meanobs"), method = "loess", span = 0.95, linewidth = 0.5) +
      gg_theme_sci()
  }
}





