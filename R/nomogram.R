#' Draw a nomogram
#'
#' @param data a data frame
#' @param dependent dependent variable name.
#' @param independents independent variable names.
#' @param points.label a character string giving the axis label for the points scale.
#' @param total.points.label a character string giving the axis label for the total points scale.
#' @param funlabel label for fun axis.
#' @param maxscale default maximum point score is 100.
#' @param xfrac fraction of horizontal plot to set aside for axis titles.
#' @param font.size font size.
#' @param ... settings of variables to use in constructing axes.
#'
#' @export
nomogram <- function(data,
                     dependent = NULL,
                     independents = NULL,
                     points.label = "Points",
                     total.points.label = "Total points",
                     funlabel = "Risk",
                     maxscale = 100,
                     xfrac = 0.35,
                     font.size = 11,
                     ...){

  pos <- 1
  envir = as.environment(pos)
  assign("dddd", rms::datadist(data), envir = envir)
  options(datadist = "dddd")

  model <- logistic(data = data, dependent = dependent, independents = independents)
  nom <- rms::nomogram(model, fun = stats::plogis, lp = FALSE, funlabel = funlabel, maxscale = maxscale)

  print(model)

  plot(rms::calibrate(model), xlab = "Predicted probability", ylab = "Actual probability", subtitles = FALSE)

  if(exists("dddd")){
    rm("dddd", inherits = TRUE, envir = parent.frame())
  }
  options(datadist = NULL)

  font.size <- font.size * 0.0834

  plot(nom,
       xfrac = xfrac,
       points.label = points.label,
       total.points.label = total.points.label, cex.var = font.size, cex.axis = font.size, ...)
}
