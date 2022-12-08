#' Draw a nomogram
#'
#' @param data a data frame
#' @param outcome predict outcome.
#' @param predictors predictors.
#' @param points.label a character string giving the axis label for the points scale.
#' @param total.points.label a character string giving the axis label for the total points scale.
#' @param funlabel label for fun axis.
#' @param maxscale default maximum point score is 100.
#' @param xfrac fraction of horizontal plot to set aside for axis titles.
#' @param font.size font size.
#' @param fun.at function values to label on axis.
#' @param ... settings of variables to use in constructing axes.
#'
#' @export
nom <- function(data,
                outcome = NULL,
                predictors = NULL,
                points.label = "Points",
                total.points.label = "Total points",
                funlabel = "Risk",
                maxscale = 100,
                xfrac = 0.35,
                font.size = 12,
                fun.at = NULL,
                ...){

  UseMethod("nom")
}


#' @rdname nom
#' @export
nom.data.frame <- function(data,
                           outcome = NULL,
                           predictors = NULL,
                           points.label = "Points",
                           total.points.label = "Total points",
                           funlabel = "Risk",
                           maxscale = 100,
                           xfrac = 0.35,
                           font.size = 12,
                           fun.at = NULL,
                           ...){

  pos <- 1
  envir = as.environment(pos)
  assign("dddd", rms::datadist(data), envir = envir)
  options(datadist = "dddd")

  model <- logistic(data = data, outcome = outcome, predictors = predictors)

  # print(model)

  if(is.null(fun.at)){
    nom <- rms::nomogram(model,
                         fun = stats::plogis,
                         lp = FALSE,
                         funlabel = funlabel,
                         maxscale = maxscale, ...)
  }else{
    nom <- rms::nomogram(model,
                         fun = stats::plogis,
                         lp = FALSE,
                         funlabel = funlabel,
                         maxscale = maxscale,
                         fun.at = fun.at, ...)
  }



  if(exists("dddd")){
    rm("dddd", inherits = TRUE, envir = envir)
  }
  options(datadist = NULL)

  font.size <- font.size * 0.0834

  suppressWarnings(print(nom))

  plot(nom,
       xfrac = xfrac,
       points.label = points.label,
       total.points.label = total.points.label,
       cex.var = font.size,
       cex.axis = font.size)

  invisible(nom)
}


#' @rdname nom
#' @export
nom.nmtask <- function(data,
                        outcome = NULL,
                        predictors = NULL,
                        points.label = "Points",
                        total.points.label = "Total points",
                        funlabel = "Risk",
                        maxscale = 100,
                        xfrac = 0.35,
                        font.size = 12,
                        fun.at = NULL,
                        ...){

  train.data <- data$train.data

  if(is.null(outcome)){
    outcome <- data$outcome
  }

  if(is.null(predictors)){
    predictors <- data$predictors
  }

  nom.data.frame(data = train.data,
                 outcome = outcome,
                 predictors = predictors,
                 points.label = points.label,
                 total.points.label = total.points.label,
                 funlabel = funlabel,
                 maxscale = maxscale,
                 xfrac = xfrac,
                 font.size = font.size,
                 fun.at = fun.at,
                 ...)
}