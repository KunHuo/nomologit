#' Draw a nomogram
#'
#' @description
#' The nomogram can be interpreted as follows: (1) for each  variable,
#' draw a straight line up to the points axis to determine the points for that
#' variable, (2) repeat this process for each variable, (3) add the points for
#' all variables and locate the sum on the total points axis, and (4) draw a
#' straight line from total points down to risk.
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
#' @param show.points show points, default FALSE..
#' @param show.model show model, default FALSE.
#' @param show.explain explain the figure, default TRUE.
#' @param ... settings of variables to use in constructing axes.
#'
#' @export
#' @examples
#' head(aps)
#'
#' # Basic usage
#' nom(data = aps,
#'     outcome = "elope",
#'     predictors = c("age", "gender", "place3", "neuro"))
#'
#' # Set labels
#' attr(aps$age, "label") <- "Age at Admission (Years)"
#' attr(aps$gender, "label") <- "Gender"
#' attr(aps$place3, "label") <- "Placement"
#' attr(aps$neuro, "label") <- "Neuropsychiatric Disturbance"
#'
#' nom(data = aps,
#'     outcome = "elope",
#'     predictors = c("age", "gender", "place3", "neuro"),
#'     funlabel = "Risk of Elopement")
#'
#' # Set the scale of the risk axis.
#' nom(data = aps,
#'     outcome = "elope",
#'     predictors = c("age", "gender", "place3", "neuro"),
#'     funlabel = "Risk of Elopement",
#'     fun.at = c(0.1, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65))
#'
#' # Fraction of horizontal plot to set aside for axis titles.
#' nom(data = aps,
#'     outcome = "elope",
#'     predictors = c("age", "gender", "place3", "neuro"),
#'     funlabel = "Risk of Elopement",
#'     fun.at = c(0.1, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65),
#'     xfrac = 0.65)
#'
#' # show model and points
#' nom(data = aps,
#'     outcome = "elope",
#'     predictors = c("age", "gender", "place3", "neuro"),
#'     funlabel = "Risk of Elopement",
#'     fun.at = c(0.1, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65),
#'     xfrac = 0.65,
#'     show.points = TRUE,
#'     show.model = TRUE)
#'
#' # From nmtask
#' tk <- nmtask(train.data = aps,
#'              outcome = "elope",
#'              predictors = c("age", "gender", "place3", "neuro"))
#' nom(tk,
#'     funlabel = "Risk of Elopement",
#'     fun.at = c(0.1, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65),
#'     xfrac = 0.65)
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
                show.points = FALSE,
                show.model = FALSE,
                show.explain = TRUE,
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
                           show.points = FALSE,
                           show.model = FALSE,
                           show.explain = TRUE,
                           ...){

  pos <- 1
  envir = as.environment(pos)
  assign("dddd", rms::datadist(data), envir = envir)
  options(datadist = "dddd")

  model <- logistic(data = data, outcome = outcome, predictors = predictors)

  if(show.model){
    print(model)
  }

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

  if(show.points){
    suppressWarnings(print(nom))
  }

  if(show.explain){
    cat(sprintf("Figure: Nomogram of risk model for predicting %s.\n", outcome))
    cat("The nomogram can be interpreted as follows: (1) for each variable, draw a straight line up to the points axis to determine the points for that variable, (2) repeat this process for each variable, (3) add the points for all variables and locate the sum on the total points axis, and (4) draw a straight line from total points down to risk.")
  }

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
                        show.points = FALSE,
                        show.model = FALSE,
                        show.explain = TRUE,
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
                 show.points = show.points,
                 show.model = show.model,
                 show.explain = show.explain,
                 ...)
}
