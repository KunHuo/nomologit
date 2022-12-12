#' Draw a nomogram
#'
#' @description
#' The nomogram can be interpreted as follows: (1) for each  variable,
#' draw a straight line up to the points axis to determine the points for that
#' variable, (2) repeat this process for each variable, (3) add the points for
#' all variables and locate the sum on the total points axis, and (4) draw a
#' straight line from total points down to risk.
#'
#' @details Set labels use [set_variable_labels] and [set_category_labels] function.
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
#' @param labels a list, labels for variables.
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
#' nm <- nom(data = aps,
#'           outcome = "elope",
#'           predictors = c("age", "gender", "place3"))
#' nm |>
#'   set_variable_labels(age = "Age (years)") |>
#'   set_variable_labels(gender = "Gender", place3 = "Placement") |>
#'   set_category_labels("Gender", Male = "M", Female = "F") |>
#'   set_category_labels("Placement", OutDay = "Outday")
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
                labels = NULL,
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
                           labels = NULL,
                           show.points = FALSE,
                           show.model = FALSE,
                           show.explain = TRUE,
                           ...){

  if(!is.null(labels)){
    for(i in 1:length(labels)){
      if(names(labels[i]) %in% names(data)){
        attr(data[[names(labels[i])]], "label") <- labels[[i]]
      }
    }
  }

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

  attr(nom, "xfrac") <- xfrac
  attr(nom, "points.label") <- points.label
  attr(nom, "total.points.label") <- total.points.label
  attr(nom, "cex.var") <- font.size
  attr(nom, "cex.axis") <- font.size
  attr(nom, "show.explain") <- show.explain
  attr(nom, "outcome") <- outcome

  class(nom) <- c("nomologit", "nomogram")

  nom
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
                        labels = NULL,
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
                 labels = labels,
                 show.points = show.points,
                 show.model = show.model,
                 show.explain = show.explain,
                 ...)
}


#' @rdname nom
#' @export
nom.glm <- function(data,
                       outcome = NULL,
                       predictors = NULL,
                       points.label = "Points",
                       total.points.label = "Total points",
                       funlabel = "Risk",
                       maxscale = 100,
                       xfrac = 0.35,
                       font.size = 12,
                       fun.at = NULL,
                       labels = NULL,
                       show.points = FALSE,
                       show.model = FALSE,
                       show.explain = TRUE,
                       ...){

  if(data$family[[1]] == "binomial"){
    train.data <- data$data
    outcome <- all.vars(data$formula)[1]
    predictors <- all.vars(data$formula)[-1]

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
                   labels = labels,
                   show.points = show.points,
                   show.model = show.model,
                   show.explain = show.explain,
                   ...)
  }
}


#' Print nomogram
#'
#' @param x an object of 'nomologit'.
#' @param ... more arguments
#'
#' @keywords internal
#' @export
print.nomologit <- function(x, ...){

  xfrac <- attr(x, "xfrac")
  points.label <- attr(x, "points.label")
  total.points.label <- attr(x, "total.points.label")
  font.size <- attr(x, "cex.var")

  show.explain <- attr(x, "show.explain")
  outcome <- attr(x, "outcome")

  plot(x,
       xfrac = xfrac,
       points.label = points.label,
       total.points.label = total.points.label,
       cex.var = font.size,
       cex.axis = font.size, ...)

  if(show.explain){
    cat(sprintf("Figure: Nomogram of risk model for predicting %s.\n", outcome))
    cat("The nomogram can be interpreted as follows: (1) for each variable, draw a straight line up to the points axis to determine the points for that variable, (2) repeat this process for each variable, (3) add the points for all variables and locate the sum on the total points axis, and (4) draw a straight line from total points down to risk.")
  }
}


#' Set labels for nomogram
#'
#' @param x a nomogram object from nom() function
#' @param ... "vairable=label"
#'
#' @seealso [set_category_labels]
#'
#' @return a nomogram.
#' @export
#'
#' @examples
#' nm <- nom(data = aps,
#'           outcome = "elope",
#'           predictors = c("age", "gender", "place3"))
#'
#' nm |>
#'   set_variable_labels(age = "Age (years)") |>
#'   set_variable_labels(gender = "Gender", place3 = "Placement") |>
#'   set_category_labels("Gender", Male = "M", Female = "F") |>
#'   set_category_labels("Placement", OutDay = "Outday")
set_variable_labels <- function(x, ...){
  labels <- list(...)
  for(i in 1:length(labels)){
    names(x)[names(x) == names(labels[i])] <- labels[[i]]
  }
  x
}


#' Set labels for category of variable
#'
#' @param x a nomogram object from [nom] function
#' @param variable variable name from nomogram.
#' @param ... "category=label"
#'
#' @seealso [set_variable_labels]
#'
#' @return a nomogram.
#' @export
#'
#' @examples
#' nm <- nom(data = aps,
#'           outcome = "elope",
#'           predictors = c("age", "gender", "place3"))
#'
#' nm |>
#'   set_variable_labels(age = "Age (years)") |>
#'   set_variable_labels(gender = "Gender", place3 = "Placement") |>
#'   set_category_labels("Gender", Male = "M", Female = "F") |>
#'   set_category_labels("Placement", OutDay = "Outday")
set_category_labels <- function(x, variable, ...){
  if(variable %in% names(x)){
    labels <- list(...)
    for(i in 1:length(labels)){
      x[[variable]][[1]][x[[variable]][[1]] == names(labels[i])] <- labels[[i]]
    }
  }
  x
}
