#' Draw a nomogram
#'
#' @description
#' The nomogram can be interpreted as follows: (1) for each  variable,
#' draw a straight line up to the points axis to determine the points for that
#' variable, (2) repeat this process for each variable, (3) add the points for
#' all variables and locate the sum on the total points axis, and (4) draw a
#' straight line from total points down to risk.
#'
#' @details Set labels use [label_variable] and [label_category] function.
#'
#' @param data a data frame
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
#'
#' @examples
#' # View data
#' head(HCC)
#'
#' # Randomly divide the data into training set and verification set
#' index <- sample(1:nrow(HCC), 12000)
#' train <- HCC[index, ]
#' test  <- HCC[-index, ]
#'
#' # Develop a prediction model task.
#' # The prediction outcome is status,
#' # and the prediction variables are AJCC_T, AJCC_M, and AJCC_N.
#' tk1 <- nmtask(train.data = train,
#'               test.data  = test,
#'               outcome    = "status",
#'               predictors = c("AJCC_T", "AJCC_M", "AJCC_N"))
#' # View task
#' tk1
#'
#' # Draw a nomogram
#' nom(tk1)
#'
#' # Set function values to label on axis
#' nom(tk1,
#'     fun.at = c(0.5, 0.6, 0.7, 0.8, 0.85, 0.9, 0.95))
nom <- function(data,
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
                show.explain = TRUE, ...) {

  if(!is.null(labels)){
    for(i in 1:length(labels)){
      if(names(labels[i]) %in% names(data)){
        attr(data[[names(labels[i])]], "label") <- labels[[i]]
      }
    }
  }

  train.data <- data$train.data
  outcome <- data$outcome
  predictors <- data$predictors

  pos <- 1
  envir = as.environment(pos)
  assign("dddd", rms::datadist(train.data), envir = envir)
  options(datadist = "dddd")

  model <- logistic(data = train.data, outcome = outcome, predictors = predictors)

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

#' Set labels for variable of nomogram
#'
#' @param x a nomogram object from nom() function
#' @param ... "vairable=label"
#'
#' @seealso [label_category]
#'
#' @return a nomogram.
#' @export
label_variable <- function(x, ...){
  labels <- list(...)
  for(i in 1:length(labels)){
    names(x)[names(x) == names(labels[i])] <- labels[[i]]
  }
  x
}


#' Set labels for category of nomogram
#'
#' @param x a nomogram object from [nom] function
#' @param variable variable name from nomogram.
#' @param ... "category=label"
#'
#' @seealso [label_variable]
#'
#' @return a nomogram.
#' @export
label_category <- function(x, variable, ...){
  if(variable %in% names(x)){
    labels <- list(...)
    for(i in 1:length(labels)){
      x[[variable]][[1]][x[[variable]][[1]] == names(labels[i])] <- labels[[i]]
    }
  }
  x
}
