#' Evaluate model performance
#'
#' @param data data
#' @param outcome predict outcome.
#' @param predictors predictors.
#' @param newdata new data for verification.
#' is printed about which variables were selected in each model re-fit.
#' @param digits digits, defalut 3.
#' @param filename file name to save performance metrics.
#' @param ... further arguments.
#'
#' @export
evaluate <- function(data, outcome = NULL, predictors = NULL, newdata = NULL, digits = 3, filename = "", ...){
  UseMethod("evaluate")
}



#' @rdname evaluate
#' @export
evaluate.data.frame <- function(data, outcome = NULL, predictors = NULL, newdata = NULL, digits = 3, filename = "", ...){

  outcome    <- select_variable(data, outcome)
  predictors <- select_variable(data, predictors)

  perA <- roc_exec(data = data,
                   outcome = outcome,
                   exposure = predictors,
                   combine = TRUE,
                   combine.only = TRUE,
                   digits = digits)

  names(perA) <- c("Items", "Training set")

  if(is.null(newdata)){
    if(filename == ""){
      perA
    }else{
      write_docx(perA, path = filename)
    }
  }else{
    frm <- paste(predictors, collapse = " + ")
    frm <- paste(outcome, frm, sep = " ~ ")
    frm <- stats::as.formula(frm)
    fit <- stats::glm(formula = frm, family = stats::binomial(), data = data)
    pre <- stats::predict(fit, newdata = newdata, type = "response")
    newdata$.pre <- pre

    perB <- roc_exec(data = newdata,
                     outcome = outcome,
                     exposure = ".pre",
                     digits = digits)

    names(perB) <- c("Items", "Validation set")

    per <- merge_left(perA, perB, by = "Items")
    attr(per, "title") <- attr(perB, "title")
    attr(per, "note") <- attr(perB, "note")
    if(filename == ""){
      per
    }else{
      write_docx(per, path = filename)
    }
  }
}



#' @rdname evaluate
#' @export
evaluate.nmtask <- function(data, outcome = NULL, predictors = NULL, newdata = NULL, digits = 3, filename = "", ...){

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

  evaluate.data.frame(data = train.data,
                      outcome = outcome,
                      predictors = predictors,
                      newdata = newdata,
                      digits = digits,
                      filename = filename,
                      ...)
}
