#' Task for nomogram
#'
#' @param train.data train data.
#' @param test.data test data.
#' @param outcome predict outcome.
#' @param predictors predictors.
#'
#' @return a list.
#' @export
nmtask <- function(train.data = NULL, test.data = NULL, outcome = NULL, predictors = NULL){

  check_name(train.data, outcome)
  check_name(train.data, predictors)

  if(!is.null(test.data)){
    check_name(test.data, outcome)
    check_name(test.data, predictors)
  }

  if(is.null(predictors)){
    predictors <- names(train.data)
  }

  predictors <- setdiff(predictors, outcome)

  out <- list(train.data = train.data,
       test.data  = test.data,
       outcome    = outcome,
       predictors = predictors)

  class(out) <- c("nmtask", "list")

  out
}
