#' Task for nomogram
#'
#' @param train.data train data.
#' @param test.data test data, If there is no test set, it can not be specified.
#' If specified, it must have the same data structure as train data.
#' @param outcome predict outcome.
#' @param predictors predictors.
#'
#' @return a object of 'nmtask' class.
#' @export
#'
#' @examples
#' index <- sample(1:nrow(aps), 300)
#' # Training set
#' train <- aps[index, ]
#' # Validation set
#' test  <- aps[-index, ]
#'
#' # Set task of nomogram model
#' tk <- nmtask(train.data = train,
#'              test.data = test,
#'              outcome = "elope",
#'              predictors = c("age", "gender", "place3"))
#' # View task
#' tk
#'
#' # Hosmer-Lemeshow test
#' hoslem_test(tk)
#'
#' # Draw a nomogram
#' nom(tk)
#'
#' # Draw calibrate curve
#' cal(tk, B = 10)
#'
#' # Draw ROC curve
#' roc(tk)
#'
#' # Draw decision curve
#' dca(tk, B = 10)
nmtask <- function(train.data = NULL, test.data = NULL, outcome = NULL, predictors = NULL){

  outcome    <- select_variable(train.data, outcome)
  predictors <- select_variable(train.data, predictors)

  DNAMETRAIN <- deparse(substitute(train.data))

  if(!is.null(test.data)){
    check_name(test.data, outcome)
    check_name(test.data, predictors)
    DNAMETEST  <- deparse(substitute(test.data))
  }else{
    DNAMETEST  <- ""
  }

  if(is.null(predictors)){
    predictors <- names(train.data)
  }

  predictors <- setdiff(predictors, outcome)

  out <- list(train.data = train.data,
       test.data  = test.data,
       DNAMETRAIN = DNAMETRAIN,
       DNAMETEST = DNAMETEST,
       outcome    = outcome,
       predictors = predictors)

  class(out) <- c("nmtask", "list")

  out
}
