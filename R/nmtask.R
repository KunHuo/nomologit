#' Task for nomogram
#'
#' @param train.data train data.
#' @param test.data test data, If there is no test set, it can not be specified.
#' If specified, it must have the same data structure as train data.
#' @param outcome predict outcome.
#' @param predictors predictors.
#' @param positive in which positive of outcome variable to make the comparison.
#' By default, positive is automatically defined. If outcome is a factor variable,
#' then positive is defined as the highest level. If outcome is a numerical
#' variable, then positive is defined as the largest value.
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
#' hl_test(tk)
#'
#' # Draw a nomogram
#' nom(tk)
#'
#' # Draw calibrate curve
#' cal(tk)
#'
#' # Draw ROC curve
#' roc(tk)
#'
#' # Draw decision curve
#' dca(tk)
nmtask <- function(train.data, test.data = NULL, outcome = NULL, positive = NULL, predictors = NULL){
  if(is.list(predictors)){
    lapply(predictors, \(x){
      nmtask(train.data = train.data,
             test.data = test.data,
             outcome = outcome,
             positive = positive,
             predictors = x)
    })
  }else{
    outcome    <- select_variable(train.data, outcome)
    predictors <- select_variable(train.data, predictors)
    DNAMETRAIN <- deparse(substitute(train.data))

    if(is.null(positive)){
      if(is.numeric(train.data[[outcome]])){
        positive <- max(train.data[[outcome]])
      }else if(is.factor(train.data[[outcome]])){
        positive <- levels(train.data[[outcome]])[2]
      }else{
        stop("You need to specify the positive event of outcome.", call. = FALSE)
      }
    }

    negative <- setdiff(unique(train.data[[outcome]]), positive)
    train.data[[outcome]] <-  ifelse( train.data[[outcome]] == positive, 1, 0)

    if(!is.null(test.data)){
      check_name(test.data, outcome)
      check_name(test.data, predictors)
      DNAMETEST  <- deparse(substitute(test.data))

      test.data[[outcome]] <- ifelse(test.data[[outcome]] == positive, 1, 0)

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
                DNAMETEST  = DNAMETEST,
                outcome    = outcome,
                positive   = positive,
                negative   = negative,
                predictors = predictors)

    class(out) <- c("nmtask", "list")

    out
  }
}



#' Print nmtask
#'
#' @param x an object of class 'nmtask'.
#' @param ... further arguments.
#'
#' @keywords internal
#'
#' @export
print.nmtask <- function(x, ...){
  cat("<Nomogram task for logistic>\n")
  cat("* Data set:\n")

  cat(sprintf("  - training: %s [%dx%d]\n", x$DNAMETRAIN, nrow(x$train.data), ncol(x$train.data)))

  if(!is.null(x$test.data)){
    cat(sprintf("  - validation: %s [%dx%d]\n", x$DNAMETEST, nrow(x$test.data), ncol(x$test.data)))
  }

  cat(sprintf("* Outcome: %s\n", x$outcome))

  train <- x$train.data
  test  <- x$test.data

  cat(sprintf("* Positive event: %s\n", x$positive))

  n.train.event <- sum(train[[x$outcome]])

  cat(sprintf("  - training (%d): %.1f%%\n", n.train.event, n.train.event / nrow(train) * 100))

  if(!is.null(x$test.data)){
    n.test.event  <- sum(test[[x$outcome]])
    cat(sprintf("  - validation (%d): %.1f%%\n", n.test.event, n.test.event / nrow(test) * 100))
  }

  cat(sprintf("* Predictors (%d):\n", length(x$predictors)))

  index.num  <- sapply(train[x$predictors], is.numeric)
  index.fct  <- sapply(train[x$predictors], is.factor)
  index.chr <- sapply(train[x$predictors], is.character)

  if(sum(index.num) > 0L){

    names <- paste(names(train[x$predictors])[index.num], collapse = ", ")

    cat(sprintf("  - num (%d): %s\n", sum(index.num), names))
  }

  if(sum(index.fct) > 0L){
    names <- paste(names(train[x$predictors])[index.fct], collapse = ", ")
    cat(sprintf("  - fct (%d): %s\n", sum(index.fct), names))
  }

  if(sum(index.chr) > 0L){
    names <- paste(names(train[x$predictors])[index.chr], collapse = ", ")
    cat(sprintf("  - chr (%d): elope\n", sum(index.chr), names))
  }
}
