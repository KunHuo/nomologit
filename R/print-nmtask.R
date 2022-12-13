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

  event <- levels(as.factor(train[[x$outcome]]))[2]
  cat(sprintf("* Positive event: %s\n", event))

  n.train.event <- sum(as.factor(train[[x$outcome]]) == event)

  cat(sprintf("  - training (%d): %.1f%%\n", n.train.event, n.train.event / nrow(train) * 100))

  if(!is.null(x$test.data)){
    n.test.event  <- sum(as.factor(test[[x$outcome]])  == event)
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
