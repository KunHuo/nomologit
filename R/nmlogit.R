nmlogit <- function(train.data = NULL, test.data = NULL, outcome = NULL, predictors = NULL){
  out <- list(train.data = train.data,
       test.data  = test.data,
       outcome    = outcome,
       predictors = predictors)
  class(out) <- c("nmlogit", "list")
  out
}
