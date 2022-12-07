logistic <- function(data, outcome = NULL, predictors = NULL){
  if(is.null(predictors)){
    predictors <- names(data)
  }
  predictors <- setdiff(predictors, outcome)

  frm <- paste(outcome, paste(predictors, collapse = " + "), sep = " ~ ")
  frm <- stats::as.formula(frm)
  rms::lrm(frm, data = data, x = TRUE, y = TRUE)
}
