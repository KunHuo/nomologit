logistic <- function(data, dependent = NULL, independents = NULL){
  if(is.null(independents)){
    independents <- names(data)
  }
  independents <- setdiff(independents, dependent)

  frm <- paste(dependent, paste(independents, collapse = " + "), sep = " ~ ")
  frm <- stats::as.formula(frm)
  rms::lrm(frm, data = data, x = TRUE, y = TRUE)
}
