logistic <- function(data, outcome, predictors, method = c("lrm", "glm")){
  method <- match.arg(method)

  frm <- paste(outcome, paste(predictors, collapse = " + "), sep = " ~ ")
  frm <- stats::as.formula(frm)
  if(method == "lrm"){
    rms::lrm(frm, data = data, x = TRUE, y = TRUE)
  }else{
    stats::glm(formula = frm, data = data, family = stats::binomial(link = "logit"))
  }
}
