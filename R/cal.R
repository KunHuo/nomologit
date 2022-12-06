#' Draw calibrate curve
#'
#' @export
cal <- function(data, outcome, covariates, newdata = NULL){

   train  <- data[c(outcome, covariates)]
   dnames <- names(train)[sapply(train, \(x) is.factor(x) | is.character(x))][-1]
   train  <- srpubr::dummy(train, varnames = dnames)

   model <- logistic(data = train, dependent = outcome, independents = names(train)[-1])

   if(!is.null(newdata)){
     test <- newdata[c(outcome, covariates)]
     test <- srpubr::dummy(test, varnames = dnames)

     pred_f_validation <- predict(model, test)

     fit.vad <-
       rms::lrm(
         test[[outcome]] ~ pred_f_validation,
         data = test,
         x = T,
         y = T
       )

     plot(rms::calibrate(fit.vad),
          xlab = "Predicted probability",
          ylab = "Actual probability",
          subtitles = FALSE)
   }else{
     plot(rms::calibrate(model),
          xlab = "Predicted probability",
          ylab = "Actual probability",
          subtitles = FALSE)
   }

}
