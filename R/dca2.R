#' Draw DCA curves
#'
#' @inheritParams cal
#' @param thresholds Numeric vector of high risk thresholds to use when plotting
#' and calculating net benefit values.
#'
#' @inherit cal return
#' @export
dca2 <- function(...,
                newdata = NULL,
                boot = 10,
                thresholds = seq(0, 1, by = 0.01),
                facet = c("data", "model"),
                linewidth = 0.5,
                linecolor = NULL,
                xlab = "Risk threshold",
                ylab = "Standardized net benefit",
                xbreaks = NULL,
                ybreaks = NULL,
                fontfamily = "serif",
                fontsize = 12,
                explain = TRUE,
                seed = 1234) {

  facet <- match.arg(facet)

  tasks <- list(...)
  tasks <- flatten_list(tasks)

  # supoort glm
  tasks <- lapply(tasks, \(tk){
    if("glm" %in% class(tk)){
      as_nmtask(tk)
    }else{
      tk
    }
  })

  plotdata <- lapply(tasks, \(tk){
    train.data <- tk$train.data

    if(is.null(newdata)){
      test.data  <- tk$test.data
    }else{
      test.data  <- newdata
    }

    outcome    <- tk$outcome
    predictors <- tk$predictors

    train.frm <- paste(predictors, collapse = " + ")
    train.frm <- paste(outcome, train.frm, sep = " ~ ")
    train.frm <- stats::as.formula(train.frm)
    set.seed(seed)
    train.dca <- rmda::decision_curve(formula = train.frm, data = train.data, thresholds = thresholds, bootstraps = boot)

    train.plotdata <- train.dca$derived.data
    train.plotdata$group <- "Training set"

    if(!is.null(test.data)){

      train.fit <- logistic(data = train.data, outcome = outcome, predictors = predictors, method = "glm")
      test.pred <- stats::predict(train.fit, test.data)

      test.data$.pre <- test.pred
      test.frm <- paste(outcome, ".pre", sep = " ~ ")
      test.frm <- stats::as.formula(test.frm)
      set.seed(seed)
      test.dca <- rmda::decision_curve(formula = test.frm, data = test.data, thresholds = thresholds, bootstraps = boot)

      test.plotdata <- test.dca$derived.data
      test.plotdata$group <- "Validation set"
      rbind(train.plotdata, test.plotdata)

    }else{
      train.plotdata
    }
  })

  # set names
  if(is.null(names(tasks))){
    names(plotdata) <- sprintf("Model %d", 1:length(tasks))
  }else{
    names(plotdata) <- names(tasks)
  }

  plotdata <- Map(function(d, name){
    d$model[!(d$model == "None" | d$model == "All")] <- name
    d
  }, plotdata, names(plotdata))

  plotdata

}
