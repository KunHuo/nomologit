roc2 <- function(...,
                 newdata = NULL,
                 boot = 10,
                 facet = c("data", "model"),
                 linewidth = 0.5,
                 linecolor = NULL,
                 xlab = "Predicted probability",
                 ylab = "Actual probability",
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

  # plot data
  plotdata <- lapply(tasks, \(tk){
    train.data <- tk$train.data

    if(is.null(newdata)){
      test.data  <- tk$test.data
    }else{
      test.data  <- newdata
    }

    outcome    <- tk$outcome
    predictors <- tk$predictors

    if(!is.factor(train.data[[outcome]])){
      train.data[[outcome]] <- factor(train.data[[outcome]])
    }

    train.fit <- logistic(data = train.data, outcome = outcome, predictors = predictors, method = "glm")

    pROC::roc(response  = train.data[[outcome]],
              predictor = train.fit$fitted.values,
              direction = "<",
              levels    = levels(train.data[[outcome]]))
  })

  plotdata

}



.roc <- function(data,
                 outcome,
                 exposure,
                 positive = NULL,
                 combine = FALSE,
                 combine.only = FALSE,
                 smooth = FALSE,
                 smooth.args = list()){

  if(!is.factor(data[[outcome]])){
    data[[outcome]] <- factor(data[[outcome]])
  }

  if(!is.null(positive)){
    positive <- as.character(positive)
    negative <- setdiff(unique(data[[outcome]]), positive)
    data[[outcome]] <-  factor(data[[outcome]],  levels = c(negative, positive))
  }

  if(combine & length(exposure) != 1L){
    data$combine <- .pred_prob(data = data, outcome = outcome, exposure = exposure)
    if(combine.only){
      exposure <- "combine"
    }else{
      exposure <- c(exposure, "combine")
    }
  }

  data[exposure] <- lapply(data[exposure], function(x){
    if(is.factor(x)){
      factor(x, ordered = TRUE)
    }else if(is.character(x)){
      factor(x, ordered = TRUE)
    }else{
      x
    }
  })

  names(exposure) <- exposure

  lapply(exposure, function(x){
    res <- pROC::roc(response  = data[[outcome]],
                     predictor = data[[x]],
                     direction = "<",
                     levels    = levels(data[[outcome]]))
    if(smooth){
      res <- do_call(pROC::smooth, roc = res, smooth.args)
    }
    res
  })
}



.pred_prob <- function(data, outcome, exposure, newdata = NULL){
  frm <- paste(exposure, collapse = " + ")
  frm <- paste(outcome, frm, sep  = " ~ ")
  frm <- stats::as.formula(frm)

  fit <- stats::glm(formula = frm, data = data, family = stats::binomial(link = "logit"))

  if(is.null(newdata)){
    stats::predict(fit, type = "response")
  }else{
    stats::predict(fit, type = "response", newdata = newdata)
  }
}


.auc_string <- function(x, auc.ci = TRUE, digits = 2, method = "delong", boot = 1000, seed = 1234, progress = "text"){
  set.seed(seed)
  if(auc.ci){
    res <- pROC::ci.auc(x, method = method, boot.n = boot, progress = progress)
    sprintf("AUC = %s, 95%% CI: %s\u2013%s",
            format_digits(res[[2]], digits),
            format_digits(res[[1]], digits),
            format_digits(res[[3]], digits))
  }else{
    sprintf("AUC = %s", format_digits(pROC::auc(x), digits))
  }
}
