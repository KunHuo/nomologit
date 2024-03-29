#' Evaluate model performance
#'
#' @description
#' Evaluate the performance of the model, including the area under the ROC curve,
#' accuracy, sensitivity, specificity, positive predictive value, negative
#' predictive value, and Brier score.
#'
#' @param ... one or more object of 'nmtask' or 'glm'.
#' @param newdata new data for verification.
#' @param cutoff Cutoff is the prediction probability of logistic regression model,
#' between 0 and 1. By default, the best cut-off value is calculated according to
#' the maximum Yodon index.
#' @param probability Whether to calculate the probability of the best cutoff value.
#' @param digits digits, default 3.
#' @param filename filename, if you want to save to word.
#'
#' @details
#' About Brier score:
#'
#' The Brier score is a proper score function that measures the accuracy of
#' probabilistic predictions. It is applicable to tasks in which predictions
#' must assign probabilities to a set of mutually exclusive discrete outcomes.
#' The set of possible outcomes can be either binary or categorical in nature,
#' and the probabilities assigned to this set of outcomes must sum to one (where
#' each individual probability is in the range of 0 to 1).
#'
#' The lower the Brier score is for a set of predictions, the better the
#' predictions are calibrated. Note that the Brier score, in its most common
#' formulation, takes on a value between zero and one, since this is the largest
#' possible difference between a predicted probability (which must be between zero
#' and one) and the actual outcome (which can take on values of only 0 and 1).
#' (In the original (1950) formulation of the Brier score, the range is double,
#' from zero to two.)
#'
#' @references
#' Brier, G. W. (1950) Verification of forecasts expressed in terms of probability. Monthly Weather Review, 78, 1-3.
#'
#' @export
#'
#' @examples
#' # Examples see in [nmtask] function.
perf <- function(...,  newdata = NULL, cutoff = "best", probability = TRUE, digits = 3, filename = ""){

  tasks <- flatten_list(list(...))

  tasks <- lapply(tasks, \(tk){
    if("glm" %in% class(tk)){
      as_nmtask(tk)
    }else{
      tk
    }
  })

  exec <- function(tk){
    train.data <- tk$train.data

    if(is.null(newdata)){
      test.data  <- tk$test.data
    }else{
      test.data <- newdata
    }

    outcome <- tk$outcome
    predictors <- tk$predictors

    if(length(predictors) == 1L){
      if(!is.numeric(train.data[[predictors]])){
        probability <- TRUE
      }
    }else{
      probability <- TRUE
    }

    train.fit <- logistic(data = train.data, outcome = outcome, predictors = predictors, method = "glm")

    if(probability){
      train.data$.pred <- train.fit$fitted.values

      train.roc <- roc_exec(
        data = train.data,
        outcome = outcome,
        exposure = ".pred",
        threshold = cutoff,
        digits = digits
      )
    }else{
      train.roc <- roc_exec(
        data = train.data,
        outcome = outcome,
        exposure = predictors,
        threshold = cutoff,
        digits = digits
      )
    }

    names(train.roc) <- c("Items", sprintf("Training set (n=%d)", nrow(train.data)))
    train.roc[1, 1] <- "Cut-off *"

    # Brier score
    train.brier <- brier_score(train.data[[outcome]], train.fit$fitted.values)
    train.brier <- format_digits(train.brier, digits = digits)
    train.brier <- data.frame(brier = "Brier score", value = train.brier)
    names(train.brier) <- names(train.roc)
    train.roc <- rbind(train.roc, train.brier)

    if(!is.null(test.data)){

      test.pre <- stats::predict(train.fit, newdata = test.data, type = "response")
      test.fit <- stats::glm(test.data[[outcome]] ~ test.pre, family = stats::binomial())
      if(probability){
        test.data$.pred <- test.fit$fitted.values
        threshold <- as.numeric(train.roc[1, 2][[1]])

        test.roc <- roc_exec(data = test.data,
                             outcome = outcome,
                             exposure = ".pred",
                             threshold = threshold,
                             digits = digits)
      }else{
        threshold <- as.numeric(train.roc[1, 2][[1]])

        test.roc <- roc_exec(data = test.data,
                             outcome = outcome,
                             exposure = predictors,
                             threshold = threshold,
                             digits = digits)
      }

      test.roc[1, 2] <- "-"
      test.roc[1, 1] <- "Cut-off *"
      names(test.roc) <- c("Items", sprintf("Validation set (n=%d)", nrow(test.data)))

      # Brier score
      test.brier <- brier_score(test.data[[outcome]], test.fit$fitted.values)
      test.brier <- format_digits(test.brier, digits = digits)
      test.brier <- data.frame(brier = "Brier score", value = test.brier)
      names(test.brier) <- names(test.roc)
      test.roc <- rbind(test.roc, test.brier)

      merge_left(train.roc, test.roc, by = "Items")
    }else{
      train.roc
    }
  }

  out <- lapply(tasks, exec)

  # set names
  if(is.null(names(tasks))){
    names(out) <- sprintf("Nomogram %d", 1:length(tasks))
  }else{
    names(out) <- names(tasks)
  }

  if(length(tasks) == 1L){
    out <- list_rbind(out, collapse.names = FALSE, names.as.column = FALSE)
  }else{
    out <- list_rbind(out, collapse.names = TRUE, varname = "Models")
  }

  class(out) <- c("performance", class(out))

  attr(out, "title") <- string_title_roc(language = "en")
  attr(out, "note")  <- "Abbreviations: AUC, Area under the curve; PPV, Positive predictive value; NPV, Negative predictive value.\n* The best cut-off value is the predicted probability by the logistic model."

  if(filename == ""){
    out
  }else{
    write_docx.data.frame(out, path = filename)
  }
}


#' Print performance
#'
#' @param x a object of performance
#' @param ... more.
#'
#' @keywords internal
#' @export
print.performance <- function(x, ...){
  print_booktabs(x, adj = c("l", "c"))
}


brier_score <-function (resp, pred, scaled = FALSE, ...){
  res <- mean(resp * (1 - pred)^2 + (1 - resp) * pred^2)
  if (scaled) {
    mean_y <- mean(resp)
    Bmax <- mean_y * (1 - mean_y)^2 + (1 - mean_y) * mean_y^2
    res <- 1 - res/Bmax
  }
  return(res)
}


roc_exec <- function(data,
                     outcome,
                     exposure,
                     positive = NULL,
                     threshold = "best",
                     combine = FALSE,
                     combine.only = FALSE,
                     ci = TRUE,
                     ci.method = c("delong", "bootstrap"),
                     ci.sep = NULL,
                     ci.branket = c("(", "["),
                     smooth = FALSE,
                     smooth.args = list(),
                     digits = 2,
                     percent = FALSE,
                     language  = NULL,
                     table.number = NULL,
                     progress = "win",
                     boot.n = 1000,
                     seed = 1234,
                     ...){


  ci.method  <- match.arg(ci.method)
  language   <- "en"
  ci.branket <- match.arg(ci.branket)

  if(ci.method == "bootstrap"){
    smooth <- FALSE
  }

  outcome  <- select_variable(data, outcome)
  exposure <- select_variable(data, exposure)

  roclist <- .roc(data = data,
                  outcome  = outcome,
                  exposure = exposure,
                  positive = positive,
                  combine  = combine,
                  combine.only = combine.only,
                  smooth = smooth,
                  smooth.args = smooth.args)

  res <- Map(function(x, th){
    if(ci.method == "delong"){
      .delong(x, ci = ci, digits = digits, threshold = th, ci.sep = ci.sep, ci.branket = ci.branket, percent = percent)
    }else{
      .bootstrap(x, threshold = th, digits = digits, progress = progress, boot.n = boot.n, seed = seed, ci.sep = ci.sep, ci.branket = ci.branket, percent = percent)
    }
  }, roclist, threshold)

  res <- list_rbind(res, varname = string_variable(language))


  names(res)[names(res) == "Threshold"]   <- string_threshold(language)
  res[1][res[1] == "combine"]             <- string_combine(language)

  if(ci){
    names(res)[names(res) == "AUC"]         <- paste("AUC",                        "(95% CI)", sep = " ")
    names(res)[names(res) == "Accuracy"]    <- paste(string_accuracy(language),    "(95% CI)", sep = " ")
    names(res)[names(res) == "Specificity"] <- paste(string_specificity(language), "(95% CI)", sep = " ")
    names(res)[names(res) == "Sensitivity"] <- paste(string_sensitivity(language), "(95% CI)", sep = " ")
    names(res)[names(res) == "PPV"]         <- paste(string_PPV(language),         "(95% CI)", sep = " ")
    names(res)[names(res) == "NPV"]         <- paste(string_NPV(language),         "(95% CI)", sep = " ")
  }else{
    names(res)[names(res) == "Accuracy"]    <- string_accuracy(language)
    names(res)[names(res) == "Specificity"] <- string_specificity(language)
    names(res)[names(res) == "Sensitivity"] <- string_sensitivity(language)
    names(res)[names(res) == "PPV"]         <- string_PPV(language)
    names(res)[names(res) == "NPV"]         <- string_NPV(language)
  }

  res <- tibble::as_tibble(res)

  if(nrow(res) <= 3L){
    res <- transpose(res)
  }

  args <- list(
    data = data,
    outcome = outcome,
    exposure = exposure,
    positive = positive,
    threshold = threshold,
    combine = combine,
    combine.only = combine.only,
    ci = ci,
    ci.method = ci.method,
    smooth = smooth,
    smooth.args = smooth.args,
    digits = digits,
    language  = language,
    progress = progress,
    boot.n = boot.n,
    seed = seed
  )

  attr(res, "args") <- args

  class(res) <- c("srp.roc", class(res))
  res <- add_title(res, string_title_roc(language, table.number))
  res <- add_note(res, string_note_roc(language, ci = ci))


  if(ci){
    if(ci.method == "bootstrap"){
      res <- add_note(res, sprintf("The 95%% CI is computed with %d stratified bootstrap replicates.", boot.n))
    }else{
      res <- add_note(res, "The 95% CI is computed with delong method.")
    }
  }

  res

}



.delong <- function(object, threshold = "best", percent = FALSE, ci = TRUE, digits = 2, ci.sep = NULL, ci.branket = "("){

  fmt  <- fmt_ci_3(digits = digits, sep = ci.sep, bracket = ci.branket)
  fmt1 <- sprintf("%%.%df", digits)

  rets <- c("threshold", "accuracy", "sensitivity", "specificity", "ppv", "npv", "tp", "fp", "fn","tn")

  coords <- pROC::coords(object,
                         x = threshold,
                         ret = rets,
                         transpose = TRUE)

  tp <- coords[["tp"]]
  fp <- coords[["fp"]]
  fn <- coords[["fn"]]
  tn <- coords[["tn"]]

  q <- stats::qnorm(0.025, lower.tail = FALSE)

  acc       <- coords[["accuracy"]]
  acc.lower <- acc - q * (acc * (1 - acc) / (tp + fp + fn + tn))
  acc.upper <- acc + q * (acc * (1 - acc) / (tp + fp + fn + tn))

  se       <- coords[["sensitivity"]]
  se.lower <- se - q * sqrt(se * (1 - se) / (tp + fn))
  se.upper <- se + q * sqrt(se * (1 - se) / (tp + fn))

  sp       <- coords[["specificity"]]
  sp.lower <- sp - q * sqrt(sp * (1 - sp) / (fp + tn))
  sp.upper <- sp + q * sqrt(sp * (1 - sp) / (fp + tn))

  PPV       <- coords[["ppv"]]
  PPV.lower <- PPV - q * sqrt(PPV * (1 - PPV) / (tp + fp))
  PPV.upper <- PPV + q * sqrt(PPV * (1 - PPV) / (tp + fp))

  NPV       <- coords[["npv"]]
  NPV.lower <- NPV - q * sqrt(NPV * (1 - NPV) / (tn + fn))
  NPV.upper <- NPV + q * sqrt(NPV * (1 - NPV) / (tn + fn))


  if(percent){
    acc       <- acc * 100
    acc.lower <- acc.lower * 100
    acc.upper <- acc.upper * 100

    se       <- se * 100
    se.lower <- se.lower * 100
    se.upper <- se.upper * 100

    sp       <- sp * 100
    sp.lower <- sp.lower * 100
    sp.upper <- sp.upper * 100

    PPV       <- PPV * 100
    PPV.lower <- PPV.lower * 100
    PPV.upper <- PPV.upper * 100

    NPV       <- NPV * 100
    NPV.lower <- NPV.lower * 100
    NPV.upper <- NPV.upper * 100
  }

  AUC <- pROC::ci.auc(object)

  if(ci){
    data.frame(Threshold   = sprintf(fmt1, coords[["threshold"]]),
               AUC         = sprintf(fmt,  AUC[2], AUC[1],    AUC[3]),
               Accuracy    = sprintf(fmt,  acc,    acc.lower, acc.upper),
               Sensitivity = sprintf(fmt,  se,     se.lower,  se.upper),
               Specificity = sprintf(fmt,  sp,     sp.lower,  sp.upper),
               PPV         = sprintf(fmt,  PPV,    PPV.lower, PPV.upper),
               NPV         = sprintf(fmt,  NPV,    NPV.lower, NPV.upper),
               stringsAsFactors = FALSE)
  }else{
    data.frame(Threshold   = sprintf(fmt1, coords[["threshold"]]),
               AUC         = sprintf(fmt1, AUC[2]),
               Accuracy    = sprintf(fmt1, acc),
               Sensitivity = sprintf(fmt1, se),
               Specificity = sprintf(fmt1, sp),
               PPV         = sprintf(fmt1, PPV),
               NPV         = sprintf(fmt1, NPV),
               stringsAsFactors = FALSE)
  }
}


.bootstrap <- function(object, threshold = "best", percent = FALSE, digits = 2, ci.sep = NULL, ci.branket = "(", progress = "text", boot.n = 1000, seed = 1234){
  # set.seed(seed)
  rets <- c("threshold", "accuracy", "sensitivity", "specificity", "ppv", "npv")
  names(rets) <- c("Threshold", "Accuracy", "Sensitivity", "Specificity", "PPV", "NPV")

  res1 <- pROC::coords(object,
                       x = threshold,
                       ret = rets,
                       transpose = TRUE)

  res2 <- pROC::ci.coords(object,
                          x = threshold,
                          ret = rets,
                          transpose = TRUE,
                          boot.n = boot.n,
                          progress = progress)

  fmt1 <- sprintf("%%.%df", digits)
  fmt3 <- fmt_ci_3(digits = digits, sep = ci.sep, bracket = ci.branket)

  out <- lapply(rets, function(x){
    if(x == "threshold"){
      sprintf(fmt1, res1[x])
    }else{
      if(percent){
        sprintf(fmt3, res1[x] * 100, res2[[x]][1] * 100, res2[[x]][3] * 100)
      }else{
        sprintf(fmt3, res1[x], res2[[x]][1], res2[[x]][3])
      }
    }
  })

  AUC <- pROC::ci.auc(object, method = "bootstrap", boot.n = boot.n, progress = progress)
  AUC <- sprintf("%s (%s-%s)",
                 format_digits(pROC::auc(object), digits),
                 format_digits(AUC[[1]], digits),
                 format_digits(AUC[[3]], digits))

  out <- as.list(out)
  out <- as.data.frame(out)
  out <- append2(out, AUC, after = 1)
  names(out)[2] <- "AUC"
  out
}




roc_test <- function(object, ...){
  if(length(object) != 1L){
    comp <- utils::combn(names(object), 2)
    comp <- as.data.frame(comp, stringsAsFactors = FALSE)

    out <- lapply(comp, function(x){
      comparision <- paste(x[1], x[2], sep = " vs ")
      test <- pROC::roc.test(object[[x[1]]], object[[x[2]]])
      stat <- sprintf("%.3f", test$statistic)
      pvalue <- test$p.value
      pvalue <- ifelse(pvalue < 0.001, "<0.001", sprintf("%.3f", pvalue))
      data.frame(Comparision = comparision,
                 Statistic = stat,
                 P =  pvalue,
                 stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, out)
    row.names(out) <- NULL
    out
  }
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


string_variable <- function(language){
  switch(language,
         en = "Variable",
         zh = "\u53d8\u91cf")
}


string_threshold <- function(language){
  switch(language,
         en = "Cut-off",
         zh = "\u4e34\u754c\u503c")
}


string_accuracy <- function(language){
  switch(language,
         en = "Accuracy",
         zh = "\u51c6\u786e\u5ea6")
}

string_combine <- function(language){
  switch(language,
         en = "Combine",
         zh = "\u8054\u5408")
}


string_sensitivity <- function(language){
  switch(language,
         en = "Sensitivity",
         zh = "\u654f\u611f\u5ea6")
}


string_specificity <- function(language){
  switch(language,
         en = "Specificity",
         zh = "\u7279\u5f02\u5ea6")
}


string_PPV <- function(language){
  switch(language,
         en = "PPV",
         zh = "\u9633\u6027\u9884\u6d4b\u503c")
}


string_NPV <- function(language){
  switch(language,
         en = "NPV",
         zh = "\u9634\u6027\u9884\u6d4b\u503c")
}

string_title_roc <- function(language, number = NULL){
  title <- switch(language,
                  en = "Performance metrics",
                  zh = "\u0052\u004f\u0043\u66f2\u7ebf\u8bc4\u4ef7\u6307\u6807")
  if(!is.null(number)){
    title <- switch(language,
                    en = paste(sprintf("Table %d:", number), title, sep = " "),
                    zh = paste(sprintf("\u8868%d", number),  title, sep = " "))
  }
  title
}


string_note_roc <- function(language, ci = TRUE){
  if(ci){
    switch(language,
           en = "Abbreviations: AUC, Area under the curve; CI, Confidence interval; PPV, Positive predictive value; NPV, Negative predictive value.",
           zh = "\u7f29\u7565\u8bcd\uff1a\u0041\u0055\u0043\u002c\u0020\u66f2\u7ebf\u4e0b\u9762\u79ef\u003b\u0020\u0043\u0049\u002c\u0020\u53ef\u4fe1\u533a\u95f4\u3002")
  }else{
    switch(language,
           en = "Abbreviations: AUC, Area under the curve; PPV, Positive predictive value; NPV, Negative predictive value.",
           zh = "\u7f29\u7565\u8bcd\uff1a\u0041\u0055\u0043\u002c\u0020\u66f2\u7ebf\u4e0b\u9762\u79ef\u3002")
  }
}
