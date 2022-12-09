#' Draw ROC curve
#' @param data data
#' @param outcome predict outcome.
#' @param predictors predictors.
#' @param newdata new data for verification.
#' is printed about which variables were selected in each model re-fit.
#' @param linesize line size, default 0.25.
#' @param linecolor line color.
#' @param xlab label for X axis.
#' @param ylab label for Y axis.
#' @param filename file name to save performance metrics.
#' @param ... further arguments.
#'
#' @export
roc <- function(data, outcome = NULL, predictors = NULL, newdata = NULL, linesize =0.5, linecolor = NULL, xlab = NULL, ylab = NULL, filename = "", ...){
  UseMethod("roc")
}


#' @rdname roc
#' @export
roc.data.frame <- function(data, outcome = NULL, predictors = NULL, newdata = NULL, linesize =0.5, linecolor = NULL, xlab = NULL, ylab = NULL, filename = "", ...){

  if(is.null(xlab)){
    xlab <- "1 - Specificity"
  }

  if(is.null(ylab)){
    ylab <- "Sensitivity"
  }

  if(is.null(linecolor)){
    linecolor <- c("#00A1D5FF")
  }

  plotA <- gg_roc(data = data,
                  outcome = outcome,
                  exposure = predictors,
                  combine = TRUE,
                  combine.only = TRUE,
                  show.cutoff = FALSE,
                  line.size = linesize,
                  line.color = linecolor,
                  ...) +
    gg_delete_legend() +
    gg_xlab(xlab) +
    gg_ylab(ylab)

  perA <- roc_exec(data = data,
                 outcome = outcome,
                 exposure = predictors,
                 combine = TRUE,
                 combine.only = TRUE,
                 digits = 3)

  names(perA) <- c("Items", "Training set")

  if(is.null(newdata)){

    if(filename == ""){
      print_booktabs(perA)
      cat("\n")
    }else{
      write_docx(perA, path = filename)
    }

    cat("Figure: ROC curves of the nomogram for training set.\n")

    plotA
  }else{
    frm <- paste(predictors, collapse = " + ")
    frm <- paste(outcome, frm, sep = " ~ ")
    frm <- stats::as.formula(frm)
    fit <- stats::glm(formula = frm, family = stats::binomial(), data = data)
    pre <- stats::predict(fit, newdata = newdata, type = "response")
    newdata$.pre <- pre

    perB <- roc_exec(data = newdata,
                   outcome = outcome,
                   exposure = ".pre",
                   digits = 3)
    # perB[1, 2] <- ""
    names(perB) <- c("Items", "Validation set")

    per <- merge_left(perA, perB, by = "Items")
    attr(per, "title") <- attr(perB, "title")
    attr(per, "note") <- attr(perB, "note")
    if(filename == ""){
      print_booktabs(per)

      cat("\n")
    }else{
      write_docx(per, path = filename)
    }

    cat("Figure: ROC curves of the nomogram for training set (A) and validation set (B).\n")

    plotB <- gg_roc(data = newdata,
                    outcome = outcome,
                    exposure = ".pre",
                    show.cutoff = FALSE,
                    line.size = linesize,
                    line.color = linecolor, ...) +
      gg_delete_legend()  +
      gg_xlab(xlab) +
      gg_ylab(ylab)

    plotA <- plotA + gg_tags("A")
    plotB <- plotB + gg_tags("B")

    patchwork::wrap_plots(plotA, plotB)
  }
}


#' @rdname roc
#' @export
roc.nmtask <- function(data, outcome = NULL, predictors = NULL, newdata = NULL, linesize = 0.5, linecolor = NULL, xlab = NULL, ylab = NULL, filename = "",...){
  train.data <- data$train.data

  if(is.null(newdata)){
    newdata  <- data$test.data
  }

  if(is.null(outcome)){
    outcome <- data$outcome
  }

  if(is.null(predictors)){
    predictors <- data$predictors
  }

  roc.data.frame(data = train.data,
                 outcome = outcome,
                 predictors = predictors,
                 newdata = newdata,
                 xlab = xlab,
                 ylab = ylab,
                 linesize = linesize,
                 linecolor = linecolor, filename = filename, ...)
}



gg_roc <- function(data,
                   outcome,
                   exposure,
                   positive = NULL,
                   combine = FALSE,
                   combine.only = FALSE,
                   smooth = FALSE,
                   smooth.args = list(),
                   auc = FALSE,
                   auc.ci = FALSE,
                   auc.digits = 2,
                   auc.ci.method = c("delong", "bootstrap"),
                   show.cutoff = TRUE,
                   line.size = 0.5,
                   line.color = NULL,
                   line.type = NULL,
                   language  = NULL,
                   font.family = NULL,
                   font.size = NULL,
                   progress = "win",
                   boot.n = 1000,
                   seed = 1234,
                   ...){

  set.seed(seed)

  auc.ci.method <- match.arg(auc.ci.method)

  language    <- get_global_languange(language, default = "en")
  font.family <- get_global_family(font.family, default = "serif")
  font.size   <- get_global_fontsize(font.size, default = 12)
  line.color  <- get_global_palette(line.color)

  outcome  <- select_variable(data, outcome)
  exposure <- select_variable(data, exposure)

  roclist <- .roc(data = data,
                  outcome = outcome,
                  exposure = exposure,
                  positive = positive,
                  combine = combine,
                  combine.only = combine.only,
                  smooth = smooth,
                  smooth.args = smooth.args)

  if("combine" %in% names(roclist)){
    names(roclist)[names(roclist) == "combine"] <- string_combine(language)
  }

  if (auc) {
    auc.string <- sapply(roclist, function(x) {
      .auc_string(
        x,
        auc.ci = auc.ci,
        digits = auc.digits,
        method = auc.ci.method,
        boot.n = boot.n,
        seed = seed,
        progress = progress
      )
    })
    legends  <- sprintf("%s (%s)", names(roclist), auc.string)
    names(roclist) <- legends
  }
#
#   if(language == "zh"){
#     sysfonts::font_add("simsun", "simsun.ttc")
#     font.family <- "simsun"
#   }else{
#     font.family <- font.family
#   }

  if(is.null(line.type)){
    aes <- c("colour")
  }else{
    aes <- c("linetype", "colour")
  }

  p <- suppressMessages(
    pROC::ggroc(roclist, legacy.axes = TRUE, size = line.size, aes = aes) +
      ggplot2::geom_abline(intercept = 0, color = "#374E55FF", linetype = "dashed", size = line.size) +
      gg_xbreaks_continuous(0, 1, by = 0.2) +
      gg_ybreaks_continuous(0, 1, by = 0.2) +
      gg_xlab(sprintf("1 - %s", string_specificity(language))) +
      gg_ylab(string_sensitivity(language)) +
      gg_theme_sci(font.family = font.family, font.size = font.size, ...) +
      gg_legend_title(NULL) +
      gg_legend_position(c(1, 0))
  )

  if(show.cutoff){
    threshold.data <- lapply(names(roclist), function(x){

      object <- roclist[[x]]

      rets <- c("threshold", "sensitivity", "specificity")
      res <- coords <- pROC::coords(object,
                                    x = "best",
                                    ret = rets,
                                    transpose = TRUE)
      res <- as.data.frame(as.list(res))
      res$specificity <- 1 - res$specificity
      res
    })

    names(threshold.data) <- names(roclist)

    threshold.data <- list_rbind(threshold.data)

    p <- p +
      ggplot2::geom_point(data = threshold.data,
                          ggplot2::aes_string(x = "specificity", y = "sensitivity", color = "variable"),
                          show.legend = FALSE, size = 2.5)
  }


  if(!is.null(line.color)){
    p <- p +
      ggplot2::scale_color_manual(values = line.color)
  }

  if(!is.null(line.type)){
    p <- p +
      ggplot2::scale_linetype_manual(values = line.type)
  }

  p

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


.auc_string <- function(x, auc.ci = TRUE, digits = 2, method = "delong", boot.n = 1000, seed = 1234, progress = "text"){
  set.seed(seed)
  if(auc.ci){
    res <- pROC::ci.auc(x, method = method, boot.n = boot.n, progress = progress)
    sprintf("AUC = %s, 95%% CI: %s\u2013%s",
            format_digits(res[[2]], digits),
            format_digits(res[[1]], digits),
            format_digits(res[[3]], digits))
  }else{
    sprintf("AUC = %s", format_digits(pROC::auc(x), digits))
  }
}


#' Build ROCs
#'
#' @param data a data frame.
#' @param outcome the outcome variable name contain in data, which can be a factor
#' or a numerical variable, but must be a binary variable.
#' @param exposure one or more exposure variable names are included in data.
#' @param positive in which  positive of outcome variable to make the comparison.
#'  By default, positive is automatically defined. If outcome is a factor variable,
#'  then positive is defined as the highest level. If outcome is a numerical
#'  variable, then positive is defined as the largest value.
#' @param combine a logical, indicating whether to use multiple exposure variables
#' for joint prediction using logistic regression.
#' @param combine.only a logical, indicating whether to show only the results of
#' joint predictions.
#' @param smooth a logical, indicating whether to smooth a ROC curve.
#' @param smooth.args further arguments for smooth, see [pROC::smooth] function.
#' @param ci a logical, indicating whether to show ci.
#' @param ci.method the method to use, either “delong” or “bootstrap”. The
#' first letter is sufficient. If omitted, the appropriate method is selected as
#' explained in details.
#' @param ci.sep the separator for CI.
#' @param ci.branket the branket for CI.
#' @param digits digits, default 2.
#' @param percent percenttage, default FALSE.
#' @param threshold threshold, default "best", the optimal cut-off is the threshold
#' that calculating by youden index.
#' @param language language, typically “en”, or "zh", default "en".
#' @param table.number table number.
#' @param progress the name of progress bar to display. Typically “none”, “win”,
#' “tk” or “text”.
#' @param boot.n the number of bootstrap replicates, default 1000.
#' @param seed seed, default 1234.
#' @param ... unused arguments.
#'
#' @return a data frame.
#' @keywords internal
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
  language   <- get_global_languange(language, default = "en")
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

string_variable <- function(language){
  switch(language,
         en = "Variable",
         zh = "\u53d8\u91cf")
}


string_threshold <- function(language){
  switch(language,
         en = "Threshold",
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
