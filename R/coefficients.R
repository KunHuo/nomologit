#' Model coefficients of logistic regression
#'
#' @param ... one or more object of 'nmtask' or 'glm'.
#' @param multivariable logical variable indicating whether to perform univariable
#' logistic analysis or multivariable logistic analysis (default).
#' @param effect.values Effect value.
#' @param conf.level Effect value level, defalut 0.95.
#' @param conf.brackets Confidence interval bracket, default '('.
#' @param conf.separator Confidence interval separator.
#' @param digits.pvalue digits for p value, default 3.
#' @param digits.effect digits for effect value, default 2.
#' @param ref.value Reference Category name, default 'Reference'.
#' @param filename filename, if you want to save to word.
#'
#' @return a list.
#' @export
coefs <- function(...,
                  multivariable = TRUE,
                  effect.values = c("net", "b", "se", "OR", "p"),
                  conf.level = 0.95,
                  conf.brackets = "()",
                  conf.separator = NULL,
                  digits.pvalue = 3,
                  digits.effect = 2,
                  ref.value = "Reference",
                  filename = "") {

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

  out <- lapply(tasks, \(tk){
    data <- tk$train.data
    outcome <- tk$outcome
    predictors <- tk$predictors

    if(multivariable){
      model <- logistic(data = data,
                        outcome = outcome,
                        predictors = predictors,
                        method = "glm")
      res <- typeset.glm(model,
                  conf.level = conf.level,
                  conf.brackets = conf.brackets,
                  conf.separator = conf.separator,
                  digits.pvalue = digits.pvalue,
                  digits.effect = digits.effect,
                  ref.value = ref.value,
                  select = effect.values)
      attr(res, "title") <- "Multivariable logistic regression analysis results"
      res
    }else{
      res <- lapply(predictors, \(x){
        model <- logistic(data = data,
                          outcome = outcome,
                          predictors = x,
                          method = "glm")
        typeset.glm(model,
                    conf.level = conf.level,
                    conf.brackets = conf.brackets,
                    conf.separator = conf.separator,
                    digits.pvalue = digits.pvalue,
                    digits.effect = digits.effect,
                    ref.value = ref.value,
                    select = effect.values,
                    filter = x)

      })
      res <- do.call(rbind, res)
      attr(res, "title") <- "Univariable logistic regression analysis results"
      res
    }
  })

  class(out) <- c("coefs", "list")

  if(filename == ""){
    out
  }else{

  }

}


typeset.glm <- function(x,
                        data = NULL,
                        outcome = NULL,
                        varnames = NULL,
                        conf.level = 0.95,
                        conf.brackets = NULL,
                        conf.separator = NULL,
                        digits.pvalue = 3,
                        digits.effect = 2,
                        ref.value = "Reference",
                        select = NULL,
                        filter = NULL,
                        fold = FALSE,
                        exp = FALSE,
                        term = FALSE,
                        ...){

  data <- extract_data(fit = x, data = data)

  varnames <- extract_varnames(fit = x, data = data, varnames = varnames)

  if(is.null(outcome)){
    event <- all.vars(x$formula)[1]
  }else{
    event <- outcome
  }

  if(x$family$family == "gaussian"){
    estimate.name <- "\u03b2"
    effect.name <- "\u03b2"
    statistic.name <-  "t"
    conf.separator <- " to "
  }else{
    estimate.name <- "B"
    effect.name <- "OR"
    statistic.name <- "Wald"
  }

  # tidy coefficients
  if(x$family$link == "logit" | x$family$link == "log"){
    coefs <- tidy_glm(x, conf.level = conf.level, exp = TRUE, ...)
  }else{
    coefs <- tidy_glm(x, conf.level = conf.level, exp = FALSE, ...)
  }

  # format coefficients
  coefs <- format_coefs(coefs,
                        conf.brackets = conf.brackets,
                        conf.separator = conf.separator,
                        digits.pvalue = digits.pvalue,
                        digits.effect = digits.effect)

  out <- format_variable_coefs(data = data, varnames = varnames, fold = fold, coefs = coefs)

  if(!fold){
    if(length(unique(data[[event]])) == 2L){
      desc <- describe_event(data = data, event = event, varnames = varnames)
      if(is.null(select)){
        select <- c("net", "effect", "p.value")
      }
    }else{
      desc <- describe_event(data = data, varnames = varnames)
      if(is.null(select)){
        select <- c("n", "effect", "p.value")
      }
    }
    out <- merge_left(out, desc, by = "term")
  }


  out <- merge_left(out, coefs, by = "term")

  out <- subset_stat(out, select)

  out <- set_reference(out, value = ref.value, digits.effect = digits.effect)


  out <- rename_output(out,
                       estimate = estimate.name,
                       effect = effect.name,
                       statistic = statistic.name,
                       conf.level = conf.level)

  if(!is.null(filter)){
    out <- out[out$varname %in% filter, ]
  }

  out <- delete_terms(out, term)

  #attr(out, "title") <- "Multivariable logistic regression"
  class(out) <- c("typeset", "data.frame")

  out
}


tidy_glm <- function(x, conf.int = TRUE, conf.level = 0.95, exp = TRUE, ...) {

  warn_on_appropriated_glm_class(x)
  warn_on_subclass(x)

  ret <- as.data.frame(summary(x)$coefficients)
  ret <- cbind(data.frame(term = row.names(ret)), ret)
  row.names(ret) <- NULL
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")

  # summary(x)$coefficients misses rank deficient rows (i.e. coefs that
  # summary.lm() sets to NA), catch them here and add them back
  coefs <- as.data.frame(stats::coef(x))
  names(coefs) <- "estimate"
  coefs <- cbind(data.frame(term = row.names(coefs)), coefs)
  row.names(coefs) <- NULL
  ret <- merge_left(coefs, y = ret[-2], by = "term")

  if (conf.int) {
    ci <-  confint_terms(x, level = conf.level)
    ret <- merge_left(ret, ci, by = "term")
  }

  ret$statistic <- ret$statistic ^ 2
  ret$effect <- ret$estimate

  if (exp) {
    ret <- exponentiate(ret)
  }

  ret
}


exponentiate <- function(data) {
  data$effect <- exp(data$effect)

  if ("conf.low" %in% colnames(data)) {
    data$conf.low <- exp(data$conf.low)
    data$conf.high <- exp(data$conf.high)
  }
  data
}


warn_on_appropriated_glm_class <- function(x) {
  warn_on_glm2(x)
  warn_on_stanreg(x)

  invisible(TRUE)
}

# the output of glm2::glm2 has the same class as objects outputted
# by stats::glm2. glm2 outputs are currently not supported (intentionally)
# so warn that output is not maintained.
warn_on_glm2 <- function(x) {
  if (!is.null(x$method)) {
    if (x$method == "glm.fit2") {
      warning("The supplied model object seems to be outputted from the glm2 ",
              "package. Tidiers for glm2 output are currently not ",
              "maintained; please use caution in interpreting broom output.")
    }
  }

  invisible(TRUE)
}

# stanreg objects subclass glm, glm tidiers error out (uninformatively),
# and the maintained stanreg tidiers live in broom.mixed.
warn_on_stanreg <- function(x) {
  if (!is.null(x$stan_function)) {
    stop("The supplied model object seems to be outputted from the rstanarm ",
         "package. Tidiers for mixed model output now live in the broom.mixed package.")
  }

  invisible(TRUE)
}


extract_data <- function(fit, data = NULL){
  if(is.null(data)){
    data <- fit$data
    if(is.null(data)){
      data <- fit$model
    }
    if(is.null(data)){
      stop("Must specify the data set to fit the model.", call. = FALSE)
    }
  }
  data
}


extract_varnames <- function(fit, data = NULL, varnames = NULL){
  if(is.null(varnames)){
    varnames <- attr(fit$terms, "term.labels")
    if(is.null(varnames)){
      stop("The variable names of the fitted model must be specified.", call. = FALSE)
    }
  }
  varnames <- varnames[varnames %in% names(data)]
  varnames
}

format_variable_coefs <- function(data, varnames, fold, coefs){
  add.first <- NULL
  add.last <- NULL
  if("(Intercept)" %in% coefs$term){
    add.first <- "(Intercept)"
  }

  if(any(regex_detect(coefs$term, pattern = ":", fixed = TRUE))){
    add.last <- coefs$term[regex_detect(coefs$term, pattern = ":", fixed = TRUE)]
  }

  format_variable(data = data,
                  varnames = varnames,
                  fold = fold,
                  add.first = add.first,
                  add.last = add.last)
}


format_coefs <- function(x,
                         conf.brackets = NULL,
                         conf.separator = NULL,
                         digits.pvalue = 3,
                         digits.effect = 2,
                         ci = TRUE){

  x$p.value <- format_pvalue(x$p.value, digits = digits.pvalue)
  x[sapply(x, is.numeric)] <- lapply(x[sapply(x, is.numeric)], function(i){
    format_digits(i, digits = digits.effect)
  })
  if(ci){
    ci.format <- format_conf(conf.brackets, conf.separator)
    x$effect <- sprintf(ci.format, x$effect, x$conf.low, x$conf.high)
  }
  x
}

format_conf <- function(conf.brackets = NULL, conf.separate = NULL){
  if(is.null(conf.brackets)){
    conf.brackets <- "()"
  }
  if(is.null(conf.separate)){
    conf.separate <- "\u2013"
  }
  sprintf("%%s %s%%s%s%%s%s",
          substr(conf.brackets, 1, 1),
          conf.separate,
          substr(conf.brackets, 2, 2))
}

format_variable <- function(data,
                            varnames = names(data),
                            fold = FALSE,
                            space = 4,
                            sep = " vs. ",
                            add.first = NULL,
                            add.last = NULL){

  space <- create_space(space)
  sep <- sprintf("%s", sep)
  execute <-  function(varname){
    label <- varname
    if(!is.null(data)){
      label <- attr(data[[varname]], "label")
      if(is.null(label)){
        label <- varname
      }
    }
    if(is.numeric(data[[varname]])){
      data.frame(term = varname, varname = varname, ref = FALSE, variable = label, stringsAsFactors = FALSE)
    }else if(is.factor(data[[varname]]) | is.character(data[[varname]])){
      levels <- extract_levels(data[[varname]])
      if(fold){
        style_factor1(varname, levels, label, space = space, sep = sep)
      }else{
        style_factor2(varname, levels, label, space = space)
      }
    }
  }

  res <- lapply(varnames, execute)
  res <- do.call(rbind, res)

  if(!is.null(add.first)){
    if(!is.data.frame(add.first)){
      first <- data.frame(term = add.first, varname = add.first, ref = FALSE,
                          variable = add.first, stringsAsFactors = FALSE)
      res <- rbind(first, res)
    }else{
      res <- rbind(add.first, res)
    }
  }

  if(!is.null(add.last)){
    if(!is.data.frame(add.last)){
      last <- data.frame(term = add.last, varname = add.last, ref = FALSE,
                         variable = add.last, stringsAsFactors = FALSE)
      res <- rbind(res, last)
    }else{
      res <- rbind(res, add.last)
    }
  }

  res
}


create_space <- function(number){
  paste0(rep(" ", number), collapse = "")
}



extract_levels <- function(x){
  if(is.character(x)){
    x <- as.factor(x)
  }
  levels(x)
}


style_factor1 <- function(varname, levels, label, space, sep) {
  if (length(levels) <= 2L) {
    variable <- sprintf("%s (%s%s%s)", label, levels[2], sep, levels[1])
    term <- paste0(varname, levels[2])
    data.frame(term = term, varname = varname, ref = FALSE, variable = variable, stringsAsFactors = FALSE)
  } else{
    term <- paste0(varname, levels[-1])
    term <- c(varname, term)
    variable <- paste(levels[-1], levels[1], sep = sep)
    variable <- paste0(space, variable)
    variable <- c(label, variable)
    data.frame(term = term, varname = varname, ref = FALSE, variable = variable, stringsAsFactors = FALSE)
  }
}


style_factor2 <- function(varname, levels, label, space) {
  term <- paste0(varname, levels)
  term <- c(varname, term)
  variable <- paste0(space, levels)
  variable <- c(label, variable)
  ref <- c(FALSE, TRUE, rep(FALSE, length(levels) - 1))
  data.frame(term = term, varname = varname, ref = ref,  variable = variable, stringsAsFactors = FALSE)
}

describe_event <- function(data, event = NULL, varnames = NULL, method = "n.total", digits = 1){
  if(length(unique(data[[event]])) != 2L){
    res <- lapply(varnames, function(x){
      if(is.factor(data[[x]]) | is.character(data[[x]])){
        res <- freq(x = data[[x]])
        res$term <- paste0(x, res$term)
        res
      }else if (is.numeric(data[[x]])){
        data.frame(term = x, overall = length(data[[x]]))
      }
    })
    res <- do.call(rbind, res)
    names(res)[2] <- "n.total"
    res[,] <- lapply(res[,], function(x) as.character(x))
    method <- "n.total"
    res

  }else{
    res <- lapply(varnames, function(x){
      if(is.factor(data[[x]]) | is.character(data[[x]])){
        res <- freq(data[[x]], g = data[[event]])
        res[[2]] <- as.numeric(res[[2]])
        res[[3]] <- as.numeric(res[[3]])
        res$total <- res[[2]] + res[[3]]
        res$event <- sprintf("%d/%d", res[[3]], res$total)
        res$term <- paste0(x, res$term)
      }else if (is.numeric(data[[x]])){
        res <- freq(rep(x, nrow(data)), g = data[[event]])
        res[[2]] <- as.numeric(res[[2]])
        res[[3]] <- as.numeric(res[[3]])
        res$total <- res[[2]] + res[[3]]
        res$event <- sprintf("%d/%d", res[[3]], res$total)
      }
      res
    })
    res <- do.call(rbind, res)
    names(res) <- c("term", "n.non.event", "n.event", "n.total", "n.event.total")
    res$r.non.event <- sprintf("%s%%", format_digits(res$n.non.event / res$n.total * 100, 1))
    res$r.event <- sprintf("%s%%", format_digits(res$n.event / res$n.total * 100, 1))
    res$n.ratio <- sprintf("%d (%s)", res$n.event, res$r.event)
    res[,] <- lapply(res[,], function(x) as.character(x))
    res
  }
}


freq <- function(x, g = NULL, type = 1, digits = NULL){
  if(is.null(g)){
    g <- rep("overall", length(x))
  }
  freqs <- table(x, g)
  ncol  <- ncol(freqs)
  rname <- row.names(freqs)
  cname <- colnames(freqs)
  freqs <- as.vector(freqs)
  if(is.null(digits)){
    freqs <- sprintf("%d", freqs)
  }
  freqs <- matrix(freqs, ncol = ncol)
  colnames(freqs)  <- cname
  freqs <- as.data.frame(freqs, stringsAsFactors = FALSE)
  cbind(data.frame(term = rname, stringsAsFactors = FALSE), freqs)
}


subset_stat <- function(data, values){

  values <- tolower(values)
  values <- sapply(values, function(x){
    if(x %in% c("b", "estimate")){
      "estimate"
    }else if(x %in% c("se", "std.error")){
      "std.error"
    }else if(x %in% c("effect", "e", "hr", "or", "rr", "pr")){
      "effect"
    }else if(x %in% c("stat", "statistic", "wald", "t", "t.value")){
      "statistic"
    }else if( x %in% c("p", "p.value", "pvalue")){
      "p.value"
    }else if(x %in% c("n.total", "ntotal", "total", "n", "nt")) {
      "n.total"
    }else if(x %in% c("n.event", "nevent", "event", "ne")) {
      "n.event"
    }else if(x %in% c("n.nonevent", "nnonevent", "nonevent", "nne")) {
      "n.non.event"
    }else if(x %in% c("n.event.total", "neventtotal", "net")) {
      "n.event.total"
    }else{
      NA
    }
  })
  values <- unique(values)
  values <- values[!is.na(values)]
  values <- values[values %in% names(data)]
  data <- data[c("term", "varname", "ref", "variable", values)]
  data
}


set_reference <- function(data, value, digits.effect = 2){
  if(is.numeric(value)){
    value <- format_digits(value, digits.effect)
  }
  if(!is.null(data$estimate)){
    data$estimate[data$ref] <- format_digits(0, digits.effect)
  }
  if(!is.null(data$effect)){
    data$effect[data$ref] <- value
  }
  data
}


rename_output <- function(out,
                          variable = "Variable",
                          estimate = "Estimate",
                          effect = "Effect",
                          statistic = "Statistic",
                          std.error = "Std. error",
                          p.value = "P value",
                          conf.level = 0.95){

  names(out)[which(names(out) == "variable")] <- variable
  names(out)[which(names(out) == "estimate")] <- estimate
  names(out)[which(names(out) == "effect")] <- sprintf("%s (%d%% CI)", effect, conf.level * 100)
  names(out)[which(names(out) == "std.error")] <- std.error
  names(out)[which(names(out) == "statistic")] <- statistic
  names(out)[which(names(out) == "p.value")] <- p.value

  names(out)[which(names(out) == "n.total")] <- "No. of total"
  names(out)[which(names(out) == "n.event")] <- "No. of event"
  names(out)[which(names(out) == "n.non.event")] <- "No. of nonevent"
  names(out)[which(names(out) == "n.event.total")] <- "No. of event/total"

  out
}

delete_terms <- function(data, term = FALSE){
  if(!term){
    if("term" %in% names(data)){
      data <- delete_column_by_name(data, "term")
    }
    if("varname" %in% names(data)){
      data <- delete_column_by_name(data, "varname")
    }
    if("ref" %in% names(data)){
      data <- delete_column_by_name(data, "ref")
    }
  }
  data
}


delete_column_by_name <- function(data, varnames){
  index <- sapply(varnames, function(x) { which(names(data) == x) })
  data[, -index, drop = FALSE]
}



warn_on_subclass <- function(x) {
  if (length(class(x)) > 1 && class(x)[1] != "glm") {
    subclass <- class(x)[1]
    dispatched_method <- class(x)[class(x) %in% c("glm", "lm")][1]

    warning(
      "Tidiers for objects of class ",
      subclass,
      " are not maintained by the broom team, and are only supported through ",
      "the ",
      dispatched_method,
      " tidier method. Please be cautious in interpreting and reporting ",
      "broom output.",
      call. = FALSE
    )
  }
}


confint_terms <- function(x, ...) {
  # warn on arguments silently being ignored
  ci <- suppressMessages(stats::confint.default(x, ...))

  # confint called on models with a single predictor
  # often returns a named vector rather than a matrix :(
  if (is.null(dim(ci))) {
    ci <- matrix(ci, nrow = 1)
    rownames(ci) <- names(stats::coef(x))[1]
  }
  ci <- as.data.frame(ci)
  ci <- cbind(data.frame(term = row.names(ci)), ci)
  row.names(ci) <- NULL
  names(ci) <- c("term", "conf.low", "conf.high")
  ci
}


