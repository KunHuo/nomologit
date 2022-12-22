dca_data <- function (formula,
                      data,
                      family = stats::binomial(link = "logit"),
                      policy = c("opt-in","opt-out"),
                      fitted.risk = FALSE,
                      thresholds = seq(0, 1, by = 0.01),
                      confidence.intervals = 0.95,
                      bootstraps = 500,
                      study.design = c("cohort", "case-control"),
                      population.prevalence){

  call <- match.call()
  stopifnot(class(formula) == "formula")
  stopifnot(is.data.frame(data))
  stopifnot(is.logical(fitted.risk))
  stopifnot(is.numeric(thresholds))
  stopifnot(all(thresholds >= 0))
  stopifnot(all(thresholds <= 1))
  if (is.numeric(confidence.intervals))
    stopifnot(confidence.intervals > 0 & confidence.intervals < 1)
  stopifnot(is.numeric(bootstraps))
  policy <- match.arg(policy)
  opt.in = policy == "opt-in"
  study.design <- match.arg(study.design)
  if (!missing(population.prevalence)) {
    stopifnot(is.numeric(population.prevalence))
    stopifnot(population.prevalence > 0 & population.prevalence < 1)
  }
  if (any(names.check <- !is.element(all.vars(formula), names(data))))
    stop(paste("variable(s)", paste(all.vars(formula)[names.check],
                                    collapse = ", "), "not found in \"data\""))
  data <- data[, all.vars(formula)]
  cc.ind <- stats::complete.cases(data)
  if (sum(cc.ind) < nrow(data))
    warning(paste(sum(1 - cc.ind), "observation(s) with missing data removed"))
  data <- data[cc.ind, ]
  if (missing(population.prevalence))
    population.prevalence <- NULL
  if (study.design == "cohort") {
    if (!is.null(population.prevalence)) {
      warning("population.prevalence was provided, but study.design = \"cohort\". The value input for population.prevalence will be ignored. If you are using case-control data, please set study.design = \"case-control\".")
    }
  }
  else {
    if (missing(population.prevalence)) {
      stop("Need to set population.prevalence to calculate decision curves using case-control data.")
      if (family$family != "binomial")
        stop("Calculations for case-control data are done assuming logistic regression (family = binomial(link = \"logit\"))")
    }
    else {
      stopifnot(0 < population.prevalence & population.prevalence <
                  1)
      message("Calculating net benefit curves for case-control data. All calculations are done conditional on the outcome prevalence provided.")
    }
  }
  outcome <- data[[all.vars(formula[[2]])]]
  predictors <- c(Reduce(paste, deparse(formula[[3]])), "All",
                  "None")
  predictor.names <- c(Reduce(paste, deparse(formula)), "All",
                       "None")
  if (length(unique(outcome)) != 2)
    stop("outcome variable is not binary (it does not take two unique values).")
  stopifnot(is.numeric(outcome))
  if (min(outcome) != 0 | max(outcome) != 1)
    stop("outcome variable must be binary taking on values 0 for control and 1 for case.")
  if (fitted.risk) {
    if (length(all.vars(formula[[3]])) > 1)
      stop("When fitted.risk = TRUE, there can only be one term  (denoting the fitted risks) on the right hand side of the formula provided.")
    provided.risks <- data[[Reduce(paste, deparse(formula[[3]]))]]
    if (min(provided.risks) < 0 | max(provided.risks) > 1)
      stop("When fitted.risks = TRUE, all risks provided must be between 0 and 1.")
  }
  else if (length(strsplit(predictors[[1]], "+", fixed = TRUE)[[1]]) >
           1) {
    message("Note:  The data provided is used to both fit a prediction model and to estimate the respective decision curve. This may cause bias in decision curve estimates leading to over-confidence in model performance. ")
  }
  formula.ind <- c(ifelse(fitted.risk, FALSE, TRUE), FALSE,
                   FALSE)
  data[["All"]] <- 1
  data[["None"]] <- 0
  n.preds <- length(predictors)
  n.out <- length(predictors) * length(thresholds)
  dc.data <- data.frame(thresholds = numeric(n.out), FPR = numeric(n.out),
                        FNR = numeric(n.out), TPR = numeric(n.out), TNR = numeric(n.out),
                        NB = numeric(n.out), sNB = numeric(n.out), rho = numeric(n.out),
                        prob.high.risk = numeric(n.out), prob.low.risk = numeric(n.out),
                        DP = numeric(n.out), nonDP = numeric(n.out), model = numeric(n.out))
  if (is.numeric(confidence.intervals)) {
    if (bootstraps < 1)
      stop("bootstraps must be greater than 0. If no confidence intervals are needed, set `confidence.intervals = \"none\"`")
    B.ind <- matrix(nrow = nrow(data), ncol = bootstraps)
    if (study.design == "cohort") {
      for (b in 1:bootstraps) B.ind[, b] <- sample.int(nrow(data),
                                                       replace = TRUE)
    }
    else {
      all.ind <- 1:nrow(data)
      uu <- unique(outcome)
      for (b in 1:bootstraps) {
        ind.1 <- sample(all.ind[outcome == uu[1]], replace = TRUE)
        ind.2 <- sample(all.ind[outcome == uu[2]], replace = TRUE)
        B.ind[, b] <- c(ind.1, ind.2)
      }
    }
    dc.data <- add.ci.columns(dc.data)
  }
  index = 1
  n.pred = 1
  for (i in 1:n.preds) {
    tmpNBdata <- calculate.nb(d = outcome, y = data[[predictors[[i]]]],
                              rH = thresholds, formula = formula, family = family,
                              data = data, formula.ind = formula.ind[i], casecontrol.rho = population.prevalence,
                              opt.in = opt.in)
    tmpNBdata$model <- predictor.names[[i]]
    if (is.numeric(confidence.intervals)) {
      boot.data <- apply(B.ind, 2, function(x) {
        calculate.nb(d = outcome[x], y = data[[predictors[[i]]]][x],
                     rH = thresholds, formula = formula, family = family,
                     data = data[x, ], formula.ind = formula.ind[i],
                     casecontrol.rho = population.prevalence, opt.in = opt.in)
      })
      alpha = 1 - confidence.intervals
      xx <- NULL
      for (rtn in names(boot.data[[1]][-1])) {
        tmpdat <- sapply(boot.data, function(xx) xx[,
                                                    rtn])
        tmpNBdata[[paste(rtn, "_lower", sep = "")]] <- apply(tmpdat,
                                                             1, stats::quantile, probs = alpha/2, type = 1, na.rm = TRUE)
        tmpNBdata[[paste(rtn, "_upper", sep = "")]] <- apply(tmpdat,
                                                             1, stats::quantile, probs = 1 - alpha/2, type = 1,
                                                             na.rm = TRUE)
      }
    }
    dc.data[index:(length(thresholds) * n.pred), ] <- tmpNBdata
    index = index + length(thresholds)
    n.pred = n.pred + 1
  }
  if (!is.numeric(confidence.intervals)) {
    dc.data <- add.ci.columns(dc.data)
  }
  dc.data$cost.benefit.ratio <- as.character(fractions(threshold_to_costbenefit(dc.data$thresholds,
                                                                                policy)))
  add.dash1 <- which(!is.element(1:nrow(dc.data), grep("/",
                                                       dc.data$cost.benefit.ratio)))
  dc.data$cost.benefit.ratio[add.dash1] <- paste(dc.data$cost.benefit.ratio[add.dash1],
                                                 "/1", sep = "")
  dc.data$cost.benefit.ratio <- gsub("/", ":", dc.data$cost.benefit.ratio)
  out <- list(derived.data = dc.data, confidence.intervals = confidence.intervals,
              policy = policy, call = call)
  class(out) = "decision_curve"
  invisible(out)
}


threshold_to_costbenefit <- function (rh, policy){
  if (policy == "opt-in") {
    out <- rh/(1 - rh)
  }
  else {
    out <- rh/(1 - rh)
  }
  out
}


add.ci.columns <- function (x){
  n.out = nrow(x)
  x$FPR_lower <- NA
  x$FPR_upper <- NA
  x$FNR_lower <- NA
  x$FNR_upper <- NA
  x$TPR_lower <- NA
  x$TPR_upper <- NA
  x$TNR_lower <- NA
  x$TNR_upper <- NA
  x$NB_lower <- NA
  x$NB_upper <- NA
  x$sNB_lower <- NA
  x$sNB_upper <- NA
  x$rho_lower <- NA
  x$rho_upper <- NA
  x$prob.high.risk_lower = NA
  x$prob.high.risk_upper = NA
  x$prob.low.risk_lower = NA
  x$prob.low.risk_upper = NA
  x$DP_lower = NA
  x$DP_upper = NA
  x$nonDP_lower = NA
  x$nonDP_upper = NA
  x
}

calculate.nb <- function (y, d, rH, formula, data, family, formula.ind, casecontrol.rho,
                          opt.in){
  if (formula.ind) {
    if (is.null(casecontrol.rho)) {
      myglm <- do.call(stats::glm, list(formula = formula, data = data,family = family))
      y <- stats::fitted(myglm)
    }
    else {
      obs.rho = mean(d)
      offset = -log((casecontrol.rho)/(1 - casecontrol.rho)) +
        log((obs.rho)/(1 - obs.rho))
      myglm <- do.call(stats::glm, list(formula = formula, data = data,
                                 family = family, offset = rep(offset, nrow(data))))
      y = stats::predict(myglm, type = "link") - offset
      y <- exp(y)/(1 + exp(y))
    }
  }
  N = length(y)
  tnf = sum.I(rH, ">", y[d == 0])/sum(d == 0)
  fnf = sum.I(rH, ">", y[d == 1])/sum(d == 1)
  tpf.den <- sum(d == 1)
  tpf <- sum.I(rH, "<", y[d == 1])/tpf.den
  fpf <- sum.I(rH, "<", y[d == 0])/(N - tpf.den)
  if (is.null(casecontrol.rho)) {
    rho = mean(d == 1)
    prob.high.risk <- sum.I(rH, "<", y)/length(y)
  }
  else {
    rho = casecontrol.rho
    prob.high.risk <- rho * sum.I(rH, "<", y[d == 1])/length(y[d == 1]) + (1 - rho) * sum.I(rH, "<", y[d == 0])/length(y[d ==
                                                                                                                        0])
  }
  if (opt.in) {
    nb = tpf * rho - (rH/(1 - rH)) * (1 - rho) * fpf
    snb = nb/rho
  }
  else {
    nb = tnf * (1 - rho) - fnf * rho * ((1 - rH)/rH)
    snb = nb/(1 - rho)
  }
  dp <- tpf * rho
  non.dp <- (1 - fpf) * (1 - rho)
  out = data.frame(threshold = rH, FPR = fpf, FNR = fnf, TPR = tpf,
                   TNR = tnf, NB = nb, sNB = snb, rho = rho, prob.high.risk = prob.high.risk,
                   prob.low.risk = 1 - prob.high.risk, DP = dp, nonDP = non.dp)
}

sum.I <- function (yy, FUN, Yi, Vi = NULL) {
  if (FUN == "<" | FUN == ">=") {
    yy <- -yy
    Yi <- -Yi
  }
  pos <- rank(c(yy, Yi), ties.method = "f")[1:length(yy)] -
    rank(yy, ties.method = "f")
  if (substring(FUN, 2, 2) == "=")
    pos <- length(Yi) - pos
  if (!is.null(Vi)) {
    if (substring(FUN, 2, 2) == "=")
      tmpind <- order(-Yi)
    else tmpind <- order(Yi)
    Vi <- apply(as.matrix(Vi)[tmpind, , drop = F], 2, cumsum)
    return(rbind(0, Vi)[pos + 1, ])
  }
  else return(pos)
}


fractions <- function (x, cycles = 10, max.denominator = 2000, ...) {
  ans <- .rat(x, cycles, max.denominator)
  ndc <- paste(ans$rat[, 1], ans$rat[, 2], sep = "/")
  int <- ans$rat[, 2] == 1
  ndc[int] <- as.character(ans$rat[int, 1])
  structure(ans$x, fracs = ndc, class = c("fractions", class(ans$x)))
}

.rat <- function (x, cycles = 10, max.denominator = 2000) {
  a0 <- rep(0, length(x))
  A <- matrix(b0 <- rep(1, length(x)))
  fin <- is.finite(x)
  B <- matrix(floor(x))
  r <- as.vector(x) - drop(B)
  len <- 0
  while (any(which <- fin & (r > 1/max.denominator)) && (len <- len +
                                                         1) <= cycles) {
    a <- a0
    b <- b0
    a[which] <- 1
    r[which] <- 1/r[which]
    b[which] <- floor(r[which])
    r[which] <- r[which] - b[which]
    A <- cbind(A, a)
    B <- cbind(B, b)
  }
  pq1 <- cbind(b0, a0)
  pq <- cbind(B[, 1], b0)
  len <- 1
  while ((len <- len + 1) <= ncol(B)) {
    pq0 <- pq1
    pq1 <- pq
    pq <- B[, len] * pq1 + A[, len] * pq0
  }
  pq[!fin, 1] <- x[!fin]
  list(rat = pq, x = x)
}
