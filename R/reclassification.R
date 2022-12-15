#' Comparision of nomogram models
#'
#' @details The function computes the categorical and continuous net reclassification
#' improvement (NRI) and integrated discrimination improvement (IDI). A reclassification
#' table indicates the number of individuals who move to another risk category or
#' remain in the same risk category as a result of updating the risk model.
#' Categorical NRI equal to x% means that compared with individuals without outcome,
#' individuals with outcome were almost x% more likely to move up a category than
#' down. The function also computes continuous NRI, which does not require any
#' discrete risk categories and relies on the proportions of individuals with
#' outcome correctly assigned a higher probability and individuals without outcome
#' correctly assigned a lower probability by an updated model compared with the
#' initial model. IDI equal to x% means that the difference in average predicted
#' risks between the individuals with and without the outcome increased by x% in
#' the updated model.
#'
#' @param ... nomogram models.
#' @param cutoff cutoff values for risk categories NRI.if is NULL, the function will compute continuous NRI.
#' @param digits digits, default 3.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#'
#' # From nomogram task
#' tk1 <- nmtask(train.data = aps,
#'               outcome = "elope",
#'               predictors = c("age", "gender"))
#'
#' tk2 <- nmtask(train.data = aps,
#'               outcome = "elope",
#'               predictors = c("age", "gender", "place3"))
#'
#' tk3 <- nmtask(train.data = aps,
#'               outcome = "elope",
#'               predictors = c("age", "gender", "place3", "neuro", "danger"))
#'
#' # Compute continuous NRI and IDI
#' compare(tk1, tk2, tk3)
#' # or
#' reclassification(tk1, tk2, tk3)
#'
#' # Compute categorical NRI and IDI
#' compare(tk1, tk2, tk3, cutoff = 0.5)
#' # or
#' reclassification(tk1, tk2, tk3, cutoff = 0.5)
#'
#' # From logistic model
#' model1 <- glm(elope ~ age + gender, data = aps, family = binomial())
#' model2 <- glm(elope ~ age + gender + place3 + neuro, data = aps, family = binomial())
#'
#' compare(model1, model2, cutoff = c(0.2, 0.7))
#' compare(tk3, model2, cutoff = c(0.2, 0.7))
compare <- function(..., cutoff = NULL, digits = 3){
  models <- list(...)

  models <- lapply(models, function(m){
    if("nmtask" %in% class(m)){
      frm <- paste(m$predictors, collapse = " + ")
      frm <- paste(m$outcome, frm, sep = " ~ ")
      frm <- stats::as.formula(frm)
      stats::glm(formula = frm, data = m$train.data, family = stats::binomial())
    }else{
      m
    }
  })

  comp <- utils::combn(1:length(models), 2)
  comp <- as.data.frame(comp, stringsAsFactors = FALSE)

  out <- lapply(comp, function(x){

    Comparision <- sprintf("Model %d vs. Model %d", x[2], x[1])

    res <- reclass_exec(outcome = models[[x[1]]]$y,
                     predrisk1 = models[[x[1]]]$fitted.values,
                     predrisk2 = models[[x[2]]]$fitted.values,
                     cutoff = cutoff,
                     digits = digits)
    cbind(Comparision, res)
  })

  out <- do.call(rbind, out)

  if(!is.null(cutoff)){
    attr(out, "title") <- "Comparision of models using categorical NRI and IDI"
  }else{
    attr(out, "title") <- "Comparision of models using continuous NRI and IDI"
  }

  attr(out, "note") <- "Abbreviations: NRI, Net reclassification improvement; IDI, Integrated discrimination improvement"

  class(out) <- c("compare", class(out))

  out
}


#' @rdname compare
#' @export
reclassification <- compare


#' Print object
#'
#' @param x an object.
#' @param ... more.
#'
#' @keywords internal
#' @export
print.compare <- function(x, ...){
  print_booktabs(x, adj = c("l", "c"), ...)
}


improveProb <- function (x1, x2, y) {
  s <- is.na(x1 + x2 + y)
  if (any(s)) {
    s <- !s
    x1 <- x1[s]
    x2 <- x2[s]
    y <- y[s]
  }
  n <- length(y)
  y <- as.numeric(y)
  u <- sort(unique(y))
  if (length(u) != 2 || u[1] != 0 || u[2] != 1)
    stop("y must have two values: 0 and 1")
  r <- range(x1, x2)
  if (r[1] < 0 || r[2] > 1)
    stop("x1 and x2 must be in [0,1]")
  a <- y == 1
  b <- y == 0
  na <- sum(a)
  nb <- sum(b)
  d <- x2 - x1
  nup.ev <- sum(d[a] > 0)
  pup.ev <- nup.ev/na
  nup.ne <- sum(d[b] > 0)
  pup.ne <- nup.ne/nb
  ndown.ev <- sum(d[a] < 0)
  pdown.ev <- ndown.ev/na
  ndown.ne <- sum(d[b] < 0)
  pdown.ne <- ndown.ne/nb
  nri.ev <- pup.ev - pdown.ev
  v.nri.ev <- (nup.ev + ndown.ev)/(na^2) - ((nup.ev - ndown.ev)^2)/(na^3)
  se.nri.ev <- sqrt(v.nri.ev)
  z.nri.ev <- nri.ev/se.nri.ev
  nri.ne <- pdown.ne - pup.ne
  v.nri.ne <- (ndown.ne + nup.ne)/(nb^2) - ((ndown.ne - nup.ne)^2)/(nb^3)
  se.nri.ne <- sqrt(v.nri.ne)
  z.nri.ne <- nri.ne/se.nri.ne
  nri <- pup.ev - pdown.ev - (pup.ne - pdown.ne)
  se.nri <- sqrt(v.nri.ev + v.nri.ne)
  z.nri <- nri/se.nri
  improveSens <- sum(d[a])/na
  improveSpec <- -sum(d[b])/nb
  idi <- mean(d[a]) - mean(d[b])
  var.ev <- stats::var(d[a])/na
  var.ne <- stats::var(d[b])/nb
  se.idi <- sqrt(var.ev + var.ne)
  z.idi <- idi/se.idi
  structure(llist(n, na, nb, pup.ev, pup.ne, pdown.ev, pdown.ne,
                  nri, se.nri, z.nri, nri.ev, se.nri.ev, z.nri.ev, nri.ne,
                  se.nri.ne, z.nri.ne, improveSens, improveSpec, idi, se.idi,
                  z.idi, labels = FALSE), class = "improveProb")
}


llist <- function (..., labels = TRUE) {
  dotlist <- list(...)
  lname <- names(dotlist)
  name <- vname <- as.character(sys.call())[-1]
  for (i in 1:length(dotlist)) {
    vname[i] <- if (length(lname) && lname[i] != "")
      lname[i]
    else name[i]
    lab <- vname[i]
    if (labels) {
      lab <- attr(dotlist[[i]], "label", exact = TRUE)
      if (length(lab) == 0)
        lab <- vname[i]
    }
    # label(dotlist[[i]]) <- lab
    attr(dotlist[[i]], "label") <- lab
  }
  names(dotlist) <- vname[1:length(dotlist)]
  dotlist
}


reclass_exec <- function (outcome, predrisk1, predrisk2, cutoff = NULL, digits = 3) {

  outcome <- as.numeric(as.factor(outcome)) - 1

  cutoff.new <- c(0, cutoff, 1)

  c1 <- cut(predrisk1, breaks = cutoff.new, include.lowest = TRUE, right = FALSE)
  c2 <- cut(predrisk2, breaks = cutoff.new, include.lowest = TRUE, right = FALSE)

  c11 <- factor(c1, levels = levels(c1), labels = c(1:length(levels(c1))))
  c22 <- factor(c2, levels = levels(c2), labels = c(1:length(levels(c2))))

  x <- improveProb(x1 = as.numeric(c11) * (1/(length(levels(c11)))),
                   x2 = as.numeric(c22) * (1/(length(levels(c22)))),
                   y  = outcome)

  y <- improveProb(x1 = predrisk1,
                   x2 = predrisk2,
                   y  = outcome)

  fmt <- fmt_ci_3(digits = digits, sep = ", ")

  NRI.cat <- sprintf(fmt, x$nri, x$nri - 1.96 * x$se.nri, x$nri + 1.96 * x$se.nri)
  NRI.con <- sprintf(fmt, y$nri, y$nri - 1.96 * y$se.nri, y$nri + 1.96 * y$se.nri)
  IDI     <- sprintf(fmt, y$idi, y$idi - 1.96 * y$se.idi, y$idi + 1.96 * y$se.idi)

  NRI.cat.pvalue <- format_pvalue(2 * stats::pnorm(-abs(x$z.nri)), digits)
  NRI.con.pvalue <- format_pvalue(2 * stats::pnorm(-abs(y$z.nri)), digits)
  IDI.pvalue     <- format_pvalue(2 * stats::pnorm(-abs(y$z.idi)), digits)

  NRI.cat.res <- tibble::tibble("Categorical NRI (95% CI)" = NRI.cat, "P for NRI" = NRI.cat.pvalue)
  NRI.con.res <- tibble::tibble("Continuous NRI (95% CI)" = NRI.con, "P for NRI" = NRI.con.pvalue)
  IDI.res     <- tibble::tibble("IDI (95% CI)" = IDI, "P for IDI" = IDI.pvalue)

  if(is.null(cutoff)){
    cbind(NRI.con.res, IDI.res)
  }else{
    cbind(NRI.cat.res, IDI.res)
  }
}

