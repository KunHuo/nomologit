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
  var.ev <- var(d[a])/na
  var.ne <- var(d[b])/nb
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


reclassification <- function (outcome, predrisk1, predrisk2, cutoff = NULL, digits = 3) {

  outcome <- as.numeric(as.factor(outcome)) - 1

  cutoff <- c(0, cutoff, 1)

  c1 <- cut(predrisk1, breaks = cutoff, include.lowest = TRUE, right = FALSE)
  c2 <- cut(predrisk2, breaks = cutoff, include.lowest = TRUE, right = FALSE)

  c11 <- factor(c1, levels = levels(c1), labels = c(1:length(levels(c1))))
  c22 <- factor(c2, levels = levels(c2), labels = c(1:length(levels(c2))))

  x <- improveProb(x1 = as.numeric(c11) * (1/(length(levels(c11)))),
                   x2 = as.numeric(c22) * (1/(length(levels(c22)))),
                   y  = outcome)

  y <- improveProb(x1 = predrisk1,
                   x2 = predrisk2,
                   y  = outcome)

  NRI.cat <- sprintf("%.3f (%.3f, %.3f)", x$nri, x$nri - 1.96 * x$se.nri, x$nri + 1.96 * x$se.nri)
  NRI.con <- sprintf("%.3f (%.3f, %.3f)", y$nri, y$nri - 1.96 * y$se.nri, y$nri + 1.96 * y$se.nri)
  IDI     <- sprintf("%.3f (%.3f, %.3f)", y$idi, y$idi - 1.96 * y$se.idi, y$idi + 1.96 * y$se.idi)

  NRI.cat.pvalue <- sprintf("%.3f", 2 * stats::pnorm(-abs(x$z.nri)))
  NRI.con.pvalue <- sprintf("%.3f", 2 * stats::pnorm(-abs(y$z.nri)))
  IDI.pvalue     <- sprintf("%.3f", 2 * stats::pnorm(-abs(y$z.idi)))


  NRI.cat.res <- tibble::tibble("NRI (95% CI)" = NRI.cat, "P for NRI" = NRI.cat.pvalue)
  NRI.con.res <- tibble::tibble("NRI (95% CI)" = NRI.con, "P for NRI" = NRI.con.pvalue)
  IDI.res     <- tibble::tibble("IDI (95% CI)" = IDI, "P for IDI" = IDI.pvalue)

  cbind(NRI.cat.res, IDI.res)
}




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
    comparision <- paste(x[2], x[1], sep = " vs ")

    res <- reclassification(outcome = models[[x[1]]]$y,
                     predrisk1 = models[[x[1]]]$fitted.values,
                     predrisk2 = models[[x[2]]]$fitted.values,
                     cutoff = cutoff,
                     digits = digits)
    cbind(comparision, res)
  })

  out <- do.call(rbind, out)
  attr(out, "title") <- "Comparision"
  out
}


