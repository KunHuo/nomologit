dummy <- function(x,
                  method = c("treatment", "sum", "helmert", "poly", "full"),
                  base = 1,
                  levels = NULL,
                  ...){
  UseMethod("dummy")
}


dummy.default <- function (x,
                           method = c("treatment", "sum", "helmert", "poly", "full"),
                           base = 1,
                           levels = NULL,
                           ...) {

  if (is.null(levels))
    x <- factor(x)
  else
    x <- factor(x, levels = levels)
  if (!is.numeric(base))
    base <- match(base, levels(x))
  method <- match.arg(arg = method,
                      choices = c("treatment", "sum", "helmert", "poly", "full"))
  switch(
    method,
    treatment = {
      res <- stats::contr.treatment(n = nlevels(x), base = base)[x,, drop = FALSE]
    },
    sum = {
      res <- stats::contr.sum(n = nlevels(x))[x, , drop = FALSE]
    },
    helmert = {
      res <- stats::contr.helmert(n = nlevels(x))[x, ,drop = FALSE]
    },
    poly = {
      res <- stats::contr.poly(n = nlevels(x))[x, ,drop = FALSE]
    },
    full = {
      res <- diag(nlevels(x))[x, ,drop = FALSE]
    }
  )
  res <- as.matrix(res)
  if (method == "full") {
    dimnames(res) <-
      list(if (is.null(names(x)))
        1L:length(x)
        else
          names(x),
        levels(x))
    attr(res, "base") <- NA
  }
  else {
    dimnames(res) <-
      list(if (is.null(names(x)))
        1L:length(x)
        else
          names(x),
        levels(x)[-base])
    attr(res, "base") <- levels(x)[base]
  }
  return(res)
}


dummy.data.frame <- function(x,
                             method = c("treatment", "sum", "helmert", "poly", "full"),
                             base = 1,
                             levels = NULL,
                             varnames = NULL,
                             keep = FALSE, sep = "_", ...) {

  method <- match.arg(method)

  if(is.null(varnames)){
    varnames <- names(x)[sapply(x, function(i) is.factor(i) | is.character(i))]
  }else{
    check_name(x, varnames)
  }

  if(length(varnames) == 0L){
    return(x)
  }

  for(i in seq_along(varnames)){
    res <- dummy(x[[varnames[i]]], method = method, base = base)
    res <- as.data.frame(res)
    names(res) <- paste(varnames[i], names(res), sep = sep)
    x <- append2(x, res, after = varnames[i])
    if(!keep){
      x[which(names(x) == varnames[i])] <- NULL
    }
  }
  x
}


check_name <- function(data, varnames){
  tmp <- varnames %in% names(data)
  if(!all(tmp)){
    tmpname <- varnames[!tmp]
    tmpname <- paste(tmpname, collapse = ", ")
    message <- sprintf("%s are (is) not included in the data frame.", tmpname)
    stop(message, call. = FALSE)
  }
}


set_names <- function (x, ...) {
  args <- list(...)
  if (is.null(names(args)))
    names(args) <- "names"
  names(args) <- lapply(names(args), match.arg, c("names",
                                                  "rownames", "colnames", "dimnames"))
  if ("dimnames" %in% names(args)) {
    if (is.null(args[["dimnames"]]))
      dimnames(x) <- NULL
    else dimnames(x) <- args[["dimnames"]]
  }
  if ("rownames" %in% names(args)) {
    if (is.null(args[["rownames"]]))
      rownames(x) <- NULL
    else rownames(x) <- rep_len(args[["rownames"]],
                                dim(x)[1])
  }
  if ("colnames" %in% names(args)) {
    if (is.null(args[["colnames"]]))
      colnames(x) <- NULL
    else colnames(x) <- rep_len(args[["colnames"]],
                                dim(x)[2])
  }
  if ("names" %in% names(args)) {
    if (is.null(args[["names"]]))
      names(x) <- NULL
    else names(x) <- rep_len(args[["names"]], length(x))
  }
  x
}


append2 <- function(x, values, after = NULL, ...){
  UseMethod("append2")
}


append2.default <- function(x, values, after = NULL, ...){
  if(is.character(after)){
    after <- which(names(x) == after)
  }
  if (is.null(after)) {
    after <- length(x)
  }
  append(x, values, after)
}

append2.matrix <- function (x, values, after = NULL, rows = FALSE, names = NULL, ...) {
  if(is.character(after)){
    after <- which(names(x) == after)
  }
  if (rows) {
    nr <- dim(x)[1]
    if (is.null(after))
      after <- nr
    values <- matrix(values, ncol = ncol(x))
    if (!is.null(names)) {
      err <- try(row.names(x) <- names, silent = TRUE)
      if (inherits(err, "try-error"))
        warning("Could not set rownames.")
    }
    if (!after)
      res <- rbind(values, x)
    else if (after >= nr)
      res <- rbind(x, values)
    else res <- rbind(x[1L:after, , drop = FALSE], values,
                      x[(after + 1L):nr, , drop = FALSE])
    colnames(res) <- colnames(x)
  }
  else {
    nc <- dim(x)[2]
    if (missing(after))
      after <- nc
    values <- matrix(values, nrow = nrow(x))
    if (!is.null(names))
      colnames(values) <- names
    if (!after)
      res <- cbind(values, x)
    else if (after >= nc)
      res <- cbind(x, values)
    else res <- cbind(x[, 1L:after, drop = FALSE], values,
                      x[, (after + 1L):nc, drop = FALSE])
    rownames(res) <- rownames(x)
  }
  return(res)
}


append2.data.frame <- function (x, values, after = NULL, rows = FALSE, names = NULL, ...) {

  if(is.character(after)){
    after <- which(names(x) == after)
  }

  .InsertRow <- function(x, val, after = nrow(x)) {
    x[seq(after + 1, nrow(x) + 1), ] <- x[seq(after, nrow(x)), ]
    x[after, ] <- val
    x
  }
  if (rows) {
    .InsertRow(x, values, after = after)
  }else {
    as.data.frame(append(x, set_names(list(values), names = names), after = after))
  }
}
