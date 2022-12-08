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


format_digits <- function(x, digits){
  sapply(x, function(i){
    if(is.na(i)){
      i
    }else{
      fmt <- sprintf("%%.%df", digits)
      sprintf(fmt = fmt, i)
    }
  })
}

fmt_ci_3 <- function(sep = NULL, digits = 2, bracket = c("(", "[")){
  bracket <- match.arg(bracket)
  if(bracket == "("){
    bracket <- c("(", ")")
  }else{
    bracket <- c("[", "]")
  }
  if(is.null(sep)){
    sep <- "\u2013"
  }
  sprintf("%%.%df %s%%.%df%s%%.%df%s", digits, bracket[1], digits, sep, digits, bracket[2])
}


do_call <- function(what, ..., envir = parent.frame()){
  args <- list(...)
  args <- flatten_list(args)
  do.call(what, args = args, quote = FALSE, envir = envir)
}


select_variable <- function(data, ..., type = c("name", "data", "index")){

  type  <- match.arg(type)

  if(length(c(...)) == 0L){
    return(NULL)
  }

  index <- .col_index(data, ...)

  if(length(index) == 0L){
    return(NULL)
  }

  switch(type,
         data  = data[index],
         name  = {
           varname <- names(data)[index]
           names(varname) <- varname
           varname
         },
         index = index)
}


.col_index <- function(data, ...){
  varnames <- list(...)
  res <- lapply(varnames, function(x){
    if(is.numeric(x)){
      if(max(x) > ncol(data) | min(x) <= 0){
        stop("Out of range for column index.", call. = FALSE)
      }
      x
    }else{
      sapply(x, function(i){
        if(regex_detect(i, pattern = ":", fixed = TRUE)){
          st <- regex_split(i, pattern = ":", fixed = TRUE)[[1]]
          check_name(data, st[1])
          check_name(data, st[2])
          start <- which(names(data) == st[1])
          end   <- which(names(data) == st[2])
          start:end
        }else{
          check_name(data, i)
          which(names(data) == i)
        }
      })
    }
  })
  res <- unique(unlist(res))
  names(res) <- names(data)[res]
  res
}


list_rbind <- function(data,
                       names.as.column = TRUE,
                       collapse.names = FALSE,
                       collapse.one.row = FALSE,
                       varname = "variable",
                       dup.var = FALSE,
                       labels = NULL){

  if(!inherits(data, "list")){
    stop("Data must be a list.", call. = FALSE)
  }

  # Index for NULL or nrow < 1L in a data frame.
  index <- sapply(data, \(d){
    if(is_empty(d)){
      TRUE
    }else{
      nrow(d) <1L
    }
  })


  # If all elements are empty, return NULL.
  if(all(index)){
    return(NULL)
  }

  # Get non-null data.
  data <- data[!index]

  NAMES <- names(data)

  if(is.null(NAMES)){
    NAMES <- sprintf("%d", 1:length(data))
  }

  collapse <- function(d, nm){
    d[[1]] <- paste0("    ", d[[1]])
    res <- rbind(c(rep(NA, ncol(d))), d)
    res[1, 1] <- nm
    res
  }

  collapse_column <- function(d, nm){
    if(collapse.one.row){
      collapse(d, nm)
    }else{
      if(nrow(d) == 1L){
        tmpname <- names(d)
        d <- cbind(data.frame(nm), d[, -1, drop = FALSE])
        names(d) <- tmpname
        d
      }else{
        collapse(d, nm)
      }
    }
  }

  out <- Map(function(d, nm){

    nm <- find_labels(labels, varname = nm, defalut = nm)

    if(names.as.column){
      if(collapse.names){
        collapse_column(d, nm)
      }else{
        cbind(data.frame(variable = nm), d)
      }
    }else{
      d
    }
  }, data, NAMES)

  out <- do.call(rbind, out)
  row.names(out) <- NULL
  if(names.as.column){
    names(out)[1] <- varname
    if(dup.var){
      out[[1]] <- delete_duplicate_values(out[[1]])
    }
  }
  out
}


flatten_list <- function(x) {
  morelists <- sapply(x, function(xprime)
    class(xprime)[1] == "list")
  out <- c(x[!morelists], unlist(x[morelists], recursive = FALSE))
  if (sum(morelists)) {
    Recall(out)
  } else{
    return(out)
  }
}


add_title <- function(x, value = NULL){
  attr(x, "title") <- value
  x
}


add_note <- function(x, value = NULL, append = TRUE){
  if(is_empty(value)){
    attr(x, "note") <- NULL
  }else{
    if(append){
      note <- attr(x, "note")
      if(is_empty(note)){
        attr(x, "note") <- value
      }else{
        attr(x, "note") <- paste(note, value, sep = "\n")
      }
    }else{
      attr(x, "note") <- value
    }
  }
  x
}


is_empty <- function(x){
  length(x) == 0L
}

delete_duplicate_values <- function(x){
  for(i in rev(seq_along(x))){
    if(i != 1){
      if(x[i] == x[i - 1]){
        x[i] <- NA
      }
    }
  }
  return(x)
}

transpose <- function(x, row.names.col = 1, varname = NULL){

  title <- attr(x, "title")
  note  <- attr(x, "note")
  args  <- attr(x, "args")

  if(row.names.col == 0){
    x <- rownames_to_column(x, varname = "variable")
    row.names.col <- 1
  }

  if(is.null(varname)){
    varname <- names(x)[row.names.col]
  }

  o.class <- class(x)
  row.names <- x[[row.names.col]]
  x <- x[-row.names.col]
  col.names <- names(x)
  x <- t(x)
  x <- as.data.frame(x)
  names(x) <- row.names
  x <- append2(x, col.names, after = 0)
  names(x)[1] <- varname

  class(x) <- o.class
  attr(x, "title") <- title
  attr(x, "note")  <- note
  attr(x, "args")  <- args
  x
}


tidy_labels <- function(data = NULL){

  if(is_empty(data)){
    return(NULL)
  }
  term0 <- data[[1]]

  data[, 1] <- lapply(data[, 1], function(v) {
    for (i in seq_along(v)) {
      if (i != 1) {
        if (is.na(v[i])) {
          v[i] <- v[i - 1]
        }
      }
    }
    v
  })
  names(data) <- c(".varname", ".code", ".label")
  data <- tibble::add_column(data, .term = data[[1]], .after = 0)

  for(i in 1:nrow(data)){
    if(is.na(term0[i])){
      if(!is.na(data$.code[i])){
        data$.term[i] <- paste0(data$.term[i], data$.code[i])
      }
    }
  }
  data
}


# If can not find return NULL.
find_labels <- function(data, varname, code = NA, defalut = NULL){

  data <- tidy_labels(data)

  if(is_empty(data)){
    return(defalut)
  }
  if(is.na(code) | code == ""){
    x <- varname
  }else{
    x <- paste(varname, code, sep = "")
  }

  if(is_empty(which(as.character(data$.term) == as.character(x)))){
    return(defalut)
  }

  data$.label[which(as.character(data$.term )== as.character(x))]
}


rownames_to_column <- function(data, varname = "term"){
  if(!is.data.frame(data)){
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }
  data <- cbind(data.frame(term = row.names(data)), data)
  row.names(data) <- NULL
  names(data)[1] <- varname
  data
}


reshape_long <- function(data,
                         cols = names(data),
                         names.to = ".name",
                         values.to = ".value",
                         add.id.col = FALSE,
                         id.name = ".id",
                         ...){

  class(data) <- "data.frame"

  if(is.numeric(cols)){
    cols <- names(data)[cols]
  }

  cols <- select_variable(data, cols)

  res <- stats::reshape(data,
                        direction = "long",
                        idvar = id.name,
                        ids = as.character(1:nrow(data)),
                        times   = cols,
                        timevar = names.to,
                        v.names = values.to,
                        varying = list(cols))

  if(add.id.col){
    res <- relocate(res, variables = id.name, before = 1)
  }else{
    res <- res[, -which(names(res) == id.name), drop = FALSE]
  }
  tibble::as_tibble(res)
}
