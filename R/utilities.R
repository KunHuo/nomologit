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

check_index <- function(data, index){
  tmp <- index >=1 & index <= ncol(data)
  if(!all(tmp)){
    meaasge <- sprintf("Index must be between 1 and %d.", ncol(data))
    stop(meaasge, call. = FALSE)
  }
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


relocate <- function(data, variables, before = NULL, after = NULL) {
  if (is.numeric(variables)) {
    check_index(data, variables)
    to_move <- variables
  } else{
    check_name(data, variables)
    to_move <- sapply(variables, function(x) { which(names(data) == x) })
    names(to_move) <- NULL
  }

  if (!is.null(before) && !is.null(after)) {
    stop("Must supply only one of `.before` and `.after`.")
  } else if (!is.null(before)) {
    if (is.numeric(before)) {
      check_index(data, before)
      where <- before
    } else{
      check_name(data, before)
      where <- which(names(data) == before)
    }
    if (!where %in% to_move) {
      to_move <- c(to_move, where)
    }
  } else if (!is.null(after)) {
    if (is.numeric(after)) {
      check_index(data, after)
      where <- after
    } else{
      check_name(data, after)
      where <- which(names(data) == after)
    }
    if (!where %in% to_move) {
      to_move <- c(where, to_move)
    }
  } else {
    where <- 1L
    if (!where %in% to_move) {
      to_move <- c(to_move, where)
    }
  }

  lhs <- setdiff(seq2(1, where - 1), to_move)
  rhs <- setdiff(seq2(where + 1, ncol(data)), to_move)

  pos <- unique(c(lhs, to_move, rhs))
  out <- data[pos]
  out
}

seq2 <- function(from, to) {
  if (length(from) != 1) {
    stop("`from` must be length one")
  }
  if (length(to) != 1) {
    stop("`to` must be length one")
  }
  if (from > to) {
    integer()
  }
  else {
    seq.int(from, to)
  }
}


merge_left <- function(x, y, by){
  x$.id <- 1:nrow(x)
  res <- merge(x, y, sort = FALSE, by = by, all.x = TRUE)
  res <- res[order(res$.id), ]
  res[, -which(names(res) == ".id")]
}


print_booktabs <- function(data, sep = "__", adj = NULL, ...){

  title <- attr(data, "title")
  note  <- attr(data, "note")

  if(!is.null(title)){
    cat("\n")
    cat(title, "\n")
  }

  pad_df <- function(data, adj = "left"){
    if(length(adj) != ncol(data)){
      adj <- c(adj,  rep(adj[length(adj)], ncol(data) - length(adj) ))
    }

    data[,] <-  Map(function(x, a){
      str_pad(x, width = max_nchar(x), adj = a)
    }, data, adj)
    data
  }

  print_lines <- function(data, n.title = 1, n.space = 3){
    for(i in 1:nrow(data)){
      row <- data[i, ,drop = TRUE]
      row <- paste(row, collapse = strrep(" ", n.space))
      lines <- strrep("-", nchar(row))
      if(n.title == 1){
        if(i == 1 | i == 2){
          cat(lines, "\n")
        }
      }else{
        if(i == 1 | i == 4){
          cat(lines, "\n")
        }
      }
      cat(row, "\n")
      if(i== nrow(data)){
        cat(lines, "\n")
      }
    }
  }

  if(is.null(adj)){
    adj <- sapply(data, function(x){
      ifelse(is.numeric(x), "center", "left")
    })
  }

  data[, ] <- lapply(data[, ], function(x){
    if(is.numeric(x)){
      sapply(x, \(i){
        if(is.na(i)){
          ""
        }else{
          fmt <- sprintf("%%.%df", max_digits(i))
          sprintf(fmt, i)
        }
      })
    }else{
      sapply(x, function(i){
        if(is.na(i)){
          ""
        }else{
          as.character(i)
        }
      })
    }
  })

  if(any(regex_detect(names(data), pattern = sep, fixed = TRUE))){
    titles <- names(data)
    titles <- strsplit(names(data), split = sep, fixed = TRUE)
    titles <- do.call(cbind, titles)
    title1 <- titles[1, ]
    title2 <- titles[2, ]

    data <- rbind(title2, data)
    data <- pad_df(data, adj = adj)
    data <- lapply(data, function(x){ c(strrep("-", max_nchar(x)), x) })
    data <- as.data.frame(data)

    cdata <- lapply(unique(title1), function(x){
      colindex <- which(x == title1)
      tmpdata <- data[, colindex, drop = FALSE]
      tmpdata <- apply(tmpdata, 1, paste, collapse = strrep(" ", 3))
      tmpdata[1] <- strrep("-", nchar(tmpdata[1]))
      tmpdata
    })

    names(cdata) <- unique(title1)
    cdata <- as.data.frame(cdata)
    cdata <- rbind(unique(title1), cdata)
    cdata <- pad_df(cdata, adj = c("left", "center"))

    for(i in seq_along(cdata)){
      if(str_trim(cdata[1, i]) == str_trim(cdata[3, i])){
        cdata[2, i] <- cdata[1, i]
        cdata[1, i] <- strrep(" ", nchar(cdata[1, i]))
        cdata[3, i] <- strrep(" ", nchar(cdata[3, i]))
      }
    }

    print_lines(cdata, n.title = 2, n.space = 3)
  }else{
    data <- rbind(names(data), data)
    data <- pad_df(data, adj = adj)
    print_lines(data, n.title = 1, n.space = 3)
  }

  if(!is.null(note)){
    cat(note)
    cat("\n\n")
  }
}


n_digits <- function(x){
  x <- as.character(x)
  sapply(x, function(i) {
    i <- regex_split(i, pattern = ".", fixed = TRUE)
    i <- unlist(i)
    if (length(i) == 1L) {
      0
    } else{
      nchar(i[2])
    }
  })
}


max_digits <- function(x){
  max(n_digits(x), na.rm = TRUE)
}


max_nchar <- function(x){
  max(sapply(x, nchar), na.rm = TRUE)
}

str_pad <- function (x, width = NULL, pad = " ", adj = "left") {
  .pad <- function(x, width, pad = " ", adj = "left") {
    if (is.na(x))
      return(NA)
    mto <- match.arg(adj, c("left", "right", "center"))
    free <- max(0, width - nchar(x))
    fill <- substring(paste(rep(pad, ceiling(free/nchar(pad))),
                            collapse = ""), 1, free)
    if (free <= 0)
      x
    else if (mto == "left")
      paste(x, fill, sep = "")
    else if (mto == "right")
      paste(fill, x, sep = "")
    else paste(substring(fill, 1, free%/%2), x, substring(fill,
                                                          1 + free%/%2, free), sep = "")
  }
  if (is.null(width))
    width <- max(nchar(x), na.rm = TRUE)
  lgp <- recycle(x = x, width = width, pad = pad,
                 adj = adj)
  sapply(1:attr(lgp, "maxdim"), function(i) .pad(lgp$x[i],
                                                 lgp$width[i], lgp$pad[i], lgp$adj[i]))
}

recycle <- function (...) {
  lst <- list(...)
  maxdim <- max(lengths(lst))
  res <- lapply(lst, rep, length.out = maxdim)
  attr(res, "maxdim") <- maxdim
  return(res)
}

str_trim <- function (x, pattern = " \t\n", method = "both") {
  switch(
    match.arg(arg = method, choices = c("both", "left", "right")),
    both = {
      gsub(
        pattern = gettextf("^[%s]+|[%s]+$", pattern, pattern),
        replacement = "",
        x = x
      )
    },
    left = {
      gsub(
        pattern = gettextf("^[%s]+", pattern),
        replacement = "",
        x = x
      )
    },
    right = {
      gsub(
        pattern = gettextf("[%s]+$", pattern),
        replacement = "",
        x = x
      )
    }
  )
}


fct_reorder <- function(data, varname, order, exclude = NA) {
  levels <- levels(data[[varname]])
  index <- order %in% levels
  if (!all(index)) {
    text <- paste(order[!index], collapse = ", ")
    text <- sprintf("%s not at the level of %s.", text, varname)
    stop(text, call. = FALSE)
  }
  levels <- c(order, setdiff(levels, order))
  data[[varname]] <- factor(data[[varname]], levels = levels, exclude = exclude)
  data
}


cut2 <- function (x, cuts, m = 150, g, levels.mean = FALSE, digits, minmax = TRUE,
                  oneval = TRUE, onlycuts = FALSE, formatfun = format, ...) {
  if (inherits(formatfun, "formula")) {
    if (!requireNamespace("rlang"))
      stop("Package 'rlang' must be installed to use formula notation")
    formatfun <- getFromNamespace("as_function", "rlang")(formatfun)
  }
  method <- 1
  x.unique <- sort(unique(c(x[!is.na(x)], if (!missing(cuts)) cuts)))
  min.dif <- min(diff(x.unique))/2
  min.dif.factor <- 1
  if (missing(digits))
    digits <- if (levels.mean)
      5
  else 3
  format.args <- if (any(c("...", "digits") %in% names(formals(args(formatfun))))) {
    c(digits = digits, list(...))
  }
  else {
    list(...)
  }
  oldopt <- options("digits")
  options(digits = digits)
  on.exit(options(oldopt))
  xlab <- attr(x, "label")
  if (missing(cuts)) {
    nnm <- sum(!is.na(x))
    if (missing(g))
      g <- max(1, floor(nnm/m))
    if (g < 1)
      stop("g must be >=1, m must be positive")
    options(digits = 15)
    n <- table(x)
    xx <- as.double(names(n))
    options(digits = digits)
    cum <- cumsum(n)
    m <- length(xx)
    y <- as.integer(ifelse(is.na(x), NA, 1))
    labs <- character(g)
    cuts <- approx(cum, xx, xout = (1:g) * nnm/g, method = "constant",
                   rule = 2, f = 1)$y
    cuts[length(cuts)] <- max(xx)
    lower <- xx[1]
    upper <- 1e+45
    up <- low <- double(g)
    i <- 0
    for (j in 1:g) {
      cj <- if (method == 1 || j == 1)
        cuts[j]
      else {
        if (i == 0)
          stop("program logic error")
        s <- if (is.na(lower))
          FALSE
        else xx >= lower
        cum.used <- if (all(s))
          0
        else max(cum[!s])
        if (j == m)
          max(xx)
        else if (sum(s) < 2)
          max(xx)
        else approx(cum[s] - cum.used, xx[s], xout = (nnm -
                                                        cum.used)/(g - j + 1), method = "constant",
                    rule = 2, f = 1)$y
      }
      if (cj == upper)
        next
      i <- i + 1
      upper <- cj
      y[x >= (lower - min.dif.factor * min.dif)] <- i
      low[i] <- lower
      lower <- if (j == g)
        upper
      else min(xx[xx > upper])
      if (is.na(lower))
        lower <- upper
      up[i] <- lower
    }
    low <- low[1:i]
    up <- up[1:i]
    variation <- logical(i)
    for (ii in 1:i) {
      r <- range(x[y == ii], na.rm = TRUE)
      variation[ii] <- diff(r) > 0
    }
    if (onlycuts)
      return(unique(c(low, max(xx))))
    flow <- do.call(formatfun, c(list(low), format.args))
    fup <- do.call(formatfun, c(list(up), format.args))
    bb <- c(rep(")", i - 1), "]")
    labs <- ifelse(low == up | (oneval & !variation), flow,
                   paste("[", flow, ",", fup, bb, sep = ""))
    ss <- y == 0 & !is.na(y)
    if (any(ss))
      stop(paste("categorization error in cut2.  Values of x not appearing in any interval:\n",
                 paste(format(x[ss], digits = 12), collapse = " "),
                 "\nLower endpoints:", paste(format(low, digits = 12),
                                             collapse = " "), "\nUpper endpoints:", paste(format(up,
                                                                                                 digits = 12), collapse = " ")))
    y <- structure(y, class = "factor", levels = labs)
  }
  else {
    if (minmax) {
      r <- range(x, na.rm = TRUE)
      if (r[1] < cuts[1])
        cuts <- c(r[1], cuts)
      if (r[2] > max(cuts))
        cuts <- c(cuts, r[2])
    }
    l <- length(cuts)
    k2 <- cuts - min.dif
    k2[l] <- cuts[l]
    y <- cut(x, k2)
    if (!levels.mean) {
      brack <- rep(")", l - 1)
      brack[l - 1] <- "]"
      fmt <- do.call(formatfun, c(list(cuts), format.args))
      labs <- paste("[", fmt[1:(l - 1)], ",", fmt[2:l],
                    brack, sep = "")
      if (oneval) {
        nu <- table(cut(x.unique, k2))
        if (length(nu) != length(levels(y)))
          stop("program logic error")
        levels(y) <- ifelse(nu == 1, c(fmt[1:(l - 2)],
                                       fmt[l]), labs)
      }
      else levels(y) <- labs
    }
  }
  if (levels.mean) {
    means <- tapply(x, y, function(w) mean(w, na.rm = TRUE))
    levels(y) <- do.call(formatfun, c(list(means), format.args))
  }
  attr(y, "class") <- "factor"
  if (length(xlab))
    label(y) <- xlab
  y
}
