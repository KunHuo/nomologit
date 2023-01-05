extract_effect <- function(text){
  out <- lapply(text, function(x){
    # res <- gregexpr(pattern = "(\\d+(\\.\\d+)?)", text = x)
    res <- gregexpr(pattern = "-?([1-9]\\d*|[1-9]\\d*\\.\\d{1,3}|0\\.\\d{1,3})", text = x)
    first <- res[[1]]
    last <- first + attr(res[[1]], "match.length") - 1
    res <- substring(x, first = first, last = last)
    as.numeric(res)
  })
  out <- do.call(rbind, out)
  colnames(out) <- c("est", "lower", "upper")
  out <- as.data.frame(out)
  out
}



#' Draw forest plot
#'
#' @param data Data (a data frame, an object from [coefs],[nmtask] or [glm]) to be displayed in the forest plot
#' @param estimate Point estimation.
#' @param lower Lower bound of the confidence interval.
#' @param upper Upper bound of the confidence interval.
#' @param sizes Size of the point estimation box, can be a unit, vector or a list.
#' @param ref.line X-axis coordinates of zero line, default is 1.
#' @param col.width Adjust the column width with space, default 2 spaces.
#' @param graph.pos Column number of the data the CI will be displayed.
#' @param fontfamily The font family.
#' @param fontsize The size of text.
#' @param est.color Color fill the point estimation.
#' @param ci.color Color of the CI.
#' @param est.shape Shape of the point estimation.
#' @param graph.width Adjust the graph width with space, default 25 spaces.
#' @param axis.ticks Set X-axis tick-marks point.
#' @param axis.ticks.digits Number of digits for the x-axis, default is 1L.
#' @param axis.labels Labels for the arrows, string vector of length two (left
#' and right).
#' @param footnote Footnote for the forest plot, will be aligned at left bottom
#' of the plot. Please adjust the line length with line break to avoid the
#' overlap with the arrow and/or x-axis.
#' @param ... more arguments pass to [forest_theme] function.
#'
#' @return A gtable object.
#' @export
forest <- function(data,
                   estimate = NULL,
                   lower = NULL,
                   upper = NULL,
                   sizes = 0.4,
                   ref.line = 1,
                   col.width = 2,
                   graph.pos = NULL,
                   fontfamily = "serif",
                   fontsize = 12,
                   est.color = "#0093bd",
                   ci.color = est.color,
                   est.shape = 16,
                   graph.width = 25,
                   axis.ticks = NULL,
                   axis.ticks.digits = 1L,
                   axis.labels = NULL,
                   footnote  = NULL,
                   ...){

  theme <- forestploter::forest_theme(base_family = fontfamily,
                                      base_size = fontsize,
                                      ci_fill = est.color,
                                      ci_col  = ci.color,
                                      ci_pch = est.shape, ...)

  if("glm" %in% class(data)){
    data <- as_nmtask(data)
    ldata <- coefs(data)
    ldata <- ldata[[1]]
  }else if ("nmtask" %in% class(data)){
    ldata <- coefs(data)
    ldata <- ldata[[1]]
  }else if("coefs" %in% class(data)){
    ldata <- data[[1]]
  }else{
    ldata <- data
  }

  if("OR (95% CI)" %in% names(ldata)){
    est.col <- which(names(ldata) == "OR (95% CI)")
  }else{
    if(is.null(estimate)){
      stop("The estimate column needs to be specified by using the column index or column name.", call. = FALSE)
    }else{
       est.col <- select_variable(ldata, estimate, type = "index")
    }
  }

  pdata <- extract_effect(ldata[[est.col]])

  if(is.null(graph.pos)){
    graph.pos <- est.col + 1
  }

  ldata <- tibble::add_column(ldata, " " = paste(rep(" ", graph.width), collapse = ""), .after = graph.pos - 1)

  ldata[, ] <- lapply(ldata[, ], \(x){
    ifelse(is.na(x), "", x)
  })

  if(is.null(axis.ticks)){
    axis.ticks <- pretty(c(0, 1, max(pdata$upper,na.rm = TRUE)))
  }

  axis.limit <- c(min(axis.ticks), max(axis.ticks))

  for(i in 1:ncol(ldata)){
    if(i != 1L){
      width <- max(nchar(ldata[[i]]), nchar(names(ldata)[i]), na.rm = TRUE) + col.width
      if(i == est.col){
        ldata[[i]] <- str_pad(ldata[[i]], width = width, adj = "left")
      }
      names(ldata)[i] <-  str_pad(names(ldata)[i], width = width, adj = "center")
      ldata[[i]] <- str_pad(ldata[[i]], width = width, adj = "center")
    }
  }


  if(is.null(axis.labels)){
    forestploter::forest(data = ldata,
                         est = pdata$est,
                         lower = pdata$lower,
                         upper = pdata$upper,
                         ci_column = graph.pos,
                         xlim = axis.limit,
                         ref_line = ref.line,
                         ticks_at = axis.ticks,
                         theme = theme,
                         ticks_digits = axis.ticks.digits,
                         footnote = footnote)
  }else{
    forestploter::forest(data = ldata,
                         est = pdata$est,
                         lower = pdata$lower,
                         upper = pdata$upper,
                         ci_column = graph.pos,
                         xlim = axis.limit,
                         ref_line = 1,
                         ticks_at = axis.ticks,
                         arrow_lab = axis.labels,
                         theme = theme,
                         ticks_digits = axis.ticks.digits,
                         footnote = footnote)
  }

}
