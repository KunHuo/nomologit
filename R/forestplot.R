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



forest <- function(data,
                   estimate = NULL,
                   lower = NULL,
                   upper = NULL,
                   col.width = 2,
                   graph.pos = NULL,
                   graph.width = 30,
                   axis.limit = NULL,
                   axis.ticks = NULL,
                   axis.labels = NULL){

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

  if(is.null(axis.limit)){
    axis.limit <- c(min(axis.ticks), max(axis.ticks))
  }

  hjust <- c(0, seq(0.5, 6))


  theme <- forestploter::forest_theme(base_family = "serif",
                                      ci_fill = "#0093bd",
                                      ci_col = "#0093bd",
                                      ci_pch = 16)

  for(i in 1:ncol(ldata)){
    if(i != 1L){
      width <- max(nchar(ldata[[i]]), nchar(names(ldata)[i]), na.rm = TRUE) + col.width
      if(i == est.col){
        ldata[[i]] <- str_pad(ldata[[i]], width = width, adj = "left")
      }

      # if(names(ldata)[i] %in% c("B", "b", "P value", "P Value", "P-value", "P-Value", "P")){
      #   ldata[[i]] <- srpubr::str_align(ldata[[i]], sep = ".")
      #
      #
      # }

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
                         ref_line = 1,
                         ticks_at = axis.ticks,
                         theme = theme)
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
                         theme = theme)
  }

}
