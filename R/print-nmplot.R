#' Print plots
#'
#' @param x an object of 'nmplot'.
#' @param ... more.
#'
#' @export
print.nmplot <- function(x, ...){
  if(attr(x, "explain")){
    cat("\n")
    cat(attr(x, "title"))
    cat("\n")
    cat(attr(x, "note"))
    cat("\n\n")
  }
  plot(x)
}
