#' Draw ROC curve
#'
#' @export
roc_ <- function(){

}



gg_roc <- function(data,
                   outcome,
                   exposure,
                   positive = NULL,
                   combine = FALSE,
                   combine.only = FALSE,
                   smooth = FALSE,
                   smooth.args = list(),
                   auc = FALSE,
                   auc.ci = FALSE,
                   auc.digits = 2,
                   auc.ci.method = c("delong", "bootstrap"),
                   show.cutoff = TRUE,
                   line.size = 0.5,
                   line.color = NULL,
                   line.type = NULL,
                   language  = NULL,
                   font.family = NULL,
                   font.size = NULL,
                   progress = "win",
                   boot.n = 1000,
                   seed = 1234,
                   ...){

  set.seed(seed)

  auc.ci.method <- match.arg(auc.ci.method)

  language    <- get_global_languange(language, default = "en")
  font.family <- get_global_family(font.family, default = "serif")
  font.size   <- get_global_fontsize(font.size, default = 12)
  line.color  <- get_global_palette(line.color)

  outcome  <- select_variable(data, outcome)
  exposure <- select_variable(data, exposure)

  roclist <- .roc(data = data,
                  outcome = outcome,
                  exposure = exposure,
                  positive = positive,
                  combine = combine,
                  combine.only = combine.only,
                  smooth = smooth,
                  smooth.args = smooth.args)

  if("combine" %in% names(roclist)){
    names(roclist)[names(roclist) == "combine"] <- string_combine(language)
  }

  if (auc) {
    auc.string <- sapply(roclist, function(x) {
      .auc_string(
        x,
        auc.ci = auc.ci,
        digits = auc.digits,
        method = auc.ci.method,
        boot.n = boot.n,
        seed = seed,
        progress = progress
      )
    })
    legends  <- sprintf("%s (%s)", names(roclist), auc.string)
    names(roclist) <- legends
  }

  if(language == "zh"){
    sysfonts::font_add("simsun", "simsun.ttc")
    font.family <- "simsun"
  }else{
    font.family <- font.family
  }

  if(is.null(line.type)){
    aes <- c("colour")
  }else{
    aes <- c("linetype", "colour")
  }

  p <- suppressMessages(
    pROC::ggroc(roclist, legacy.axes = TRUE, size = line.size, aes = aes) +
      ggplot2::geom_abline(intercept = 0, color = "darkgrey", linetype = "dashed", size = line.size) +
      gg_xbreaks_continuous(0, 1, by = 0.2) +
      gg_ybreaks_continuous(0, 1, by = 0.2) +
      gg_xlab(sprintf("1 - %s", string_specificity(language))) +
      gg_ylab(string_sensitivity(language)) +
      gg_theme_sci(font.family = font.family, font.size = font.size, ...) +
      gg_legend_title(NULL) +
      gg_legend_position(c(1, 0))
  )

  if(show.cutoff){
    threshold.data <-  lapply(names(roclist), function(x){

      object <- roclist[[x]]

      rets <- c("threshold", "sensitivity", "specificity")
      res <- coords <- pROC::coords(object,
                                    x = "best",
                                    ret = rets,
                                    transpose = TRUE)
      res <- as.data.frame(as.list(res))
      res$specificity <- 1 - res$specificity
      res
    })

    names(threshold.data) <- names(roclist)

    threshold.data <- list_rbind(threshold.data)

    p <- p +
      ggplot2::geom_point(data = threshold.data,
                          ggplot2::aes_string(x = "specificity", y = "sensitivity", color = "variable"),
                          show.legend = FALSE, size = 2.5)
  }


  if(!is.null(line.color)){
    p <- p +
      ggplot2::scale_color_manual(values = line.color)
  }

  if(!is.null(line.type)){
    p <- p +
      ggplot2::scale_linetype_manual(values = line.type)
  }

  p

}

.pred_prob <- function(data, outcome, exposure, newdata = NULL){
  frm <- paste(exposure, collapse = " + ")
  frm <- paste(outcome, frm, sep  = " ~ ")
  frm <- stats::as.formula(frm)

  fit <- stats::glm(formula = frm, data = data, family = stats::binomial(link = "logit"))

  if(is.null(newdata)){
    stats::predict(fit, type = "response")
  }else{
    stats::predict(fit, type = "response", newdata = newdata)
  }
}


.auc_string <- function(x, auc.ci = TRUE, digits = 2, method = "delong", boot.n = 1000, seed = 1234, progress = "text"){
  set.seed(seed)
  if(auc.ci){
    res <- pROC::ci.auc(x, method = method, boot.n = boot.n, progress = progress)
    sprintf("AUC = %s, 95%% CI: %s\u2013%s",
            format_digits(res[[2]], digits),
            format_digits(res[[1]], digits),
            format_digits(res[[3]], digits))
  }else{
    sprintf("AUC = %s", format_digits(pROC::auc(x), digits))
  }
}
