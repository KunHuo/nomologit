# sim_dca_curve <- function(seed = 123, n = 100, se = -0.2, groups = NULL, font.size = 11, font.family = "serif", line.size = 0.25,
#                           line.color = "black", aspect.ratio = 0.875,  language = "en", bootstraps = 1){
#
#   dcadata <- sim_dca_data(seed = seed, n = n, se = se, groups = groups)
#
#   plotdata <- Map(function(dat, group){
#     res <- rmda::decision_curve(status ~ x, data = dat, bootstraps = bootstraps)
#     res <- res$derived.data
#     res$model[res$model == "status ~ x"] <- group
#
#     if(!is.null(groups)){
#       if(group != groups[1]){
#         res <- res[res$model == group, ]
#       }
#     }
#     res
#   }, dcadata, names(dcadata))
#
#   if(language == "ch"){
#     sysfonts::font_add("simsun", "simsun.ttc")
#     font.family <- "simsun"
#     xlab <- "\u9608\u503c"
#     ylab <- "\u6536\u76ca"
#   }else{
#     font.family <- "serif"
#     xlab <- "Risk throshold"
#     ylab <- " Standardized net benefit"
#   }
#
#   plotdata <- do.call(rbind, plotdata)
#
#
#   plotdata$model <- factor(plotdata$model, levels = c(setdiff(plotdata$model, c("All", "None")), "All", "None"))
#
#   ggplot2::ggplot(plotdata) +
#     ggplot2::geom_line(ggplot2::aes_string(x = "thresholds", y = "sNB", color = "model"), size = line.size) +
#     ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
#     ggplot2::scale_y_continuous(breaks = seq(-0.2, 1, 0.2), limits = c(-0.2, 1)) +
#     gp_theme_sci( font.size = font.size,
#                   font.family = font.family,
#                   line.size = line.size,
#                   line.color = "black",
#                   aspect.ratio = aspect.ratio) +
#     ggplot2::coord_cartesian(expand = FALSE) +
#     gp_legend_position() +
#     gp_delete_legend_title() +
#     ggplot2::xlab(xlab) +
#     ggplot2::ylab(ylab)
# }
