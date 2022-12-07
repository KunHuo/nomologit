#' ggplo2 theme for SCI
#'
#' @param font.size font size, default 12,
#' @param font.family font family, detault Times New Roman.
#' @param axis.line.size axis line size, default 0.5.
#' @param axis.ticks.length axis ticks length, default 0.12.
#' @param legend.key.size legend key size, dsfault 1.
#' @param face.bold a logical, whether bold the title of axis, plot, strip, and legend, default FALSE.
#' @param panel.border a logical, whether plot panel border, default FALSE.
#' @param aspect.ratio the ratio of the width to the height, default NULL.
#' @param ... further arguments pass to the [ggplot2::theme] function.
#' @param panel.grid.major panel grid major.
#' @param panel.grid.minor panel grid minor.
#' @param panel.spacing panel spacing.
#' @param strip.background strip background.
#' @param plot.margin plot margin, top, right, bottom, left.
#'
#' @keywords internal
gg_theme_sci <- function(font.size = 12,
                      font.family = "serif",
                      axis.line.size = 0.5,
                      axis.ticks.length = 0.12,
                      legend.key.size = 1.0,
                      face.bold = FALSE,
                      panel.grid.major = FALSE,
                      panel.grid.minor = FALSE,
                      panel.border = FALSE,
                      panel.spacing = 0.6,
                      strip.background = "gray90",
                      aspect.ratio = NULL,
                      plot.margin = c(0.4, 0.6, 0.4, 0.4),
                      ...) {

  face <- ifelse(face.bold, "bold", "plain")

  if(panel.grid.major){
    pg.major = ggplot2::element_line(color = "gray90", size = axis.line.size)
  }else{
    pg.major = ggplot2::element_blank()
  }

  if(panel.grid.minor){
    pg.minor = ggplot2::element_line(color = "gray90", size = axis.line.size, linetype = "dashed")
  }else{
    pg.minor = ggplot2::element_blank()
  }

  if(panel.border){
    pborder = ggplot2::element_rect(color = "black", size = axis.line.size)
  }else{
    pborder = ggplot2::element_rect(color = "NA")
  }

  ggplot2::theme_bw(
    base_size   = font.size,
    base_family = font.family,
    base_line_size = axis.line.size,
    base_rect_size = axis.line.size) +

    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = NA),
      panel.grid = ggplot2::element_blank(),
      panel.border = pborder,
      panel.grid.major = pg.major,
      panel.grid.minor = pg.minor,
      panel.spacing = ggplot2::unit(panel.spacing, "cm"),

      strip.background = ggplot2::element_rect(fill = strip.background, size = axis.line.size),

      axis.line = ggplot2::element_line(size = axis.line.size, color = "black",lineend = "square"),
      axis.ticks.length = ggplot2::unit(axis.ticks.length, "cm"),
      axis.ticks = ggplot2::element_line(color = "black", size = axis.line.size),
      axis.text  = ggplot2::element_text(color = "black", size = font.size),
      axis.title = ggplot2::element_text(color = "black", size = font.size, face = face),

      legend.background = ggplot2::element_rect(fill = "NA"),
      legend.text       = ggplot2::element_text(color = "black", size = font.size),
      legend.title      = ggplot2::element_text(face = face),
      legend.key.size   = ggplot2::unit(legend.key.size, "lines"),

      plot.title = ggplot2::element_text(size = font.size + 2, face = face),
      plot.title.position = "plot",
      plot.margin = ggplot2::unit(plot.margin, "cm"), # top, right, bottom, left

      strip.text = ggplot2::element_text(color = "black", size = font.size, face = face),
      aspect.ratio = aspect.ratio,
      complete = FALSE,
      ...
    )
}


.is_waiver <- function(value){
  class(value) == "waiver"
}

#' Bold axis title
#'
#' @param bold bold, default TRUE.
#'
#' @return a ggplot.
#' @keywords internal
gg_bold_axis_title <- function(bold = TRUE){
  face <- ifelse(bold, "bold", "plain")
  ggplot2::theme(
    axis.title = ggplot2::element_text(face = face)
  )
}


#' Bold tags
#'
#' @param bold bold, default TRUE.
#'
#' @return a ggplot.
#' @keywords internal
gg_bold_tags <- function(bold = TRUE){
  face <- ifelse(bold, "bold", "plain")
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = face)
  )
}


#' Axis line size
#'
#' @param size size, default 0.25.
#'
#' @return a ggplot.
#' @keywords internal
gg_axis_line_size <- function(size = 0.25){
  ggplot2::theme(
    axis.line  = ggplot2::element_line(size = size),
    axis.ticks = ggplot2::element_line(size = size)
  )
}


#' Axis ticks length
#'
#' @param size size, default 0.15.
#'
#' @return a ggplot.
#' @keywords internal
gg_axis_ticks_length <- function(size = 0.15){
  ggplot2::theme(
    axis.ticks.length = ggplot2::unit(size, "cm"),
  )
}


#' Legend title
#'
#' @param value title
#'
#' @return a ggplot.
#' @keywords internal
gg_legend_title <- function(value = NULL){
  if(length(value) == 0L){
    ggplot2::theme(
      legend.title = ggplot2::element_blank()
    )
  }else{
    ggplot2::labs(color = value, fill = value, alpha = value, size = value, linetype = value)
  }
}


#' Legend position
#'
#' @param position position
#'
#' @return a ggplot.
#' @keywords internal
gg_legend_position <- function(position) {
  if(length(position) == 0L){
    ggplot2::theme(
      legend.position = "none"
    )
  }else{
    if(is.character(position)){
      ggplot2::theme(
        legend.position = position
      )
    }else{
      ggplot2::theme(
        legend.position = position,
        legend.justification = position
      )
    }
  }
}


#' Rotate X axis text
#'
#' @param angle angle.
#' @param hjust hjust.
#' @param vjust vjust.
#' @param ... more.
#'
#' @keywords internal
gg_rotate_x_text <- function (angle = 45, hjust = NULL, vjust = NULL, ...) {
  if (missing(hjust) & angle > 5)
    hjust <- 1
  if (missing(vjust) & angle == 90)
    vjust <- 0.5
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angle, hjust = hjust, vjust = vjust, ...))
}


#' Rotate Y axis text
#'
#' @param angle angle.
#' @param hjust hjust.
#' @param vjust vjust.
#' @param ... more.
#'
#' @keywords internal
gg_rotate_y_text <- function (angle = 45, hjust = NULL, vjust = NULL, ...) {
  if (missing(hjust) & angle == 90)
    hjust <- 0.5
  else if (missing(hjust) & angle > 5)
    hjust <- 1
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = angle, hjust = hjust, vjust = vjust, ...))
}


.pretty_xbreaks <- function(plot, x.breaks = NULL, x.breaks.n = 5, zero = FALSE, facet = NULL){
  if(is.null(x.breaks)){
    if(!is.null(facet)){
      plot <- plot + ggplot2::facet_wrap(facet)
    }
    gdata <- ggplot2::ggplot_build(plot)$data[[1]]
    x.breaks <- gdata[["x"]]
    if(zero){
      x.breaks <- c(0, x.breaks)
    }
    x.breaks <- pretty(x.breaks, x.breaks.n)
  }
  x.breaks
}


.pretty_ybreaks <- function(plot, y.breaks = NULL, y.breaks.n = 5, zero = FALSE, facet = NULL){
  if(is.null(y.breaks)){
    if(!is.null(facet)){
      plot <- plot + ggplot2::facet_wrap(facet)
    }
    gdata <- ggplot2::ggplot_build(plot)$data[[1]]
    y.breaks <- gdata[["y"]]

    if(zero){
      y.breaks <- c(0, y.breaks)
    }

    y.breaks <- pretty(y.breaks, y.breaks.n)
  }
  y.breaks
}


.set_legend_title <- function(plot, title, data, group){
  if(.is_waiver(title)){
    label <- attr(data[[group]], "label")
    if(is.null(label)){
      plot
    }else{
      plot + gg_legend_title(label)
    }
  }else{
    plot + gg_legend_title(title)
  }
}


.set_legend_position <- function(plot, position){
  if(.is_waiver(position)){
    plot + gg_legend_position("right")
  }else{
    plot + gg_legend_position(position)
  }
}


#' Delete x title of ggplot2
#'
#' @keywords internal
gg_delete_x_title <- function() {
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank()
  )
}


#' Delete y title of ggplot2
#'
#' @keywords internal
gg_delete_y_title <- function() {
  ggplot2::theme(
    axis.title.y = ggplot2::element_blank()
  )
}


#' Delete legend title of ggplot2
#'
#' @keywords internal
gg_delete_legend_title <- function() {
  ggplot2::theme(
    legend.title = ggplot2::element_blank()
  )
}

#' Delete legend of ggplot2
#'
#' @keywords internal
gg_delete_legend <- function() {
  ggplot2::theme(
    legend.position = "none"
  )
}


#' Figure label
#'
#' @param label label.
#'
#' @keywords internal
gg_tags <- function(label){
  ggplot2::labs(title = label)
}


#' Modify axis labels
#'
#' @param label The title of the respective axis.
#'
#' @keywords internal
gg_xlab <- function(label){
  ggplot2::xlab(label = label)
}



#' Modify axis labels
#'
#' @param label The title of the respective axis.
#'
#' @keywords internal
gg_ylab <- function(label){
  ggplot2::ylab(label = label)
}


#' X breaks for continuous
#'
#' @param min min.
#' @param max max.
#' @param by by.
#' @param expand expand.
#'
#' @keywords internal
gg_xbreaks_continuous <- function(min, max, by, expand = TRUE){
  if(expand){
    ggplot2::scale_x_continuous(expand = c(0, 0), breaks = seq(min, max, by), limits = c(min, max))
  }else{
    ggplot2::scale_x_continuous(breaks = seq(min, max, by), limits = c(min, max))
  }
}


#' Y breaks for continuous
#'
#' @param min min.
#' @param max max.
#' @param by by.
#' @param expand expand.
#'
#' @keywords internal
gg_ybreaks_continuous <- function(min, max, by, expand = TRUE){
  if(expand){
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = seq(min, max, by), limits = c(min, max))
  }else{
    ggplot2::scale_y_continuous(breaks = seq(min, max, by), limits = c(min, max))
  }
}
