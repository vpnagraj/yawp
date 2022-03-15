#' Zoom in on a ggplot2 plot
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}]
#'
#' This function takes an input `ggplot2` object and will situate the original side-by-side with the same style plot with a user-defined subset of observations. The effect is that the original plot will be "zoomed" in on specific points.
#'
#' @details
#'
#' **NOTE**: The "zoomed" in plot will be autoscaled based on the values returned, so it may not match the scale of the original. Interpret with caution.
#'
#' @param .plot `ggplot2` plot object that will serve as the original plot to be "zoomed"
#' @param zoom_cmd Expression to return a logical value that, if `TRUE`, will indicate data to keep in the "zoomed" version of the plot; accepts bare column names of the data and is inherited by `...` internally by dplyr \link[dplyr]{filter} to perform the subsetting operation
#' @param draw_box Boolean indicating whether or not to try to draw a rectangle around the subset being zoomed in on for the plot. Default is `TRUE`. Note that this argument will only work if *both* the "x" and "y" axes are on continuous scales.
#' @param box_nudge Coordinates to offset box if `draw_box = TRUE`; nudge applied to min and max of x and y coordinates for the box drawn; default is `1`
#' @param to_label Boolean indicating whether or not the plot should try to label points. Default is `FALSE`.
#' @param label Bare name of the column used to label the data points if `to_label = TRUE`
#'
#' @return A \link[patchwork]{patchwork} object with the `ggplot2` plot and the "zoomed" version side-by-side. The returned object can be further customized using functions from `patchwork` including \link[patchwork]{plot_layout}.
#'
#' @export
#'
#' @md
gg_zoom <- function(.plot, zoom_cmd, draw_box = TRUE, box_nudge = 1, to_label = FALSE, label) {

  ## use enquo for tidyeval syntax
  zoom_cmd <- dplyr::enquo(zoom_cmd)

  ## subset data to zoom in on
  zoom_data <-
    .plot$data %>%
    dplyr::filter(!!zoom_cmd)

  ## build the "zoom plot" based on the original ggplot object
  zoom_plot <- .plot
  ## coerce the data element to be the filtered data
  zoom_plot$data <- zoom_data

  ## if label  arg then add a repel text label
  if(to_label) {

    ## tidyeval syntax allows for a bare column name to be supplied
    label <- dplyr::enquo(label)

    zoom_plot <-
      zoom_plot +
      ggrepel::geom_text_repel(ggplot2::aes(label = !!label))
  }

  ## if draw box then add a box around data used in zoom
  if(draw_box) {

    ## need to get x and y variables (stored as quosures) from ggplot2 object
    x <- .plot$mapping$x
    y <- .plot$mapping$y

    ## create min/max x and y values for box around full plot
    box_data <-
      zoom_data %>%
      dplyr::summarise(
        xmin = min(!!x, na.rm = TRUE),
        xmax = max(!!x, na.rm = TRUE),
        ymin = min(!!y, na.rm = TRUE),
        ymax = max(!!y, na.rm = TRUE)) %>%
      ## add to min and max so that the box sits just outside point
      dplyr::mutate(
        xmin = xmin -  box_nudge,
        xmax = xmax + box_nudge,
        ymin = ymin -  box_nudge,
        ymax = ymax +  box_nudge)

    ## use geom_rect to add to the plot
    .plot <-
      .plot +
      ggplot2::geom_rect(
        ggplot2::aes(xmin = box_data$xmin,
                     xmax = box_data$xmax,
                     ymin = box_data$ymin,
                     ymax = box_data$ymax),
        fill = NA,
        col = "grey",
        lty = "dotted")

  }

  ## return in a basic patchwork layout
  ## patchwork:::`&.gg`(.plot, zoom_plot)
  ## get this function from utils.R
  combine_plots(.plot, zoom_plot)

}


#' Custom "math book" `ggplot2` theme
#'
#' @description
#'
#' This is a custom `ggplot2` theme function that inverts the default white background / black text and customizes legend placement.
#'
#' @details
#'
#' The theme returned from this function uses:
#'
#' - Black panel, plot, and legend backgrounds
#' - White text
#' - Blank legend title
#' - Legend position on bottom
#'
#' @return Returns a \link[ggplot2]{theme} object with custom theme settings for `theme_mathbook`.
#'
#' @export
#'
#' @md
#'
#'
theme_mathbook <- function() {

  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "black",
                                                         color = "black"),
                 panel.background = ggplot2::element_rect(fill = "black",
                                                          color = "black"),
                 legend.background = ggplot2::element_rect(fill = "black"),
                 legend.title = ggplot2::element_blank(),
                 text = ggplot2::element_text(color = "white"),
                 axis.text = ggplot2::element_text(color = "white"),
                 strip.background = ggplot2::element_rect(fill="white"),
                 legend.position = "bottom")

}
