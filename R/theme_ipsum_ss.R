#' A precise & pristine [ggplot2] theme with opinionated defaults and an emphasis on typography
#'
#' This is a verbatim copy of `[hrbrthemes::theme_ipsum_ps]` with "Source Sans Pro" dropped in.
#'
#' You should [import_source_sans()] first and also install the fonts on your
#' system before trying to use this theme.
#'
#' There is an option `hrbrthemes.loadfonts` which -- if set to `TRUE` -- will
#' call `extrafont::loadfonts()` to register non-core fonts with R PDF & PostScript
#' devices. If you are running under Windows, the package calls the same function
#' to register non-core fonts with the Windows graphics device.
#'
#' @md
#' @param base_family,base_size base font family and size
#' @param plot_title_family,plot_title_face,plot_title_size,plot_title_margin plot tilte family, face, size and margin
#' @param subtitle_family,subtitle_face,subtitle_size plot subtitle family, face and size
#' @param subtitle_margin plot subtitle margin bottom (single numeric value)
#' @param strip_text_family,strip_text_face,strip_text_size facet label font family, face and size
#' @param caption_family,caption_face,caption_size,caption_margin plot caption family, face, size and margin
#' @param axis_title_family,axis_title_face,axis_title_size axis title font family, face and size
#' @param axis_title_just axis title font justificationk one of `[blmcrt]`
#' @param axis_text_size font size of axis text
#' @param plot_margin plot margin (specify with [ggplot2::margin])
#' @param panel_spacing panel spacing (use `unit()`)
#' @param grid_col grid color
#' @param grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`, `y`)
#' @param axis_col axis color
#' @param axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param ticks ticks if `TRUE` add ticks
#' @param legend_position Same as `legend.position` in `ggplot2::theme()`, defaulting
#' to `"top"`.
#' @param plot_title_position Same as `plot.title.position` in `ggplot2::theme()`,
#' defaulting to `"plot"`.
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # seminal scatterplot
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   labs(
#'     x = "Fuel efficiency (mpg)", y = "Weight (tons)",
#'     title = "Seminal ggplot2 scatterplot example",
#'     subtitle = "A plot that is only useful for demonstration purposes and the letter g",
#'     caption = "Brought to you by the letter 'g'"
#'   ) +
#'   theme_ipsum_ss()
#'
#' # seminal bar chart
#'
#' update_geom_font_defaults(family = font_ssp)
#'
#' count(mpg, class) %>%
#'   ggplot(aes(class, n)) +
#'   geom_col() +
#'   geom_text(aes(label = n), nudge_y = 3) +
#'   labs(
#'     x = "Fuel efficiency (mpg)", y = "Weight (tons)",
#'     title = "Seminal ggplot2 bar chart example",
#'     subtitle = "A plot that is only useful for demonstration purposes",
#'     caption = "Brought to you by the letter 'g'"
#'   ) +
#'   theme_ipsum_ss(grid = "Y") +
#'   theme(axis.text.y = element_blank())
#' }
theme_ipsum_ss <- function(
                           base_family = font_ssp, base_size = 10.5,
                           plot_title_family = font_ssp,
                           plot_title_size = 19,
                           plot_title_face = "bold",
                           plot_title_margin = 10,
                           subtitle_family = font_ssp_light,
                           subtitle_size = 13,
                           subtitle_face = "plain",
                           subtitle_margin = 15,
                           strip_text_family = base_family,
                           strip_text_size = 12,
                           strip_text_face = "plain",
                           caption_family = font_ssp_light,
                           caption_size = 10,
                           caption_face = "plain", caption_margin = 10,
                           axis_text_size = base_size,
                           axis_title_family = base_family,
                           axis_title_size = 11,
                           axis_title_face = "plain",
                           axis_title_just = "m", #"rt",
                           plot_margin = margin(20, 20, 20, 20),
                           panel_spacing = grid::unit(1.5, "lines"),
                           grid_col = "#cccccc", grid = "XY",
                           axis_col = "#cccccc", axis = FALSE,
                           ticks = FALSE,
                           legend_position = "top",
                           plot_title_position = "plot"
  ) {
  ret <- ggplot2::theme_minimal(base_family = base_family, base_size = base_size)

  ret <- ret + theme(legend.background = element_blank())
  ret <- ret + theme(legend.key = element_blank())

  ret <- ret + theme(plot.margin = plot_margin)
  ret <- ret + theme(panel.spacing = panel_spacing)

  ret <- ret + theme(legend.position = legend_position)
  ret <- ret + theme(plot.title.position = plot_title_position)



  if (inherits(grid, "character") | grid == TRUE) {
    ret <- ret + theme(panel.grid = element_line(color = grid_col, size = 0.2))
    ret <- ret + theme(panel.grid.major = element_line(color = grid_col, size = 0.2))
    ret <- ret + theme(panel.grid.minor = element_line(color = grid_col, size = 0.15))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x = element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y = element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x = element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y = element_blank())
    }
  } else {
    ret <- ret + theme(panel.grid = element_blank())
    ret <- ret + theme(panel.grid.major  = element_blank())
    ret <- ret + theme(panel.grid.major.x  = element_blank())
    ret <- ret + theme(panel.grid.major.y  = element_blank())
    ret <- ret + theme(panel.grid.minor  = element_blank())
    ret <- ret + theme(panel.grid.minor.x  = element_blank())
    ret <- ret + theme(panel.grid.minor.y  = element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line = element_line(color = axis_col, size = 0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x = element_blank())
      } else {
        ret <- ret + theme(axis.line.x = element_line(color = axis_col, size = 0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y = element_blank())
      } else {
        ret <- ret + theme(axis.line.y = element_line(color = axis_col, size = 0.15))
      }
    } else {
      ret <- ret + theme(axis.line.x = element_line(color = axis_col, size = 0.15))
      ret <- ret + theme(axis.line.y = element_line(color = axis_col, size = 0.15))
    }
  } else {
    ret <- ret + theme(axis.line = element_blank())
  }

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size = 0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size = 0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size = 0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)

  ret <- ret + theme(axis.text = element_text(size = axis_text_size, margin = margin(t = 0, r = 0)))
  ret <- ret + theme(axis.text.x = element_text(size = axis_text_size, margin = margin(t = 0)))
  ret <- ret + theme(axis.text.y = element_text(size = axis_text_size, margin = margin(r = 0)))

  ret <- ret + theme(axis.title = element_text(size = axis_title_size, family = axis_title_family))
  ret <- ret + theme(axis.title.x = element_text(
    hjust = xj, size = axis_title_size,
    family = axis_title_family, face = axis_title_face
  ))
  ret <- ret + theme(axis.title.y = element_text(
    hjust = yj, size = axis_title_size,
    family = axis_title_family, face = axis_title_face
  ))
  ret <- ret + theme(axis.title.y.right = element_text(
    hjust = yj, size = axis_title_size, angle = 90,
    family = axis_title_family, face = axis_title_face
  ))

  ret <- ret + theme(strip.text = element_text(
    hjust = 0, size = strip_text_size,
    face = strip_text_face, family = strip_text_family
  ))

  ret <- ret + theme(plot.title = element_text(
    hjust = 0, size = plot_title_size,
    margin = margin(b = plot_title_margin),
    family = plot_title_family, face = plot_title_face
  ))
  ret <- ret + theme(plot.subtitle = element_text(
    hjust = 0, size = subtitle_size,
    margin = margin(b = subtitle_margin),
    family = subtitle_family, face = subtitle_face
  ))
  ret <- ret + theme(plot.caption = element_text(
    hjust = 1, size = caption_size,
    margin = margin(t = caption_margin),
    family = caption_family, face = caption_face
  ))

  ret
}
