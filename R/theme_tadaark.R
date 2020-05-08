#' Official dark tadaa-data ggplot2 theme (for now)
#'
#' Because, according to Financial Times Datavizard John Burn-Murdoch,
#' "they look cool".
#'
#' @details Using this function changes the default color for many geoms via
#' `ggplot2::update_geom_defaults`, which, unfortunately, is rather complicated
#' to undo. One easy but blunt way is to restart your R session.
#'
#' @inheritParams theme_tadaa
#' @references https://twitter.com/jburnmurdoch/status/1231235675229491200
#' @note Use [`hrbrthemes::import_roboto_condensed()`] to install Roboto Condensed.
#' @import ggplot2
#' @export
#' @examples
#' library(ggplot2)
#' p <- ggplot(iris, aes(x = Sepal.Width)) +
#'   geom_histogram(binwidth = .25) +
#'   labs(
#'     title = "Yet another iris plot",
#'     subtitle = "Using theme_tadaark",
#'     caption = "Oh hi there, didn't see you walk in"
#'   )
#'
#' p + theme_tadaark()
theme_tadaark <- function(title.size = 16, text.size = 14, legend.position = "top",
                          show.axis = FALSE, show.grid = TRUE,
                          plot.margin = c(.8, .4, .4, .8),
                          base_family = "Roboto Condensed") {
  # baseline
  linecolor <- "#4d4d4d"
  textcolor <- "#d9d9d9"
  layout <- theme_classic(base_family = base_family)
  layout <- layout + theme(
    text = element_text(
      size = text.size,
      color = textcolor
    ),
    title = element_text(
      size = title.size,
      face = "bold",
      color = textcolor
    ),
    line = element_line(
      size = .5,
      color = linecolor
    )
  )

  # override geom defaults
  new_def <- "#00b0a0"
  update_geom_defaults("point", list(colour = new_def))
  update_geom_defaults("line", list(colour = new_def))
  update_geom_defaults("area", list(
    colour = new_def,
    fill = new_def
  ))
  update_geom_defaults("rect", list(
    colour = new_def,
    fill = new_def
  ))
  update_geom_defaults("density", list(
    colour = new_def,
    fill = new_def
  ))
  update_geom_defaults("bar", list(
    colour = new_def,
    fill = new_def
  ))
  update_geom_defaults("col", list(
    colour = new_def,
    fill = new_def
  ))
  update_geom_defaults("text", list(colour = new_def))

  # axis
  layout <- layout + theme(axis.line = element_line(color = "#d9d9d9"))
  if (inherits(show.axis, "character") | show.axis == FALSE) {
    if (inherits(show.axis, "character")) {
      show.axis <- tolower(show.axis)
      if (show.axis == "x") {
        layout <- layout + theme(axis.line.y = element_blank())
      }
      if (show.axis == "y") {
        layout <- layout + theme(axis.line.x = element_blank())
      }
    } else {
      layout <- layout + theme(
        axis.line.x = element_blank(),
        axis.line.y = element_blank()
      )
    }
  }

  # grid lines
  if (show.grid == TRUE) {
    layout <- layout + theme(panel.grid.major = element_line(
      size = .2,
      color = linecolor,
      linetype = "dotted"
    ))
  }

  # subtitle
  layout <- layout + theme(plot.subtitle = element_text(
    face = "plain",
    color = "#8c8c8c"
  ))

  # axis titles
  layout <- layout + theme(axis.title.x = element_text(margin = margin(t = 8)))

  # axis labels
  layout <- layout + theme(axis.text = element_text(
    face = "plain",
    color = "#8c8c8c"
  ))

  # legend
  layout <- layout + theme(
    legend.position = legend.position,
    legend.background = element_blank(),
    legend.key.height = unit(2, "line")
  )

  # facets
  layout <- layout + theme(
    strip.background = element_blank(),
    strip.text = element_text(
      size = title.size,
      color = textcolor,
      face = "plain"
    )
  )

  # misc
  layout <- layout + theme(
    panel.background = element_rect(fill = "#181818"),
    plot.background = element_rect(fill = "#181818"), # or: #1a1a1a; OR #191919?!
    plot.margin = unit(plot.margin, "cm")
  )

  layout
}
