#' Official tadaa-data ggplot2 theme (for now)
#'
#' @param title.size title element size in pts.
#' @param text.size text element size in pts.
#' @param legend.position either one of "none", "left", "right", "bottom", "top".
#' @param show.axis boolean or "x", "y"; should axis be drawn? Which?.
#' @param show.grid boolean; should grid lines be drawn?.
#' @param plot.margin margin around entire plot (unit with the sizes of the
#' top, right, bottom, and left margins).
#' @param font.base The base font, defaulting to `"Roboto Condensed"`.
#'
#' @import ggplot2
#' @export
#' @note Use [`hrbrthemes::import_roboto_condensed()`] to install Roboto Condensed.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(iris, aes(x = Sepal.Width)) +
#'   geom_histogram(binwidth = .25) +
#'   labs(
#'     title = "Yet another iris plot",
#'     subtitle = "Using theme_tadaa",
#'     caption = "Oh hi there, didn't see you walk in"
#'   )
#'
#' p + theme_tadaa()
#' }
theme_tadaa <- function(title.size = 16, text.size = 14, legend.position = "top",
                        show.axis = FALSE, show.grid = TRUE,
                        plot.margin = c(.8, .4, .4, .8),
                        font.base = "Roboto Condensed") {
  # baseline
  layout <- theme_classic(base_family = font.base)
  layout <- layout + theme(
    text = element_text(size = text.size),
    title = element_text(
      size = title.size,
      face = "bold"
    ),
    line = element_line(size = .5)
  )

  # axis
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
      color = "#333333",
      linetype = "dotted"
    ))
  }

  # axis titles
  layout <- layout + theme(axis.title.x = element_text(margin = margin(t = 8)))

  # axis labels
  layout <- layout + theme(axis.text = element_text(face = "plain"))

  # subtitle
  layout <- layout + theme(plot.subtitle = element_text(
    face = "plain",
    color = "#4d4d4d"
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
      face = "plain"
    )
  )

  # misc
  layout <- layout + theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    plot.margin = unit(plot.margin, "cm")
  )

  layout
}
