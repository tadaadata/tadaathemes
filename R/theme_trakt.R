#' Theme Matching trakt.tv (Dark Knight Mode)
#'
#' @details Using this function changes the default color for many geoms via
#' `ggplot2::update_geom_defaults`, which, unfortunately, is rather complicated
#' to undo. One easy but blunt way is to restart your R session.
#'
#' @inheritParams theme_tadaa
#' @param font.base,font.title,font.caption,font.subtitle Default font
#' family is `"Lato"`, but trakt.tv uses `"Proxima Nova Semibold"` -
#' which is a neat font, but I can neither afford nor supply it with this package.
#' @param plot.title.position Defaults to `"plot"` for left-aligned title.
#' @note See [`sysfonts::font_add_google`] and the `showtext` package to add
#' the Lato font.
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(iris, aes(x = Sepal.Width)) +
#'   geom_histogram(binwidth = .25) +
#'   labs(
#'     title = "Yet another iris plot",
#'     subtitle = "Using theme_trakt",
#'     caption = "Oh hi there, didn't see you walk in"
#'   )
#'
#' p + theme_trakt()
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_line(stat = "smooth", method = lm, formula = y ~ x) +
#'   geom_point(size = 2) +
#'   labs(
#'     title = "The ggplot we've all seen",
#'     subtitle = "Using theme_trakt",
#'     x = "Weight", y = "Miles per Gallon",
#'     color = "Gears", caption = "I am also here!"
#'   )
#' p + theme_trakt()
#' }
theme_trakt <- function(
  title.size = 16, text.size = 14, legend.position = "top",
  show.axis = FALSE, show.grid = TRUE,
  plot.margin = c(.7, .7, .7, .7),
  plot.title.position = "plot",
  font.base = font_lato,
  font.title = font_lato_semibold,
  font.subtitle = font_lato,
  font.caption = font_lato_light
) {

  # baseline
  linecolor <- "#999999"
  textcolor <- "#FFFFFF"
  layout <- theme_classic(
    base_family = font.base,
  )
  layout <- layout + theme(
    text = element_text(
      size = text.size,
      color = textcolor
    ),
    title = element_text(
      size = title.size,
      face = "plain",
      color = textcolor
    ),
    line = element_line(
      size = .5,
      color = linecolor
    )
  )

  # override geom defaults
  new_def <- "#EA212D"
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
  layout <- layout + theme(axis.line = element_line(color = "#999999"))

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
      size = .05,
      color = linecolor,
      linetype = "solid"
    ))
  }

  # title
  layout <- layout + theme(
    plot.title.position = plot.title.position,
    plot.title = element_text(
      family = font.title,
      # face = "plain",
      color = "#FFFFFF"
    )
  )

  # subtitle
  layout <- layout + theme(plot.subtitle = element_text(
    family = font.subtitle,
    # face = "plain",
    color = "#999999",
    margin = margin(b = 15),
    # debug = TRUE
  ))

  # caption
  layout <- layout + theme(plot.caption = element_text(
    family = font.caption,
    size = text.size - 2,
    # face = "plain",
    color = "#999999",
    margin = margin(t = 5)
  ))

  # axis titles
  layout <- layout + theme(
    axis.title = element_text(
      size = text.size
    ),
    axis.title.x = element_text(
      margin = margin(t = 10),
      hjust = 1
    ),
    axis.title.y = element_text(
      margin = margin(r = 10, t = 5),
      hjust = 1,
      debug = FALSE
    )
  )

  # axis labels
  layout <- layout + theme(axis.text = element_text(
    face = "plain",
    color = "#999999"
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
    panel.background = element_rect(fill = "#1D1D1D"),
    plot.background = element_rect(fill = "#1D1D1D", color = "#1D1D1D"),
    plot.margin = unit(plot.margin, "cm")
  )

  layout
}

# Colors ----

#' @rdname pal_trakt
#' @export
colors_trakt <- c(
  "red" = "#EA212D",
  "darkred" = "#A9262A",
  "dark" = "#111111",
  "gray" = "#999999",
  "white" = "#FFFFFF"
)

#' Colors matching trakt.tv
#'
#' At least its Dark Knight Mode.
#' @export
#' @examples
#' library(scales)
#' scales::show_col(pal_trakt()(5))
pal_trakt <- function() {
  scales::manual_pal(colors_trakt)
}

#' Discrete color & fill scales based on the FT palette
#'
#' See [pal_trakt()].
#'
#' @inheritDotParams ggplot2::discrete_scale -expand -position
#' @rdname pal_trakt
#' @export
scale_colour_trakt <- function(...) {
  ggplot2::discrete_scale("colour", "trakt", pal_trakt(), ...)
}

#' @export
#' @rdname pal_trakt
scale_color_trakt <- scale_colour_trakt

#' @export
#' @rdname pal_trakt
scale_fill_trakt <- function(...) {
  ggplot2::discrete_scale("fill", "trakt", pal_trakt(), ...)
}
