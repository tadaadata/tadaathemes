#' ggplot2 theme to fit the readthedown Rmd format
#'
#' A ggplot theme to fit [`rmdformats::readthedown`] in terms of background color and
#' dark grid lines.
#' @param base_size Base text size, defaults to `12`.
#' @param base_family Base text family. Use `"Roboto Slab"` to match the `readthedown`
#' headers, or `"Lato"` for the body style.
#' @param bg Background color, defaults to `rmdformats::readthedown`'s background,
#' `#fcfcfc`.
#' @param axis_emph Which axis to emphasize visually (black lines).
#' One of `"x", "y", "xy", NULL`.
#' @param ... Other arguments passed to [ggplot2::theme()].
#' @import ggplot2
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(iris, aes(x = Sepal.Width)) +
#'   geom_histogram(binwidth = .25) +
#'   labs(
#'     title = "Yet another iris plot",
#'     subtitle = "Using theme_readthedown",
#'     caption = "Oh hi there, didn't see you walk in"
#'   )
#'
#' p + theme_readthedown()
#'
#' \dontrun{
#' # These may require font installation
#' p + theme_readthedown(base_family = "Lato")
#' p + theme_readthedown(base_family = "Roboto Condensed", axis_emph = "x")
#' }
theme_readthedown <- function(base_size = 12, base_family = "",
                              bg = "#fcfcfc", axis_emph = c("xy", "x", "y"), ...) {
  axis_emph <- match.arg(axis_emph)
  template_background <- element_rect(fill = bg, color = bg)

  result <- ggplot2::theme_grey(
    base_size = base_size, base_family = base_family
  ) %+replace%
    theme(
      panel.background = template_background,
      plot.background = template_background,
      legend.background = template_background,
      legend.key = template_background,
      plot.caption = element_text(hjust = 1, vjust = 0, size = rel(.7)),
      axis.title.x = element_text(hjust = 0, margin = margin(t = 10)),
      strip.placement = "outside",
      strip.background = element_blank(),
      ...
    )

  if (axis_emph == "x") {
    result <- result +
      theme(
        panel.grid.major.x = element_line(size = .1, color = "black"),
        panel.grid.minor.x = element_line(size = .1, color = "black", linetype = "dotted")
      )
  } else if (axis_emph == "y") {
    result <- result +
      theme(
        panel.grid.major.y = element_line(size = .1, color = "black"),
        panel.grid.minor.y = element_line(size = .1, color = "black", linetype = "dotted")
      )
  } else if (axis_emph == "xy") {
    result <- result +
      theme(
        panel.grid.major.x = element_line(size = .1, color = "black"),
        panel.grid.minor.x = element_line(size = .1, color = "black", linetype = "dotted"),
        panel.grid.major.y = element_line(size = .1, color = "black"),
        panel.grid.minor.y = element_line(size = .1, color = "black", linetype = "dotted")
      )
  } else if (axis_emph == "" | is.null(axis_emph)) {
  }

  result
}
