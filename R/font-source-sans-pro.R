#' Import Source Sans font for use in charts
#'
#' Source Sans Pro is Copyright 2010, 2012 Adobe Systems Incorporated
#' (http://www.adobe.com/), with Reserved Font Name ‘Source’.
#' ([License](https://www.fontsquirrel.com/license/source-sans-pro)).
#' You can find it on [Google Fonts](https://fonts.google.com/specimen/Source+Sans+Pro).
#'
#' @details
#'
#' The font files shipped with this package were retrieved from
#' `https://github.com/adobe-fonts/source-sans-pro/releases/download/3.028R/source-sans-3v028R.zip`.
#'
#' Unfortunately there's some confusion with the name of the base font family,
#' which is "Source Sans Pro" on my `brew` installed fonts on macOS, but it's
#' "Source Sans 3" for the GitHub-installed version on a Linux machine.
#' I have not yet figured out how to handle this.
#'
#' @name SourceSansPro
#' @note Fonts are hard and platform-compatible font usage is hard to debug.
#' @export
import_source_sans <- function() {
  ssans_font_dir <- system.file("fonts", "source-sans-pro", package = "tadaathemes")

  suppressWarnings(suppressMessages(
    extrafont::font_import(ssans_font_dir, prompt = FALSE)
  ))
  message(
    sprintf(
      "You will likely need to install these fonts on your system as well.\n\nYou can find them in [%s]",
      ssans_font_dir
    )
  )
}

#' @rdname SourceSansPro
#' @title Source Sans font name R variable aliases
#' @description `font_ssp` == "`Source Sans 3`" etc.
#' @format length 1 character vector
#' @export
font_ssp <- "Source Sans 3"

#' @rdname SourceSansPro
#' @export
font_ssp_semibold <- "Source Sans 3 Semibold"

#' @rdname SourceSansPro
#' @export
font_ssp_light <- "Source Sans 3 Light"
