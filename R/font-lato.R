#' Import Lato font for use in charts
#'
#' Lato is Copyright (c) 2010-2014, Łukasz Dziedzic,
#' with Reserved Font Name Lato ([License](https://www.fontsquirrel.com/license/lato)).
#' You can find it on [Google Fonts](https://fonts.google.com/specimen/Lato).
#'
#' There is an option `hrbrthemes.loadfonts` which -- if set to `TRUE` -- will
#' call `extrafont::loadfonts()` to register non-core fonts with R PDF & PostScript
#' devices. If you are running under Windows, the package calls the same function
#' to register non-core fonts with the Windows graphics device.
#'
#' @name Lato
#' @encoding UTF-8
#' @note This will take care of ensuring PDF/PostScript usage. The location of the
#'   font directory is displayed after the base import is complete. It is highly
#'   recommended that you install them on your system the same way you would any
#'   other font you wish to use in other programs.
#' @export
#' @importFrom extrafont font_import
import_lato <- function() {
  lato_font_dir <- system.file("fonts", "lato", package = "tadaathemes")

  suppressWarnings(suppressMessages(
    extrafont::font_import(lato_font_dir, prompt = FALSE)
  ))

  message(
    sprintf(
      "You will likely need to install these fonts on your system as well.\n\nYou can find them in [%s]",
      lato_font_dir
    )
  )
}

#' @rdname Lato
#' @md
#' @title Lato font name R variable aliases
#' @description `font_lato` == "`Lato`" ("Regular")
#' @format length 1 character vector
#' @export
font_lato <- "Lato"

#' @rdname Lato
#' @md
#' @description `font_lato_bold` == "`Lato Bold`"
#' @export
font_lato_bold <- "Lato Bold"

#' @rdname Lato
#' @md
#' @description `font_lato_light` == "`Lato Light`"
#' @export
font_lato_light <- "Lato Light"

#' @rdname Lato
#' @md
#' @description `font_lato_semibold` == "`Lato Semibold`"
#' @export
font_lato_semibold <- "Lato Semibold"
