#' Import Source Sans font for use in charts
#'
#' Source Sans Pro is Copyright 2010, 2012 Adobe Systems Incorporated
#' (http://www.adobe.com/), with Reserved Font Name ‘Source’.
#' ([License](https://www.fontsquirrel.com/license/source-sans-pro)).
#' You can find it on [Google Fonts](https://fonts.google.com/specimen/Source+Sans+Pro).
#'
#' @details
#' This will add the fonts via [`sysfonts::font_add`], use
#' [`showtext::showtext_auto()`] to use the fonts. Other steps may be necessary.
#' Since this is an .otf fonts, and other fonts commonly bundled / used with R
#' packages (e.g. `hrbrthemes`) are .ttf fonts, this needs to be handled
#' differently, meaning that [`extrafont::font_import()`] will not work.
#' I am currently not sure which steps specifically are required for this to work,
#' but at least I can bundle the fonts for easier import.
#'
#' @name SourceSansPro
#' @note Fonts are hard and platform-compatible font usage is hard to debug.
#' @export
#' @importFrom sysfonts font_add
import_source_sans <- function() {

  ssans_font_dir <- system.file("fonts", "source-sans-pro", package = "tadaathemes")

  sysfonts::font_add(
    family = "Source Sans Pro Light",
    regular = file.path(ssans_font_dir, "SourceSansPro-Light.otf")
  )
  sysfonts::font_add(
    family = "Source Sans Pro",
    regular = file.path(ssans_font_dir, "SourceSansPro-Regular.otf")
  )
  sysfonts::font_add(
    family = "Source Sans Pro Semibold",
    regular = file.path(ssans_font_dir, "SourceSansPro-Semibold.otf")
  )
  sysfonts::font_add(
    family = "Source Sans Pro Bold",
    regular = file.path(ssans_font_dir, "SourceSansPro-Bold.otf")
  )

  message(
    sprintf(
      "You will likely need to install these fonts on your system as well.\n\nYou can find them in [%s]",
      ssans_font_dir)
  )

}

#' @rdname SourceSansPro
#' @title Source Sans font name R variable aliases
#' @description `font_ssp` == "`Source Sans Pro`" etc.
#' @format length 1 character vector
#' @export
font_ssp <- "Source Sans Pro"

#' @rdname SourceSansPro
#' @export
font_ssp_semibold <- "Source Sans Pro Semibold"

#' @rdname SourceSansPro
#' @export
font_ssp_bold <- "Source Sans Pro Bold"


#' @rdname SourceSansPro
#' @export
font_ssp_light <- "Source Sans Pro Light"
