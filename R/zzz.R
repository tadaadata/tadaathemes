.onAttach <- function(libname, pkgname) {
  # Adapted from hrbrmstr/hrbrthemes

  # Load fonts if either option is set, for people who have already set
  # the option for hrbrthemes, we might as well respect that as well.
  load_fonts <- getOption("hrbrthemes.loadfonts", default = FALSE) |
    getOption("tadaathemes.loadfonts", default = FALSE)

  if (load_fonts) {
    if (interactive()) packageStartupMessage("Registering PDF & PostScript fonts with R")
    extrafont::loadfonts("pdf", quiet = TRUE)
    extrafont::loadfonts("postscript", quiet = TRUE)

    if (.Platform$OS.type == "windows")  { # nocov start
      if (interactive()) packageStartupMessage("Registering Windows fonts with R")
      extrafont::loadfonts("win", quiet = TRUE)
    }
  }

}
