#' @export
#' @import grid
#'
#' @title Add a logo to a ggplot object
#'
#' @description
#' Adds an image to a ggplot object. This allows the use of package internal
#' logos associated with Mazama Science and the USFS AirFire group specified
#' with \code{brandStyle} and \code{brandName}. User provided images can be
#' specified by using \code{brandFilePath}.
#'
#' @param plot \code{ggplot} object
#' @param brandStyle \code{"logo"} or \code{"icon"}.
#' @param brandName Name of brand: \code{"MazamaScience"}, \code{"USFS"}, or
#'   \code{"AirFire"}.
#' @param brandFilePath Path to brand logo or icon. If not \code{NULL},
#'   overrides \code{brandStyle} and \code{brandName}.
#' @param location String indicating the location where the logo should be
#'   printed. Options are: \code{"topright"}, \code{"topleft"},
#'   \code{"bottomright"}, or \code{"bottomleft"}.
#' @param size Brand icon or logo width, in fraction of plot width.
#'
#' @return A \code{gTree} object, which can be printed with \code{grid.draw()}.
#'
#' @examples
#' library(AirMonitorPlots)
#'
#' monitor <- AirMonitor::Carmel_Valley
#' mts_tidy <- monitor_toTidy(monitor)
#'
#' gg <-
#'   ggplot_pm25Timeseries(mts_tidy) +
#'   stat_dailyAQCategory(adjustylim = TRUE)
#'
#' brandPlot(gg, location = "topright", size = .2)
#' brandPlot(gg, location = "bottomright", brandName = "USFS")
#' brandPlot(gg, brandName = "AirFire", location = "topleft", size = .15)
#'

brandPlot <- function(
  plot,
  brandStyle = c("logo", "icon"),
  brandName = c("MazamaScience", "USFS", "AirFire"),
  brandFilePath = NULL,
  location = c("topright", "topleft", "bottomright", "bottomleft"),
  size = .1
) {

  # NOTE:  Adapted from:
  # NOTE:    Inserting an image to ggplot outside the chart area
  # NOTE:    https://stackoverflow.com/a/12486301

  # ----- Validate Parameters --------------------------------------------------

  # Arguments are of the correct class
  if (!is.ggplot(plot)) stop("'plot' must be a ggplot object")
  if (!is.numeric(size)) stop("size must be a number")

  brandStyle <- match.arg(brandStyle)
  brandName <- match.arg(brandName)

  if ( !is.null(brandFilePath) && !file.exists(brandFilePath) )
    stop("Invalid brandFilePath")

  location <- match.arg(location)


  # ----- Parameter defaults ---------------------------------------------------

  # Get the logo path
  if ( is.null(brandFilePath) ) {

    if (brandStyle == "logo") {
      brandFileName <- "logo_"
    } else if (brandStyle == "icon") {
      brandFileName <- "icon_"
    }

    brandFileName <- paste0(brandFileName, brandName, ".png")
    brandFilePath <- system.file("brandImages", brandFileName, package = "AirMonitorPlots")

  }

  if ( !file.exists(brandFilePath) )
    stop(paste0("could not find ", brandFilePath))

  # ----- Add image ------------------------------------------------------------

  img <- suppressWarnings(png::readPNG(brandFilePath))
  g <- rasterGrob(img)

  # Get dimensions of img
  imgDim <- dim(img)
  size_w <- unit(size, "npc")
  size_h <- unit(size * imgDim[1] / imgDim[2], "npc")

  # Set up the layout for grid
  if (location == "topright") {
    heights <- unit.c(size_h, unit(1, "npc") - size_h)
    widths  <- unit.c(unit(1, "npc") - size_w, size_w)
    logoRow <- 1
    logoCol <- 2

  } else if (location == "topleft") {
    heights <- unit.c(size_h, unit(1, "npc") - size_h)
    widths  <- unit.c(size_w, unit(1, "npc") - size_w)
    logoRow <- 1
    logoCol <- 1

  } else if (location == "bottomright") {
    heights <- unit.c(unit(1, "npc") - size_h, size_h)
    widths  <- unit.c(unit(1, "npc") - size_w, size_w)
    logoRow <- 2
    logoCol <- 2

  } else if (location == "bottomleft") {
    heights <- unit.c(unit(1, "npc") - size_h, size_h)
    widths  <- unit.c(size_w, unit(1, "npc") - size_w)
    logoRow <- 2
    logoCol <- 1
  }

  layout <- grid.layout(2, 2, widths = widths, heights = heights)

  # Position elements within viewports
  grid.newpage()
  pushViewport(viewport(layout = layout))

  # Put plot in one viewpoert
  pushViewport(viewport(layout.pos.row = 1:2, layout.pos.col = 1:2))
  print(plot, newpage = FALSE)
  popViewport()

  # Put logo in another viewport
  pushViewport(viewport(layout.pos.row = logoRow, layout.pos.col = logoCol))
  print(grid.draw(g), newpage = FALSE)
  popViewport()

  popViewport() # top level Viewport

  # To save the object
  g <- grid.grab()

  # ----- Return ---------------------------------------------------------------

  return(invisible(g))

}
