#' @title Add a logo to a ggplot object
#'
#' @description
#' Add a logo to a ggplot object.
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
#' @import grid
#' @export
#'
#' @examples
#' mts_monitor <- AirMonitor::Carmel_Valley
#' mts_tidy <- monitor_toTidy(mts_monitor)
#' p <- ggplot_pm25Timeseries(mts_tidy) +
#'   stat_dailyAQCategory(adjustylim = TRUE)
#' brandPlot(p, location = "topright", size = .2)
#' brandPlot(p, location = "bottomright", brandName = "USFS")
#' brandPlot(p, brandName = "AirFire", location = "topleft", size = .15)
brandPlot <- function(
  plot,
  brandStyle = "logo",
  brandName = "MazamaScience",
  brandFilePath = NULL,
  location = c("topright", "topleft", "bottomright", "bottomleft")[1],
  size = .1
) {

  ## Adapted from:
  #  Inserting an image to ggplot outside the chart area
  #  https://stackoverflow.com/a/12486301

  # Validate Parameters --------------------------------------------------------

  # Arguments are of the correct class
  if (!is.ggplot(plot)) stop("'plot' must be a ggplot object")
  if (!is.numeric(size)) stop("size must be a number")

  # Validate strings
  if (!brandStyle %in% c("logo", "icon"))
    stop("Invalid brandStyle. Choose from 'logo' or 'icon'.")

  if (!brandName %in% c("MazamaScience", "USFS", "AirFire"))
    stop("Invalid brandName. Choose from 'MazamaScience', 'USFS', or 'AirFire'.")

  if (!is.null(brandFilePath) && !file.exists(brandFilePath))
    stop("Invalid brandFilePath")

  if (!location %in% c("topright", "topleft", "bottomright", "bottomleft"))
    stop("Invalid location. Choose from 'topright', 'topleft', bottomright', or 'bottomleft")


  # Parameter defaults ---------------------------------------------------------

  # Get the logo path
  if (is.null(brandFilePath)) {

    if (brandStyle == "logo") {
      brandFileName <- "logo_"
    } else if (brandStyle == "icon") {
      brandFileName <- "icon_"
    }

    brandFileName <- paste0(brandFileName, brandName, ".png")
    brandFilePath <- system.file("brandImages", brandFileName, package = "AirMonitorPlots")
  }

  if (!file.exists(brandFilePath)) stop(paste0("could not find ", brandFilePath))

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

  return(invisible(g))

}
