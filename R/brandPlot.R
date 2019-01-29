#' @title Add Branding to a ggplot object
#'
#' @description
#' Add a logo to a ggplot object. 
#' 
#' @param plot \code{ggplot} object
#' @param brandStyle \code{"logo"} or \code{"icond"}. 
#' @param brandName Name of brand: \code{"MazamaScience"}, \code{"USFS"}, or \code{"AirFire"}.
#' @param brandFilePath Path to brand logo or icon. If not \code{NULL}, overrides
#' \code{brandStyle} and \code{brandName}.
#' @param location string indicating the location where the logo should be 
#' printed. Options are: \code{"topright"}, \code{"topleft"}, \code{"bottomright"}, 
#' or \code{"bottomleft"}. 
#' @param size Brand icon or logo width, in fraction of plot width. 
#' 
#' @return A \code{gTree} object, which can be printed with \code{grid.draw()}. 
#'
#' @import grid
#' @export
#' 
#' @examples 
#' ws_monitor <- PWFSLSmoke::Carmel_Valley
#' ws_tidy <- monitor_toTidy(ws_monitor)
#' p <- ggplot_pm25Timeseries(ws_tidy) +
#'   stat_dailyAQILevel(adjustylim = TRUE) 
#' brandPlot(p, location = "topright", size = .2)
#' prandPlot(p, brandName = "USFS")




brandPlot <- function(plot,
                      brandStyle = 'logo',
                      brandName = 'MazamaScience',
                      brandFilePath = NULL,
                      location = c("topright","topleft","bottomright","bottomleft")[1],
                      size = .1) {
  # Adapted from 
  # https://stackoverflow.com/questions/12463691/inserting-an-image-to-ggplot-outside-the-chart-area/12486301#12486301

  # Get the logo path
  if (is.null(brandFilePath)) {
    if ( brandStyle == "logo" ) {
      brandFileName <- "logo_"
    } else if ( brandStyle == "icon" ) {
      brandFileName <- "icon_"
    }
    brandFileName <- paste0(brandFileName, brandName, ".png")
    brandFilePath <- system.file("brandImages", brandFileName, package = "PWFSLSmokePlots")
  }
  if (!file.exists(brandFilePath)) {
    stop(paste0("could not find ", brandFilePath))
  }
  
  img <- suppressWarnings(png::readPNG(brandFilePath))
  g <- rasterGrob(img)
  
  # Get dimensions of img 
  imgDim <- dim(img)
  size_w <- unit(size, "npc")
  size_h <- unit(size * imgDim[1]/imgDim[2], "npc") 
  
  
  
  
  # Set up the layout for grid 
  if (location == "topright") {
    heights = unit.c(size_h, unit(1, "npc") - size_h)
    widths = unit.c(unit(1, "npc") - size_w, size_w)
    logoRow <- 1
    logoCol <- 2
  } else if (location == "topleft") {
    heights = unit.c(size_h, unit(1, "npc") - size_h)
    widths = unit.c(size_w, unit(1, "npc") - size_w)
    logoRow <- 1
    logoCol <- 1
  } else if (location == "bottomright") {
    heights = unit.c(unit(1, "npc") - size_h, size_h)
    widths = unit.c(unit(1, "npc") - size_w, size_w)
    logoRow <- 2
    logoCol <- 2
  } else if (location == "bottomleft") {
    heights = unit.c(unit(1, "npc") - size_h, size_h)
    widths = unit.c(size_w, unit(1, "npc") - size_w)
    logoRow <- 2
    logoCol <- 1
  }
  layout = grid.layout(2, 2, widths = widths, heights = heights)
  
  # Position the elements within the viewports
  grid.newpage()
  pushViewport(viewport(layout = layout))
  
  # The plot
  pushViewport(viewport(layout.pos.row=1:2, layout.pos.col = 1:2))
  print(plot, newpage=FALSE)
  popViewport()
  
  # The logo
  pushViewport(viewport(layout.pos.row=logoRow, layout.pos.col = logoCol))
  print(grid.draw(g), newpage=FALSE)
  popViewport()
  popViewport()
  
  # To save the object
  g = grid.grab()
  
  return(invisible(g))
  
}
