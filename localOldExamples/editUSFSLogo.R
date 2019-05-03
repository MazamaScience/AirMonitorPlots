########################################################################################
img <- system.file("brandImages", "logo_USFS.png", package = "AirMonitorPlots")
icon <- system.file("brandImages", "logo_USFS.png", package = "AirMonitorPlots")
usfs <- png::readPNG(img)

# Get location of white space
whiteR <- usfs[,,1] >= .95
whiteG <- usfs[,,1] >= .95
whiteB <- usfs[,,1] >= .95
alpha  <- whiteR & whiteG & whiteB

# Add alpha layer
a <- array(c(usfs, as.numeric(!alpha)), dim = c(dim(usfs)[1:2], 4))
g <- grid::rasterGrob(a)
grid.draw(g)
usfs <- a
png::writePNG(usfs, "inst/brandImages/logo_USFS.png")
png::writePNG(usfs, "inst/brandImages/icon_USFS.png")
