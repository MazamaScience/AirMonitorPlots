#' @keywords plotting
#' @export
#' @title Add wind barbs to a map
#' @param x vector of longitudes
#' @param y vector of latitudes
#' @param speed wind speed in knots (Doesn't currently support vectorized wind speed)
#' @param dir wind direction in degrees clockwise from north
#' @param circleSize size of the circle 
#' @param circleFill circle fill color (currently not supported)
#' @param lineCol line color (currently not supported)
#' @param barbSize size of the barb 
#' @param ... additional arguments to be passed to \code{lines}
#' @description Add a multi-sided polygon to a plot.
#' @examples
#' maps::map('state', "washington")
#' x <- c(-121, -122)
#' y <- c(47.676057, 47)
#' wind_barb(x, y, speed = 125, dir = c(45, 90), lwd = 2, circleSize = .5, barbSize = 1.5)


wind_barb <- function(x, y, speed, dir, circleSize = 1, circleFill = 'transparent', lineCol = 1, barbSize = 1, ...) {
  
  
  # https://commons.wikimedia.org/wiki/Wind_speed
  
  # Wind direction is measured in degrees clockwise from north
  # We want to convert into counter-clockwise from east
  dir <- (360 - dir + 90) %% 360
  
  # Get dir in radians
  
  rad <- dir * pi / 180
  
  # Get x and y scale factors
  pin <- par("pin")
  usr <- par("usr")
  xpi <- (usr[2]-usr[1])/pin[1]
  ypi <- (usr[4]-usr[3])/pin[2]
  
  # Add a little circle
  # Default radius is 1/24 inch. Change based on circleSize 
  for (i in 1:length(x)) {
    rx <- xpi/24*circleSize
    ry <- ypi/24*circleSize
    theta <- seq(0, 2*pi, length = 50)
    xx <- x[i] + rx*cos(theta)
    yy <- y[i] + ry*sin(theta)
    
    polygon(xx, yy, col = circleFill, ...)
  }
  
  
  # The baseline barb length will be 1/4 inch
  lx <- xpi / 4 * barbSize
  ly <- ypi / 4 * barbSize
  
  # Get starting and ending points for barb
  xs <- x+rx*cos(rad)
  ys <- y+rx*sin(rad)
  xe <- xs+lx*cos(rad)
  ye <- ys+ly*sin(rad)
  
  # Plot the line
  for (i in 1:length(x)) {
    lines(c(xs[i], xe[i]), c(ys[i], ye[i]), ...)
  }
  
  
  
  # Add flags
  # flag angle 
  fa <- rad + 75*pi/180
  
  # 5 knots
  # start at 5/6 of the way up the barb
  # length is 1/4 of the barb length
  # position counts in by 1/6th of the barb from outside
  add_5 <- function(position) {
    fxs <- xs + lx*cos(rad)*(7-position)/6
    fxe <- fxs + lx/4*cos(fa)
    fys <- ys + ly*sin(rad)*(7-position)/6
    fye <- fys + ly/4*sin(fa)
    
    for (i in 1:length(x)) {
      lines(c(fxs[i], fxe[i]), c(fys[i], fye[i]), ...)
    }
    
  }
  
  
  # 10 knots
  # start at end of barb
  # length is 1/2 of barb length
  # position counts in by 1/6th of the barb from outside
  add_10 <- function(position) {
    fxs <- xs + lx*cos(rad)*(7-position)/6
    fxe <- fxs + lx/2*cos(fa)
    fys <- ys + ly*sin(rad)*(7-position)/6
    fye <- fys + ly/2*sin(fa)
    
    for (i in 1:length(x)) {
      lines(c(fxs[i], fxe[i]), c(fys[i], fye[i]), ...)
    } 
  }
  
  # 50 knots
  add_50 <- function(position) {
    fx1 <- xs + lx*cos(rad)*(6-position)/6
    fx2 <- fx1 + lx/2*cos(fa)
    fx3 <- xs + lx*cos(rad)*(7-position)/6
    fy1 <- ys + ly*sin(rad)*(6-position)/6
    fy2 <- fy1 + ly/2*sin(fa)
    fy3 <- ys + ly*sin(rad)*(7-position)/6
    
    for (i in 1:length(x)) {
      polygon(c(fx1[i], fx2[i], fx3[i], fx1[i]), c(fy1[i], fy2[i], fy3[i], fy1[i]), col = lineCol, ...) 
    }
  }
  
  
  fifties <- speed %/% 50
  tens <- (speed %% 50) %/% 10
  fives <- (speed %% 10) %/% 5
  
  if (fifties > 0) {
    for (i in 1:fifties) {
      add_50(i)
    }
    if (tens > 0) {
      for (i in (fifties+1):(fifties+1+tens)) {
        add_10(i)
      }
    }
    if (fives > 0) {
      add_5(fifties + 1)
    }
  } else {
    
    if (tens > 0) {
      for (i in 1:tens) {
        add_10(i)
      }
    }
    
    
    if (fives == 1) {
      add_5(max(c(tens, 1))+1)
    }
    
  }
  
  
  
}
