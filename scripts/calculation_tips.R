# ============================================================================ #
# 
# 
# ---- Tips For Calculations ----
# 
# 
# ============================================================================ #  


##%######################################################%##
#                                                          #
####                  Upwelling Indice                  ####
#                                                          #
##%######################################################%##
#' Upwelling Indice
#'
#' Using the x- and y- components of Ekman Transport plus coastline angle, the
#' upwelling index can be calculated. Using NOAA CoastWatch ERRDAP, a global 
#' model can be downloaded with the Ekman parameters.
#' 
#' The coastal angle will need to be calcualted separately
#'
#' @param ektrx X component of Ekman Transport
#' @param ektry Y component of Ekman Transport
#' @param coast_angle Coastal angle from 0 degrees North. This is the angle the 
#' coast makes with north in the mathematical sense, and is defined as the angle 
#' the landward side of the coastline makes with a vector pointing north 
#'
#' @details
#' Find more details here: <https://oceanview.pfeg.noaa.gov/products/upwelling/bakun>
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' 
#' ektrx <- 2000 # kg m^-1 s^-1
#' ektry <- 1000 # kg m^-1 s^-1
#' coast_angle <- 90 # degrees
#' 
#' upwell(ektrx, ektry, coast_angle)
#' 
upwell <- function(ektrx, ektry, coast_angle) {
  pi <- 3.1415927
  degtorad <- pi/180.
  alpha <- (360 - coast_angle) * degtorad
  s1 <- cos(alpha)
  t1 <- sin(alpha)
  s2 <- -1 * t1
  t2 <- s1
  perp <- (s1 * ektrx) + (t1 * ektry)
  para <- (s2 * ektrx) + (t2 * ektry)
  return(perp/10)
}

