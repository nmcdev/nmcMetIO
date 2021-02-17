#' convert wind angle and speed to u component.
#'
#' @param angle, wind angle or direction
#' @param speed, wind speed or velocity
#'
#' @return wind u component
#' @export
#'
wind2u <- function(angle, speed) {
  u <- -1.0 * sin(angle * pi/180.) * speed
  return(u)
}

#' convert wind angle and speed to v component.
#'
#' @param angle, wind angle or direction
#' @param speed, wind speed or velocity
#'
#' @return wind v component
#' @export
#'
wind2v <- function(angle, speed) {
  v <- -1.0 * cos(angle * pi/180.) * speed
  return(v)
}

#' convert u, v components to wind speed.
#'
#' @param u, u components
#' @param v, v components
#'
#' @return wind speed
#' @export
#'
uv2speed <- function(u, v){
  speed <- sqrt(u*u + v*v)
  return(speed)
}

#' convert u, v components to wind angle
#'
#' @param u, u components
#' @param v, v components
#'
#' @return wind angle
#' @export
#'
uv2angle <- function(u, v){
  angle <- 90. - atan2(-v, -u) * 180.0 / pi
  angle[angle < 0] <- angle[angle < 0] + 360.
  return(angle)
}

