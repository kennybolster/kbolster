#' Time for a ship to complete its objectives
#'
#' Given a set of station coordinates, estimated time at each station,
#' and a speed of the ship, calculate how long it will take for the ship
#' to do all that
#' @param lats latitude of each station, should be from -90 to 90. Should
#' be vector of same size as longs
#' @param longs longitude of each station, should be from -180 to 180
#' @param times estimated time at each station in hours. Should be a vector
#' @param speed estimated speed of the ship, in knots
#' @return estimated time in hours
#' @export
#' @examples
#' shipTime(c(0, 10, 10), c(0, 10, 15), c(1, 0, 0), 12)

shipTime <- function(lats, longs, times, speed){
  # take a list of stations and the amount of time spent at each of them, combined with
  # the speed of a ship, and compute the total required time
  # lats: latitudes of each station
  # longs: longitudes of each station
  # times: time to spend at each station in hours
  # speed: speed of the ship in knots
  # lats and longs should be vectors of the same length
  # example:
  # shipTime(c(0, 10, 10), c(0, 10, 15), c(1, 0, 0), 12)
  require(geosphere)
  mToNm <- 1855.325 # number of meters in a nautical mile
  npoints <- length(lats)
  distToStation <- function(s, lats = lats, longs = longs){
    # distance to a particular station from the one in front of it
    lat1 <- lats[s-1]
    lat2 <- lats[s]
    lon1 <- longs[s-1]
    lon2 <- longs[s]
    distGeo(c(lon1, lat1), c(lon2, lat2))
  }
  dists <- sapply(2:npoints, distToStation, lats = lats, longs = longs)
  distNM <- sum(dists)/mToNm
  transitTime <- distNM/speed
  totalTime <- transitTime + sum(times)
  totalTime
}



