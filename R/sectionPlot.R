#' Oceanographic section plot
#'
#' Given measurements of sample locations and the concentraiton of an
#' element in those samples, generate a blue to red contour plot. Requires
#' the akima, ggplot2, and stringr packages
#' @param dist distance along the section
#' @param depth depth of samples
#' @param c concentration of the element in the samples
#' @param title title to put on the plot, defaults to none
#' @param xlab label for the x axis, defaults to 'Distance (km)'
#' @param ylab label for the y axis, defaults to 'Depth (m)'
#' @param units what unit is being used for the concentraiton, defaults
#' to 'nM'
#' @param name what element is being plotted, defaults to 'Fe'
#' @export
#' @examples sectionPlot(dist, depth, c)
#'

sectionPlot <- function(dist, depth, c, title = '', xlab = 'Distance (km)',
                        ylab = 'Depth (m)', units = 'nM', name = 'Fe'){
  # from vectors of irregularly spaced oceanographic points, generate a section plot
  # dist: x axis coordinate. Can be distance as well as degrees longitude or latitude
  # depth: y axis coordinate
  # c: concentration of the species of interest at each data point
  # EXAMPLE:
  # lat <- runif(40)
  # depth <- runif(40)
  # c <- runif(40)
  # sectionPlot(lat, depth, c)
  require(akima)
  require(ggplot2)
  require(stringr)
  interped <- interp2xyz(interp(dist, depth, c), data.frame = T)
  ggplot(interped, mapping = aes(x, y)) +
    geom_raster(aes(fill = z)) +
    geom_contour(aes(z = z), color = 'white', alpha = .5) +
    scale_y_reverse() +
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab) +
    scale_fill_gradient(low = 'blue', high = 'red',
                        name = str_c(name, ' (', units, ')'))
}

