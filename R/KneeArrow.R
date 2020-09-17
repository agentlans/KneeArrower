# KneeArrower: Finds Cutoff Points on Knee Curves
# Copyright 2018, 2019, 2020 Alan Tseng
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


#' Derivative of a function with respect to x
#'
#' @param x x coordinates of points in function's domain
#' @param y y coordinates of points in function's range
#' @param m the order of the derivative (0 for y, 1 for y', 2 for y'')
#' @param n number of points in the domain for interpolation
#' @return a function representing the mth derivative of y(x) with respect to x
#' @importFrom stats approxfun approx smooth.spline
#' @importFrom signal sgolayfilt
#' @export
#' @examples
#' x <- seq(0,5,0.1)
#' y <- x^2 - 2*x + 3 # So dy/dx = 2x - 2
#' fp <- derivative(x, y, 1)
#' fp(2) # 2
#' fp(5) # 8
derivative <- function(x, y, m=0, n=50) {
  xmin <- min(x)
  xmax <- max(x)

  delta_x <- (xmax-xmin)/(n-1)
  new_x <- seq(xmin, xmax, length.out=n)
  xy <- as.data.frame(approx(x, y, new_x, rule=2))
  # Do another round of smoothing
  sp <- smooth.spline(xy$x, xy$y)
  xy$y <- sp$y

  new_y <- sgolayfilt(xy$y, m=m, ts=delta_x)
  approxfun(new_x, new_y)
}

#' Inverse of a function
#'
#' @param f univariate function
#' @param domain domain of f given as (min, max) interval
#' @return a function g such that f(x) = y and g(y) = x
#' @importFrom stats optimize
#' @export
#' @examples
#' expinv <- inverse(exp, c(0,3))
#' expinv(exp(1))
inverse <- function(f, domain) {
  function(y) {
    # Minimize |f(x)-y|
    opt <- optimize(function(x) {
      abs(f(x)-y)
    }, domain)
    # If we couldn't optimize it to 0, then no solution
    # if (opt$objective < .Machine$double.eps^0.1) {
    #   opt$minimum
    # } else {
    #   NA
    # }
    opt$minimum
  }
}

#' Finds the point where the derivative is a fraction of the steepest slope
#'
#' @param x x coordinates of points around the curve
#' @param y y coordinates of points around the curve
#' @param slope_ratio the fraction of the steepest slope that defines knee point
#' @return (x, y) coordinates of the knee point
#' @importFrom stats optimize
findCutoffFirstDerivative <- function(x, y, slope_ratio=0.5) {
  yf <- derivative(x, y, 0)
  yp <- derivative(x, y, 1)
  # Find the steepest slope either up or down
  xrange <- range(x)
  max_slope <- optimize(yp, xrange, maximum=TRUE)$objective
  min_slope <- optimize(yp, xrange)$objective
  steepest <- if (abs(max_slope) > abs(min_slope)) {
    max_slope
  } else {
    min_slope
  }
  # Want to find x that has the required slope
  slope <- steepest * slope_ratio
  yi <- inverse(yp, xrange)
  knee_x <- yi(slope)
  list(x=as.numeric(knee_x), y=as.numeric(yf(knee_x)))
}

#' Finds the point on the curve that has the maximum curvature
#'
#' @param x x coordinates of points around the curve
#' @param y y coordinates of points around the curve
#' @return (x, y) coordinates of the point with the greatest curvature
#' @importFrom stats optimize
findCutoffCurvature <- function(x, y) {
  yf <- derivative(x, y, 0)
  yp <- derivative(x, y, 1)
  ypp <- derivative(x, y, 2)
  curvature <- function(x) {
    abs(ypp(x)/(1+yp(x)^2)^(3/2))
  }
  knee_x <- optimize(curvature, range(x), maximum=TRUE)$maximum
  list(x=as.numeric(knee_x), y=as.numeric(yf(knee_x)))
}

#' Finds cutoff point on knee curve
#'
#' @param x vector of x coordinates of points around curve
#' @param y vector of y coordinates of points around curve
#' @param method the method to define the knee point.
#'    Value can be "first" for first derivative cutoff or "curvature" for maximum curvature cutoff.
#' @param frac.of.steepest.slope the slope at the cutoff point relative to the steepest (positive or negative) slope on the curve.
#'    Only used if method is set to "first". Can be set to any number > 0 or <= 1. If the knee curve is increasing and concave down,
#'    then lower numbers will lead to higher knee points, and higher numbers will lead to lower knee points.
#' @return a list containing the (x, y) coordinates of the knee point chosen using the specified method
#' @examples
#' # Generate some knee data
#' x <- runif(100, min=-3, max=3)
#' y <- -exp(-x) * (1+rnorm(100)/3)
#' plot(x, y)
#' # Plot knee points calculated using two different methods
#' points(findCutoff(x,y), col="red", pch=20, cex=3)
#' points(findCutoff(x,y, method="curvature"), col="blue", pch=20, cex=3)
#' @export
findCutoff <- function(x, y, method="first", frac.of.steepest.slope=0.5) {
  stopifnot(length(x) == length(y),
            length(x) >= 4,
            !(any(is.na(x)) || any(is.na(y)) || any(is.infinite(x)) || any(is.infinite(y))))
  if (method == "first") {
    stopifnot(0 < frac.of.steepest.slope,
              frac.of.steepest.slope <= 1)
    findCutoffFirstDerivative(x, y, frac.of.steepest.slope)
  } else if (method == "curvature") {
    findCutoffCurvature(x, y)
  } else {
    stop("Method must be either 'first' or 'curvature'.")
  }
}

