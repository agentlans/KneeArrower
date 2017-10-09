#' Finds value of x whose y value is y0
#'
#' @param x x coordinates
#' @param y y coordinates
#' @param y0 value for y for which we want corresponding x value
#' @return value of the x coordinate whose y value is y0.
#'     If such x isn't in the domain of the function, then this function will return NA.
#' @importFrom stats approxfun optimize
findInverse <- function(x, y, y0) {
  if (y0 < min(y) | max(y) < y0) {
    return(NA)
  } else {
    # Interpolation function
    f <- approxfun(x, y, rule=1)
    # Minimize |f(x) - y0| over range of x
    return(optimize( function(x) abs(f(x) - y0), range(x))$minimum)
  }
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
#' @importFrom stats approx cor median predict smooth.spline
#' @export
findCutoff <- function(x, y, method="first", frac.of.steepest.slope=0.5) {
  # Test for non-numeric or infinite values
  is.invalid <- function(x) {
    any((!is.numeric(x)) | is.infinite(x))
  }
  if (is.invalid(x) || is.invalid(y)) {
    stop("x and y must be numeric and finite. Missing values not allowed.")
  }
  if (length(x) != length(y)) {
    stop("x and y must be of equal length.")
  }

  # Get value of curve at equally-spaced points
  new.x <- seq(from=min(x), to=max(x), length.out=length(x))
  # Use a spline which automatically smooths out noise
  sp <- smooth.spline(x, y)
  new.y <- predict(sp, new.x)$y

  # Finds largest odd number below given number
  largest.odd.num.lte <- function(x) {
    x.int <- floor(x)
    if (x.int %% 2 == 0) {
      x.int - 1
    } else {
      x.int
    }
  }

  # Use Savitzky-Golay filter to get derivatives
  smoothen <- function(y, p=p, filt.length=NULL, ...) {
    # Time scaling factor so that the derivatives are on same scale as original data
    ts <- (max(new.x) - min(new.x)) / length(new.x)
    p <- 3 # Degree of polynomial to estimate curve
    # Set filter length to be fraction of length of data
    # (must be an odd number)
    if (is.null(filt.length)) {
      filt.length <- min(largest.odd.num.lte(length(new.x)), 7)
    }
    if (filt.length <= p) {
      stop("Need more points to find cutoff.")
    }
    signal::sgolayfilt(y, p=p, n=filt.length, ts=ts, ...)
  }

  # Calculate first and second derivatives
  first.deriv <- smoothen(new.y, m=1)
  second.deriv <- smoothen(new.y, m=2)

  # Check the signs of the 2 derivatives to see whether to flip the curve
  # (Pick sign of the most extreme observation)
  pick.sign <- function(x) {
    most.extreme <- which(abs(x) == max(abs(x), na.rm=TRUE))[1]
    sign(x[most.extreme])
  }
  first.deriv.sign <- pick.sign(first.deriv)
  second.deriv.sign <- pick.sign(second.deriv)

  # The signs for which to flip the x and y axes
  x.sign <- 1
  y.sign <- 1
  if ((first.deriv.sign == -1) && (second.deriv.sign == -1)) {
    x.sign <- -1
  } else if ((first.deriv.sign == -1) && (second.deriv.sign == 1)) {
    y.sign <- -1
  } else if ((first.deriv.sign == 1) && (second.deriv.sign == 1)) {
    x.sign <- -1
    y.sign <- -1
  }
  # If curve needs flipping, then run same routine on flipped curve then
  # flip the results back
  if ((x.sign == -1) || (y.sign == -1)) {
    results <- findCutoff(x.sign * x, y.sign * y,
               method=method, frac.of.steepest.slope=frac.of.steepest.slope)
    return(list(x = x.sign * results$x, y = y.sign * results$y))
  }

  # Find cutoff point for x depending on method
  cutoff.x <- NA
  if (method == "first") {
    if (is.invalid(frac.of.steepest.slope)) {
      stop("Need to specify fraction of maximum slope.")
    }
    if (frac.of.steepest.slope <= 0 || frac.of.steepest.slope > 1) {
      stop("Fraction of maximum slope must be positive and be less than or equal to 1.")
    }
    # Find x where first derivative reaches cutoff
    slope.cutoff <- frac.of.steepest.slope * max(first.deriv)
    cutoff.x <- findInverse(new.x, first.deriv, slope.cutoff)
  } else if (method == "curvature") {
    # Find x where curvature is maximum
    curvature <- abs(second.deriv) / (1 + first.deriv^2)^(3/2)
    cutoff.x <- findInverse(new.x, curvature, max(curvature))
  } else {
    stop("Method must be either 'first' or 'curvature'.")
  }
  if (is.na(cutoff.x)) {
    warning("Cutoff point is beyond range. Returning NA.")
    list(x=NA, y=NA)
  } else {
    # Return cutoff point on curve
    approx(new.x, new.y, cutoff.x)
  }
}

