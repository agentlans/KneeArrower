#' Finds value of x whose y value is y0
#'
#' @param x x coordinates
#' @param y y coordinates
#' @param y0 value for y for which we want corresponding x value
#' @return value of the x coordinate whose y value is close to y0
#' @importFrom stats approxfun optimize
findInverse <- function(x, y, y0) {
  if (y0 < min(y) | max(y) < y0) {
    warning("y value is outside the range of function. x coordinate may not exist. Returning NA.")
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
#' @param frac.of.max.slope cutoff value for the first derivative relative to the maximum slope.
#'    Only used if method is set to "first". Can be set to any number > 0 or <= 1. Lower numbers will lead to higher knee points.
#'    Higher numbers will lead to lower knee points.
#' @return a list containing the (x, y) coordinates of the knee point chosen using the specified method
#' @examples
#' # Generate some knee data
#' x <- runif(100, min=-3, max=3)
#' y <- -exp(-x) * (1+rnorm(100)/3)
#' plot(x, y)
#' # Plot knee points calculated using two different methods
#' points(findCutoff(x,y), col="red", pch=20, cex=3)
#' points(findCutoff(x,y, method="curvature"), col="blue", pch=20, cex=3)
#' @importFrom stats approx cor median
#' @export
findCutoff <- function(x, y, method="first", frac.of.max.slope=0.5) {
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
  new.y <- approx(x, y, new.x)$y

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
      filt.length <- min(largest.odd.num.lte(length(new.x)), 17)
    }
    if (filt.length <= p) {
      stop("Need more points to find cutoff.")
    }
    signal::sgolayfilt(y, p=p, n=filt.length, ts=ts, ...)
  }

  # Calculate first and second derivatives
  zero.deriv <- smoothen(new.y, m=0)
  # Reason for calling smoothen twice is to make sure 1st and 2nd derivatives are smooth
  # if original data has noise
  first.deriv <- smoothen(smoothen(new.y, m=1), p=1,
                          filt.length=min(7, largest.odd.num.lte(length(new.x))))
  second.deriv <- smoothen(smoothen(new.y, m=2), p=1,
                           filt.length=min(7, largest.odd.num.lte(length(new.x))))

  # Check the signs of the 2 derivatives to see whether to flip the curve
  first.deriv.sign <- sign(median(first.deriv))
  second.deriv.sign <- sign(median(second.deriv))
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
               method=method, frac.of.max.slope=frac.of.max.slope)
    return(list(x = x.sign * results$x, y = y.sign * results$y))
  }

  # Find cutoff point for x depending on method
  cutoff.x <- NA
  if (method == "first") {
    if (is.invalid(frac.of.max.slope)) {
      stop("Need to specify fraction of maximum slope.")
    }
    if (frac.of.max.slope <= 0 || frac.of.max.slope > 1) {
      stop("Fraction of maximum slope must be positive and be less than or equal to 1.")
    }
    # Find x where first derivative reaches cutoff
    slope.cutoff <- frac.of.max.slope * max(first.deriv)
    #cutoff.x <- approx(first.deriv, new.x, slope.cutoff)$y
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
    approx(new.x, zero.deriv, cutoff.x)
  }
}

