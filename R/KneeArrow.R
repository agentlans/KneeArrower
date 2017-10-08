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
#' @importFrom stats approx cor
#' @export
findCutoff <- function(x, y, method="first", frac.of.max.slope=0.5) {
  if (min(length(x), length(y)) < 5) {
    stop("Should have at least 5 points for a knee curve.")
  }
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

  # Use Savitzky-Golay filter to get derivatives
  smoothen <- function(y, ...) {
    # Set filter length to be fraction of length of data
    # (must be an odd number)
    filt.length <- ceiling(length(new.x) / 3) -
      (ceiling(length(new.x) / 3) %% 2 + 1)
    # Time scaling factor so that the derivatives are on same scale as original data
    ts <- (max(new.x) - min(new.x)) / length(new.x)
    #filt.length <- 5
    signal::sgolayfilt(y, p=4, n=filt.length, ts=ts, ...)
  }

  # Calculate first and second derivatives
  zero.deriv <- smoothen(new.y, m=0)
  first.deriv <- smoothen(new.y, m=1)
  second.deriv <- smoothen(new.y, m=2)

  # Check that first derivative is decreasing
  if (cor(first.deriv, 1:length(first.deriv), method="kendall") >= 0) {
    stop("First derivative should be decreasing for a knee curve.")
  }
  # Check that second derivative is increasing
  if (cor(second.deriv, 1:length(second.deriv), method="kendall") < 0) {
    stop("Second derivative should be increasing for a knee curve.")
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
    cutoff.x <- approx(first.deriv, new.x, slope.cutoff)$y
  } else if (method == "curvature") {
    # Find x where curvature is maximum
    curvature <- abs(second.deriv) / (1 + first.deriv^2)^(3/2)
    cutoff.x <- approx(curvature, new.x, max(curvature))$y
  } else {
    stop("Method must be either 'first' or 'curvature'.")
  }
  if (cutoff.x > max(x)) {
    warning("Cutoff point is outside range of x. y value will be returned as NA.")
  }
  # Return cutoff point on curve
  approx(new.x, zero.deriv, cutoff.x)
}

