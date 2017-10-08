library(KneeArrower)

x <- seq(0, 5, 0.05)
y <- log(1+x)

# Tests whether the x values are close enough
# Can't get exact value because of numeric differentiation on discrete data
# (should be 2 times the difference between successive x)
close_enough <- function(list1, list2) {
  x.diff <- abs(list1$x - list2$x)
  x.diff <= 0.1
}

test_that("First derivative method works", {
  expect_equal(close_enough(findCutoff(x, y, method="first", 1), list(x=0, y=0)), TRUE)
  expect_equal(close_enough(findCutoff(x, y, method="first", 0.5), list(x=1, y=log(2))), TRUE)
})

test_that("Curvature method works", {
  expect_equal(close_enough(findCutoff(x, y, method="curvature"), list(x=0, y=0)), TRUE)
})


x <- seq(-0.7, 3, 0.05)
y <- log(1+x)

test_that("Curvature method works on another part of curve", {
  expect_equal(close_enough(findCutoff(x, y, method="curvature"), list(x=1/sqrt(2)-1, y=log(1/sqrt(2)))), TRUE)
})

