library(KneeArrower)

x <- seq(0, 5, 0.05)
y <- log(1+x)

# Tests whether the x values are close enough
# Can't get exact value because of numeric differentiation on discrete data
# (should be less than the difference between successive x)
close_enough <- function(list1, list2) {
  x.diff <- abs(list1$x - list2$x)
  x.diff <= 0.05
}

test_that("Inverse function works", {
  # Estimate the square roots of 0 and 3
  expect_lt(abs(findInverse(seq(0, 5, 0.1), (seq(0, 5, 0.1))^2, 0)), 2E-2)
  expect_lt(abs(findInverse(seq(0, 5, 0.1), (seq(0, 5, 0.1))^2, 3) - sqrt(3)), 2E-2)
})

test_that("First derivative method works", {
  expect_true(close_enough(findCutoff(x, y, method="first", 1), list(x=0, y=0)))
  expect_true(close_enough(findCutoff(x, y, method="first", 0.5), list(x=1, y=log(2))))
})

#test_that("Curvature method works", {
  #expect_true(close_enough(findCutoff(x, y, method="curvature"), list(x=0, y=0)))
#})

x <- seq(-0.7, 3, 0.05)
y <- log(1+x)

test_that("Curvature method works on another part of curve", {
  expect_true(close_enough(findCutoff(x, y, method="curvature"), list(x=1/sqrt(2)-1, y=log(1/sqrt(2)))))
})

x <- seq(0, 5, 0.05)
y <- 1/(1+x)
x.cutoff <- sqrt(2) - 1
y.cutoff <- 1/sqrt(2)

test_that("Can find cutoffs on flipped versions of curves", {
  expect_true(close_enough(findCutoff(x, y), list(x=x.cutoff, y=y.cutoff)))
  expect_true(close_enough(findCutoff(-x, y), list(x=-x.cutoff, y=y.cutoff)))
  expect_true(close_enough(findCutoff(x, -y), list(x=x.cutoff, y=-y.cutoff)))
  expect_true(close_enough(findCutoff(-x, -y), list(x=-x.cutoff, y=-y.cutoff)))
})


