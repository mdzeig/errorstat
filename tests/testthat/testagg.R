context("Combining errors")

binned_truth <- list(runif(10, -5, -2), runif(20, -2, -1), runif(50, -1, 0),
                     runif(50, 0, 1), runif(20, 1, 2), runif(10, 2, 5))
binned_estimate <- Map("+", binned_truth, -2.5:2.5)
truth <- do.call(c, binned_truth)
estimate <- do.call(c, binned_estimate)

check_valid <- function(x, n = lengths(binned_truth),
                        lwr = c(-Inf, -2:2), upr = c(-2:2, Inf),
                        mse = (-2.5:2.5)^2, bias = -2.5:2.5) {
  
  expect_is(x, "errorcalc")
  expect_named(x, c("n", "lwr", "upr", "mse", "bias"))
  expect_equal(x$n, n)
  expect_equal(x$lwr, lwr)
  expect_equal(x$upr, upr)
  expect_equal(x$mse, mse)
  expect_equal(x$bias, bias)
}

nx2 <- 2*lengths(binned_truth)

test_that("errorcalc objects correctly combined", {

  x <- errorstat(truth, estimate)
  check_valid(aggregate_errors(list(x, x)), 320, -Inf, Inf, 1.5, 0)
  x <- errorstat(truth, estimate, -2:2)
  check_valid(aggregate_errors(list(x, x)), nx2)
})

x <- errorstat(truth, estimate, -2:2)

test_that("zero/NA handling", {
  
  y <- x
  y$n[1] <- 0
  y$mse[1] <- NA
  y$bias[1] <- NA
  z <- x
  z$n[6] <- 0
  z$mse[6] <- NA
  z$bias[6] <- NA
  n <- lengths(binned_truth)
  n[c(1, 6)] <- 2*n[c(1, 6)]
  n[-c(1, 6)] <- 3*n[-c(1, 6)]
  check_valid(aggregate_errors(list(x, y, z)), n)
})

x <- errorstat(truth, estimate, -2:2)
y <- list(list(a = x, b = x), list(a = x, b = x))

test_that("list handling", {
  
  z <- aggregate_errors(y)
  expect_named(z, c("a", "b"))
  check_valid(z$a, nx2)
  check_valid(z$b, nx2)
})

u <- list(u = list(a = x, b = x), v = list(a = x, b = x))

test_that("nested list handing", {
  
  z <- aggregate_errors(list(u, u))
  expect_named(z, c("u", "v"))
  expect_named(z$u, c("a", "b"))
  expect_named(z$v, c("a", "b"))
  check_valid(z$u$a, nx2)
  check_valid(z$u$b, nx2)
  check_valid(z$v$a, nx2)
  check_valid(z$v$b, nx2)
})

test_that("... combine correctly", {
  
  nx3 <- 3*lengths(binned_truth)
  check_valid(aggregate_errors(x, x), nx2)
  check_valid(aggregate_errors(x, x, x), nx3)
  z <- aggregate_errors(u, u, u)
  expect_named(z, c("u", "v"))
  expect_named(z$u, c("a", "b"))
  expect_named(z$v, c("a", "b"))
  check_valid(z$u$a, nx3)
  check_valid(z$u$b, nx3)
  check_valid(z$v$a, nx3)
  check_valid(z$v$b, nx3)
})


