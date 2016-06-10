context("Computing errors")

binned_truth <- list(runif(10, -5, -2), runif(20, -2, -1), runif(50, -1, 0),
                     runif(50, 0, 1), runif(20, 1, 2), runif(10, 2, 5))
binned_estimate <- Map("+", binned_truth, -2.5:2.5)
truth <- do.call(c, binned_truth)
estimate <- do.call(c, binned_estimate)

check_errorstat_result <- function(x, n, mse, bias,
                                   lwr = c(-Inf, -2, -1, 0, 1, 2), 
                                   upr = c(-2, -1, 0, 1, 2, Inf)) {
  
  expect_is(x, "errorcalc")
  expect_named(x, 
               c("n", "lwr", "upr", 
                 if (!is.null(mse)) "mse", 
                 if (!is.null(bias)) "bias"))
  expect_equal(x$n, n)
  expect_equal(x$lwr, lwr)
  expect_equal(x$upr, upr)
  if (!is.null(mse) )
    expect_equal(x$mse, mse)
  if (!is.null(bias))
    expect_equal(x$bias, bias)
}

test_that("Unsplit MSE/bias calculations work.", {
  
  check_errorstat_result(errorstat(truth, estimate),
                         160, 1.5, 0, -Inf, Inf)
})

test_that("Split MSE/bias calculations work.", {

  check_errorstat_result(errorstat(truth, estimate, -2:2),
                         c(10, 20, 50, 50, 20, 10),
                         (-2.5:2.5)^2, 
                         -2.5:2.5)
})

test_that("Bins with n=0 work.", {
  
  check_errorstat_result(errorstat(do.call(c, binned_truth[-c(1, 6)]),
                                   do.call(c, binned_estimate[-c(1, 6)]),
                                   -2:2),
                         c(0, 20, 50, 50, 20, 0),
                         c(NA, (-1.5:1.5)^2, NA),
                         c(NA, -1.5:1.5, NA))
  
  check_errorstat_result(errorstat(do.call(c, binned_truth[-3]),
                                   do.call(c, binned_estimate[-3]),
                                   -2:2),
                         c(10, 20, 0, 50, 20, 10),
                         c((-2.5)^2, (-1.5)^2, NA, (.5:2.5)^2),
                         c(-2.5, -1.5, NA, .5:2.5))
})

test_that("Nonstandard function sets.", {
  
  expect_error(errorstat(truth, estimate, -2:2, FALSE, FALSE))
  check_errorstat_result(errorstat(truth, estimate, -2:2, mse = FALSE),
                         c(10, 20, 50, 50, 20, 10),
                         NULL,
                         -2.5:2.5)
  check_errorstat_result(errorstat(truth, estimate, -2:2, bias = FALSE),
                         c(10, 20, 50, 50, 20, 10),
                         (-2.5:2.5)^2,
                         NULL)
})

