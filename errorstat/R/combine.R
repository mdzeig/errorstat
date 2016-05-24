## Part of the rstanarm package for estimating model parameters
## Copyright (C) 2016 Matthew D. Zeigenfuse
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License
## as published by the Free Software Foundation; either version 3
## of the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

##' \code{aggregate_errors} combines two or more error objects
##'
##' When only a single argument is provided, it should be a list
##' of error objects. When multiple arguments are provided, they
##' will be combined into a list using \code{list(x, ...)}.
##'
##' In the simplest case where \code{x} is a list of error objects,
##' the function will return the error statistics of the combined
##' sample, which can be done by appropriately weighting the error
##' statistics themselves.
##'
##' When additional objects are provided through the dots argument,
##' they will be combined with x through \code{list(x, ...)}.
##'
##' @title Aggregate a list of errors
##' @param x an error object or a list of error objects
##' @param ... additional objects (see details)
##' @return A single error object or a list of error objects
##' @author Matthew Zeigenfuse
##' @export
aggregate_errors <- function (x, ...)
    Reduce(combine_errors, list(x, ...))

## combine two error objects or two lists of error objects
combine_errors <- function (x, y) UseMethod("combine_errors")

## try to convert x to an error_list, then an error, then bail out.
combine_errors.default <- function (x, y) {

    tryCatch(xel <- as_error_list(x), error = function(e) NULL)
    if (is.null(xel))
        tryCatch(xel <- as_error(x),
                 error = function(e) stop("x argument invalid"))
    combine_errors(x, y)
}

## combine error lists
combine_errors.error_list <- function (x, y)
    as_error_list(Map(combine_errors, x, y))

## base case where x and y are both error objects
combine_errors.error <- function (x, y) {

    tryCatch(y <- as_error(y),
             error = function(e) stop("y argument invalid"))

    n <- x$n + y$n
    error(n = n,
          mapply(combine_terms,
                 x[names(x) != "n"], y[names(y) != "n"],
                 MoreArgs = list(xn = x$n, yn = y$n, dn = n),
                 SIMPLIFY = FALSE))
}

## dn = denominator n (= xn + yn)
combine_terms <- function (xv, yv, xn, yn, dn = xn + yn)
    error_term(xv, xn, dn) + error_term(yv, yn, dn)

error_term <- function (v, nn, dn)
    sign(v) * exp(log(nn) - log(dn) + log(abs(v)))

## Local Variables:
## ess-r-package-info: ("errorstat" . "~/Projects/errorstat/errorstat")
## End:
