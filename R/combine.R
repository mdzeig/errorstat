## Part of the errorstat package for computing error statistics
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
##' @title Aggregate a list of errors
##' @param x an error object or a list of error objects
##' @param max_depth depth to check for an error object
##' @return A single error object or a list of error objects
##' @author Matthew Zeigenfuse
##' @export
aggregate_errors <- function (x, ...) {
  
  y <- list(...)
  Reduce(combine_errors, if (!length(y)) x else c(list(x), y))
}

## combine two error objects or two lists of error objects
combine_errors <- function (x, y) {
  
  if (is.null(x) || is.null(y))
    stop("x and/or y argument invalid")
  else if (inherits(x, "errorcalc") && inherits(y, "errorcalc")) {
    stopifnot(compatible(x, y))
    statnms <- get_stat_names(x)
    n <- x$n + y$n
    errorcalc(n, x$lwr, x$upr,
              mapply(combine_terms, x[statnms], y[statnms],
                     MoreArgs = list(xn = x$n, yn = y$n, dn = n),
                     SIMPLIFY = FALSE))
  } else if (is.list(x) && is.list(y))
    Map(combine_errors, x, y)
  else
    stop("x and/or y argument invalid")
}

## dn = denominator n (= xn + yn)
combine_terms <- function (xv, yv, xn, yn, dn = xn + yn) {
  
  z <- rep(NA, length(dn))
  z[xn == 0 & yn > 0] <- yv[xn == 0 & yn > 0]
  z[xn > 0 & yn == 0] <- xv[xn > 0 & yn == 0]
  ii <- xn > 0 & yn > 0
  z[ii] <- error_term(xv[ii], xn[ii], dn[ii]) + 
    error_term(yv[ii], yn[ii], dn[ii])
  z
}

error_term <- function (v, nn, dn)
  sign(v) * exp(log(nn) - log(dn) + log(abs(v)))

# are two errorcalc objects compatible?
compatible <- function (x, y) {
  
  inherits(x, "errorcalc") && 
    inherits(y, "errorcalc") &&
    nrow(x) == nrow(y) && 
    all(x$lwr == y$lwr) &&
    all(x$upr == y$upr) &&
    setequal(names(x), names(y))
}

get_stat_names <- function(x)
  names(x)[ !(names(x) %in% c("n", "lwr", "upr")) ]



