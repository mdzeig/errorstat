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

##' "Flatten" error objects
##'
##' Flatten nested error objects into data.
##'
##' The purpose of this function is to convert lists of error statistics into
##' a single data.frame, so that they can be easily plotted or summarized. The
##' result data.frame contains the various error computations stacked vertically
##' with one or more additional columns indicating each computation's place in
##' original list. The names of the addional columns are given by the \code{labels}
##' argument.
##'
##' @param x an errorcalc object of a list of errorcalc object
##' @param ... optionally, addition errorcalc objects
##' @param labels optional column labels for the resulting data.frame
##' @return a \code{data.frame}
##' @export
flatten <- function(x, ..., labels = NULL) {

  stopifnot(is.character(labels) || is.null(labels))
  flat <- flatten_base(form_list(x, ...),
                       if (is.null(labels)) 1L else labels)
  rownames(flat) <- NULL
  flat
}

##' @describeIn flatten Convert an errorcalc object to a data.frame
##' @export
as.data.frame.errorcalc <- function(x, ...) {

  class(x) <- class(x)[-1]
  x
}

## flattening workhorse
flatten_base <- function(x, labels) {

  if (inherits(x, "errorcalc"))
    return(as.data.frame(x))

  xdfs <- lapply(x, flatten_base,
                 if (is.character(labels)) labels[-1]
                 else labels + 1)
  xdfs <- lapply(x, flatten_base, labels[-1])
  xflat <- data.frame(field = rep(if (is.null(names(x))) seq_along(x)
                                  else names(x),
                                  sapply(xdfs, nrow)),
                      do.call(rbind, xdfs))
  names(xflat)[1] <- if (is.character(labels)) labels[1]
                     else paste0("Var", labels)
  xflat
}

## create an errorcalc object
errorcalc <- function (n, lwr, upr, statlist) {

  x <- data.frame(n = n,
                  ival = interval_factor(lwr, upr),
                  statlist)
  class(x) <- c("errorcalc", class(x))
  x
}

interval_factor <- function(lwr, upr) {

  lwrfac <- ordered(lwr)
  ordered(
    lwrfac,
    levels(lwrfac),
    gsub("Inf]", "Inf)",
         sprintf("(%s, %s]",
                 levels(lwrfac),
                 levels(ordered(upr))))
  )
}
