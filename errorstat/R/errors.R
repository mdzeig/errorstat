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

##' Create a data.frame
##'
##' \code{as.data.frame} converts an error or error_list object to a data.frame
##'
##' For error_list objects, \code{as.data.frame} will be called for each list
##' element and the resulting \code{data.frame}s will be concatentated row-wise.
##' An addition column labelling the elements will be appended.
##'
##' @param x an error or error_list object
##' @param labels labels to use instead of \code{names(x)}
##' @param label_name name of the label column
##' @param ... additional arguments to \code{data.frame}
##' @return a \code{data.frame}
##' @name data.frame-methods
NULL

##' @rdname data.frame-methods
##' @export
as.data.frame.error <- function(x, ...) {

    class(x) <- class(x)[class(x) != "error"]
    x
}

##' @rdname data.frame-methods
##' @export
as.data.frame.error_list <- function(x, labels = NULL, label_name = "label",
                                     ...) {

    dfs <- lapply(x, as.data.frame)
    y <- data.frame(rep(if (is.null(labels)) names(x) else labels,
                        sapply(dfs, nrow)),
                    do.call(rbind, dfs), ...)
    names(y)[1] <- label_name
    rownames(y) <- NULL
    y
}

## create an error object
error <- function (n, ...)
    as_error(data.frame(n = n, list(...)))

## convert to an error object
as_error <- function (x, ...) UseMethod("as_error")

as_error.error <- function (x, ...) x

as_error.data.frame <- function (x, ...) {

    if (!("n" %in% names(x)))
        stop("cannot convert to error")
    class(x) <- c("error", class(x))
    x
}

as_error.default <- function (x, ...)
    as_error(as.data.frame(x, ...))

## convert to an error_list object
as_error_list <- function (x, ...) UseMethod("as_error_list")

as_error_list.error_list <- function (x, ...) x

as_error_list.default <- function (x, ...) {

    y <- lapply(x, as_error)
    class(y)  <- c("error_list", class(y))
    y
}

## Local Variables:
## ess-r-package-info: ("errorstat" . "~/Projects/errorstat/errorstat")
## End:
