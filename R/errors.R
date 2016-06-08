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

##' Create or bind error objects
##'
##' \code{as.data.frame} converts an error object to a data.frame and \code{rbind}
##' vertically concatenates two error objects
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
as.data.frame.errorcalc <- function(x, ...) {

  class(x) <- class(x)[-1]
  x
}

##' @rdname data.frame-methods
##' @export
rbind.errorcalc <- function (...) {
  
  x <- list(...)
  if (is.null(names(x)))
    names(x) <- paste0("Var", length(x))
  data.frame(label = rep(names(x), lengths(x)),
             do.call(rbind, x))
}
 
## create an errorcalc object
errorcalc <- function (n, lwr, upr, statlist) {
  
  x <- data.frame(n = n, lwr = lwr, upr = upr, statlist)
  class(x) <- c("errorcalc", class(x))
  x
}


