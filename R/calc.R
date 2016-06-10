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

##' Compute one or more error summaries
##'
##' \code{errorstat} computes one or more error summaries (e.g., MSE, bias)
##' in one or more bins defined by \code{uppers}
##'
##' \code{errorstat} is primarily a utility for determine how MSE and bias
##' change with the level of a true parameter. It allows the user to partition
##' the range true values into disjoint intervals and compute the error
##' summaries for each of these intervals.
##'
##' Optionally, the user can define additional error summaries through the
##' dots argument. Each summary should be a function of two variables. The
##' first of these variables is the true parameter value, and the second is
##' the estimated parameter value.
##'
##' @param truth true values of the parameter
##' @param estimate estimated values of the parameter
##' @param uppers upper bounds of each interval
##' @param mse should the MSE be computed?
##' @param bias should the bias be computed?
##' @param ... additional error summaries.
##' @return A data.frame containing the number of elements in each interval
##' and the values of each error summary.
##' @author Matthew Zeigenfuse
##' @examples
##' \dontrun{
##' library(eRm)
##' truth <- list(trait = rnorm(100), thresh = rnorm(20))
##' resp <- evalq(sim.rasch(trait, thresh), truth)
##' ermfit <- RM(resp)
##' ppfit <- person.parameter(ermfit)
##' errorstat(truth, list(trait = coef(ppfit), thresh = -coef(ermfit)), -2:2)
##' }
##' @export
errorstat <- function (truth, estimate, uppers = NULL,
                       mse = TRUE, bias = TRUE, ...) {

  calc_stats(truth, estimate, get_stats(mse, bias, ...), uppers)
}

## get functions for computing error statistics
get_stats <- function (mse, bias, ...) {

  stats <- Filter(Negate(is.null),
                  c(list(mse = if (mse) calc_mse,
                         bias = if (bias) calc_bias),
                    list(...)))
  if (!length(stats))
    stop("no statistics requested")
  stats
}

## sort upper bounds if needed
handle_uppers <- function (uppers) {

  if (is.unsorted (uppers)) {
    warning ("upper unsorted. sorting")
    sort(uppers)
  } else
    uppers
}

## route
calc_stats <- function (truth, estimate, stats, uppers) {

  if (is.list(truth))   #repeatedly apply for lists
    mapply(calc_stats, truth, estimate,
           if (is.list(uppers))
             uppers
           else
             replicate(length(truth),
                       uppers,
                       simplify = FALSE),
           MoreArgs = list(stats = stats),
           SIMPLIFY = FALSE)
  else if (is.null(uppers))
    calc_stats_whole(truth, estimate, stats)
  else
    calc_stats_split(truth, estimate, stats, uppers)
}

## compute when uppers not given
calc_stats_whole <- function(truth, estimate, stats) {

  errorcalc(length(truth), -Inf, Inf,
            lapply(stats, do.call, list(truth, estimate)))
}

## compute when uppers given
calc_stats_split <- function (truth, estimate, stats, uppers) {

  inc_order <- order (truth)
  truth <- truth[inc_order]
  last <- findInterval(uppers, truth)
  n <- diff(c(0, last, length(truth)))
  lastnz <- cumsum(n[n > 0])
  inds <- Map(seq, c(1, 1+front(lastnz)), lastnz)
  errorcalc(n, c(-Inf, uppers), c(uppers, Inf),
            lapply(lapply(stats, map_stat,
                          split_by_indices(truth, inds),
                          split_by_indices(estimate[inc_order], inds)),
                   zeros_to_NA, n))
}

## wrapper to mapply to avoid do.call
map_stat <- function (stat, truth, est) mapply(stat, truth, est)

zeros_to_NA <- function (v, n) {

  y <- rep(NA, length(n))
  y[n > 0] <- v
  y
}

## split a vector into list of chunks defined by indices
split_by_indices <- function (x, indices)
  lapply(indices, get_indices_from_x, x)

get_indices_from_x <- function (indices, x) x[indices]

## predefined statistics
calc_mse <- function (truth, estimate) mean((truth - estimate)^2)
calc_bias <- function (truth, estimate) mean(estimate - truth)

## utility for getting last element of a vector
back <- function (x) x[length(x)]
front <- function (x) x[-length(x)]
