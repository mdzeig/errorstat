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
##' @title Compute one or more error summaries
##' @param truth true values of the parameter
##' @param estimate estimated values of the parameter
##' @param uppers upper bounds of each interval
##' @param mse should the MSE be computed?
##' @param bias should the bias be computed?
##' @param ... additional error summaries.
##' @return A data.frame containing the number of elements in each interval
##' and the values of each error summary.
##' @author Matthew Zeigenfuse
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
calc_stats <- function (truth, estimate, stats, uppers)
    UseMethod("calc_stats")

calc_stats.numeric <- function (truth, estimate, stats, uppers) {

    if (is.null(uppers))
        calc_stats_whole(stats, truth, estimate)
    else
        calc_stats_split(stats, truth, estimate, uppers)
}

calc_stats.list <- function (truth, estimate, stats, uppers) {

    as_error_list(mapply(calc_stat, truth, estimate,
                         if (is.list(uppers))
                             uppers
                         else
                             replicate(length(truth),
                                       uppers,
                                       simplify = FALSE),
                         MoreArgs = list(stats = stats),
                         SIMPLIFY = FALSE))
}

## compute when uppers not given
calc_stats_whole <- function(truth, estimate, stats) {

    error(length(truth),
          lapply(stats, do.call, list(truth, estimate)))
}

## compute when uppers given
calc_stats_split <- function (truth, estimate, stats, uppers) {

    inc_order <- order (truth)
    truth <- truth[inc_order]
    last <- findInterval (uppers, truth)
    inds <- Map(seq, c(1, 1+last), c(last, length(truth)))
    error(lengths(inds),
          lapply(stats, map_stat,
                 split_by_indices(truth, inds),
                 split_by_indices(estimate[inc_order], inds)))
}

## wrapper to mapply to avoid do.call
map_stat <- function (stat, truth, est) mapply(stat, truth, est)

## split a vector into list of chunks defined by indices
split_by_indices <- function (x, indices)
    lapply(indices, get_indices_from_x, x)

get_indices_from_x <- function (indices, x) x[indices]

## predefined statistics
calc_mse <- function (truth, estimate) mean((truth - estimate)^2)
calc_bias <- function (truth, estimate) mean(estimate - truth)

## utility for getting last element of a vector
back <- function (x) x[length(x)]
