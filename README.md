The `errorstat` Package
================
Matthew D. Zeigenfuse
June 8, 2016

Overview
--------

The `errorstat` package provides convenient routines for computing mean-squared error (MSE), bias and other statistics. These other statstics can be provided by the user as two-argument functions, where the first argument is the true parameter values and the second is the estimated parameter values. It provides three functions -- `errorstat`, `aggregate_errors` and `flatten` -- which together provide a workflow for computing estimation error for individual simulations and aggregating across a study.

The `errorstat` function
========================

The `errorstat` function computes the desired error statistics. Its first two arguments, `truth` and `estimate`, correspond to the true and estimated values. Its third argument, `uppers` allows the user to specify a partition via the upper bounds of each constituent intervals (except \(\infty\), which is included by default). When `truth` and `estimate` are vectors, `errorstat` computes each error statistic for each partition element. For example, if `uppers = -2:2`, then `errorstat` will compute the desired error statistics for \((-\infty, -2]\), \((-2, -1]\), \((-1, 0]\), \((0, 1]\), \((1, 2]\) and \((2, \infty)\).

The arguments `truth` and `estimate` can also lists of vectors of the same size. The length of each of these lists must be the same as well as the lengths of the corresponding vector elements. Lists of depth greater than one will be evaluated recursively, resulting in a list whose structure is identical to that of `truth` and `estimate` and whose leaves are the result of applying `errorstat` to each corresponding pair of leaves in `truth` and `estimate`. This is useful for computing error statistics for multiple variables.

The following example illustrates how `errorstat` might be applied using the Rasch model. The Rasch model is a two-parameter model of test-taker performance. The first parameter \(\theta\) is a length \(P\) vector comprised of the abilities of the \(P\) test takers, and the second parameter \(\beta\) is a length \(I\) vector comprised of the difficulties of the \(I\) test items. It models the probability that test taker \(p\) correctly responds to item \(i\) as the inverse logit of \(\theta_p - \beta_i\).

``` r
library(errorstat)
library(eRm) # sim.rasch, RM, person.parameter functions
ability <- rnorm(100)
difflty <- rnorm(20)
resp <- sim.rasch(ability, difflty)
rmfit <- RM(resp)
ppfit <- person.parameter(rmfit)
errorstat(list(ability = ability, difflty = difflty),
          list(ability = coef(ppfit), difflty = -coef(rmfit)),
          -2:2)
```

    ## $ability
    ##    n       ival       mse      bias
    ## 1  3 (-Inf, -2] 0.1256102 0.2057861
    ## 2 18   (-2, -1] 0.7663484 0.1731269
    ## 3 31    (-1, 0] 0.4515078 0.3178738
    ## 4 29     (0, 1] 0.4232712 0.2741479
    ## 5 18     (1, 2] 0.8833904 0.3979069
    ## 6  1   (2, Inf) 0.8181884 0.9045377
    ## 
    ## $difflty
    ##   n       ival        mse       bias
    ## 1 2 (-Inf, -2] 0.04072188 0.18341970
    ## 2 1   (-2, -1] 0.01931849 0.13899097
    ## 3 8    (-1, 0] 0.09237183 0.14080046
    ## 4 5     (0, 1] 0.13451456 0.28661669
    ## 5 4     (1, 2] 0.04296765 0.09962288
    ## 6 0   (2, Inf)         NA         NA

For a more complicated example, suppose we would like to compute error statistics separately for items 1-10 and 11-20. With `errorstat`, this can be accomplished by

``` r
errorstat(list(ability = ability, 
              difflty = list(first10 = difflty[1:10], 
                             last10 = difflty[11:20])),
          list(ability = coef(ppfit), 
               difflty = list(first10 = -coef(rmfit)[1:10],
                              last10 = -coef(rmfit)[11:20])),
          -2:2)
```

    ## $ability
    ##    n       ival       mse      bias
    ## 1  3 (-Inf, -2] 0.1256102 0.2057861
    ## 2 18   (-2, -1] 0.7663484 0.1731269
    ## 3 31    (-1, 0] 0.4515078 0.3178738
    ## 4 29     (0, 1] 0.4232712 0.2741479
    ## 5 18     (1, 2] 0.8833904 0.3979069
    ## 6  1   (2, Inf) 0.8181884 0.9045377
    ## 
    ## $difflty
    ## $difflty$first10
    ##   n       ival        mse       bias
    ## 1 2 (-Inf, -2] 0.04072188 0.18341970
    ## 2 0   (-2, -1]         NA         NA
    ## 3 5    (-1, 0] 0.09525447 0.05487687
    ## 4 2     (0, 1] 0.25227195 0.41671079
    ## 5 1     (1, 2] 0.02921399 0.17092100
    ## 6 0   (2, Inf)         NA         NA
    ## 
    ## $difflty$last10
    ##   n       ival        mse       bias
    ## 1 0 (-Inf, -2]         NA         NA
    ## 2 1   (-2, -1] 0.01931849 0.13899097
    ## 3 3    (-1, 0] 0.08756744 0.28400645
    ## 4 3     (0, 1] 0.05600964 0.19988729
    ## 5 3     (1, 2] 0.04755221 0.07585684
    ## 6 0   (2, Inf)         NA         NA

The `aggregate_errors` function
===============================

The `aggregate_errors` function is used to combine error statistics across multiple simulations. Each pair of error statistics is combined as follows. Let \(T_1\) and \(T_2\) be error statistics corresponding to two different simulations and let \(n_1\) and \(n_2\) be their corresponding sample sizes. Then, the combined statistic is

\[ T_{1 + 2} = \frac{n_1}{n_1 + n_2} T_1 + \frac{n_2}{n_1 + n_2} T_2. \]

For \(m\) simulations \(T_1, \ldots, T_m\), the corresponding statistic will be

\[ T_{1 + \ldots + m} = \frac{n_1}{n_1 + \ldots + n_m} T_1 + \ldots + 
\frac{n_1}{n_1 + \ldots + n_m} T_m. \]

When \(T_1, \ldots, T_m\) can be interpretted as the sample mean of some quantity -- as is the case with MSE and bias -- then \(T_{1 + \ldots + m}\) is equivalent to the value of the statistic that would have been computed for the whole sample.

As an example, we simulate and fit three different data sets and compute the combined MSE and bias in each of the regions \((-\infty, -2], \ldots, (2, \infty)\).

``` r
# simulate 3 datasets
truth <- replicate(3, simplify = FALSE, {
  ability <- rnorm(100)
  difflty <- rnorm(20)
  list(ability = ability, 
       difflty = difflty,
       resp = sim.rasch(ability, difflty))
})
# fit each data set
ests <- lapply(truth, 
               function(x) {
                 rmfit <- RM(x$resp)
                 ppfit <- person.parameter(rmfit)
                 list(ability = coef(ppfit),
                      difflty = -coef(rmfit))
               })
# compute & aggregate the statistics 
aggregate_errors(
  errorstat(lapply(truth, "[", -3), ests, -2:2)
)
```

    ## $ability
    ##     n       ival       mse         bias
    ## 1   8 (-Inf, -2] 0.4163801 -0.288407799
    ## 2  45   (-2, -1] 0.6031102 -0.178171991
    ## 3 112    (-1, 0] 0.3410118  0.004088996
    ## 4  82     (0, 1] 0.3743609 -0.004022767
    ## 5  45     (1, 2] 0.6799557  0.260647294
    ## 6   8   (2, Inf) 1.0269986  0.194179149
    ## 
    ## $difflty
    ##    n       ival         mse        bias
    ## 1  1 (-Inf, -2] 0.002588679 -0.05087906
    ## 2  9   (-2, -1] 0.098306323 -0.02457959
    ## 3 24    (-1, 0] 0.133257104  0.01689882
    ## 4 14     (0, 1] 0.103851326  0.14618287
    ## 5 11     (1, 2] 0.048150936 -0.10728406
    ## 6  1   (2, Inf) 0.342083128 -0.58487873

The `aggregate_errors` function can also aggregate across more complicated list structures. For example, we could separate the MSE and bias of items 1-10 from 11-20 as follows.

``` r
split_difflty <- function(x) {
  list(ability = x$ability,
       difflty = list(first10 = x$difflty[1:10],
                      last10 = x$difflty[11:20]))
}
(split_error <- aggregate_errors(
  errorstat(lapply(truth, split_difflty),
            lapply(ests, split_difflty),
            -2:2)
))
```

    ## $ability
    ##     n       ival       mse         bias
    ## 1   8 (-Inf, -2] 0.4163801 -0.288407799
    ## 2  45   (-2, -1] 0.6031102 -0.178171991
    ## 3 112    (-1, 0] 0.3410118  0.004088996
    ## 4  82     (0, 1] 0.3743609 -0.004022767
    ## 5  45     (1, 2] 0.6799557  0.260647294
    ## 6   8   (2, Inf) 1.0269986  0.194179149
    ## 
    ## $difflty
    ## $difflty$first10
    ##    n       ival        mse        bias
    ## 1  0 (-Inf, -2]         NA          NA
    ## 2  5   (-2, -1] 0.03622968  0.10823357
    ## 3 14    (-1, 0] 0.13122531 -0.04688822
    ## 4  5     (0, 1] 0.08323396  0.06619882
    ## 5  5     (1, 2] 0.06566561 -0.04960033
    ## 6  1   (2, Inf) 0.34208313 -0.58487873
    ## 
    ## $difflty$last10
    ##    n       ival         mse        bias
    ## 1  1 (-Inf, -2] 0.002588679 -0.05087906
    ## 2  4   (-2, -1] 0.175902132 -0.19059605
    ## 3 10    (-1, 0] 0.136101613  0.10620067
    ## 4  9     (0, 1] 0.115305418  0.19061845
    ## 5  6     (1, 2] 0.033555375 -0.15535384
    ## 6  0   (2, Inf)          NA          NA

The `flatten` function
======================

The `flatten` function creates a `data.frame` from a list of errors. The resulting `data.frame` will contain the error statistics of each list element concatenated vertically and one or more columns of labels. The names of these columns can be provided by the user via the `labels` argument. The value associated with each element is determined either by the `names` attribute or list index.

Suppose for example that we would like to combine and plot MSE statistics in `split_error`. We can achieve this using `ggplot2` as follows.

``` r
(flat_errors <- flatten(split_error$difflty, labels = "items"))
```

    ##      items  n       ival         mse        bias
    ## 1  first10  0 (-Inf, -2]          NA          NA
    ## 2  first10  5   (-2, -1] 0.036229675  0.10823357
    ## 3  first10 14    (-1, 0] 0.131225311 -0.04688822
    ## 4  first10  5     (0, 1] 0.083233959  0.06619882
    ## 5  first10  5     (1, 2] 0.065665609 -0.04960033
    ## 6  first10  1   (2, Inf) 0.342083128 -0.58487873
    ## 7   last10  1 (-Inf, -2] 0.002588679 -0.05087906
    ## 8   last10  4   (-2, -1] 0.175902132 -0.19059605
    ## 9   last10 10    (-1, 0] 0.136101613  0.10620067
    ## 10  last10  9     (0, 1] 0.115305418  0.19061845
    ## 11  last10  6     (1, 2] 0.033555375 -0.15535384
    ## 12  last10  0   (2, Inf)          NA          NA

``` r
library(ggplot2)
ggplot(flat_errors, aes(ival, mse, fill = items)) + 
  geom_bar(stat = "identity", position = "dodge")
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)
