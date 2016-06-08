The `errorstat` Package
================
Matthew D. Zeigenfuse
June 8, 2016

Overview
--------

The `errorstat` package provides convenient routines for computing mean-squared error (MSE), bias and other statistics. These other statstics can be provided by the user as two-argument functions, where the first argument is the true parameter values and the second is the estimated parameter values. It provides two functions: `errorstat` and `aggregate_errors`.

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
    ##    n  lwr upr       mse      bias
    ## 1  1 -Inf  -2 0.1486522 0.3855545
    ## 2  8   -2  -1 0.2855503 0.4238816
    ## 3 36   -1   0 0.6627515 0.4969039
    ## 4 34    0   1 0.5693715 0.5579290
    ## 5 19    1   2 1.7835212 0.8348691
    ## 6  2    2 Inf 1.5904620 1.2280329
    ## 
    ## $difflty
    ##   n  lwr upr       mse      bias
    ## 1 0 -Inf  -2        NA        NA
    ## 2 6   -2  -1 0.2822356 0.4324382
    ## 3 8   -1   0 0.2718680 0.4715784
    ## 4 5    0   1 0.1259115 0.2668605
    ## 5 1    1   2 0.2244335 0.4737442
    ## 6 0    2 Inf        NA        NA

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
    ##    n  lwr upr       mse      bias
    ## 1  1 -Inf  -2 0.1486522 0.3855545
    ## 2  8   -2  -1 0.2855503 0.4238816
    ## 3 36   -1   0 0.6627515 0.4969039
    ## 4 34    0   1 0.5693715 0.5579290
    ## 5 19    1   2 1.7835212 0.8348691
    ## 6  2    2 Inf 1.5904620 1.2280329
    ## 
    ## $difflty
    ## $difflty$first10
    ##   n  lwr upr       mse      bias
    ## 1 0 -Inf  -2        NA        NA
    ## 2 3   -2  -1 0.4824749 0.5942681
    ## 3 3   -1   0 0.2249083 0.4260898
    ## 4 3    0   1 0.1697268 0.2845410
    ## 5 1    1   2 0.2244335 0.4737442
    ## 6 0    2 Inf        NA        NA
    ## 
    ## $difflty$last10
    ##   n  lwr upr        mse      bias
    ## 1 0 -Inf  -2         NA        NA
    ## 2 3   -2  -1 0.08199619 0.2706082
    ## 3 5   -1   0 0.30004382 0.4988716
    ## 4 2    0   1 0.06018863 0.2403398
    ## 5 0    1   2         NA        NA
    ## 6 0    2 Inf         NA        NA

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
    ##     n  lwr upr       mse        bias
    ## 1   9 -Inf  -2 0.7548158 -0.24692693
    ## 2  44   -2  -1 0.2084815 -0.01406018
    ## 3  98   -1   0 0.2947903  0.08918933
    ## 4 104    0   1 0.2376677  0.12948329
    ## 5  39    1   2 0.6157623  0.25775407
    ## 6   6    2 Inf 0.3449253 -0.06516213
    ## 
    ## $difflty
    ##    n  lwr upr        mse       bias
    ## 1  0 -Inf  -2         NA         NA
    ## 2  9   -2  -1 0.03883069 0.07828052
    ## 3 25   -1   0 0.06939020 0.06623807
    ## 4 18    0   1 0.08756423 0.15188967
    ## 5  8    1   2 0.04021369 0.12884639
    ## 6  0    2 Inf         NA         NA

The `aggregate_errors` function can also aggregate across more complicated list structures. For example, we could separate the MSE and bias of items 1-10 from 11-20 as follows.

``` r
split_difflty <- function(x) {
  list(ability = x$ability,
       difflty = list(first10 = x$difflty[1:10],
                      last10 = x$difflty[11:20]))
}
aggregate_errors(
  errorstat(lapply(truth, split_difflty),
            lapply(ests, split_difflty),
            -2:2)
)
```

    ## $ability
    ##     n  lwr upr       mse        bias
    ## 1   9 -Inf  -2 0.7548158 -0.24692693
    ## 2  44   -2  -1 0.2084815 -0.01406018
    ## 3  98   -1   0 0.2947903  0.08918933
    ## 4 104    0   1 0.2376677  0.12948329
    ## 5  39    1   2 0.6157623  0.25775407
    ## 6   6    2 Inf 0.3449253 -0.06516213
    ## 
    ## $difflty
    ## $difflty$first10
    ##    n  lwr upr        mse        bias
    ## 1  0 -Inf  -2         NA          NA
    ## 2  7   -2  -1 0.04645690 0.131777002
    ## 3 11   -1   0 0.08832693 0.002452345
    ## 4  9    0   1 0.03716130 0.067322572
    ## 5  3    1   2 0.01693161 0.129159426
    ## 6  0    2 Inf         NA          NA
    ## 
    ## $difflty$last10
    ##    n  lwr upr        mse       bias
    ## 1  0 -Inf  -2         NA         NA
    ## 2  2   -2  -1 0.01213898 -0.1089571
    ## 3 14   -1   0 0.05451134  0.1163554
    ## 4  9    0   1 0.13796716  0.2364568
    ## 5  5    1   2 0.05418293  0.1286586
    ## 6  0    2 Inf         NA         NA

The `flatten` function
======================

TODO
