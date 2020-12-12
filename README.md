
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sfcr

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/joaomacalos/sfcr.svg?branch=master)](https://travis-ci.com/joaomacalos/sfcr)
<!-- badges: end -->

The goal of the `sfcr` package is to provide an intuitive and `tidy` way
to estimate stock-flow consistent (SFC) models with R.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("joaomacalos/sfcr")
```

## Example

This is a basic example which shows how to simulate the “SIM” model from
Godley and Lavoie (2007 ch. 3), as well as how to add scenarios to this
baseline model.

The `sfcr_set()` function is used to create define the equations and
external variables of the model.

These sets are used to simulate the baseline scenario of the model with
the `sfcr_baseline()` function:

``` r
library(sfcr)

eqs <- sfcr_set(
  TXs ~ TXd,
  YD ~ W * Ns - TXs,
  Cd ~ alpha1 * YD + alpha2 * Hh[-1],
  Hh ~ YD - Cd + Hh[-1],
  Ns ~ Nd,
  Nd ~ Y / W,
  Cs ~ Cd,
  Gs ~ Gd,
  Y ~ Cs + Gs,
  TXd ~ theta * W * Ns,
  Hs ~ Gd - TXd + Hs[-1]
)

external <- sfcr_set(
  Gd ~ 20, 
  W ~ 1,
  alpha1 ~ 0.6,
  alpha2 ~ 0.4,
  theta ~ 0.2
  )

sim <- sfcr_baseline(
  equations = eqs, 
  external = external,
  periods = 60, 
  
  )

sim
#> # A tibble: 60 x 17
#>    period      TXs       YD       Cd       Hh       Ns       Nd       Cs
#>  *  <int>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1      1 1.00e-15 1.00e-15 1.00e-15 1.00e-15 1.00e-15 1.00e-15 1.00e-15
#>  2      2 7.69e+ 0 3.08e+ 1 1.85e+ 1 1.23e+ 1 3.85e+ 1 3.85e+ 1 1.85e+ 1
#>  3      3 9.59e+ 0 3.83e+ 1 2.79e+ 1 2.27e+ 1 4.79e+ 1 4.79e+ 1 2.79e+ 1
#>  4      4 1.12e+ 1 4.48e+ 1 3.59e+ 1 3.15e+ 1 5.59e+ 1 5.59e+ 1 3.59e+ 1
#>  5      5 1.25e+ 1 5.02e+ 1 4.27e+ 1 3.90e+ 1 6.27e+ 1 6.27e+ 1 4.27e+ 1
#>  6      6 1.37e+ 1 5.48e+ 1 4.85e+ 1 4.53e+ 1 6.85e+ 1 6.85e+ 1 4.85e+ 1
#>  7      7 1.47e+ 1 5.86e+ 1 5.33e+ 1 5.06e+ 1 7.33e+ 1 7.33e+ 1 5.33e+ 1
#>  8      8 1.55e+ 1 6.19e+ 1 5.74e+ 1 5.52e+ 1 7.74e+ 1 7.74e+ 1 5.74e+ 1
#>  9      9 1.62e+ 1 6.47e+ 1 6.09e+ 1 5.90e+ 1 8.09e+ 1 8.09e+ 1 6.09e+ 1
#> 10     10 1.68e+ 1 6.71e+ 1 6.38e+ 1 6.22e+ 1 8.38e+ 1 8.38e+ 1 6.38e+ 1
#> # ... with 50 more rows, and 9 more variables: Gs <dbl>, Y <dbl>, TXd <dbl>,
#> #   Hs <dbl>, Gd <dbl>, W <dbl>, alpha1 <dbl>, alpha2 <dbl>, theta <dbl>
```

With the steady state values at hand, we can use the `sfcr_scenario()`
function to see what happens if we increase government expenditures `Gd`
from 20 to 30:

``` r
shock <- sfcr_shock(
  variables = sfcr_set(
    Gd ~ 30
  ),
  start = 5,
  end = 60
)

sim2 <- sfcr_scenario(
  baseline = sim,
  scenario = shock,
  periods = 60
  )

sim2
#> # A tibble: 60 x 17
#>    period   TXs    YD    Cd    Hh    Ns    Nd    Cs    Gs     Y   TXd    Hs
#>  *  <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1      1  20.0  80.0  80.0  80.0  100.  100.  80.0    20  100.  20.0  80.0
#>  2      2  20.0  80.0  80.0  80.0  100.  100.  80.0    20  100.  20.0  80.0
#>  3      3  20.0  80.0  80.0  80.0  100.  100.  80.0    20  100.  20.0  80.0
#>  4      4  20.0  80.0  80.0  80.0  100.  100.  80.0    20  100.  20.0  80.0
#>  5      5  23.8  95.4  89.2  86.2  119.  119.  89.2    30  119.  23.8  86.2
#>  6      6  24.8  99.2  94.0  91.4  124.  124.  94.0    30  124.  24.8  91.4
#>  7      7  25.6 102.   98.0  95.8  128.  128.  98.0    30  128.  25.6  95.8
#>  8      8  26.3 105.  101.   99.5  131.  131. 101.     30  131.  26.3  99.5
#>  9      9  26.8 107.  104.  103.   134.  134. 104.     30  134.  26.8 103. 
#> 10     10  27.3 109.  107.  105.   137.  137. 107.     30  137.  27.3 105. 
#> # ... with 50 more rows, and 5 more variables: Gd <dbl>, W <dbl>, alpha1 <dbl>,
#> #   alpha2 <dbl>, theta <dbl>
```

With `sfcr`, the models are written entirely within R and use the
standard R syntax. Furthermore, their output is a `tibble`, meaning that
it can be easily manipulated with `dplyr` and other `tidyverse` tools
and plotted with `ggplot2`.

### References

<div id="refs" class="references hanging-indent">

<div id="ref-godley2007monetary">

Godley, Wynne, and Marc Lavoie. 2007. *Monetary Economics: An Integrated
Approach to Credit, Money, Income, Production and Wealth*. Palgrave
Macmillan.

</div>

</div>
