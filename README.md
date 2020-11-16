
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sfcr

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/joaomacalos/sfcr.svg?branch=master)](https://travis-ci.com/joaomacalos/sfcr)
<!-- badges: end -->

The goal of `sfcr` is to provide an intuitive and `tidy` way to estimate
stock-flow consistent (SFC) models with R.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("joaomacalos/sfcr")
```

## Example

This is a basic example which shows you how to simulate the steady state
of the “SIM” model from Godley and Lavoie (2007 ch. 3) and simulate a
scenario where the government increase its expenditures.

A more complete description of the `sfcr_sim()` and `sfcr_scenario()`
variables can be found at the `vignette("sfcr")`.

The first step is to simulate the steady state of the model with the
`sfcr_sim()` function:

``` r
library(sfcr)

eqs <- list(
  TX_s ~ TX_d,
  YD ~ W * N_s - TX_s,
  C_d ~ alpha1 * YD + alpha2 * H_h[-1],
  H_h ~ YD - C_d + H_h[-1],
  N_s ~ N_d,
  N_d ~ Y / W,
  C_s ~ C_d,
  G_s ~ G_d,
  Y ~ C_s + G_s,
  TX_d ~ theta * W * N_s,
  H_s ~ G_d - TX_d + H_s[-1]
)

exg <- list(
  G_d ~ 20, 
  W ~ 1
  )

params <- list(
  alpha1 ~ 0.6,
  alpha2 ~ 0.4,
  theta ~ 0.2
  )

sim <- sfcr_sim(
  equations = eqs, 
  periods = 60, 
  exogenous = exg, 
  parameters = params
  )

sim
#> # A tibble: 60 x 17
#>    period  TX_s    YD   C_d   H_h   N_s   N_d   C_s   G_s     Y  TX_d   H_s
#>     <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1      1  0      0     0     0     0     0     0       0   0    0      0  
#>  2      2  7.69  30.8  18.5  12.3  38.5  38.5  18.5    20  38.5  7.69  12.3
#>  3      3  9.58  38.3  27.9  22.7  47.9  47.9  27.9    20  47.9  9.58  22.7
#>  4      4 11.2   44.7  35.9  31.5  55.9  55.9  35.9    20  55.9 11.2   31.5
#>  5      5 12.5   50.2  42.7  39.0  62.7  62.7  42.7    20  62.7 12.5   39.0
#>  6      6 13.7   54.8  48.4  45.3  68.4  68.4  48.4    20  68.4 13.7   45.3
#>  7      7 14.7   58.6  53.3  50.6  73.3  73.3  53.3    20  73.3 14.7   50.7
#>  8      8 15.5   61.9  57.4  55.1  77.4  77.4  57.4    20  77.4 15.5   55.2
#>  9      9 16.2   64.7  60.9  59.0  80.9  80.9  60.9    20  80.9 16.2   59.0
#> 10     10 16.8   67.0  63.8  62.2  83.8  83.8  63.8    20  83.8 16.8   62.2
#> # ... with 50 more rows, and 5 more variables: G_d <dbl>, W <dbl>,
#> #   alpha1 <dbl>, alpha2 <dbl>, theta <dbl>
```

With the steady state values at hand, we can use the `sfcr_scenario()`
function to see what happens if we increase government expenditures
`G_d` from 20 to 30:

``` r
shock <- sfcr_shock(
  variables = list(
    G_d ~ 30
  ),
  start = 5,
  end = 60
)

sim2 <- sfcr_scenario(
  sfcr_sim = sim,
  scenario = list(shock),
  periods = 60
  )

sim2
#> # A tibble: 60 x 17
#>    period  TX_s    YD   C_d   H_h   N_s   N_d   C_s   G_s     Y  TX_d   H_s
#>     <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1      1  20.0  80.0  80.0  80.0  100.  100.  80.0    20  100.  20.0  80.4
#>  2      2  20.0  80.0  80.0  80.0  100.  100.  80.0    20  100.  20.0  80.4
#>  3      3  20.0  80.0  80.0  80.0  100.  100.  80.0    20  100.  20.0  80.4
#>  4      4  20.0  80.0  80.0  80.0  100.  100.  80.0    20  100.  20.0  80.4
#>  5      5  23.8  95.4  89.2  86.1  119.  119.  89.2    30  119.  23.8  86.6
#>  6      6  24.8  99.1  93.9  91.3  124.  124.  93.9    30  124.  24.8  91.8
#>  7      7  25.6 102.   97.9  95.7  128.  128.  97.9    30  128.  25.6  96.2
#>  8      8  26.3 105.  101.   99.5  131.  131. 101.     30  131.  26.3  99.9
#>  9      9  26.8 107.  104.  103.   134.  134. 104.     30  134.  26.8 103. 
#> 10     10  27.3 109.  107.  105.   137.  137. 107.     30  137.  27.3 106. 
#> # ... with 50 more rows, and 5 more variables: G_d <dbl>, W <dbl>,
#> #   alpha1 <dbl>, alpha2 <dbl>, theta <dbl>
```

With `sfcr`, the models are written entirely with R using the standard R
syntax. Furthermore, their output is a `tibble` that is easily
manipulated with the `tidyverse` tools and plotted with `ggplot2`.

### References

<div id="refs" class="references hanging-indent">

<div id="ref-godley2007monetary">

Godley, Wynne, and Marc Lavoie. 2007. *Monetary Economics: An Integrated
Approach to Credit, Money, Income, Production and Wealth*. Palgrave
Macmillan.

</div>

</div>
