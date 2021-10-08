
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sfcr <img src='man/figures/sfcr.png' align="right" height="139" />

<!-- badges: start -->

[![Travis Build
Status](https://app.travis-ci.com/joaomacalos/sfcr.svg?branch=main)](https://app.travis-ci.com/joaomacalos/sfcr)
[![CRAN\_Release\_Badge](https://www.r-pkg.org/badges/version/sfcr)](https://CRAN.R-project.org/package=sfcr)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/sfcr)](https://CRAN.R-project.org/package=sfcr)
[![R build
status](https://github.com/joaomacalos/sfcr/workflows/R-CMD-check/badge.svg)](https://github.com/joaomacalos/sfcr/actions)
<!-- badges: end -->

The goal of the `sfcr` package is to provide an intuitive and `tidy` way
to estimate stock-flow consistent (SFC) models with R.

## Installation

`sfcr` is on CRAN and can be installed with:

``` r
install.packages("sfcr")
```

For the development version available on [GitHub](https://github.com/),
use the `devtools` package for installation:

``` r
# install.packages("devtools")
devtools::install_github("joaomacalos/sfcr")
```

## Example

This is a basic example which shows how to simulate the “SIM” model from
Godley and Lavoie (2007ch. 3), as well as how to add scenarios to this
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

Check the [notebooks](https://joaomacalos.github.io/sfcr/articles/) that
replicate the models in Godley and Lavoie (2007) for more detailed
examples on the usage of the package.

### Frequently Asked Questions

**Q: Can you add exogenous series to a `sfcr` model?**

A: Since version 0.2, the `sfcr` package recommends the utilization of
exogenous variables only in the `sfcr_scenario()` function. This
functionality is going to be excluded from `sfcr_baseline()` function in
the future because it led to unexpected behavior when calculating
scenarios on the top of those baseline models.

The exogenous series can be added to the model with the help of
`sfcr_shock()` and `sfcr_set()` functions. It is further required that
the length of the exogenous time series being supplied be either 1 or
exactly equal to length of the shock.

For example, the code supplied above can be modified to make `Gd`
increase from 30 to 40 between periods 1 and 60 of the scenario:

``` r
library(dplyr) # for select() and everything() functions

shock <- sfcr_shock(
  variables = sfcr_set(
    Gd ~ seq(30, 40, length.out=60)
  ),
  start = 1,
  end = 60
)

sim2 <- sfcr_scenario(
  baseline = sim,
  scenario = shock,
  periods = 60
  )
#> Warning: Passing exogenous series with a shock can lead to unexpected behavior if the length of the series is smaller than the periods to the end of the scenario. Be cautious when using this functionality.
#> This warning is displayed once per session.

select(sim2, period, Gd, everything())
#> # A tibble: 60 x 17
#>    period    Gd   TXs    YD    Cd    Hh    Ns    Nd    Cs    Gs     Y   TXd
#>     <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1      1  30    20.0  80.0  80.0  80.0  100.  100.  80.0  20    100.  20.0
#>  2      2  30.2  23.9  95.6  89.4  86.3  120.  120.  89.4  30.2  120.  23.9
#>  3      3  30.3  24.9  99.8  94.4  91.7  125.  125.  94.4  30.3  125.  24.9
#>  4      4  30.5  25.8 103.   98.7  96.3  129.  129.  98.7  30.5  129.  25.8
#>  5      5  30.7  26.6 106.  102.  100.   133.  133. 102.   30.7  133.  26.6
#>  6      6  30.8  27.3 109.  106.  104.   137.  137. 106.   30.8  137.  27.3
#>  7      7  31.0  27.9 112.  109.  107.   140.  140. 109.   31.0  140.  27.9
#>  8      8  31.2  28.5 114.  111.  110.   142.  142. 111.   31.2  142.  28.5
#>  9      9  31.4  28.9 116.  113.  112.   145.  145. 113.   31.4  145.  28.9
#> 10     10  31.5  29.4 118.  115.  114.   147.  147. 115.   31.5  147.  29.4
#> # ... with 50 more rows, and 5 more variables: Hs <dbl>, W <dbl>, alpha1 <dbl>,
#> #   alpha2 <dbl>, theta <dbl>
```

**Q: How to add random variation to endogenous variables?**

A: The recommended way to add random variation to endogenous variables
is with the `sfcr_random()` function. This function can only be used
inside `sfcr_set()`, be it when you’re creating a set of exogenous
variables or when defining the variables inside a `sfcr_shock()`. The
advantage of utilizing this function is that it smartly guesses the
length of the models, avoiding any unwanted mistake.

The `sfcr_random()` function can accept three arguments as its first
`.f` argument: `"rnorm"`, `"rbinom"`, and `"runif"`. These arguments
implement wrappers around the built-in functions `rnorm()`, `rbinom()`,
and `runif()` – random series generator function – but guessing the
correct length of the `sfcr_baseline()`, `sfcr_scenario()`, or
`sfcr_shock()` from where they are called. The `sfcr_random()` function
also accepts any extra argument that can be passed to these functions.

Snippet:

``` r
sfcr_set(
  Ra ~ sfcr_random("rnorm", sd=0.05)
)
#> [[1]]
#> Ra ~ sfcr_random("rnorm", sd = 0.05)
#> 
#> attr(,"class")
#> [1] "sfcr_set" "list"
```

An utilization of this functionality in practice is provided in the
article replicating the Portfolio Choice model from Godley and Lavoie
(2007ch. 4).

Alternatively, the direct utilization of the random generator functions
from `stats` are still allowed to ensure the compatibility with the
v0.1.1 of the package. Nonetheless, the user must be careful when using
this functionality at the `sfcr_baseline()` since this expression is
going to be evaluated again at the `sfcr_scenario()` level. The safest
way to use these functions is by passing `periods` instead of an integer
as their first argument.

Snippet:

``` r
# Not recommended but work:
sfcr_set(
  Ra ~ rnorm(periods, sd=0.05)
)
#> [[1]]
#> Ra ~ rnorm(periods, sd = 0.05)
#> 
#> attr(,"class")
#> [1] "sfcr_set" "list"

# NOT RECOMMENDED!
sfcr_set(
  Ra ~ rnorm(60, sd=0.05)
)
#> [[1]]
#> Ra ~ rnorm(60, sd = 0.05)
#> 
#> attr(,"class")
#> [1] "sfcr_set" "list"
```

**Q: Can you add endogenous variables with more than one lag?**

A: Yes, you can, but you need to use auxiliary variables.

For example, say that you want to modify model SIM to have Consumption
`Cd` in period `t` defined as function of the moving average of
disposable income. In this situation, you would have to code the
variables as:

``` r
eqs <- sfcr_set(
  TXs ~ TXd,
  YD ~ W * Ns - TXs,
  YDlag1 ~ YD[-1],
  YDlag2 ~ YDlag1[-1],
  YDlag3 ~ YDlag2[-1],
  YDmav ~ (YD + YDlag1 + YDlag2 + YDlag3) / 4,
  Cd ~ alpha1 * YDmav + alpha2 * Hh[-1],
  Hh ~ YD - Cd + Hh[-1],
  Ns ~ Nd,
  Nd ~ Y / W,
  Cs ~ Cd,
  Gs ~ Gd,
  Y ~ Cs + Gs,
  TXd ~ theta * W * Ns,
  Hs ~ Gd - TXd + Hs[-1]
)
```

### Submitting your code to the package repository

Everyone is invited to submit your published SFC models developed with
the `sfcr` package to the package repository to be displayed together
with the models of Godley and Lavoie (2007).

To do so, please submit a pull request or send me an email.

### Acknowledgments

I’m grateful to Severin Reissl for his very useful comments and for
always pointing me in the right direction, to Marc Lavoie for answering
all my questions about SFC modeling, and to Italo Pedrosa for our
discussions about the state of the SFC field.

I’d also like to acknowledge all the developers and academics that share
their code and make the SFC field alive. In particular, many thanks to
Antoine Godin for answering all my queries about the `PKSFC`
[package](https://github.com/S120/PKSFC), from which I draw much
inspiration, specially in the DAGs section of the package, to Gabriel
Petrini da Silveira and Kenn Takara for their `pysolve3`
[package](https://github.com/gpetrini/pysolve3), from which I found the
references to implement the Broyden solver in R, and to Gennaro Zezza
for his invaluable
[macros](http://gennaro.zezza.it/software/eviews/gl2006.php) to simulate
the models in Godley and Lavoie (2007).

### References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-godley2007monetary" class="csl-entry">

Godley, Wynne, and Marc Lavoie. 2007. *Monetary Economics: An Integrated
Approach To Credit, Money, Income, Production and Wealth*. Palgrave
Macmillan.

</div>

</div>
