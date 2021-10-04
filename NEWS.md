# sfcr 0.2.0
## Major changes
Constrained the utilization of exogenous series to avoid unexpected behavior and undetected bugs in the models.

More specifically:
* Modified the behavior of `sfcr_baseline()` to disallow the utilization of exogenous series.
* constrained the utilization of exogenous series in the `sfcr_scenario()` to make sure that the length of the series matches with the length of the shock.

# sfcr 0.1.1
* Add `sfcr_portfolio()` function that calculates the matrix of portfolio parameters from a limited vector of parameters by imposing the adding-up and symmetry constraints.

# sfcr 0.1.0

First public release.
