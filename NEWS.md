# sfcr 0.2.1
## Major changes
* The `sfcr_random()` function that smartly allows the user to include random variables in the models was introduced in this version. This function is a wrapper around the built-in `rnorm()`, `rbinom()`, and `runif()` functions. The main benefit of using `sfcr_random()` is that it smartly guesses the size of the `sfcr_baseline()` and `sfcr_scenario()` models they're inserted, as well as the size of the `sfcr_shock()`.

The `sfcr_random()` function should only be used inside `sfcr_set()`s. If used outside this function, it will only print a message saying that it should not be used in this way.

## Bug fixes
* The disallowance of exogenous series introduced in v0.2.0 broke the models that used functions like `rnorm()` to add random variation. This version undo this change, allowing exogenous series to be passed along `sfcr_baseline()` again. Nonetheless, this utilization is discouraged since it generates unexpected behavior at the `sfcr_scenario()` level. Warning messages are raised about these issues.

* Prior to v0.2.1, the inclusion of exogenous series at the `sfcr_baseline()` level would not be transmitted to the `sfcr_scenario()` level since the previous infrastructure of the model only replicated the final values of the `sfcr_baseline()` object to fill in the matrix used to generate the initial values at the `sfcr_scenario()` level. 

This version re-evaluates the expressions created with `sfcr_set()` and passed as the `external` argument to the `sfcr_baseline()` function at the `sfcr_scenario()` level. In this way, if a user passes creates a random shock to influence the evolution of endogenous variables at the `sfcr_baseline()` level, this behavior will be reproduced at the `sfcr_scenario()` level. To avoid complications with different lengths between the two objects, the utilization of `sfcr_random()` is recommended.

Note that if an exogenous series is passed to the `sfcr_baseline()` function, it will result in an error if the lengths of the models do not coincide, it will generate an error. If the lengths coincide, the model will run, but the exogenous series will be replicated from the start, which is almost never the intended behavior. That's why this functionality will be removed in later versions of the package when the `sfcr_random()` function becomes widely adopted.


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
