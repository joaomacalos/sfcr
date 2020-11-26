test_that("Quasiquotation works", {
  external <- sfcr_set(alpha1 ~ 0.7)
  expect_s3_class(sfcr_expand(x = external, variable = alpha1, values = c(0.6, 0.8)), "sfcr_mlt_set")
})

test_that("Find correct name", {
  external <- sfcr_set(G_d ~ 20, W ~ 1, alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)
  expect_s3_class(sfcr_expand(x = external, variable = alpha1, values = c(0.6, 0.8)), "sfcr_mlt_set")
})

test_that("Missing variable in x returns an error", {
  external <- sfcr_set(alpha1 ~ 0.7)
  expect_error(sfcr_expand(x = external, variable = alpha2, values = c(0.6, 0.8)),
               "Please supply a valid variable name that is present in the external set.")
})

test_that("Error if invalid name", {
  external <- sfcr_set(theta ~ 0.2)
  expect_error(sfcr_expand(x = external, variable = 0.1, values = c(0.1, 0.2)),
               "Please supply a valid variable name.")
})

test_that("Error if invalid name", {
  external <- sfcr_set(theta ~ 0.2)
  expect_error(sfcr_expand(x = external, variable = external, values = c(0.1, 0.2)),
               "Please supply a valid variable name.")
})

test_that("Error if invalid values", {
  external <- sfcr_set(theta ~ 0.2)
  expect_error(sfcr_expand(external, variable = "theta", c("a", "b")),
               "Please supply a numeric vector as values.")
})

