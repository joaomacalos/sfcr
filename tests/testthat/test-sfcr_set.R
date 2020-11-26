test_that("Error if invalid formulas", {
  expect_error(sfcr_set(G_d = 20, W ~ 1, alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2),
               "Please use the R equations syntax to define the formulas.")
})


test_that("Error if invalid formulas", {
  expect_error(sfcr_set(G_d ~ 20, W ~ 1, "alpha1", alpha2 ~ 0.4, theta ~ 0.2),
               "Invalid arguments. Please use the R equations syntax to define the formulas.")
})
