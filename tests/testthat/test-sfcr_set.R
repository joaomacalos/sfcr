test_that("Error if invalid formulas", {
  expect_error(sfcr_set(W ~ 1, alpha1 ~ 0.6, G_d = 20, alpha2 ~ 0.4, theta ~ 0.2),
               "Invalid arguments. Please use the R formula syntax \\(`~` instead of `=`\\) to separate the equations.")
})


test_that("Error if invalid formulas", {
  expect_error(sfcr_set(G_d ~ 20, W ~ 1, "alpha1", alpha2 ~ 0.4, theta ~ 0.2),
               "Invalid arguments. Please use the R formula syntax \\(`~` instead of `=`\\) to separate the equations.")
})
