test_that("Test that `sfcr_random()` are only evaluated inside `sfcr_set()`", {
  expect_output(sfcr_random("rnorm", mean=0, sd=1), "This function only returns an output when supplied inside `sfcr_set\\(\\)`")
})
