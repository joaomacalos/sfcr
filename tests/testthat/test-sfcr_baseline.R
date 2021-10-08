eqs <- sfcr_set(
  e1 = TX_s ~ TX_d,
  e2 = YD ~ W * N_s - TX_s,
  e3 = C_d ~ alpha1 * YD + alpha2 * H_h[-1],
  e4 = H_h ~ YD - C_d + H_h[-1],
  e5 = N_s ~ N_d,
  e6 = N_d ~ Y / W,
  e7 = C_s ~ C_d,
  e8 = G_s ~ G_d,
  e9 = Y ~ C_s + G_s,
  e10 = TX_d ~ theta * W * N_s,
  e11 = H_s ~ G_d - TX_d + H_s[-1]
)

ext <- sfcr_set(G_d ~ 20, W ~ 1, alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)

test_that("Missing exogenous values returns an error", {
  ext <- list(W ~ 1, alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)
  expect_error(sfcr_baseline(eqs, ext, periods = 10), "object 'G_d' not found")
})

test_that("Number of columns equals to number of endogenous, exogenous, and parameters plus one", {
  expect_equal(ncol(sfcr_baseline(eqs, ext, periods = 10)), (length(eqs) + length(ext) + 1))
})

test_that("Error if random noise is included", {
  eqs$e11 <- H_s ~ G_d + TX_d + H_s[-1] + rnorm(1)
  expect_error(sfcr_baseline(eqs, ext, periods = 5), 'Please define random variations as an external parameter.')
})

test_that("Error if lag > 2 is included", {
  eqs$e11 <- H_s ~ G_d + TX_d + H_s[-2]
  expect_error(sfcr_baeline(eqs, exg, params, periods = 5))
})

test_that("No error if name overlap with leading '.' and '_'", {
  eqs$e6 <- d.N ~ Y / W
  eqs$e5 <- s.N ~ d.N
  eqs$e2 <- YD ~ W * s.N - TX_s
  eqs$e10 <- TX_d ~ theta * W * s.N
  eqs$e8 <- G_s ~ bar_G_s
  eqs$e11 <- H_s ~ bar_G_s - TX_d + H_s[-1]
  ext <- list(bar_G_s ~ 20, W ~ 1, G_d ~ 20, alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)
  expect_s3_class(sfcr_baseline(eqs, ext, periods = 5), 'sfcr_tbl')
})

test_that("No error if name overlap with trailing '.' and '_'", {
  eqs$e6 <- N.d ~ Y / W
  eqs$e5 <- N.s ~ N.d
  eqs$e2 <- YD ~ W * N.s - TX_s
  eqs$e10 <- TX_d ~ theta * W * N.s
  eqs$e8 <- G_s ~ G_s_bar
  eqs$e11 <- H_s ~ G_s_bar - TX_d + H_s[-1]
  ext <- list(G_s_bar ~ 20, W ~ 1, alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)
  expect_s3_class(sfcr_baseline(eqs, ext, periods = 5, method = "Gauss"), 'sfcr_tbl')
})


test_that("Error if duplicated endogenous variables", {
  eqs$e12 <- H_s ~ TX_d
  expect_error(sfcr_baseline(eqs, ext, periods = 2), "The endogenous variable `H_s` was defined more than once. Please check your model and try again.")
})

test_that("Error if two duplicated endogenous variables", {
  eqs$e12 <- H_s ~ TX_d
  eqs$e13 <- N_d ~ Y
  expect_error(sfcr_baseline(eqs, ext, periods = 2), "The endogenous variables `H_s, N_d` were defined more than once. Please check your model and try again.")
})


test_that("Error if invalid name .i in endogenous variables", {
  eqs$e12 <- .i ~ TX_d
  expect_error(sfcr_baseline(eqs, ext, periods = 2), "Invalid name detected! Please don't use \".i\" to name any variable.")
})

test_that("Error if invalid name .i in external variables", {
  ext <- sfcr_set(ext, .i ~ 15)
  expect_error(sfcr_baseline(eqs, ext, periods = 2), "Invalid name detected! Please don't use \".i\" to name any variable.")
})

test_that("Error if length of exogenous variables is not equal to one or to the length of the model.", {
 ext <- sfcr_set(G_d ~ 20, W ~ seq(1, 10), alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)
 expect_error(sfcr_baseline(eqs, ext, periods = 15), "The exogenous variables must have either length 1 or exactly the same length as the baseline model.")
})

test_that("Only warning if exogenous variable has the same length as periods", {
  ext <- sfcr_set(G_d ~ 20, W ~ rnorm(15, mean=1, sd=.1), alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)
  expect_warning(sfcr_baseline(eqs, ext, periods = 15), 'The utilization exogenous series within a baseline model is not recommended and will be disallowed in the future. Be careful when using this functionality.')
})

test_that("Periods cannot be smaller than 2", {
  ext <- sfcr_set(G_d ~ 20, W ~ 1, alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)
  expect_error(sfcr_baseline(eqs, ext, periods = 1), "The minimum periods required to simulate a model is 2.")
})

test_that("Periods cannot be smaller than 2", {
  ext <- sfcr_set(G_d ~ 20, W ~ 1, alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)
  expect_error(sfcr_baseline(eqs, ext, periods = -10), "The minimum periods required to simulate a model is 2.")
})

test_that("Periods cannot be smaller than 2", {
  ext <- sfcr_set(G_d ~ 20, W ~ 1, alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)
  expect_error(sfcr_baseline(eqs, ext, periods = 0), "The minimum periods required to simulate a model is 2.")
})

test_that("No error if periods are defined outside function call", {
  periods <- 50
  ext <- sfcr_set(G_d ~ 20, W ~ sfcr_random("rnorm", mean=1, sd=.1), alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)
  expect_length(sfcr_baseline(eqs, ext, periods = 15)$W, 15)
})
