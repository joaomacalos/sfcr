eqs <- list(
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

exg <- list(G_d ~ 20, W ~ 1)
params <- c(alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)

test_that("Missing exogenous values returns an error", {
  exg <- list(W ~ 1)
  params <- list(alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)
  expect_error(sfcr_sim(eqs, exg, params, periods = 10), "object 'G_d' not found")
})

test_that("Number of columns equals to number of endogenous, exogenous, and parameters plus one", {
  expect_equal(ncol(sfcr_sim(eqs, exg, params, periods = 10)), (length(eqs) + length(exg) + length(params) + 1))
})

test_that("Error if random noise is included", {
  eqs$e11 <- H_s ~ G_d + TX_d + H_s[-1] + rnorm(1)
  expect_error(sfcr_sim(eqs, exg, params, periods = 10), 'Please define random variations as an external parameter.')
})

test_that("Error if lag > 2 is included", {
  eqs$e11 <- H_s ~ G_d + TX_d + H_s[-2]
  expect_error(sfcr_sim(eqs, exg, params, periods = 10))
})

test_that("No error if name overlap with leading '.' and '_'", {
  eqs$e6 <- d.N ~ Y / W
  eqs$e5 <- s.N ~ d.N
  eqs$e2 <- YD ~ W * s.N - TX_s
  eqs$e10 <- TX_d ~ theta * W * s.N
  eqs$e8 <- G_s ~ bar_G_s
  eqs$e11 <- H_s ~ bar_G_s - TX_d + H_s[-1]
  exg <- list(bar_G_s ~ 20, W ~ 1)
  expect_s3_class(sfcr_sim(eqs, exg, params, periods = 10), 'sfcr_tbl')
})

test_that("No error if name overlap with trailing '.' and '_'", {
  eqs$e6 <- N.d ~ Y / W
  eqs$e5 <- N.s ~ N.d
  eqs$e2 <- YD ~ W * N.s - TX_s
  eqs$e10 <- TX_d ~ theta * W * N.s
  eqs$e8 <- G_s ~ G_s_bar
  eqs$e11 <- H_s ~ G_s_bar - TX_d + H_s[-1]
  exg <- list(G_s_bar ~ 20, W ~ 1)
  expect_s3_class(sfcr_sim(eqs, exg, params, periods = 10), 'sfcr_tbl')
})
