eqs <- list(
  TX_s[t] ~ TX_d[t],
  YD[t] ~ W[t] * N_s[t] - TX_s[t],
  C_d[t] ~ alpha1 * YD[t] + alpha2 * H_h[t-1],
  H_h[t] ~ YD[t] - C_d[t] + H_h[t-1],
  N_s[t] ~ N_d[t],
  N_d[t] ~ Y[t] / W[t],
  C_s[t] ~ C_d[t],
  G_s[t] ~ G_d[t],
  Y[t] ~ C_s[t] + G_s[t],
  TX_d[t] ~ theta * W[t] * N_s[t],
  H_s[t] ~ G_d[t] - TX_d[t] + H_s[t-1]
)

test_that("non-list parameters return an error", {
  exg <- list("G_d" = 20, "W" = 1)
  params <- c("alpha1" = 0.6, "alpha2" = 0.4, "theta" = 0.2)
  expect_error(sfcr_sim(eqs, t = 3, exogenous = exg, parameters = params), "`parameters` must be a list.")
})

test_that("non-list exogenous return an error", {
  exg <- c("G_d" = 20, "W" = 1)
  params <- list("alpha1" = 0.6, "alpha2" = 0.4, "theta" = 0.2)
  expect_error(sfcr_sim(eqs, t = 3, exogenous = exg, parameters = params), "`exogenous` must be a list.")
})

test_that("Missing exogenous values returns an error", {
  exg <- list("W" = 1)
  params <- list("alpha1" = 0.6, "alpha2" = 0.4, "theta" = 0.2)
  expect_error(sfcr_sim(eqs, t = 3, exogenous = exg, parameters = params), "object 'G_d' not found")
})

test_that("Missing parameter returns an error", {
  exg <- list("G_d" = 20, "W" = 1)
  params <- list("alpha1" = 0.6, "alpha2" = 0.4)
  expect_error(sfcr_sim(eqs, t = 3, exogenous = exg, parameters = params), "object 'theta' not found")
})

test_that("Return an error when hidden equation condition is not fulfilled", {
  exg <- list("G_d" = 20, "W" = 1)
  params <- list("alpha1" = 0.6, "alpha2" = 0.4, "theta" = 0.2)
  hidden <- list("H_h", "C_d")
  expect_error(sfcr_sim(eqs, t = 3, exogenous = exg, parameters = params, hidden = hidden),
               "Hidden equation is not fulfilled. Check again the equations in the model.")
})
