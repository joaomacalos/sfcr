eqs <- sfcr_set(
  e1 = TXs ~ TXd,
  e2 = YD ~ W * Ns - TXs,
  e3 = Cd ~ alpha1 * YD + alpha2 * Hh[-1],
  e4 = Hh ~ YD - Cd + Hh[-1],
  e5 = Ns ~ Nd,
  e6 = Nd ~ Y / W,
  e7 = Cs ~ Cd,
  e8 = Gs ~ Gd,
  e9 = Y ~ Cs + Gs,
  e10 = TXd ~ theta * W * Ns,
  e11 = Hs ~ Gd - TXd + Hs[-1]
)

ext <- sfcr_set(Gd ~ 20, W ~ 1, alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)

sim <- sfcr_baseline(eqs, ext, 50)

tfm_sim <- sfcr_matrix(
  columns = c("h", "p", "g"),
  codes = c("h", "p", "g"),
  c("Consumption", h = "-Cd", p = "+Cs"),
  c("Govt. Expenditures", p = "+Gs", g = "-Gd"),
  c("Wages", h = "+W*Ns", p = "-W*Nd"),
  c("Taxes", h = "-TXd", g = "+TXs"),
  c("Ch. Money", h = "-(Hh - Hh[-1])", g = "+(Hs - Hs[-1])")
)


test_that("Error non-recognized argument", {
  expect_error(sfcr_sankey(tfm_sim, sim, "what"))
})
