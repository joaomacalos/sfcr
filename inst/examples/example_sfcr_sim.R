eqs <- list(
  TX_s ~ TX_d,
  YD ~ W * N_s - TX_s,
  C_d ~ alpha1 * YD + alpha2 * H_h[-1],
  H_h ~ YD - C_d + H_h[-1],
  N_s ~ N_d,
  N_d ~ Y / W,
  C_s ~ C_d,
  G_s ~ G_d,
  Y ~ C_s + G_s,
  TX_d ~ theta * W * N_s,
  H_s ~ G_d - TX_d + H_s[-1]
)

exg <- list(G_d ~ 20, W ~ 1)
params <- list(alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)

# `periods` is set to 10 to run faster. A usual model should run at
# least 50 periods to find a steady state
sfcr_sim(equations = eqs, exogenous = exg, parameters = params, periods = 10)

