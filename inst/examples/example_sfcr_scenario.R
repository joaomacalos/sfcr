eqs <- sfcr_set(
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

external <- sfcr_set(G_d ~ 20, W ~ 1, alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)

# t is set to 10 to run faster. A usual model should run at least 50 periods to find a steady state
steady_state <- sfcr_baseline(eqs, external, periods = 10)

# Increase G_d from 20 to 30 between periods 5 and 10
shock1 <- sfcr_shock(sfcr_set(G_d ~ 30), 5, 10)

sfcr_scenario(steady_state, scenario = list(shock1), 10)

# Increase W to 2, alpha2 to 0.5, and decrease theta to 0.15
shock2 <- sfcr_shock(
  variables = sfcr_set(
  W ~ 2,
  alpha2 ~ 0.5,
  theta ~ 0.15
  ),
  start = 5,
  end = 10)

sfcr_scenario(steady_state, list(shock2), 10)


