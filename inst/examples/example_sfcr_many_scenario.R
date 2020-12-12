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

shock <- sfcr_shock(
  variables = sfcr_set(
    alpha2 ~ 0.3
  ),
  start = 1,
  end = 3
)

baseline <- sfcr_baseline(eqs, external, periods = 5)

# Example 1: Many external sets, 1 set of equations:
expanded1 <- sfcr_expand(external, alpha1, c(0.7, 0.8))
multis1 <- sfcr_multis(expanded = expanded1, fixed = eqs, periods = 5)

# Example 2: Many shocks, 1 baseline model:
expanded2 <- sfcr_expand(shock, alpha2, c(0.1, 0.2))
multis2 <- sfcr_multis(expanded = expanded2, fixed = baseline, periods = 5)

# Example 3: Many baseline models, 1 shock:
multis3 <- sfcr_multis(expanded = multis1, fixed = shock, periods = 5)

sfcr_many_scenario(baseline = many_baseline, external = list(shock1), periods = 5)
