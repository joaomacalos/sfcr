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

exg <- list("G_d" = 20, "W" = 1)
params <- list("alpha1" = 0.6, "alpha2" = 0.4, "theta" = 0.2)

# t is set to 10 to run faster. A usual model should run at least 50 periods to find a steady state
steady_state <- sfcr_sim(equations = eqs, t = 10, exogenous = exg,
                         parameters = params, initial = NULL)

# Increase G_d from 20 to 30
sfcr_scenario(steady_state, eqs, t = 10, exg, params, shock_exg = list("G_d" = 30))

# Increase alpha1 from 0.6 to 0.8
sfcr_scenario(steady_state, eqs, t = 10, exg, params, shock_param = list("alpha1" = 0.8))

# Increase W to 2, alpha2 to 0.5, and decrease theta to 0.15
sfcr_scenario(steady_state, eqs, t = 10, exg, params, shock_exg = list("W" = 2),
              shock_param = list("alpha2" = 0.5, "theta" = 0.15))


