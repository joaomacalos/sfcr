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

# t is set to 10 to run faster. A usual model should run at
# least 50 periods to find a steady state
sfcr_sim(equations = eqs, t = 10, exogenous = exg, parameters = params, initial = NULL)

