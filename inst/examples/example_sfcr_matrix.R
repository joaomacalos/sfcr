# Balance-sheet matrix

bs_pc <- sfcr_matrix(
  columns = c("Households", "Firms", "Government", "Central bank", "sum"),
  codes = c("h", "f", "g", "cb", "s"),
  r1 = c("Money", h = "+Hh", cb = "-Hs"),
  r2 = c("Bills", h = "+Bh", g = "-Bs", cb = "+Bcb"),
  r3 = c("Balance", h = "-V", g = "+V")
)


# Transactions-flow matrix
tfm_pc <- sfcr_matrix(
  columns = c("Households", "Firms", "Government", "CB current", "CB capital"),
  codes = c("h", "f", "g", "cbc", "cbk"),
  c("Consumption", h = "-C", f = "+C"),
  c("Govt. Expenditures", f = "+G", g = "-G"),
  c("Income", h = "+Y", f = "-Y"),
  c("Int. payments", h = "+r[-1] * Bh[-1]", g = "-r[-1] * Bs[-1]", cbc = "+r[-1] * Bcb[-1]"),
  c("CB profits", g = "+r[-1] * Bcb[-1]", cbc = "-r[-1] * Bcb[-1]"),
  c("Taxes", h = "-TX", g = "+TX"),
  c("Ch. Money", h = "-(Hh - Hh[-1])", cbk = "+(Hs - Hs[-1])"),
  c("Ch. Bills", h = "-(Bh - Bh[-1])", g = "+(Bs - Bs[-1])", cbk = "-(Bcb - Bcb[-1])")
)
