# Balance-sheet matrix

bs_insout <- sfcr_matrix(
  columns = c("Households", "Firms", "Government", "Central bank", "Banks", "Sum"),
  codes = c("h", "f", "g", "cb", "b", "s"),
  r1 = c("Inventories", f = "+INV", s = "+INV"),
  r2 = c("HPM", h = "+Hhd", cb = "-Hs", b = "+Hbd"),
  r3 = c("Advances", cb = "+As", b = "-Ad"),
  r4 = c("Checking deposits", h = "+M1h", b = "-M1s"),
  r5 = c("Time deposits", h = "+M2h", b = "-M2s"),
  r6 = c("Bills", h = "+Bhh", g = "-Bs", cb = "+Bcb", b = "+Bbd"),
  r7 = c("Bonds", h = "+BLh * pbl", g = "-BLs * pbl"),
  r8 = c("Loans", f = "-Ld", b = "+Ls"),
  r9 = c("Balance", h = "-V", f = 0, g = "+GD", cb = 0, b = 0, s = "-INV")
)

sfcr_matrix_display(bs_insout)
