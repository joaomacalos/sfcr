eqs <- sfcr_set(
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

ext <- sfcr_set(G_d ~ 20, W ~ 1, alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)

s1 <- .sfcr_find_order(eqs)
eq_ext <- .eq_as_tb(ext)
s2 <- .prep_equations(s1, eq_ext)
s3 <- .make_matrix(s2, eq_ext, 10)
s4 <- .sfcr_gauss_seidel(s3, s2, 10, max_ite = 2, tol = 1e-5)
s5 <- tibble::tibble(data.frame(s4))

test_that("stop works", {
  colnames(s2) <- c("one", "two", "three", "four")
  expect_error(new_sfcr_tbl(tbl = s5, matrix = s4, calls = s2))
})

test_that("stop works", {
  expect_error(new_sfcr_tbl(tbl = s5, matrix = s4, calls = data.frame(s2)))
})

test_that("stop works", {
  expect_error(new_sfcr_tbl(tbl = s4, matrix = s4, calls = ext))
})
