test_that("build_correlation returns a symmetric clamped matrix", {
  edges <- tibble::tibble(
    i = c("CL", "V", "KA", "CL"),
    j = c("V", "CL", "CL", "CL"),
    rho = c(2, -2, 0.25, 0.8)
  )

  corr_matrix <- PmetricsLitSim:::build_correlation(c("CL", "V"), edges)

  expect_equal(dim(corr_matrix), c(2, 2))
  expect_equal(unname(diag(corr_matrix)), c(1, 1))
  expect_equal(corr_matrix["CL", "V"], -0.9999)
  expect_equal(corr_matrix["V", "CL"], -0.9999)
  expect_equal(corr_matrix, t(corr_matrix))
})

test_that("draw_omega returns zeros for missing or non-positive variability", {
  expect_equal(PmetricsLitSim:::draw_omega(3, NA_real_, 10), c(0, 0, 0))
  expect_equal(PmetricsLitSim:::draw_omega(3, 20, NA_real_), c(0, 0, 0))
  expect_equal(PmetricsLitSim:::draw_omega(3, 0, 10), c(0, 0, 0))
})

test_that("draw_omega returns positive samples when variability is present", {
  set.seed(123)

  omega_values <- PmetricsLitSim:::draw_omega(5, 20, 15)

  expect_length(omega_values, 5)
  expect_true(all(is.finite(omega_values)))
  expect_true(all(omega_values > 0))
})

test_that("make_omega_cov returns a symmetric positive semi-definite matrix", {
  omega_cov <- PmetricsLitSim:::make_omega_cov(
    omega_vec = c(0.2, 0.5),
    R = matrix(c(1, 2, 2, 1), nrow = 2)
  )

  expect_equal(omega_cov, t(omega_cov), tolerance = 1e-10)
  expect_true(all(eigen(omega_cov, symmetric = TRUE, only.values = TRUE)$values >= -1e-8))
})

test_that("sample_etas respects zero-omega columns and validates dimensions", {
  omega_samples <- matrix(
    c(0, 0.2,
      0, 0.3,
      0, 0.4),
    ncol = 2,
    byrow = TRUE
  )

  set.seed(42)
  eta_values <- PmetricsLitSim:::sample_etas(
    n = 3,
    theta_names = c("CL", "V"),
    omega_samples_by_param = omega_samples,
    R = diag(2)
  )

  expect_s3_class(eta_values, "tbl_df")
  expect_equal(names(eta_values), c("eta_CL", "eta_V"))
  expect_equal(eta_values$eta_CL, c(0, 0, 0))

  expect_error(
    PmetricsLitSim:::sample_etas(
      n = 1,
      theta_names = c("CL", "V"),
      omega_samples_by_param = matrix(0.2, nrow = 1, ncol = 1),
      R = diag(2)
    ),
    "omega vector length does not match number of parameters"
  )
})

test_that("sample_thetas applies explicit bounds", {
  set.seed(99)
  theta_values <- PmetricsLitSim:::sample_thetas(
    n = 20,
    params_tbl = tibble::tibble(
      param = "CL",
      theta = 10,
      rse_theta = 20,
      min = 8,
      max = 12
    )
  )

  expect_s3_class(theta_values, "tbl_df")
  expect_equal(names(theta_values), "theta_CL")
  expect_true(all(theta_values$theta_CL >= 8 & theta_values$theta_CL <= 12))
})

test_that("sample_thetas defaults to a nonnegative lower bound when min is absent", {
  theta_values <- PmetricsLitSim:::sample_thetas(
    n = 5,
    params_tbl = tibble::tibble(
      param = "V",
      theta = -5,
      rse_theta = 0
    )
  )

  expect_equal(theta_values$theta_V, c(0, 0, 0, 0, 0))
})

test_that("sample_omegas returns one column per parameter", {
  set.seed(321)
  omega_values <- PmetricsLitSim:::sample_omegas(
    n = 4,
    params_tbl = tibble::tibble(
      param = c("CL", "V"),
      cv_iiv = c(20, 0),
      rse_iiv = c(10, 5)
    )
  )

  expect_s3_class(omega_values, "tbl_df")
  expect_equal(names(omega_values), c("omega_CL", "omega_V"))
  expect_equal(omega_values$omega_V, c(0, 0, 0, 0))
  expect_true(all(omega_values$omega_CL > 0))
})

test_that("compose_params combines theta and eta values and enforces bounds", {
  theta_values <- tibble::tibble(theta_CL = c(10, 3), theta_V = c(5, 7))
  eta_values <- tibble::tibble(eta_CL = c(log(2), log(0.5)))
  params_tbl <- tibble::tibble(
    param = c("CL", "V"),
    min = c(0, 6),
    max = c(15, 6.5)
  )

  composed <- PmetricsLitSim:::compose_params(
    theta_df = theta_values,
    eta_df = eta_values,
    param_names = c("CL", "V"),
    params_tbl = params_tbl
  )

  expect_s3_class(composed, "tbl_df")
  expect_equal(composed$CL, c(15, 1.5))
  expect_equal(composed$V, c(6, 6.5))
})

test_that("maybe_transpose keeps or transposes output orientation", {
  input_df <- tibble::tibble(CL = c(1, 2), V = c(3, 4))

  expect_equal(PmetricsLitSim:::maybe_transpose(input_df, transpose = FALSE), input_df)

  transposed <- PmetricsLitSim:::maybe_transpose(input_df, transpose = TRUE)

  expect_s3_class(transposed, "tbl_df")
  expect_equal(names(transposed), c("parameter", "V1", "V2"))
  expect_equal(transposed$parameter, c("CL", "V"))
  expect_equal(transposed$V1, c(1, 3))
  expect_equal(transposed$V2, c(2, 4))
})