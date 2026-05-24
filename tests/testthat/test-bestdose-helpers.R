test_that("named_bestdose_code_block preserves lines and derives names", {
  code_block <- "ke = ke0 * weight\nv = v0 * weight"

  named_block <- PmetricsLitSim:::named_bestdose_code_block(code_block, prefix = "sec", use_lhs = TRUE)

  expect_equal(names(named_block), c("ke", "v"))
  expect_equal(unname(unlist(named_block)), c("ke = ke0 * weight", "v = v0 * weight"))
})

test_that("build_bestdose_primary uses sampled ranges for open bounds", {
  params_tbl <- tibble::tibble(
    param = c("CL", "V"),
    min = c(0, NA_real_),
    max = c(Inf, 20)
  )
  sample_df <- tibble::tibble(CL = c(1, 2, 3), V = c(8, 10, 12))

  primary <- PmetricsLitSim:::build_bestdose_primary(params_tbl, sample_df)

  expect_equal(primary$CL$type, "ab")
  expect_equal(primary$CL$min, 0)
  expect_equal(primary$CL$max, 3)
  expect_equal(primary$V$min, 8)
  expect_equal(primary$V$max, 20)
})

test_that("build_bestdose_support_points returns equally weighted rows", {
  sample_df <- tibble::tibble(CL = c(1, 2, 3, 4), V = c(10, 11, 12, 13))

  support_points <- PmetricsLitSim:::build_bestdose_support_points(sample_df, c("CL", "V"), max_points = 2)
  support_df <- PmetricsLitSim:::bestdose_support_points_to_df(support_points)

  expect_equal(nrow(support_df), 2)
  expect_equal(sum(support_df$prob), 1)
  expect_equal(unname(support_df$prob), c(0.5, 0.5))
})

test_that("parse_bestdose_covariate_specs generates defaults when missing", {
  covariates <- PmetricsLitSim:::parse_bestdose_covariate_specs("", fallback_names = c("weight", "crcl"))

  expect_equal(covariates$name, c("weight", "crcl"))
  expect_equal(covariates$default, c(70, 100))
})

test_that("simulate_bestdose_profiles handles a simple bolus one-compartment model", {
  support_points_df <- tibble::tibble(ke = 1, prob = 1)

  profiles <- PmetricsLitSim:::simulate_bestdose_profiles(
    support_points_df = support_points_df,
    param_names = "ke",
    secondary_lines = character(),
    ode_lines = c("dx[1] = -ke * X[1]"),
    output_lines = c("Y[1] = X[1]"),
    covariates = list(),
    dose_amount = 100,
    dose_times = 0,
    infusion_duration = 0,
    sample_times = c(0, 1, 2),
    n_compartments = 1,
    dose_compartment = 1
  )

  expect_true(all(c("support_id", "time", "prob", "Y1") %in% names(profiles)))
  expect_equal(profiles$Y1[profiles$time == 0], 100, tolerance = 1e-6)
  expect_equal(
    profiles$Y1[profiles$time == 1],
    100 * exp(-1),
    tolerance = 1e-3
  )
})