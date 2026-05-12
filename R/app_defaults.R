# Default parameter table used by the simulator.
default_params <- tibble::tibble(
  param = c("Km", "Vc", "Q", "Vp", "Ka", "Fa", "CL"),
  theta = c(3030, 0.807, 0.609, 2.17, 0.849, 44.6, 0.582),
  rse_theta = c(45, 14, 13, 11, 40, 14, 19),
  cv_iiv = c(131, 0, 0, 0, 0, 69.7, 52.8),
  rse_iiv = c(31, 0, 0, 0, 0, 41, 48),
  min = c(0, 0, 0, 0, 0, 0, 0),
  max = c(Inf, Inf, Inf, Inf, Inf, Inf, Inf)
)

# Default correlation edges used by the simulator.
default_corr_edges <- tibble::tibble(
  i = c("Km", "Fa", "Km"),
  j = c("CL", "CL", "Fa"),
  rho = c(-0.685, 0.66, -0.646)
)