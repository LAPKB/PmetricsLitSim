#' Build a symmetric correlation matrix from pairwise edges
#'
#' @param params Character vector of parameter names.
#' @param edges Data frame with `i`, `j`, and `rho` columns describing
#'   pairwise correlations.
#'
#' @return A symmetric correlation matrix with unit diagonal.
#' @keywords internal
#' @noRd
build_correlation <- function(params, edges) {
  p <- length(params)
  R <- diag(1, p)
  dimnames(R) <- list(params, params)

  if (!is.null(edges) && nrow(edges) > 0) {
    edges2 <- edges |>
      dplyr::mutate(
        i = as.character(.data$i),
        j = as.character(.data$j)
      ) |>
      dplyr::filter(.data$i %in% params, .data$j %in% params, .data$i != .data$j) |>
      dplyr::mutate(rho = pmax(pmin(as.numeric(.data$rho), 0.9999), -0.9999))

    for (k in seq_len(nrow(edges2))) {
      ii <- edges2$i[k]
      jj <- edges2$j[k]
      r <- edges2$rho[k]
      R[ii, jj] <- r
      R[jj, ii] <- r
    }
  }

  R
}

#' Sample inter-individual variability values for a parameter
#'
#' @param n Number of samples to draw.
#' @param cv_percent Typical coefficient of variation in percent.
#' @param rse_iiv_percent Relative standard error for the IIV value in percent.
#'
#' @return Numeric vector of sampled variability values on the CV scale.
#' @keywords internal
#' @noRd
draw_omega <- function(n, cv_percent, rse_iiv_percent) {
  if (is.na(cv_percent) || is.na(rse_iiv_percent) || cv_percent <= 0) {
    return(rep(0, n))
  }

  cv <- cv_percent / 100
  se_cv <- (rse_iiv_percent / 100) * cv_percent
  sdlog <- sqrt(log(1 + (se_cv / cv_percent)^2))
  stats::rlnorm(n, meanlog = log(cv), sdlog = sdlog)
}

#' Convert sampled variability values to a covariance matrix
#'
#' @param omega_vec Numeric vector of sampled variability values.
#' @param R Correlation matrix between parameters.
#'
#' @return A symmetric positive semi-definite covariance matrix.
#' @keywords internal
#' @noRd
make_omega_cov <- function(omega_vec, R) {
  omega_vec <- as.numeric(omega_vec)
  p <- length(omega_vec)

  R <- as.matrix(R)
  if (is.null(dim(R))) {
    R <- matrix(R, nrow = p, ncol = p)
  }

  D <- diag(omega_vec, nrow = p, ncol = p)
  Omega <- as.matrix(D %*% R %*% D)
  if (is.null(dim(Omega))) {
    Omega <- matrix(Omega, nrow = p, ncol = p)
  }
  Omega <- (Omega + t(Omega)) / 2

  if (all(abs(Omega) < .Machine$double.eps^0.5)) {
    return(Omega)
  }

  npd <- tryCatch(Matrix::nearPD(Omega, corr = FALSE), error = function(e) NULL)
  if (!is.null(npd)) {
    npd_mat <- as.matrix(npd$mat)
    (npd_mat + t(npd_mat)) / 2
  } else {
    as.matrix(Omega + diag(1e-12, nrow(Omega)))
  }
}

#' Sample eta values for each parameter set
#'
#' @param n Number of samples to draw.
#' @param theta_names Character vector of parameter names.
#' @param omega_samples_by_param Matrix or data frame of sampled variability
#'   values by parameter.
#' @param R Correlation matrix between parameters.
#'
#' @return Tibble of eta samples with one column per parameter.
#' @keywords internal
#' @noRd
sample_etas <- function(n, theta_names, omega_samples_by_param, R) {
  p <- length(theta_names)
  out <- matrix(0, nrow = n, ncol = p)
  colnames(out) <- paste0("eta_", theta_names)

  for (i in seq_len(n)) {
    omegas <- as.numeric(omega_samples_by_param[i, , drop = FALSE])
    if (length(omegas) != p) {
      stop("omega vector length does not match number of parameters", call. = FALSE)
    }
    if (all(omegas == 0)) {
      next
    }

    Omega <- make_omega_cov(omegas, R)
    eta_i <- tryCatch(
      MASS::mvrnorm(n = 1, mu = rep(0, p), Sigma = Omega),
      error = function(e) {
        Omega2 <- as.matrix(Omega + diag(1e-10, p))
        MASS::mvrnorm(n = 1, mu = rep(0, p), Sigma = Omega2)
      }
    )

    zero_idx <- which(omegas == 0)
    if (length(zero_idx) > 0) {
      eta_i[zero_idx] <- 0
    }

    out[i, ] <- eta_i
  }

  tibble::as_tibble(out)
}

#' Sample theta values under optional bounds
#'
#' @param n Number of samples to draw.
#' @param params_tbl Parameter table containing `param`, `theta`,
#'   `rse_theta`, and optional `min` and `max` columns.
#'
#' @return Tibble of theta samples with one column per parameter.
#' @keywords internal
#' @noRd
sample_thetas <- function(n, params_tbl) {
  if (!"min" %in% names(params_tbl)) {
    params_tbl$min <- 0
  }
  if (!"max" %in% names(params_tbl)) {
    params_tbl$max <- Inf
  }

  purrr::pmap_dfc(
    list(
      params_tbl$param,
      params_tbl$theta,
      params_tbl$rse_theta,
      params_tbl$min,
      params_tbl$max
    ),
    function(par, th, rse, min_val, max_val) {
      min_val <- ifelse(is.na(min_val), -Inf, as.numeric(min_val))
      max_val <- ifelse(is.na(max_val), Inf, as.numeric(max_val))

      cv <- ifelse(is.na(rse) || rse == 0, 0, rse / 100)
      sd <- cv * abs(th)

      samples <- numeric(n)
      remaining <- seq_len(n)
      max_attempts <- 100
      attempt <- 0

      while (length(remaining) > 0 && attempt < max_attempts) {
        attempt <- attempt + 1
        new_samples <- stats::rnorm(length(remaining), mean = th, sd = sd)
        valid <- new_samples >= min_val & new_samples <= max_val
        samples[remaining[valid]] <- new_samples[valid]
        remaining <- remaining[!valid]
      }

      if (length(remaining) > 0) {
        samples[remaining] <- pmax(
          pmin(stats::rnorm(length(remaining), mean = th, sd = sd), max_val),
          min_val
        )
      }

      col <- list(samples)
      names(col) <- paste0("theta_", par)
      tibble::as_tibble(col)
    }
  )
}

#' Sample variability values for all parameters
#'
#' @param n Number of samples to draw.
#' @param params_tbl Parameter table containing `param`, `cv_iiv`, and
#'   `rse_iiv` columns.
#'
#' @return Tibble of sampled variability values with one column per parameter.
#' @keywords internal
#' @noRd
sample_omegas <- function(n, params_tbl) {
  ome <- purrr::pmap(
    list(params_tbl$cv_iiv, params_tbl$rse_iiv),
    ~ draw_omega(n, ..1, ..2)
  )
  names(ome) <- paste0("omega_", params_tbl$param)
  dplyr::bind_cols(ome)
}

#' Combine theta and eta samples into final parameter values
#'
#' @param theta_df Tibble of sampled theta values.
#' @param eta_df Tibble of sampled eta values.
#' @param param_names Character vector of parameter names to compose.
#' @param params_tbl Optional parameter table containing parameter bounds.
#'
#' @return Tibble of simulated parameter values.
#' @keywords internal
#' @noRd
compose_params <- function(theta_df, eta_df, param_names, params_tbl = NULL) {
  out <- tibble::tibble(.rows = nrow(theta_df))

  for (par in param_names) {
    tcol <- paste0("theta_", par)
    ecol <- paste0("eta_", par)

    if (!ecol %in% names(eta_df)) {
      out[[par]] <- theta_df[[tcol]]
    } else {
      out[[par]] <- theta_df[[tcol]] * exp(eta_df[[ecol]])
    }

    if (!is.null(params_tbl) && par %in% params_tbl$param) {
      par_row <- params_tbl[params_tbl$param == par, ]
      min_val <- if ("min" %in% names(par_row)) par_row$min else 0
      max_val <- if ("max" %in% names(par_row)) par_row$max else Inf
      min_val <- ifelse(is.na(min_val), -Inf, min_val)
      max_val <- ifelse(is.na(max_val), Inf, max_val)
      out[[par]] <- pmax(pmin(out[[par]], max_val), min_val)
    }
  }

  out
}

#' Optionally transpose the output table for display
#'
#' @param df Data frame or tibble to transform.
#' @param transpose Logical flag indicating whether rows and columns should be
#'   swapped.
#'
#' @return Tibble in the original or transposed orientation.
#' @keywords internal
#' @noRd
maybe_transpose <- function(df, transpose = FALSE) {
  if (!transpose) {
    return(df)
  }

  out <- tibble::as_tibble(t(df), rownames = "parameter")
  out$parameter <- as.character(out$parameter)
  out
}