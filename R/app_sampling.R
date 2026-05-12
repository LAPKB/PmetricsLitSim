build_correlation <- function(params, edges) {
  p <- length(params)
  R <- diag(1, p)
  dimnames(R) <- list(params, params)

  if (!is.null(edges) && nrow(edges) > 0) {
    edges2 <- edges |>
      dplyr::mutate(
        i = as.character(i),
        j = as.character(j)
      ) |>
      dplyr::filter(i %in% params, j %in% params, i != j) |>
      dplyr::mutate(rho = pmax(pmin(as.numeric(rho), 0.9999), -0.9999))

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

draw_omega <- function(n, cv_percent, rse_iiv_percent) {
  if (is.na(cv_percent) || is.na(rse_iiv_percent) || cv_percent <= 0) {
    return(rep(0, n))
  }

  cv <- cv_percent / 100
  se_cv <- (rse_iiv_percent / 100) * cv_percent
  sdlog <- sqrt(log(1 + (se_cv / cv_percent)^2))
  stats::rlnorm(n, meanlog = log(cv), sdlog = sdlog)
}

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

      tibble::tibble(!!paste0("theta_", par) := samples)
    }
  )
}

sample_omegas <- function(n, params_tbl) {
  ome <- purrr::pmap(
    list(params_tbl$cv_iiv, params_tbl$rse_iiv),
    ~ draw_omega(n, ..1, ..2)
  )
  names(ome) <- paste0("omega_", params_tbl$param)
  dplyr::bind_cols(ome)
}

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

maybe_transpose <- function(df, transpose = FALSE) {
  if (!transpose) {
    return(df)
  }

  tibble::as_tibble(t(df), rownames = "parameter") |>
    dplyr::rename_with(~ "parameter", 1) |>
    dplyr::mutate(parameter = as.character(parameter))
}