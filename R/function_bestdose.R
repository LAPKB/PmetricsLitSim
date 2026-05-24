#' Split a multi-line BestDose code block into trimmed lines
#'
#' @param text Character scalar containing one or more lines of code.
#'
#' @return Character vector of non-empty trimmed lines.
#' @noRd
split_bestdose_code_block <- function(text) {
  if (is.null(text)) {
    return(character())
  }

  lines <- unlist(strsplit(as.character(text), "\\r?\\n", perl = TRUE), use.names = FALSE)
  lines <- trimws(lines)
  lines[nzchar(lines)]
}

#' Normalize assignment operators in BestDose code
#'
#' @param line Character scalar containing a single code line.
#'
#' @return Character scalar with `=` normalized to `<-` for evaluation.
#' @noRd
normalize_bestdose_assignment <- function(line) {
  line <- trimws(line)
  if (!nzchar(line) || grepl("<-", line, fixed = TRUE)) {
    return(line)
  }

  sub("=", "<-", line, fixed = TRUE)
}

#' Extract the left-hand side symbol from an assignment line
#'
#' @param line Character scalar containing a single code line.
#'
#' @return Character scalar or `NULL` if no simple symbol is found.
#' @noRd
extract_bestdose_lhs <- function(line) {
  matched <- regmatches(
    line,
    regexec("^\\s*([A-Za-z][A-Za-z0-9_.]*)\\s*(<-|=)", line, perl = TRUE)
  )[[1]]

  if (length(matched) < 2) {
    return(NULL)
  }

  matched[2]
}

#' Convert a text block into a named list of BestDose code lines
#'
#' @param text Character scalar containing a multi-line code block.
#' @param prefix Prefix used when names cannot be derived from the code.
#' @param use_lhs Logical indicating whether the left-hand side should be used
#'   as the list name when possible.
#'
#' @return Named list of code lines.
#' @noRd
named_bestdose_code_block <- function(text, prefix = "line", use_lhs = FALSE) {
  lines <- split_bestdose_code_block(text)
  if (length(lines) == 0) {
    return(list())
  }

  out <- as.list(lines)
  if (isTRUE(use_lhs)) {
    names(out) <- vapply(seq_along(lines), function(index) {
      lhs <- extract_bestdose_lhs(lines[[index]])
      if (is.null(lhs) || !nzchar(lhs)) {
        paste0(prefix, index)
      } else {
        lhs
      }
    }, character(1))
    names(out) <- make.unique(names(out), sep = "_")
  } else {
    names(out) <- paste0(prefix, seq_along(lines))
  }

  out
}

#' Extract symbols from a BestDose expression
#'
#' @param text Character scalar containing an expression.
#'
#' @return Character vector of unique symbols.
#' @noRd
extract_bestdose_symbols <- function(text) {
  matches <- regmatches(
    text,
    gregexpr("\\b[A-Za-z][A-Za-z0-9_.]*\\b", text, perl = TRUE)
  )[[1]]

  unique(matches)
}

#' Infer covariate names referenced by BestDose secondary equations
#'
#' @param model_code_text Character scalar containing secondary parameter code.
#' @param param_names Character vector of primary parameter names.
#'
#' @return Character vector of inferred covariate names.
#' @noRd
infer_bestdose_covariates <- function(model_code_text, param_names = character()) {
  lines <- split_bestdose_code_block(model_code_text)
  if (length(lines) == 0) {
    return(character())
  }

  lhs_names <- vapply(seq_along(lines), function(index) {
    lhs <- extract_bestdose_lhs(lines[[index]])
    if (is.null(lhs)) {
      ""
    } else {
      lhs
    }
  }, character(1))

  rhs_text <- vapply(lines, function(line) {
    normalized <- normalize_bestdose_assignment(line)
    pieces <- strsplit(normalized, "<-", fixed = TRUE)[[1]]
    if (length(pieces) >= 2) {
      trimws(pieces[2])
    } else {
      normalized
    }
  }, character(1))

  symbols <- unique(unlist(lapply(rhs_text, extract_bestdose_symbols), use.names = FALSE))
  reserved <- c(
    param_names,
    lhs_names[nzchar(lhs_names)],
    "X", "Y", "dx", "r", "t", "time",
    "exp", "log", "sqrt", "abs", "pmin", "pmax",
    "min", "max", "ifelse", "pi", "c", "TRUE", "FALSE"
  )

  symbols[!symbols %in% reserved]
}

#' Build default covariate specification text for the export modal
#'
#' @param covariate_name Character scalar.
#'
#' @return Character scalar in `name|label|unit|type|min|max|default|description` format.
#' @noRd
default_bestdose_covariate_line <- function(covariate_name) {
  label <- tools::toTitleCase(gsub("_", " ", covariate_name))
  unit <- ""
  min_value <- 0
  max_value <- 100
  default_value <- 1
  description <- paste(label, "covariate")

  if (grepl("weight|^wt$", covariate_name, ignore.case = TRUE)) {
    label <- "Weight"
    unit <- "kg"
    max_value <- 250
    default_value <- 70
    description <- "Patient weight"
  } else if (grepl("crcl|creat|egfr|renal", covariate_name, ignore.case = TRUE)) {
    label <- "Creatinine clearance"
    unit <- "mL/min"
    max_value <- 200
    default_value <- 100
    description <- "Renal function covariate"
  }

  paste(
    covariate_name,
    label,
    unit,
    "numeric",
    min_value,
    max_value,
    default_value,
    description,
    sep = " | "
  )
}

#' Build default covariate specification text for multiple covariates
#'
#' @param covariate_names Character vector.
#'
#' @return Character scalar containing one formatted line per covariate.
#' @noRd
default_bestdose_covariate_text <- function(covariate_names) {
  if (length(covariate_names) == 0) {
    return("")
  }

  paste(vapply(covariate_names, default_bestdose_covariate_line, character(1)), collapse = "\n")
}

#' Parse editable covariate specification text
#'
#' @param text Character scalar containing one formatted covariate definition per line.
#' @param fallback_names Character vector used to generate defaults when `text` is blank.
#'
#' @return Data frame with covariate metadata.
#' @noRd
parse_bestdose_covariate_specs <- function(text, fallback_names = character()) {
  lines <- split_bestdose_code_block(text)
  if (length(lines) == 0 && length(fallback_names) > 0) {
    lines <- split_bestdose_code_block(default_bestdose_covariate_text(fallback_names))
  }

  if (length(lines) == 0) {
    return(
      data.frame(
        name = character(),
        label = character(),
        unit = character(),
        type = character(),
        min = numeric(),
        max = numeric(),
        default = numeric(),
        description = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  parsed <- lapply(lines, function(line) {
    parts <- trimws(strsplit(line, "|", fixed = TRUE)[[1]])
    if (length(parts) < 8) {
      parts <- c(parts, rep("", 8 - length(parts)))
    }

    min_value <- suppressWarnings(as.numeric(parts[5]))
    max_value <- suppressWarnings(as.numeric(parts[6]))
    default_value <- suppressWarnings(as.numeric(parts[7]))

    if (is.na(min_value)) {
      min_value <- 0
    }
    if (is.na(max_value)) {
      max_value <- if (is.finite(min_value)) max(min_value, 1) else 1
    }
    if (is.na(default_value)) {
      default_value <- if (is.finite(min_value) && is.finite(max_value)) {
        (min_value + max_value) / 2
      } else {
        0
      }
    }

    data.frame(
      name = parts[1],
      label = if (nzchar(parts[2])) parts[2] else parts[1],
      unit = parts[3],
      type = if (nzchar(parts[4])) parts[4] else "numeric",
      min = min_value,
      max = max_value,
      default = default_value,
      description = if (nzchar(parts[8])) parts[8] else paste(parts[1], "covariate"),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, parsed)
}

#' Build model-level covariate interpolation settings
#'
#' @param covariate_specs Data frame returned by `parse_bestdose_covariate_specs()`.
#'
#' @return Named list of interpolation settings.
#' @noRd
build_bestdose_model_covariates <- function(covariate_specs) {
  if (nrow(covariate_specs) == 0) {
    return(NULL)
  }

  stats::setNames(
    replicate(nrow(covariate_specs), list(interp = "linear"), simplify = FALSE),
    covariate_specs$name
  )
}

#' Build export arguments for BestDose covariate metadata
#'
#' @param covariate_specs Data frame returned by `parse_bestdose_covariate_specs()`.
#'
#' @return Named list of arguments accepted by `saveModelFile()`.
#' @noRd
build_bestdose_covariate_args <- function(covariate_specs) {
  if (nrow(covariate_specs) == 0) {
    return(list(
      cov_number = 0,
      cov_names = list(),
      cov_labels = list(),
      cov_units = list(),
      cov_types = list(),
      cov_values = list(),
      cov_descriptions = list(),
      cov_defaults = list()
    ))
  }

  list(
    cov_number = nrow(covariate_specs),
    cov_names = as.list(covariate_specs$name),
    cov_labels = as.list(covariate_specs$label),
    cov_units = as.list(covariate_specs$unit),
    cov_types = as.list(covariate_specs$type),
    cov_values = lapply(seq_len(nrow(covariate_specs)), function(index) {
      list(list(
        min = covariate_specs$min[index],
        max = covariate_specs$max[index],
        default = covariate_specs$default[index]
      ))
    }),
    cov_descriptions = as.list(covariate_specs$description),
    cov_defaults = stats::setNames(as.list(covariate_specs$default), covariate_specs$name)
  )
}

#' Infer the number of compartments referenced by the model equations
#'
#' @param ode_text Character scalar containing ODE code.
#' @param output_text Character scalar containing output equations.
#'
#' @return Integer scalar.
#' @noRd
infer_bestdose_compartments <- function(ode_text, output_text = NULL) {
  all_text <- paste(c(split_bestdose_code_block(ode_text), split_bestdose_code_block(output_text)), collapse = " ")
  matched <- gregexpr("(?:X|dx)\\[(\\d+)\\]", all_text, perl = TRUE)
  tokens <- regmatches(all_text, matched)[[1]]

  if (length(tokens) == 0) {
    return(1L)
  }

  indices <- suppressWarnings(as.integer(gsub("[^0-9]", "", tokens)))
  indices <- indices[!is.na(indices)]
  if (length(indices) == 0) {
    1L
  } else {
    max(indices)
  }
}

#' Build BestDose primary parameter bounds from the simulated profiles
#'
#' @param params_tbl Parameter table used by the app.
#' @param sample_df Data frame containing sampled parameter values.
#'
#' @return Named list compatible with the BestDose JSON `primary` section.
#' @noRd
build_bestdose_primary <- function(params_tbl, sample_df) {
  param_names <- params_tbl$param
  param_types <- rep("ab", length(param_names))
  param_mins <- numeric(length(param_names))
  param_maxs <- numeric(length(param_names))

  for (index in seq_along(param_names)) {
    param_name <- param_names[index]
    sampled_values <- if (param_name %in% names(sample_df)) {
      as.numeric(sample_df[[param_name]])
    } else {
      numeric()
    }
    sampled_values <- sampled_values[is.finite(sampled_values)]

    sampled_min <- if (length(sampled_values) > 0) min(sampled_values) else NA_real_
    sampled_max <- if (length(sampled_values) > 0) max(sampled_values) else NA_real_

    requested_min <- suppressWarnings(as.numeric(params_tbl$min[index]))
    requested_max <- suppressWarnings(as.numeric(params_tbl$max[index]))

    lower <- if (!is.na(requested_min) && is.finite(requested_min)) requested_min else sampled_min
    upper <- if (!is.na(requested_max) && is.finite(requested_max)) requested_max else sampled_max

    if (!is.finite(lower)) {
      lower <- if (is.finite(sampled_min)) sampled_min else 0
    }
    if (!is.finite(upper)) {
      upper <- if (is.finite(sampled_max)) sampled_max else lower + 1
    }

    if (isTRUE(all(sampled_values >= 0, na.rm = TRUE)) && lower < 0) {
      lower <- 0
    }

    if (abs(upper - lower) < .Machine$double.eps^0.5) {
      delta <- max(abs(lower) * 0.1, 1e-6)
      lower <- if (lower >= 0) max(0, lower - delta) else lower - delta
      upper <- upper + delta
    }

    param_mins[index] <- lower
    param_maxs[index] <- upper
  }

  build_primary_parameters(param_names, param_types, param_mins, param_maxs)
}

#' Convert sampled parameter rows into BestDose support points
#'
#' @param sample_df Data frame containing sampled parameter values.
#' @param param_names Character vector of parameter names.
#' @param max_points Maximum number of support points to retain.
#'
#' @return List of support-point records.
#' @noRd
build_bestdose_support_points <- function(sample_df, param_names, max_points = 50) {
  if (nrow(sample_df) == 0 || length(param_names) == 0) {
    return(list())
  }

  point_count <- min(max(1L, as.integer(max_points)), nrow(sample_df))
  indices <- unique(as.integer(round(seq(1, nrow(sample_df), length.out = point_count))))
  selected <- sample_df[indices, param_names, drop = FALSE]
  probability <- 1 / length(indices)

  lapply(seq_len(nrow(selected)), function(index) {
    point <- as.list(as.numeric(selected[index, , drop = TRUE]))
    names(point) <- param_names
    point$prob <- probability
    point
  })
}

#' Convert support-point records to a preview table
#'
#' @param support_points List returned by `build_bestdose_support_points()`.
#'
#' @return Data frame.
#' @noRd
bestdose_support_points_to_df <- function(support_points) {
  if (length(support_points) == 0) {
    return(data.frame())
  }

  do.call(rbind.data.frame, c(lapply(support_points, as.data.frame), stringsAsFactors = FALSE))
}

#' Parse a numeric sequence from free-text input
#'
#' @param text Character scalar containing comma or whitespace separated numbers.
#' @param default Numeric vector returned when parsing yields no values.
#'
#' @return Numeric vector.
#' @noRd
parse_bestdose_numeric_sequence <- function(text, default = numeric()) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(default)
  }

  pieces <- unlist(strsplit(trimws(text), "[,;[:space:]]+", perl = TRUE), use.names = FALSE)
  values <- suppressWarnings(as.numeric(pieces))
  values <- values[is.finite(values)]

  if (length(values) == 0) {
    default
  } else {
    sort(unique(values))
  }
}

#' Evaluate a series of assignment expressions inside an environment
#'
#' @param lines Character vector of code lines.
#' @param env Environment in which the code should be evaluated.
#'
#' @return Invisibly returns `NULL`.
#' @noRd
evaluate_bestdose_assignments <- function(lines, env) {
  if (length(lines) == 0) {
    return(invisible(NULL))
  }

  for (line in lines) {
    expression <- parse(text = normalize_bestdose_assignment(line))[[1]]
    eval(expression, envir = env)
  }

  invisible(NULL)
}

#' Simulate BestDose concentration profiles from support points
#'
#' @param support_points_df Data frame containing primary parameters and `prob`.
#' @param param_names Character vector of primary parameter names.
#' @param secondary_lines Character vector of secondary equations.
#' @param ode_lines Character vector of ODE expressions.
#' @param output_lines Character vector of output equations.
#' @param covariates Named list of covariate default values.
#' @param dose_amount Numeric scalar dose amount.
#' @param dose_times Numeric vector of dose start times.
#' @param infusion_duration Numeric scalar infusion duration. Use `0` for bolus.
#' @param sample_times Numeric vector of time points to return.
#' @param n_compartments Integer number of compartments.
#' @param dose_compartment Integer compartment receiving the dose.
#'
#' @return Data frame containing simulated outputs for each support point and time.
#' @noRd
simulate_bestdose_profiles <- function(
  support_points_df,
  param_names,
  secondary_lines,
  ode_lines,
  output_lines,
  covariates,
  dose_amount,
  dose_times,
  infusion_duration,
  sample_times,
  n_compartments,
  dose_compartment = 1L
) {
  if (nrow(support_points_df) == 0 || length(param_names) == 0 || length(sample_times) == 0) {
    return(data.frame())
  }

  n_compartments <- max(1L, as.integer(n_compartments))
  dose_compartment <- min(max(1L, as.integer(dose_compartment)), n_compartments)
  state_names <- paste0("comp", seq_len(n_compartments))
  output_count <- max(1L, length(output_lines))
  output_names <- paste0("Y", seq_len(output_count))
  integration_times <- sort(unique(c(
    0,
    sample_times,
    dose_times,
    if (is.finite(infusion_duration) && infusion_duration > 0) dose_times + infusion_duration else numeric()
  )))

  covariate_values <- if (length(covariates) == 0) list() else covariates
  profile_list <- vector("list", nrow(support_points_df))

  for (index in seq_len(nrow(support_points_df))) {
    primary_values <- as.list(as.numeric(support_points_df[index, param_names, drop = TRUE]))
    names(primary_values) <- param_names

    ode_function <- function(time, state, parameters) {
      env <- new.env(parent = baseenv())
      list2env(primary_values, env)
      if (length(covariate_values) > 0) {
        list2env(covariate_values, env)
      }

      env$time <- time
      env$t <- time
      env$X <- as.numeric(state)
      env$r <- rep(0, n_compartments)
      if (is.finite(infusion_duration) && infusion_duration > 0 && length(dose_times) > 0) {
        active_doses <- dose_times[time >= dose_times & time < (dose_times + infusion_duration)]
        if (length(active_doses) > 0) {
          env$r[dose_compartment] <- length(active_doses) * dose_amount / infusion_duration
        }
      }

      env$dx <- rep(0, n_compartments)
      evaluate_bestdose_assignments(secondary_lines, env)
      evaluate_bestdose_assignments(ode_lines, env)
      list(as.numeric(env$dx))
    }

    events_data <- NULL
    if ((!is.finite(infusion_duration) || infusion_duration <= 0) && length(dose_times) > 0) {
      events_data <- data.frame(
        var = rep(state_names[dose_compartment], length(dose_times)),
        time = dose_times,
        value = rep(dose_amount, length(dose_times)),
        method = rep("add", length(dose_times)),
        stringsAsFactors = FALSE
      )
    }

    solved <- deSolve::ode(
      y = stats::setNames(rep(0, n_compartments), state_names),
      times = integration_times,
      func = ode_function,
      parms = NULL,
      events = if (is.null(events_data)) NULL else list(data = events_data),
      method = "lsoda"
    )

    solved_df <- as.data.frame(solved)
    output_matrix <- matrix(NA_real_, nrow = nrow(solved_df), ncol = output_count)
    colnames(output_matrix) <- output_names

    for (time_index in seq_len(nrow(solved_df))) {
      env <- new.env(parent = baseenv())
      list2env(primary_values, env)
      if (length(covariate_values) > 0) {
        list2env(covariate_values, env)
      }

      env$time <- solved_df$time[time_index]
      env$t <- solved_df$time[time_index]
      env$X <- as.numeric(solved_df[time_index, state_names, drop = TRUE])
      env$Y <- rep(NA_real_, output_count)

      evaluate_bestdose_assignments(secondary_lines, env)
      if (length(output_lines) > 0) {
        evaluate_bestdose_assignments(output_lines, env)
        output_matrix[time_index, ] <- as.numeric(env$Y[seq_len(output_count)])
      } else {
        output_matrix[time_index, 1] <- env$X[dose_compartment]
      }
    }

    profile_df <- data.frame(
      support_id = index,
      time = solved_df$time,
      prob = support_points_df$prob[index],
      output_matrix,
      check.names = FALSE
    )
    profile_list[[index]] <- profile_df[profile_df$time %in% sample_times, , drop = FALSE]
  }

  do.call(rbind, profile_list)
}

#' Summarize simulated BestDose profiles across support points
#'
#' @param profile_df Data frame returned by `simulate_bestdose_profiles()`.
#' @param output_name Output column to summarize.
#'
#' @return Data frame with mean and percentile summaries by time.
#' @noRd
summarise_bestdose_profiles <- function(profile_df, output_name = "Y1") {
  if (nrow(profile_df) == 0 || !output_name %in% names(profile_df)) {
    return(data.frame())
  }

  summary_rows <- lapply(sort(unique(profile_df$time)), function(current_time) {
    values <- profile_df[profile_df$time == current_time, output_name]
    probs <- profile_df[profile_df$time == current_time, "prob"]

    data.frame(
      time = current_time,
      mean = stats::weighted.mean(values, probs),
      p05 = as.numeric(stats::quantile(values, probs = 0.05, names = FALSE, type = 7)),
      p50 = as.numeric(stats::quantile(values, probs = 0.50, names = FALSE, type = 7)),
      p95 = as.numeric(stats::quantile(values, probs = 0.95, names = FALSE, type = 7)),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, summary_rows)
}

#' Build primary parameter definitions from the app parameter table
#'
#' @param params_tbl Parameter table from the Shiny app.
#' @param sample_df Data frame containing simulated parameter values.
#'
#' @return Named list compatible with the BestDose JSON `primary` section.
#' @noRd
getPrimary <- function(params_tbl, sample_df) {
  build_bestdose_primary(params_tbl, sample_df)
}
