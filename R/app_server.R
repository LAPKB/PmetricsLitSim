#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  params_tbl <- shiny::reactiveVal(default_params)
  edges_tbl  <- shiny::reactiveVal(default_corr_edges)
  last_saved_path <- shiny::reactiveVal(file.path(getwd(), "theta.csv")) # Default path
  
  
  # Toggle visibility for format info panels
  show_param_format_visible <- shiny::reactiveVal(FALSE)
  show_corr_format_visible <- shiny::reactiveVal(FALSE)
  show_uncommon_settings <- shiny::reactiveVal(FALSE)
  
  shiny::observeEvent(input$show_param_format, {
    show_param_format_visible(!show_param_format_visible())
  }, ignoreInit = TRUE)
  
  shiny::observeEvent(input$show_corr_format, {
    show_corr_format_visible(!show_corr_format_visible())
  }, ignoreInit = TRUE)
  
  shiny::observeEvent(input$show_uncommon_settings, {
    show_uncommon_settings(!show_uncommon_settings())
  }, ignoreInit = TRUE)
  
  # Uncommon settings UI
  output$uncommon_settings_ui <- shiny::renderUI({
    shiny::req(show_uncommon_settings())
    shiny::div(style = "margin-top: 10px; padding-top: 10px; border-top: 1px solid #e9ecef;",
    shiny::numericInput("seed", "Seed:", value = -17, min = -20, step = 1),
    shiny::checkboxInput("include_internals", "Include theta/eta columns", value = FALSE),
    shiny::checkboxInput("transpose_out", "Transpose output", value = FALSE)
  )
})

# Dynamic CSV format info based on IIV and Uncertainty selections
output$param_format_info <- shiny::renderUI({
  shiny::req(show_param_format_visible())
  
  # Build column names based on selections
  iiv_col <- if (input$iiv_input_type == "omega2") "omega2_iiv" else "cv_iiv"
  theta_unc_col <- if (input$rse_input_type == "se") "se_theta" else "rse_theta"
  iiv_unc_col <- if (input$rse_input_type == "se") "se_iiv" else "rse_iiv"
  
  # Build example values
  example_iiv <- if (input$iiv_input_type == "omega2") "0.25" else "52.8"
  example_theta_unc <- if (input$rse_input_type == "se") "0.11" else "19"
  example_iiv_unc <- if (input$rse_input_type == "se") "0.12" else "48"
  
  example_line <- paste0("CL,0.582,", example_theta_unc, ",", example_iiv, ",", example_iiv_unc)
  
  shiny::div(class = "format-info",
  shiny::tags$strong("Required:"), paste(" param, theta,", theta_unc_col, ",", iiv_col, ",", iiv_unc_col),
  shiny::tags$br(),
  shiny::tags$strong("Optional:"), " min, max",
  shiny::tags$pre(paste0("param,theta,", theta_unc_col, ",", iiv_col, ",", iiv_unc_col, "\n", example_line))
)
})

# Correlation CSV format info
output$corr_format_info <- shiny::renderUI({
  shiny::req(show_corr_format_visible())
  
  shiny::div(class = "format-info",
  shiny::tags$strong("Required:"), " i, j, rho",
  shiny::tags$pre("i,j,rho\nKm,CL,-0.685")
)
})

# --- PARAM editor (manual mode) ---
output$param_editor <- DT::renderDT({
  df <- params_tbl()
  # Dynamic column names based on input types
  iiv_col_name <- if (input$iiv_input_type == "omega2") "IIV (ω²)" else "IIV (CV%)"
  theta_unc_name <- if (input$rse_input_type == "se") "Theta SE" else "Theta RSE%"
  iiv_unc_name <- if (input$rse_input_type == "se") "IIV SE" else "IIV RSE%"
  DT::datatable(
    df,
    colnames = c("Name", "Theta TV", theta_unc_name, iiv_col_name, iiv_unc_name, "Min", "Max"),
    selection = "multiple",
    rownames = FALSE,
    editable = list(target = "cell", disable = list(columns = c())), 
    options = list(dom = 'tip', scrollX = TRUE, pageLength = 8)
  )
})

shiny::observeEvent(input$param_editor_cell_edit, {
  info <- input$param_editor_cell_edit
  df <- params_tbl()
  r <- info$row 
  c <- info$col + 1
  val <- info$value
  num_cols <- c("theta", "rse_theta", "cv_iiv", "rse_iiv", "min", "max")
  if (names(df)[c] %in% num_cols) {
    # Handle "Inf" and "-Inf" strings
    val_str <- toupper(trimws(as.character(val)))
    if (val_str == "INF") {
      val <- Inf
    } else if (val_str == "-INF") {
      val <- -Inf
    } else {
      val <- suppressWarnings(as.numeric(val))
    }
  }
  df[r, c] <- val
  params_tbl(df)
})

shiny::observeEvent(input$param_add_row, {
  df <- params_tbl()
  new <- tibble::tibble(param = "New", theta = 0, rse_theta = 0, cv_iiv = 0, rse_iiv = 0, min = 0, max = Inf)
  params_tbl(dplyr::bind_rows(df, new))
})

shiny::observeEvent(input$param_del_row, {
  sel <- input$param_editor_rows_selected
  shiny::req(length(sel) > 0)
  df <- params_tbl()
  params_tbl(df[-sel, , drop = FALSE])
})

shiny::observe({
  p <- params_tbl()$param
  shiny::req(length(p) > 0)
  shiny::updateSelectInput(session, "edge_param_i", choices = p)
  shiny::updateSelectInput(session, "edge_param_j", choices = p, selected = if(length(p)>1) p[2] else p[1])
})

# --- EDGE editor (manual mode) ---
output$edges_editor <- DT::renderDT({
  df <- edges_tbl()
  DT::datatable(
    df,
    colnames = c("Parameter i", "Parameter j", "Rho"),
    selection = "multiple",
    rownames = FALSE,
    editable = list(target = "cell", disable = list(columns = c(0, 1))),
    options = list(dom = 't', scrollX = TRUE, pageLength = 6)
  )
})

shiny::observeEvent(input$edges_editor_cell_edit, {
  info <- input$edges_editor_cell_edit
  df <- edges_tbl()
  r <- info$row
  c <- info$col + 1
  val <- info$value
  if (names(df)[c] == "rho") {
    val <- suppressWarnings(as.numeric(val))
    df[r, c] <- val
    edges_tbl(df)
  }
})

shiny::observeEvent(input$edge_add_btn, {
  shiny::req(input$edge_param_i, input$edge_param_j, input$edge_rho)
  if(input$edge_param_i == input$edge_param_j) {
    shiny::showNotification("Parameter I and J cannot be the same.", type = "error")
    return()
  }
  df <- edges_tbl()
  is_match <- (df$i == input$edge_param_i & df$j == input$edge_param_j) |
  (df$i == input$edge_param_j & df$j == input$edge_param_i)
  if(any(is_match)) {
    df <- df[!is_match, ]
  }
  new_row <- tibble::tibble(
    i   = input$edge_param_i,
    j   = input$edge_param_j,
    rho = as.numeric(input$edge_rho)
  )
  edges_tbl(dplyr::bind_rows(df, new_row))
})

shiny::observeEvent(input$edges_del_row, {
  sel <- input$edges_editor_rows_selected
  shiny::req(length(sel) > 0)
  df <- edges_tbl()
  edges_tbl(df[-sel, , drop = FALSE])
})

shiny::observeEvent(input$load_defaults, { params_tbl(default_params) }, ignoreInit = TRUE)

shiny::observeEvent(input$load_corr_defaults, {
  edges_tbl(default_corr_edges)
}, ignoreInit = TRUE)

# --- SAVE PARAMETERS CSV (downloadHandler) ---
output$download_params_csv <- shiny::downloadHandler(
  filename = function() {
    "parameters.csv"
  },
  content = function(file) {
    readr::write_csv(params_tbl(), file)
  }
)

# --- SAVE CORRELATIONS CSV (downloadHandler) ---
output$download_corr_csv <- shiny::downloadHandler(
  filename = function() {
    "correlations.csv"
  },
  content = function(file) {
    readr::write_csv(edges_tbl(), file)
  }
)

shiny::observeEvent(input$param_file, {
  shiny::req(input$param_file)
  df <- readr::read_csv(input$param_file$datapath, show_col_types = FALSE)
  
  # Expected column names based on current settings
  iiv_col <- if (input$iiv_input_type == "omega2") "omega2_iiv" else "cv_iiv"
  theta_unc_col <- if (input$rse_input_type == "se") "se_theta" else "rse_theta"
  iiv_unc_col <- if (input$rse_input_type == "se") "se_iiv" else "rse_iiv"
  
  need_cols <- c("param", "theta", theta_unc_col, iiv_col, iiv_unc_col)
  miss <- setdiff(need_cols, names(df))
  if (length(miss) > 0) {
    shiny::showNotification(
      paste0("Error: Missing columns in parameters CSV: ", paste(miss, collapse=", "),
      "\nExpected: ", paste(need_cols, collapse=", ")), type = "error", duration = 8
    )
    return()
  }
  
  # Rename to internal standard names
  df <- df |>
  dplyr::rename(rse_theta = !!theta_unc_col, cv_iiv = !!iiv_col, rse_iiv = !!iiv_unc_col) |>
  dplyr::mutate(param = as.character(param)) |>
  dplyr::mutate(dplyr::across(c(theta, rse_theta, cv_iiv, rse_iiv), as.numeric))
  
  # Add min/max columns if missing, with defaults
  if (!"min" %in% names(df)) {
    df$min <- 0
  } else {
    df$min <- as.numeric(df$min)
  }
  if (!"max" %in% names(df)) {
    df$max <- Inf
  } else {
    df$max <- as.numeric(df$max)
  }
  df <- df |> dplyr::arrange(param)
  params_tbl(df)
  shiny::showNotification("Parameters updated from uploaded CSV.", type = "message", duration = 4)
  shiny::updateRadioButtons(session, "param_mode", selected = "manual")
}, ignoreInit = TRUE)

shiny::observeEvent(input$corr_file, {
  shiny::req(input$corr_file)
  df <- readr::read_csv(input$corr_file$datapath, show_col_types = FALSE)
  need_cols <- c("i","j","rho")
  miss <- setdiff(need_cols, names(df))
  shiny::validate(shiny::need(length(miss) == 0, paste("Missing columns in correlation edges CSV:", paste(miss, collapse=", "))))
  df <- df |>
  dplyr::mutate(i = as.character(i),
  j = as.character(j),
  rho = as.numeric(rho))
  edges_tbl(df)
  shiny::updateRadioButtons(session, "corr_mode", selected = "manual")
}, ignoreInit = TRUE)

# Simulation Logic
sim_result <- shiny::eventReactive(input$simulate, {
  n <- as.integer(input$n)
  shiny::validate(shiny::need(!is.na(n) && n > 0, "N must be a positive integer"))
  if (!is.null(input$seed) && !is.na(input$seed)) set.seed(as.integer(input$seed))
  
  par_df <- params_tbl()
  shiny::validate(shiny::need(nrow(par_df) > 0, "Parameter table is empty."))
  
  # Convert SE to RSE% if needed: RSE% = (SE / theta) * 100
  if (input$rse_input_type == "se") {
    par_df <- par_df |>
    dplyr::mutate(
      rse_theta = ifelse(theta != 0 & !is.na(rse_theta), (rse_theta / abs(theta)) * 100, 0),
      rse_iiv = ifelse(cv_iiv != 0 & !is.na(rse_iiv), (rse_iiv / cv_iiv) * 100, 0)
    )
  }
  
  # Convert omega² to CV% if needed: CV = sqrt(exp(omega²) - 1) * 100
  if (input$iiv_input_type == "omega2") {
    par_df <- par_df |>
    dplyr::mutate(cv_iiv = ifelse(cv_iiv > 0, sqrt(exp(cv_iiv) - 1) * 100, 0))
  }
  
  theta_df <- sample_thetas(n, par_df)
  omega_df <- sample_omegas(n, par_df)
  
  pname <- par_df$param
  R <- build_correlation(pname, edges_tbl())
  
  # Check if the requested correlation matrix is positive definite
  ev <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
  if (any(ev <= 0)) {
    min_ev <- min(ev)
    shiny::showNotification(
      paste0("Warning: The requested ETA correlation matrix is not positive definite ",
      "(min eigenvalue: ", round(min_ev, 4), "). ",
      "The matrix will be adjusted using nearPD() during sampling."),
      type = "warning",
      duration = 10
    )
  }
  
  omega_mat <- as.matrix(omega_df[, paste0("omega_", pname), drop = FALSE])
  eta_df <- sample_etas(n, pname, omega_mat, R)
  
  param_samples <- compose_params(theta_df, eta_df, pname, par_df)
  
  out <- param_samples
  if (isTRUE(input$include_internals)) {
    out <- dplyr::bind_cols(param_samples, theta_df, eta_df)
  }
  out <- maybe_transpose(out, transpose = isTRUE(input$transpose_out))
  
  list(
    samples = out,
    theta   = theta_df,
    omega   = tibble::as_tibble(omega_mat) |> purrr::set_names(paste0("omega_", pname)),
    params  = par_df,
    edges   = edges_tbl(),
    R       = R
  )
}, ignoreInit = TRUE)

# ---- Outputs ----
output$sample_table <- DT::renderDT({
  shiny::req(sim_result())
  df <- sim_result()$samples |>
  dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ round(., 3)))
  DT::datatable(df, options = list(scrollX = TRUE, pageLength = 10))
})

# --- SIMULATION RESULTS DOWNLOAD BUTTON ---
output$download_sim_csv_btn <- shiny::downloadHandler(
  filename = function() {
    "theta.csv"
  },
  content = function(file) {
    shiny::req(sim_result())
    readr::write_csv(sim_result()$samples, file)
  }
)

# --- COPY AS R DATA FRAME BUTTON ---
shiny::observeEvent(input$copy_df_btn, {
  shiny::req(sim_result())
  
  df <- sim_result()$samples
  
  # If transposed, we need to handle differently
  if (isTRUE(input$transpose_out)) {
    # Transposed: first column is parameter names, rest are samples
    # Need to convert back to wide format for matrix
    param_names <- df$parameter
    mat_data <- as.matrix(df[, -1])
    mat_data <- t(mat_data)  # Transpose so rows are samples
    colnames(mat_data) <- param_names
  } else {
    # Normal format: rows are samples, columns are parameters
    mat_data <- as.matrix(df)
  }
  
  # Build R code for data frame definition
  n_row <- nrow(mat_data)
  n_col <- ncol(mat_data)
  col_names <- colnames(mat_data)

  # Guardrails for editor/clipboard limits (especially long single-line paste in RStudio)
  max_cells_for_clipboard <- 50000L
  max_chars_for_clipboard <- 1000000L
  values_per_line <- 20L

  if ((n_row * n_col) > max_cells_for_clipboard) {
    shiny::showNotification(
      paste0(
        "Copy cancelled: dataset is too large for reliable script paste (",
        n_row, " x ", n_col, "). Use Save as CSV instead."
      ),
      type = "warning",
      duration = 8
    )
    return(invisible(NULL))
  }
  
  # Format each column as a vector
  col_strings <- sapply(seq_len(n_col), function(j) {
    col_vals <- mat_data[, j]
    formatted_vals <- format(col_vals, scientific = FALSE, trim = TRUE)
    value_groups <- split(formatted_vals, ceiling(seq_along(formatted_vals) / values_per_line))
    value_block <- paste(vapply(value_groups, paste, character(1), collapse = ", "), collapse = ",\n    ")
    sprintf('  %s = c(\n    %s\n  )', col_names[j], value_block)
  })
  
  # Join columns with comma and newline
  cols_str <- paste(col_strings, collapse = ",\n")
  
  # Build the full R expression for data frame
  r_code <- sprintf(
    'theta <- data.frame(\n%s\n)',
    cols_str
  )

  if (nchar(r_code, type = "chars") > max_chars_for_clipboard) {
    shiny::showNotification(
      paste0(
        "Copy cancelled: generated R code is too large for reliable clipboard/script paste. ",
        "Use Save as CSV instead."
      ),
      type = "warning",
      duration = 8
    )
    return(invisible(NULL))
  }
  
  # Copy to clipboard
  tryCatch({
    clipr::write_clip(r_code)
    shiny::showNotification(
      paste0("Copied theta data frame (", n_row, " x ", n_col, ") to clipboard. Paste into R to define."),
      type = "message",
      duration = 5
    )
  }, error = function(e) {
    shiny::showNotification(paste("Error copying to clipboard:", e$message), type = "error")
  })
})

# --- EXIT BUTTON LOGIC ---
shiny::observeEvent(input$exit_app_btn, {
  shiny::stopApp()
})

# Sampled parameter summary table - shows statistics from theta samples with requested values
output$sampled_summary <- DT::renderDT({
  shiny::req(sim_result())
  s <- sim_result()
  
  param_names <- s$params$param
  theta_df <- s$theta  # Raw theta samples (before eta applied)
  omega_df <- s$omega  # Sampled omega values (CV as decimal)
  n_obs <- nrow(theta_df)
  
  # Get the ORIGINAL input values (before any conversions)
  input_params <- params_tbl()
  
  # Calculate statistics from the RAW THETA samples
  theta_means <- sapply(param_names, function(p) {
    mean(theta_df[[paste0("theta_", p)]], na.rm = TRUE)
  })
  theta_sds <- sapply(param_names, function(p) {
    sd(theta_df[[paste0("theta_", p)]], na.rm = TRUE)
  })
  
  # Theta RSE% from samples: (SD/mean)*100
  theta_rse_sampled <- ifelse(theta_means != 0, abs(theta_sds / theta_means) * 100, 0)
  theta_se_sampled <- theta_sds
  
  # Omega/IIV from samples: mean of the sampled omega values (already in CV decimal)
  omega_means <- sapply(param_names, function(p) {
    mean(omega_df[[paste0("omega_", p)]], na.rm = TRUE)
  })
  omega_sds <- sapply(param_names, function(p) {
    sd(omega_df[[paste0("omega_", p)]], na.rm = TRUE)
  })
  
  # Convert omega (CV as decimal) to display format
  if (input$iiv_input_type == "cv") {
    iiv_sampled <- omega_means * 100
    iiv_se_sampled <- omega_sds * 100
  } else {
    iiv_sampled <- log(1 + omega_means^2)
    iiv_se_sampled <- ifelse(omega_means > 0, 
      2 * omega_means / (1 + omega_means^2) * omega_sds, 
      0)
    }
    
    # RSE% of IIV: (SE/mean)*100
    iiv_rse_sampled <- ifelse(iiv_sampled != 0, abs(iiv_se_sampled / iiv_sampled) * 100, 0)
    
    # Theta uncertainty in requested format
    if (input$rse_input_type == "rse") {
      theta_unc_sampled <- theta_rse_sampled
    } else {
      theta_unc_sampled <- theta_se_sampled
    }
    
    # IIV uncertainty in requested format
    if (input$rse_input_type == "rse") {
      iiv_unc_sampled <- iiv_rse_sampled
    } else {
      iiv_unc_sampled <- iiv_se_sampled
    }
    
    # Min/max from the FINAL composed samples (theta * exp(eta))
    samples <- s$samples
    if (isTRUE(input$transpose_out)) {
      pnames <- samples$parameter
      mat_data <- as.matrix(samples[, -1])
      mat_data <- t(mat_data)
      colnames(mat_data) <- pnames
      samples <- tibble::as_tibble(mat_data)
    }
    
    # Get requested (input) values
    req_theta <- input_params$theta[match(param_names, input_params$param)]
    req_theta_unc <- input_params$rse_theta[match(param_names, input_params$param)]
    req_iiv <- input_params$cv_iiv[match(param_names, input_params$param)]
    req_iiv_unc <- input_params$rse_iiv[match(param_names, input_params$param)]
    req_min <- input_params$min[match(param_names, input_params$param)]
    req_max <- input_params$max[match(param_names, input_params$param)]
    
    # Helper to format cell with sampled and requested values
    format_cell <- function(sampled, requested) {
      sampled_str <- round(sampled, 3)
      requested_str <- if (is.infinite(requested)) {
        ifelse(requested > 0, "Inf", "-Inf")
      } else {
        round(requested, 3)
      }
      paste0('<span style="font-weight:500;">', sampled_str, '</span>',
      '<br><span style="color:#888; font-style:italic; font-size:11px;">', 
      requested_str, '</span>')
    }
    
    # Sampled min/max from the FINAL composed samples
    sampled_min <- sapply(param_names, function(p) min(samples[[p]], na.rm = TRUE))
    sampled_max <- sapply(param_names, function(p) max(samples[[p]], na.rm = TRUE))
    
    # Build display dataframe with HTML formatting
    summary_df <- tibble::tibble(
      param = param_names,
      theta = mapply(format_cell, theta_means, req_theta),
      rse_theta = mapply(format_cell, theta_unc_sampled, req_theta_unc),
      cv_iiv = mapply(format_cell, iiv_sampled, req_iiv),
      rse_cv = mapply(format_cell, iiv_unc_sampled, req_iiv_unc),
      min = mapply(format_cell, sampled_min, req_min),
      max = mapply(format_cell, sampled_max, req_max)
    )
    
    # Dynamic column names based on input types
    iiv_col_name <- if (input$iiv_input_type == "omega2") "IIV (ω²)" else "IIV (CV%)"
    theta_unc_name <- if (input$rse_input_type == "se") "Theta SE" else "Theta RSE%"
    iiv_unc_name <- if (input$rse_input_type == "se") "IIV SE" else "IIV RSE%"
    
    DT::datatable(summary_df, rownames = FALSE, escape = FALSE,
      colnames = c("Name", "Theta TV", theta_unc_name, iiv_col_name, iiv_unc_name, "Min", "Max"),
      options = list(dom = 't', scrollX = TRUE, pageLength = 20))
    })
    
    # Sampled ETA correlation matrix
    output$sampled_corr_matrix <- DT::renderDT({
      shiny::req(sim_result())
      s <- sim_result()
      
      param_names <- s$params$param
      
      # Get eta samples - need to compute them from theta and final samples
      theta_df <- s$theta
      samples <- s$samples
      if (isTRUE(input$transpose_out)) {
        pnames <- samples$parameter
        mat_data <- as.matrix(samples[, -1])
        mat_data <- t(mat_data)
        colnames(mat_data) <- pnames
        samples <- tibble::as_tibble(mat_data)
      }
      
      # Compute eta = log(final / theta) for each parameter
      # Only for parameters with IIV > 0
      eta_df <- tibble::tibble(.rows = nrow(samples))
      params_with_iiv <- s$params$param[s$params$cv_iiv > 0]
      
      for (p in params_with_iiv) {
        theta_col <- paste0("theta_", p)
        final_vals <- samples[[p]]
        theta_vals <- theta_df[[theta_col]]
        # eta = log(final / theta), but handle zeros/negatives
        eta_vals <- ifelse(final_vals > 0 & theta_vals > 0, 
          log(final_vals / theta_vals), 
          0)
          eta_df[[paste0("eta_", p)]] <- eta_vals
        }
        
        if (ncol(eta_df) < 2) {
          # Not enough parameters with IIV for correlation
          return(DT::datatable(
            data.frame(Note = "Need at least 2 parameters with IIV to compute correlation"),
            rownames = FALSE,
            options = list(dom = 't')
          ))
        }
        
        # Calculate correlation matrix of etas
        corr_mat <- cor(eta_df, use = "pairwise.complete.obs")
        
        # Clean up column names (remove "eta_" prefix for display)
        clean_names <- gsub("^eta_", "", colnames(corr_mat))
        colnames(corr_mat) <- clean_names
        rownames(corr_mat) <- clean_names
        
        # Build requested correlation matrix from edges
        req_corr_mat <- diag(1, length(clean_names))
        dimnames(req_corr_mat) <- list(clean_names, clean_names)
        edges <- edges_tbl()
        if (!is.null(edges) && nrow(edges) > 0) {
          for (k in seq_len(nrow(edges))) {
            pi <- as.character(edges$i[k])
            pj <- as.character(edges$j[k])
            rho <- as.numeric(edges$rho[k])
            if (pi %in% clean_names && pj %in% clean_names) {
              req_corr_mat[pi, pj] <- rho
              req_corr_mat[pj, pi] <- rho
            }
          }
        }
        
        # Format cells with sampled and requested values
        format_corr_cell <- function(sampled, requested) {
          paste0('<span style="font-weight:500;">', round(sampled, 3), '</span>',
          '<br><span style="color:#888; font-style:italic; font-size:11px;">', 
          round(requested, 3), '</span>')
        }
        
        # Build formatted matrix
        formatted_mat <- matrix("", nrow = length(clean_names), ncol = length(clean_names))
        for (i in seq_along(clean_names)) {
          for (j in seq_along(clean_names)) {
            formatted_mat[i, j] <- format_corr_cell(
              corr_mat[clean_names[i], clean_names[j]],
              req_corr_mat[clean_names[i], clean_names[j]]
            )
          }
        }
        
        corr_df <- as.data.frame(formatted_mat)
        colnames(corr_df) <- clean_names
        corr_df <- cbind(Parameter = clean_names, corr_df)
        
        DT::datatable(corr_df, rownames = FALSE, escape = FALSE,
          options = list(dom = 't', scrollX = TRUE, pageLength = 20))
        })
        
        # --- HISTOGRAMS TAB ---
        output$param_histograms <- shiny::renderPlot({
          shiny::req(sim_result())
          
          # Get the samples (handle transposed case)
          df <- sim_result()$samples
          if (isTRUE(input$transpose_out)) {
            # Transposed: first column is parameter names, rest are samples
            param_names <- df$parameter
            mat_data <- as.matrix(df[, -1])
            mat_data <- t(mat_data)
            colnames(mat_data) <- param_names
            df <- tibble::as_tibble(mat_data)
          }
          
          # Get parameter names (exclude internal columns if present)
          param_names <- sim_result()$params$param
          df_plot <- df[, param_names, drop = FALSE]
          
          n_params <- length(param_names)
          n_bins <- if (is.null(input)) 30 else input
          
          # Calculate grid dimensions
          n_cols <- min(3, n_params)
          n_rows <- ceiling(n_params / n_cols)
          
          # Set up multi-panel plot
          par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 3, 1))
          
          for (par in param_names) {
            vals <- df_plot[[par]]
            hist(vals, breaks = n_bins, main = par, xlab = "", col = "steelblue", border = "white")
          }
        }, height = function() {
          shiny::req(sim_result())
          n_params <- length(sim_result()$params$param)
          n_cols <- min(3, n_params)
          n_rows <- ceiling(n_params / n_cols)
          n_rows * 250  # 250 pixels per row
        })
}
