#' @title saveModelFile
#' @description
#' Save a pharmacokinetic model to a JSON file following the new BestDose model structure.
#' The structure includes: description (drug info, routes, covariates), model (primary params,
#' secondary equations, differential equations, output, error), and support_point.
#'
#' @param model_name Character string. Name of the model (without .json extension)
#' @param drug_name Character string. Name of the drug
#' @param route Character or list. Administration route(s) - can be "IV" or list of route objects
#' @param compartment Integer or list. Legacy number of compartments or
#'   object with number/central/peripheral output mapping.
#' @param model_version Numeric. Version number of the model
#' @param target Character. Target type (concentration, auc, other)
#' @param dose_unit Character. Dose unit (mg, g, etc.)
#' @param model_description Character string. Description of the model
#' @param model_citation Character string. Citation/reference for the model
#' @param pmx_path Character or NULL. Path to Pmetrics model file
#' @param primary_params List. Primary parameters with type/min/max
#' @param model_covariates List or NULL. Covariate interpolation settings
#' @param secondary_params List or NULL. Secondary parameter equations
#' @param initial_conditions List or NULL. Initial conditions
#' @param fa List or NULL. Bioavailability settings
#' @param lag List or NULL. Lag time settings
#' @param equations List. Differential equations
#' @param output_equations List. Output equations
#' @param error_type Character. Error model type (additive, proportional, gamma)
#' @param error_initial Numeric. Initial error value
#' @param error_c0 Numeric. Error coefficient c0
#' @param error_c1 Numeric. Error coefficient c1
#' @param error_c2 Numeric. Error coefficient c2
#' @param error_c3 Numeric. Error coefficient c3
#' @param cov_number Integer. Number of covariates
#' @param cov_names List. Covariate names
#' @param cov_labels List. Covariate labels
#' @param cov_units List. Covariate units
#' @param cov_types List. Covariate types (numeric, categorical, binary)
#' @param cov_values List. Covariate value ranges or categories
#' @param cov_descriptions List. Covariate descriptions
#' @param support_points List or NULL. Support points data
#' @param file_path Character string or NULL. Custom file path (optional)
#'
#' @return A list with success (logical), file_path (character), and error (character if failed)
#'
#' @export
saveModelFile <- function(
  model_name,
  drug_name,
  route,
  compartment,
  model_version = 1.0,
  target = "concentration",
  dose_unit = "mg",
  model_description = NULL,
  model_citation = NULL,
  pmx_path = NULL,
  primary_params = list(),
  model_covariates = NULL,
  secondary_params = NULL,
  initial_conditions = NULL,
  fa = NULL,
  lag = NULL,
  equations = list(),
  output_equations = list(),
  error_type = "additive",
  error_initial = 0.1,
  error_c0 = 0,
  error_c1 = 0.1,
  error_c2 = 0,
  error_c3 = 0,
  cov_number = 0,
  cov_names = list(),
  cov_labels = list(),
  cov_units = list(),
  cov_types = list(),
  cov_values = list(),
  cov_descriptions = list(),
  support_points = NULL,
  file_path = NULL
) {
  tryCatch(
    {
      # Clean model name
      model_name <- gsub("\\.json$", "", model_name)
      file_name <- paste0(model_name, ".json")

      # Construct file path if not provided
      if (is.null(file_path)) {
        file_path <- get_writable_model_file_path(model_name)
      }

      # Build covariates structure for description section
      covariates_desc <- NULL
      if (cov_number > 0 && length(cov_names) > 0) {
        cov_values_formatted <- list()
        for (i in seq_along(cov_names)) {
          cov_values_formatted[[cov_names[[i]]]] <- if (
            length(cov_values) >= i
          ) {
            cov_values[[i]]
          } else {
            list()
          }
        }

        covariates_desc <- list(
          number = cov_number,
          names = cov_names,
          label = cov_labels,
          units = cov_units,
          types = cov_types,
          value = cov_values_formatted,
          description = cov_descriptions
        )
      }

      # create the name for the description (named list)
      if (!is.null(covariates_desc)) {
        names(covariates_desc$description) <- cov_names
      }

      # Build model structure following new format
      compartment_definition <- if (is.list(compartment)) {
        list(
          number = as.integer(compartment$number %||% 1L),
          central = as.integer(compartment$central %||% 1L),
          peripheral = if (
            !is.null(compartment$peripheral) &&
            !is.na(suppressWarnings(as.integer(compartment$peripheral)))
          ) {
            as.integer(compartment$peripheral)
          } else {
            NULL
          }
        )
      } else {
        compartment_number <- suppressWarnings(as.integer(compartment))
        if (is.na(compartment_number) || compartment_number < 1L) compartment_number <- 1L
        list(
          number = compartment_number,
          central = 1L,
          peripheral = if (compartment_number >= 2L) 2L else NULL
        )
      }

      model_data <- list(
        description = list(
          drug = drug_name,
          route = route,
          name = file_name,
          pmx_path = pmx_path,
          compartment = compartment_definition,
          version = model_version,
          target = target,
          dose_unit = dose_unit,
          target_unit = "mg/L", # placeholder for now. Might all be handled by the drug file in the future
          description = if (
            !is.null(model_description) && model_description != ""
          ) {
            model_description
          } else {
            NULL
          },
          reference = if (!is.null(model_citation) && model_citation != "") {
            model_citation
          } else {
            NULL
          },
          covariates = covariates_desc
        ),
        model = list(
          primary = primary_params,
          covariates = model_covariates,
          secondary = secondary_params,
          initial_conditions = initial_conditions,
          fa = fa,
          lag = lag,
          equation = equations,
          out = output_equations,
          error = list(
            type = error_type,
            initial_value = error_initial,
            coefficient = list(list(
              c0 = error_c0,
              c1 = error_c1,
              c2 = error_c2,
              c3 = error_c3
            ))
          )
        ),
        support_point = support_points
      )

      # Write JSON file
      dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
      jsonlite::write_json(
        model_data,
        path = file_path,
        pretty = TRUE,
        auto_unbox = TRUE,
        null = "null"
      )

      list(success = TRUE, file_path = file_path)
    },
    error = function(e) {
      warning(paste("Failed to save model file:", e$message))
      list(success = FALSE, error = e$message)
    }
  )
}


