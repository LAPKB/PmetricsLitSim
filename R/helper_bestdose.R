# ============================================================================____
# Model Structure Parsing Functions ====
# ============================================================================____

#' @title parse_route_structure
#' @description Parse route from model - can be string or array of objects
#' @param route Route data from model JSON
#' @return List with route names and compartment mappings
#' @export
parse_route_structure <- function(route) {
  if (is.null(route)) return(list(routes = character(0), compartments = list()))
  
  if (is.character(route)) return(list(routes = route, compartments = list()))
  
  if (is.list(route)) {
    routes <- vapply(route, function(r) r$route, character(1))
    compartments <- lapply(route, function(r) list(route = r$route, compartment = r$compartment))
    return(list(routes = routes, compartments = compartments))
  }
  
  list(routes = character(0), compartments = list())
}

#' @title parse_compartment_definition
#' @description Parse compartment definition from model description.
#' Supports both legacy integer format and new object format:
#' { number, central, peripheral }.
#' @param compartment Compartment definition from model JSON
#' @return List with number, central, peripheral fields
#' @export
parse_compartment_definition <- function(compartment) {
  if (is.null(compartment)) {
    return(list(number = 1L, central = 1L, peripheral = NULL))
  }

  if (is.list(compartment)) {
    number <- as.integer(compartment$number %||% 1L)
    central <- as.integer(compartment$central %||% 1L)
    peripheral <- compartment$peripheral
    if (!is.null(peripheral) && !is.na(suppressWarnings(as.integer(peripheral)))) {
      peripheral <- as.integer(peripheral)
    } else {
      peripheral <- NULL
    }

    if (is.na(number) || number < 1L) number <- 1L
    if (is.na(central) || central < 1L) central <- 1L

    return(list(number = number, central = central, peripheral = peripheral))
  }

  number <- suppressWarnings(as.integer(compartment))
  if (is.na(number) || number < 1L) number <- 1L

  list(
    number = number,
    central = 1L,
    peripheral = if (number >= 2L) 2L else NULL
  )
}

#' @title parse_primary_parameters
#' @description Parse primary parameters from model structure
#' @param primary Primary parameter list from model JSON
#' @return Data frame with parameter info
#' @export
parse_primary_parameters <- function(primary) {
  if (is.null(primary)) return(data.frame(name = character(), type = character(), min = numeric(), max = numeric()))
  
  param_names <- names(primary)
  params <- lapply(param_names, function(name) {
    p <- primary[[name]]
    list(name = name, type = p$type, min = p$min, max = p$max)
  })
  do.call(rbind.data.frame, c(params, stringsAsFactors = FALSE))
}

#' @title parse_secondary_parameters
#' @description Parse secondary parameter equations from model structure
#' @param secondary Secondary parameter list from model JSON
#' @return Named list of equations
#' @export
parse_secondary_parameters <- function(secondary) {
  if (is.null(secondary)) return(list())
  secondary
}

#' @title parse_model_equations
#' @description Parse differential equations from model structure
#' @param equation Equation list from model JSON
#' @return Character vector of equations
#' @export
parse_model_equations <- function(equation) {
  if (is.null(equation)) return(character())
  unlist(equation)
}

#' @title parse_output_equations
#' @description Parse output equations from model structure
#' @param out Output list from model JSON
#' @return Character vector of output equations
#' @export
parse_output_equations <- function(out) {
  if (is.null(out)) return(character())
  unlist(out)
}

#' @title parse_error_model
#' @description Parse error model from model structure
#' @param error Error list from model JSON
#' @return List with error model details
#' @export
parse_error_model <- function(error) {
  if (is.null(error)) return(list(type = "additive", initial_value = 0.1, coeff = list(c0 = 0, c1 = 0.1, c2 = 0, c3 = 0)))

  # Support both "coefficient" (original model format) and "coeff" (legacy)
  raw_coeff <- error$coefficient %||% error$coeff
  coeff <- if (!is.null(raw_coeff)) {
    if (is.list(raw_coeff) && length(raw_coeff) > 0) raw_coeff[[1]] else raw_coeff
  } else {
    list(c0 = 0, c1 = 0.1, c2 = 0, c3 = 0)
  }

  list(type = error$type %||% "additive", initial_value = error$initial_value %||% 0.1, coeff = coeff)
}

#' @title parse_covariates_description
#' @description Parse covariates from model description
#' @param covariates Covariates list from model description
#' @return List with structured covariate data
#' @export
parse_covariates_description <- function(covariates) {
  if (is.null(covariates)) return(list(number = 0, data = list()))
  
  list(
    number = covariates$number %||% length(covariates$names),
    names = covariates$names %||% list(),
    labels = covariates$label %||% list(),
    units = covariates$units %||% list(),
    types = covariates$types %||% list(),
    values = covariates$values %||% covariates$value %||% list(),
    descriptions = covariates$description %||% list()
  )
}

#' @title parse_model_covariates
#' @description Parse covariate interpolation settings from model section
#' @param covariates Covariates from model section
#' @return Named list with interpolation methods
#' @export
parse_model_covariates <- function(covariates) {
  if (is.null(covariates)) return(list())
  
  lapply(covariates, function(cov) {
    list(interp = cov$interp %||% "linear")
  })
}



# ============================================================================____
# Model Building Helper Functions ====
# ============================================================================____

#' @title build_route_structure
#' @description Build route structure for model JSON
#' @param routes Character vector of routes
#' @param compartments Optional named list mapping routes to compartments
#' @return Route structure for JSON
#' @export
build_route_structure <- function(routes, compartments = NULL) {
  if (length(routes) == 1 && is.null(compartments)) return(routes)
  
  lapply(seq_along(routes), function(i) {
    comp <- if (!is.null(compartments) && length(compartments) >= i) compartments[[i]] else i
    list(route = routes[i], compartment = comp)
  })
}

#' @title build_primary_parameters
#' @description Build primary parameters structure for model JSON
#' @param param_names Character vector of parameter names
#' @param param_types Character vector of parameter types (ab, fx, etc.)
#' @param param_mins Numeric vector of minimum values
#' @param param_maxs Numeric vector of maximum values
#' @return Primary parameters structure
#' @export
build_primary_parameters <- function(param_names, param_types, param_mins, param_maxs) {
  params <- list()
  for (i in seq_along(param_names)) {
    params[[param_names[i]]] <- list(type = param_types[i], min = param_mins[i], max = param_maxs[i])
  }
  params
}

#' @title build_error_structure
#' @description Build error model structure for model JSON
#' @param type Error type (additive, proportional, gamma)
#' @param initial_value Initial error value
#' @param c0 Coefficient c0
#' @param c1 Coefficient c1
#' @param c2 Coefficient c2
#' @param c3 Coefficient c3
#' @return Error structure
#' @export
build_error_structure <- function(type = "additive", initial_value = 0.1, c0 = 0, c1 = 0.1, c2 = 0, c3 = 0) {
  list(type = type, initial_value = initial_value, coeff = list(list(c0 = c0, c1 = c1, c2 = c2, c3 = c3)))
}

#' @title build_covariates_description
#' @description Build covariates structure for model description section
#' @param cov_names Character vector of covariate names
#' @param cov_labels Character vector of covariate labels
#' @param cov_units Character vector of covariate units
#' @param cov_types Character vector of covariate types
#' @param cov_values List of covariate value ranges
#' @param cov_descriptions Character vector of descriptions
#' @return Covariates structure for description section
#' @export
build_covariates_description <- function(cov_names, cov_labels, cov_units, cov_types, cov_values, cov_descriptions) {
  if (length(cov_names) == 0) return(NULL)
  
  values <- lapply(seq_along(cov_names), function(i) {
    val <- list()
    val[[cov_names[i]]] <- cov_values[[i]]
    val
  })
  
  list(number = length(cov_names), names = as.list(cov_names), label = as.list(cov_labels), units = as.list(cov_units), types = as.list(cov_types), value = values, description = as.list(cov_descriptions))
}




#' Null-coalescing operator
#' @param x Value to check
#' @param y Default value if x is NULL
`%||%` <- function(x, y) if (is.null(x)) y else x
