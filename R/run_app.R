#' Run the Shiny Application
#'
#' @param launch.browser Logical, passed to [shiny::runApp()].
#' @param ... arguments to pass to golem options.
#'
#' @export
#' @importFrom shiny shinyApp runApp
#' @importFrom golem with_golem_options
run_app <- function(launch.browser = TRUE, ...) {
  app <- golem::with_golem_options(
    app = shiny::shinyApp(ui = app_ui, server = app_server),
    golem_opts = list(...)
  )
  shiny::runApp(app, launch.browser = launch.browser)
}

#' Launch the Literature Simulator app
#'
#' @param launch.browser Logical, passed to [shiny::runApp()].
#' @param ... arguments to pass to golem options.
#'
#' @export
lit_sim <- function(launch.browser = TRUE, ...) {
  run_app(launch.browser = launch.browser, ...)
}
