#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    shiny::fluidPage(
      title = "Pmetrics Literature Simulator",
      
      shiny::titlePanel(
        shiny::div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px;",
        shiny::div(
          "Sampling from Parametric Literature Models",
          shiny::tags$small(style = "color: #6c757d; font-size: 12px; display: block; margin-top: 2px;",
          "Monte Carlo sampling with parameter uncertainty")
        ),
        shiny::img(src = "Pmetrics_logo.png", height = "60px", style = "margin-left: 15px;")
      )
    ),
    
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        
        # --- Simulation Settings ---
        shiny::div(class = "section-card",
        shiny::div(class = "section-title", shiny::icon("cog"), " Simulation Settings"),
        shiny::numericInput("n", "Samples:", value = 1000, min = 1, step = 100),
        shiny::actionLink("show_uncommon_settings", "Uncommon options...", class = "info-link"),
        shiny::uiOutput("uncommon_settings_ui")
      ),
      
      # --- Parameters Section ---
      shiny::div(class = "section-card",
      shiny::div(class = "section-title", shiny::icon("table"), " Parameters"),
      shiny::radioButtons("param_mode", NULL,
      choices = c("Manual" = "manual", "Upload CSV" = "upload"),
      inline = TRUE, selected = "manual"),
      shiny::fluidRow(
        shiny::column(6, shiny::radioButtons("iiv_input_type", "IIV:",
        choices = c("CV%" = "cv", "omega^2" = "omega2"),
        inline = TRUE, selected = "cv")),
        shiny::column(6, shiny::radioButtons("rse_input_type", "Uncertainty:",
        choices = c("RSE%" = "rse", "SE" = "se"),
        inline = TRUE, selected = "rse"))
      ),
      
      shiny::conditionalPanel(
        condition = "input.param_mode == 'manual'",
        DT::DTOutput("param_editor"),
        shiny::div(class = "btn-row",
        shiny::actionButton("param_add_row", "Add", icon = shiny::icon("plus"), class = "btn-sm btn-outline-secondary"),
        shiny::actionButton("param_del_row", "Delete", icon = shiny::icon("trash"), class = "btn-sm btn-outline-danger"),
        shiny::downloadButton("download_params_csv", "Save", icon = shiny::icon("download"), class = "btn-sm btn-outline-success")
      ),
      shiny::tags$small(
        shiny::actionLink("load_defaults", "Load example", style = "color: #6c757d;")
      )
    ),
    shiny::conditionalPanel(
      condition = "input.param_mode == 'upload'",
      shiny::fileInput("param_file", NULL, accept = ".csv", 
      placeholder = "Choose CSV file..."),
      shiny::actionLink("show_param_format", "Show CSV format", class = "info-link"),
      shiny::uiOutput("param_format_info")
    )
  ),
  
  # --- Correlations Section ---
  shiny::div(class = "section-card",
  shiny::div(class = "section-title", shiny::icon("project-diagram"), " ETA Correlations ", 
  shiny::tags$small("(optional)", style = "color: #adb5bd; font-weight: normal;")),
  shiny::radioButtons("corr_mode", NULL,
  choices = c("Manual" = "manual", "Upload CSV" = "upload"),
  inline = TRUE, selected = "manual"),
  
  shiny::conditionalPanel(
    condition = "input.corr_mode == 'manual'",
    shiny::fluidRow(
      shiny::column(4, shiny::selectInput("edge_param_i", NULL, choices = NULL, selectize = FALSE)),
      shiny::column(4, shiny::selectInput("edge_param_j", NULL, choices = NULL, selectize = FALSE)),
      shiny::column(4, shiny::numericInput("edge_rho", NULL, value = 0, min = -1, max = 1, step = 0.05))
    ),
    shiny::actionButton("edge_add_btn", "Add", icon = shiny::icon("plus"), class = "btn-sm btn-info"),
    shiny::tags$hr(style = "margin: 10px 0;"),
    DT::DTOutput("edges_editor"),
    shiny::div(class = "btn-row",
    shiny::actionButton("edges_del_row", "Delete", icon = shiny::icon("trash"), class = "btn-sm btn-outline-danger"),
    shiny::downloadButton("download_corr_csv", "Save", icon = shiny::icon("download"), class = "btn-sm btn-outline-success")
  ),
  shiny::tags$small(
    shiny::actionLink("load_corr_defaults", "Load example", style = "color: #6c757d;")
  )
),
shiny::conditionalPanel(
  condition = "input.corr_mode == 'upload'",
  shiny::fileInput("corr_file", NULL, accept = ".csv",
  placeholder = "Choose CSV file..."),
  shiny::actionLink("show_corr_format", "Show CSV format", class = "info-link"),
  shiny::uiOutput("corr_format_info")
)
),

# --- Simulate Button ---
shiny::div(style = "text-align: center; margin-top: 10px;",
shiny::actionButton("simulate", "Simulate", icon = shiny::icon("play"), 
class = "btn-primary btn-lg", style = "width: 100%;")
)
),

shiny::mainPanel(
  width = 8,
  shiny::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      title = shiny::tagList(shiny::icon("table"), " Samples"),
      shiny::br(),
      DT::DTOutput("sample_table"),
      shiny::br(),
      shiny::fluidRow(
        #column(4, shiny::actionButton("save_csv_btn", "Save as CSV", icon = shiny::icon("save"), class = "btn-success")),
        shiny::column(4, shiny::downloadButton("download_sim_csv_btn", "Save as CSV", icon = shiny::icon("save"), class = "btn-success")),
        shiny::column(4, shiny::actionButton("copy_df_btn", "Copy as R Data Frame", icon = shiny::icon("clipboard"), class = "btn-info")),
        shiny::column(4, shiny::actionButton("exit_app_btn", "Exit", icon = shiny::icon("door-open"), class = "btn-danger"))
      )
    ),
    shiny::tabPanel(
      title = shiny::tagList(shiny::icon("chart-line"), " Histograms"),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(3, shiny::numericInput("hist_bins", "Bins:", value = 30, min = 5, max = 200, step = 5))
      ),
      shiny::plotOutput("param_histograms", height = "auto")
    ),
    shiny::tabPanel(
      title = shiny::tagList(shiny::icon("stethoscope"), " Diagnostics"),
      shiny::br(),
      shiny::h5(shiny::HTML('Sampled <span style="color:#888; font-style:italic;">(requested)</span> Parameter Summary')),
      DT::DTOutput("sampled_summary"),
      shiny::br(),
      shiny::h5(shiny::HTML('Sampled <span style="color:#888; font-style:italic;">(requested)</span> ETA Correlation Matrix')),
      DT::DTOutput("sampled_corr_matrix")
    )
  )
)
)
)
)
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )
  
  shiny::tags$head(
    golem::activate_js(),
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "PmetricsLitSim"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
