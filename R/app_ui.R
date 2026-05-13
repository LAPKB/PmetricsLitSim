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
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
      /* Modern styling */
      body { 
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
        background-color: #f8f9fa;
      }
  
      .well {
        background-color: #ffffff;
        border: 1px solid #e9ecef;
        border-radius: 8px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.08);
      }
      h4 { 
        color: #495057;
        font-weight: 600;
        margin-top: 0;
        padding-bottom: 8px;
        border-bottom: 2px solid #e9ecef;
      }
      .form-group { margin-bottom: 12px; }
      
      /* DataTable styling */
      .dataTable input {
        color: black !important;
        font-weight: bold;
      }
      .dataTables_wrapper { font-size: 13px; }
      
      /* Button improvements */
      .btn { 
        border-radius: 6px;
        font-weight: 500;
        transition: all 0.2s ease;
      }
      .btn-sm { padding: 4px 12px; font-size: 12px; }
      .btn-primary { background-color: #0d6efd; border-color: #0d6efd; }
      .btn-primary:hover { background-color: #0b5ed7; }
      .btn-success { background-color: #198754; border-color: #198754; }
      .btn-info { background-color: #0dcaf0; border-color: #0dcaf0; color: #000; }
      .btn-danger { background-color: #dc3545; border-color: #dc3545; }
      
      /* Compact radio buttons */
      .radio-inline { margin-right: 15px; }
      .shiny-input-container:not(.shiny-input-container-inline) { margin-bottom: 10px; }
      
      /* Section cards */
      .section-card {
        background: #fff;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
      }
      .section-title {
        font-size: 14px;
        font-weight: 600;
        color: #495057;
        margin-bottom: 12px;
      }
      
      /* Help text styling */
      .help-block { 
        font-size: 11px; 
        color: #6c757d; 
        margin-top: 4px;
        margin-bottom: 8px;
      }
      
      /* Format info panel */
      .format-info {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 6px;
        padding: 12px;
        margin-top: 8px;
        font-size: 11px;
      }
      .format-info code {
        background-color: #e9ecef;
        padding: 1px 4px;
        border-radius: 3px;
        font-size: 10px;
      }
      .format-info pre {
        background-color: #e9ecef;
        padding: 8px;
        border-radius: 4px;
        font-size: 10px;
        margin: 8px 0 0 0;
      }
      .format-info ul { 
        padding-left: 18px; 
        margin: 4px 0;
      }
      .format-info li { margin-bottom: 2px; }
      
      /* Info link styling */
      .info-link {
        color: #6c757d;
        font-size: 11px;
        text-decoration: none;
      }
      .info-link:hover { color: #0d6efd; }
      
      /* Button row spacing */
      .btn-row { margin-top: 10px; }
      .btn-row .btn { margin-right: 5px; margin-bottom: 5px; }
      
      /* Main panel tabs */
      .nav-tabs > li > a { 
        border-radius: 6px 6px 0 0;
        font-weight: 500;
      }
      .tab-content { 
        background: #fff;
        border: 1px solid #dee2e6;
        border-top: none;
        border-radius: 0 0 8px 8px;
        padding: 20px;
      }
      
      /* Checkbox and input sizing */
      .checkbox label { font-size: 13px; }
      
      /* Horizontal rule */
      hr { border-color: #e9ecef; margin: 15px 0; }
    ")),
        shiny::tags$script(shiny::HTML("
      Shiny.addCustomMessageHandler('toggleCondition', function(message) {
        Shiny.setInputValue(message.name, message.value);
      });
      
      // Enable Tab/Shift-Tab navigation in editable DataTables
      $(document).on('keydown', '.dataTable input', function(e) {
        if (e.key === 'Tab') {
          e.preventDefault();
          var $input = $(this);
          var $cell = $input.closest('td');
          var $row = $cell.closest('tr');
          var $table = $row.closest('table');
          var cellIndex = $cell.index();
          var rowIndex = $row.index();
          var $allRows = $table.find('tbody tr');
          var numCols = $row.find('td').length;
          var numRows = $allRows.length;
          
          $input.blur();
          
          setTimeout(function() {
            var newCellIndex, newRowIndex;
            
            if (e.shiftKey) {
              newCellIndex = cellIndex - 1;
              newRowIndex = rowIndex;
              if (newCellIndex < 0) {
                newCellIndex = numCols - 1;
                newRowIndex = rowIndex - 1;
                if (newRowIndex < 0) newRowIndex = numRows - 1;
              }
            } else {
              newCellIndex = cellIndex + 1;
              newRowIndex = rowIndex;
              if (newCellIndex >= numCols) {
                newCellIndex = 0;
                newRowIndex = rowIndex + 1;
                if (newRowIndex >= numRows) newRowIndex = 0;
              }
            }
            
            var $targetRow = $allRows.eq(newRowIndex);
            var $targetCell = $targetRow.find('td').eq(newCellIndex);
            $targetCell.trigger('dblclick');
          }, 50);
        }
      });
    "))
      ),
      
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
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "PmetricsLitSim"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
