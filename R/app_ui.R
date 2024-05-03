#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinydashboard dashboardPage dashboardSidebar dashboardBody dashboardHeader
#' @importFrom shinyWidgets pickerInput
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      header = dashboardHeader(title = "report generator"),
      sidebar = dashboardSidebar(
        conditionalPanel('input.main_tabs == "eer"',
                         mod_page_ui("page")
                         ),
        conditionalPanel('input.main_tabs == "cr"',
                         mod_cardioSidebar_ui("mod_cr")
                         )
      ),
      body = dashboardBody(
        tabsetPanel(
          id = "main_tabs",
          tabPanel(
            title = "Employee examination report",
            value = "eer",
            mod_pagebody_ui('page')
          ),
          tabPanel(
            title = "Cardiovascular report",
            value = "cr",
            mod_cardioReport_ui('mod_cr')
          ),
          tabPanel(
            title = "SNP report",
            value = "sr",
            mod_pagebody_ui('page')
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
  add_resource_path("www",
                    app_sys("app/www"))

  tags$head(favicon(),
            bundle_resources(path = app_sys("app/www"),
                             app_title = "reportGenerator"))
  # Add here other external resources
  # for example, you can add shinyalert::useShinyalert())
}
