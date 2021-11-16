#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    dashboardPage(
      dashboardHeader(),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Plots", tabName = "plots", icon = icon("chart-bar")),
          menuItem("Text", tabName = "text", icon = icon("book")),
          numericInput("no_topics", "Number of topics", 4, 
                       min = 2, max = 20)
        )
      ),
      dashboardBody(
        dashboardBody(
          tabItems(
            # First tab content
            tabItem(tabName = "plots",
                    "Hello",
                    mod_graphs_ui("graphs_ui_1")
            ),
            
            # Second tab content
            tabItem(tabName = "text",
                    h2("Widgets tab content")
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
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'ldaApp'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

