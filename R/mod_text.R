#' text UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_text_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      uiOutput(ns("select_topicUI"))
    ),
    
    fluidRow(
      htmlOutput(ns("all_text"))
    )
  )
}

#' text Server Functions
#'
#' @noRd 
mod_text_server <- function(id, lda_model){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$select_topicUI <- renderUI({
      
      topics <- lda_model() %>% 
        purrr::pluck("gamma") %>% 
        dplyr::pull(topic) %>% 
        unique()
      
      selectInput(session$ns("select_topic"), "Select topic", choices = topics)
      
    })
    
    output$all_text <- renderText({
      
      analysis_df <- lda_model() %>% 
        purrr::pluck("gamma") %>% 
        dplyr::group_by(document) %>% 
        dplyr::arrange(desc(gamma)) %>% 
        dplyr::slice(1) %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(topic == input$select_topic) %>% 
        dplyr::left_join(
          nov_data %>% dplyr::mutate(response_id = as.character(response_id)), 
          by = c("document" = "response_id")
        )
      
      comments_text <- analysis_df %>% 
        dplyr::pull(Detail)
      
      paste0("<p>", comments_text, "</p>", collapse = "")
      
      # return(unlist(text_return))
      
    })
  })
}
