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
    
    htmlOutput(ns("top_five"))
    
  )
}

#' text Server Functions
#'
#' @noRd 
mod_text_server <- function(id, lda_model){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$top_five <- renderText({
      
      analysis_df <- lda_model() %>% 
        purrr::pluck("gamma") %>% 
        dplyr::group_by(topic) %>% 
        dplyr::top_n(5, gamma) %>% 
        dplyr::ungroup() %>% 
        dplyr::left_join(
          nov_data %>% dplyr::mutate(response_id = as.character(response_id)), 
          by = c("document" = "response_id")
        ) %>% 
        dplyr::arrange(topic)
      
      text_return <- purrr::map(unique(analysis_df$topic), function(x) {
        
        comments_text <- analysis_df %>% 
          dplyr::filter(topic == x) %>% 
          dplyr::pull(Detail)
        
        paste0("<h3>Topic ", x, "</h3>", 
               paste0("<p>", comments_text, "</p>", collapse = "")
        )
      })

      return(unlist(text_return))
      
    })
  })
}
