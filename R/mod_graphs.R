#' graphs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_graphs_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      
      plotOutput(ns("tf_idf_plot"))
    ),
    fluidRow(
      htmlOutput(ns("top_five"))
    )
  )
}

#' graphs Server Functions
#'
#' @noRd 
mod_graphs_server <- function(id, lda_model){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$tf_idf_plot <- renderPlot({
      
      lda_model() %>%
        purrr::pluck("beta") %>%
        dplyr::group_by(topic) %>%
        dplyr::top_n(10, beta) %>%
        dplyr::ungroup() %>%
        ggplot2::ggplot(ggplot2::aes(term, beta, fill = as.factor(topic))) +
        ggplot2::geom_col(alpha = 0.8, show.legend = FALSE) +
        ggplot2::facet_wrap(~ topic, scales = "free_y") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = expression(beta),
                      title = "Highest word probabilities for each topic",
                      subtitle = "Different words are associated with different topics")
      
    })
    
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

