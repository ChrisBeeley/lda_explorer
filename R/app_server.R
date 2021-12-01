#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  # prep model
  
  lda_model <- reactive({
    
    remove_words <- stringr::str_split(input$remove_text, ",") %>% 
      unlist() %>% 
      stringr::str_trim(side = "both")
    
    unnest_data <- nov_data %>%
      dplyr::filter(!is.na(Detail)) %>%
      tidytext::unnest_tokens('word', "Detail") %>%
      consultations::text_remove_words(custom_words = remove_words)
    
    grouping_var <- "response_id"
    word_col <- "word"
    
    grouping_var <- consultations::prep_grouping_var(grouping_var)
    
    prep_data <- unnest_data %>%
      dplyr::count(!!grouping_var, word, sort = TRUE) %>%
      dplyr::group_by(!!grouping_var) %>%
      dplyr::mutate(total = sum(n)) %>%
      dplyr::arrange(-n) %>%
      dplyr::mutate(rank = dplyr::row_number(), `term frequency` = n/total) %>%
      tidytext::bind_tf_idf(word, !!grouping_var, n) %>%
      dplyr::arrange(desc(tf_idf)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(word = factor(word, levels = rev(unique(word))))
    
    dtm_prep <- tidytext::cast_dtm(prep_data, document = !!grouping_var,
                                   term = word, value = n)
    
    final_dtm <- list(data_scored = prep_data, dtm_prep = dtm_prep)
    
    consultations::text_lda_dtm(final_dtm$dtm_prep,
                                k = input$no_topics,
                                burnin = 1000,
                                iter = 1000,
                                keep = 50)
  })
  
  
  mod_graphs_server("graphs_ui_1", lda_model = lda_model)
  
  mod_text_server("text_ui_1", lda_model = lda_model)
}
