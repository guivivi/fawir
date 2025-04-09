tr <- function(text){ 
  sapply(text, function(s) translation[[s]][[input$language]], USE.NAMES = FALSE)
} 

output$choose_lang <- renderText({
  tr("choose_lang")
})


output$home_page <- renderText({
  tr("home_page")
})


output$about <- renderText({
  tr("about")
})

output$recruit <- renderText({
  tr("recruit")
})

output$thanks <- renderText({
  tr("thanks")
})

output$softw_used <- renderText({
  tr("softw_used")
})

output$interf <- renderText({
  tr("interf")
})

output$addit_pack <- renderText({
  tr("addit_pack")
})

output$load_data_txt <- renderText({
  tr("load_data_txt")
})

output$rank_value_txt <- renderText({
  tr("rank_value_txt")
})

output$skip_grams_window_value_txt <- renderText({
  tr("skip_grams_window_value_txt")
})

output$x_max_value_txt <- renderText({
  tr("x_max_value_txt")
})

output$alpha_txt <- renderText({
  tr("alpha_txt")
})

output$top_simil_txt <- renderText({
  tr("top_simil_txt")
})

output$max_scaling_txt <- renderText({
  tr("max_scaling_txt")
})

output$go_txt <- renderText({
  tr("go_txt")
})

output$results_txt <- renderText({
  tr("results_txt")
})

output$results_downl_txt <- renderText({
  tr("results_downl_txt")
})

output$info_text1 <- renderText({
  tr("info_text1")
})

output$var_query_txt <- renderText({
  tr("var_query_txt")
})
