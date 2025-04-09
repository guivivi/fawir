output$var_query <- renderUI({
  df0 <<- input$load_data
  
  if (is.null(df0)) {
    queries <<- NULL
  }else{
    df0 <- read_excel(df0$datapath, col_names = TRUE)
    queries <- df0$query
  }
  
  selectInput("var_query", textOutput("var_query_txt"), queries, queries[1], multiple = TRUE)
})

do_run_algor <- eventReactive(input$go, {
  df0 <<- input$load_data

  if (is.null(df0)) {
    df0 <<- NULL
  }else{
    df0 <- read_excel(df0$datapath, col_names = TRUE)
    
    df0_ps <- pesmaster_sample %>% 
      select(-skills, -playing_style_ai) %>%
      unite("scouting_report", c(characteristics, skills_descr, playing_style_ai_descr), sep = ", ") %>%
      mutate(scouting_report = gsub(", NA", "", scouting_report)) 
    
    if (input$rank_value == "") {
      rank_value <- 50
    }else{
      rank_value <- input$rank_value
    }
    
    if (input$skip_grams_window_value == "") {
      skip_grams_window_value <- 5
    }else{
      skip_grams_window_value <- input$skip_grams_window_value
    }
    
    if (input$x_max_value == "") {
      x_max_value <- 10
    }else{
      x_max_value <- input$x_max_value
    }
    
    if (input$top_simil == "") {
      top_simil <- 3
    }else{
      top_simil <- input$x_max_value
    }
    
    queries <- input$var_query
    if (is.null(queries)) {
      res_def <- NULL
    }else{
      res_def <- data.frame()
      for (i in 1:length(queries)) {
        res <- do_player_recruitment(df0_ps, rank_value, skip_grams_window_value, x_max_value, 
                                     input$alpha, top_simil, input$max_scaling, queries[i])
        
        res_iter <- res %>% 
          select(name, team, position, league, playing_style) %>%
          mutate(query = queries[i])
        
        res_def <- rbind(res_def, res_iter)
      }
    }
  }
  
  return(list(res_def = res_def))
})

output$results_table <- renderDataTable({
  df0 <- do_run_algor()$res_def
  
  datatable(df0, caption = paste0(ncol(df0), " variables."), 
            escape = FALSE, extensions = "Scroller", rownames = FALSE,
            options = list(dom = "Blfrtip", scrollX = TRUE, 
                           lengthMenu = c(5, 10), pageLength = 5,
                           columnDefs = list(list(className = "dt-center", targets = "_all"))))
})

output$results_downl <- downloadHandler(
  filename = "candidates.xlsx",
  content = function(file) {
    df0 <- do_run_algor()$res_def
    write.xlsx(df0, file)
  }
)
