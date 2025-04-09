# R packages and credentials:
source("helpers.R")

# Data with the translations for the multilingual app:
load("translation.bin") 

options(shiny.maxRequestSize = 100 * 1024^2) 

shinyServer(function(input, output, session) {
  # Multilingual app:
  source(file.path("server", "transl_text.R"), local = TRUE)$value

  output$sidebar <- renderMenu({
    sidebarMenu(menuItem(textOutput("home_page"), icon = icon("home"),
                         menuSubItem(textOutput("about"), 
                                     tabName = "tab_info",
                                     icon = icon("info-circle")),
                         menuSubItem("Software", 
                                     tabName = "tab_software",
                                     icon = icon("desktop"))
                ),
                menuItem(textOutput("recruit"), tabName = "tab_recruit", icon = icon("magnifying-glass")
                )
    )
  })
  
  output$tab1_ui <- renderUI({
    source(file.path("ui", "tab_info.R"), local = TRUE)$value
  })
  
  output$tab2_ui <- renderUI({
    source(file.path("ui", "tab_software.R"), local = TRUE)$value
  })
  
  output$tab3_ui <- renderUI({
    source(file.path("ui", "tab_recruit.R"), local = TRUE)$value
  })
  
  source(file.path("server", "tab_recruit_server.R"), local = TRUE)$value
})
