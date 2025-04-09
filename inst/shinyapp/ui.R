library(shiny)
library(shinydashboard)  # dashboardPage() 
library(shinyWidgets)    # pickerInput() 

# Flags for the countries of the multilingual app:
countries <- c("English", "Español", "Português")

flags <- paste0("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/", 
                c("gb", "es", "pt"), ".svg")

dashboardPage(skin = "green",
              dashboardHeader(
                  title = "2025"
                  ),
              dashboardSidebar(
                  pickerInput("language", "", choices = countries,
                              choicesOpt = list(content = mapply(countries, 
                                                                 flags, FUN = function(country, flagUrl) {
                                                                     HTML(paste(tags$img(src = flagUrl, width = 20, 
                                                                                         height = 15), country))
                                                                     }, SIMPLIFY = FALSE, USE.NAMES = FALSE))),
                  br(),
                  br(),
                  br(),
                  sidebarMenuOutput("sidebar")
                  ),
              dashboardBody(
                  tabItems(
                    # App info:
                    tabItem("tab_info", uiOutput("tab1_ui")),
                    # Software info:
                    tabItem("tab_software", uiOutput("tab2_ui")),
                    # Recruiting system:
                    tabItem("tab_recruit", uiOutput("tab3_ui"))
                    )
                  ),
              title = "2025"
              )
