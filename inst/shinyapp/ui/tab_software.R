tabItem(tabName = "tab_software",
        use_gotop(
                src = "fas fa-chevron-circle-up", # css class from Font Awesome
                color = "tomato", # color
                opacity = 0.8, # transparency
                width = 30, # size
                appear = 100 # number of pixels before appearance
        ),
        div(p(HTML("&nbsp;"), textOutput("softw_used"),
              #span("R.", style = "color:blue")
              ),
            img(src = "Rlogo.png", height = "50px", width = "50px"), 
            span(HTML("&nbsp;"),
                 a(href = "https://www.r-project.org/", "https://www.r-project.org/", 
                   target="_blank")),
                     style = "text-align: left;"),
        br(),
        br(),
        p(textOutput("interf")),
        p(a(href = "https://CRAN.R-project.org/package=shiny", "shiny: ", 
                target="_blank"), 
          HTML("&nbsp;"),
          span("Chang, W., Cheng, J., Allaire, J.J., Xie, Y. and McPherson, J. (2019)."),  
          strong("shiny: Web Application Framework for R."),  
          span("R package version 1.4.0")),
        br(),
        p(textOutput("addit_pack")),
        
        p(a(href = "https://CRAN.R-project.org/package=DT", "DT: ", 
            target="_blank"), 
          HTML("&nbsp;"),
          span("Xie, Y., Cheng, J. and Tan, X. (2024)."),  
          strong("DT: A Wrapper of the JavaScript Library 'DataTables'."), 
          span("R package version 0.33")),
        
        p(a(href = "https://github.com/guivivi/fawir", "fawir: ", 
            target="_blank"), 
          HTML("&nbsp;"),
          span("Vinue, G. (2025)."),  
          strong("fawir: Football Analytics with R."), 
          span("R package version 1.0")),
        
        p(a(href = "https://CRAN.R-project.org/package=gotop", "gotop: ", 
            target="_blank"), 
          HTML("&nbsp;"),
          span("Luginbuhl, F. (2024)."),  
          strong("gotop: Scroll Back to Top Icon in Shiny and R Markdown."), 
          span("R package version 0.1.4")),
        
        p(a(href = "https://CRAN.R-project.org/package=readxl", "readxl: ", 
            target="_blank"), 
          HTML("&nbsp;"),
          span("Wickham, H. and Bryan, J. (2023)."),  
          strong("readxl: Read Excel Files."), 
          span("R package version 1.4.3")),
        
        p(a(href = "https://CRAN.R-project.org/package=shinydashboard", "shinydashboard: ", 
            target="_blank"), 
          HTML("&nbsp;"),
          span("Chang, W. and Borges Ribeiro, B. (2018)."),  
          strong("shinydashboard: Create Dashboards with 'Shiny'."), 
          span("R package version 0.7.1")),
        
        p(a(href = "https://CRAN.R-project.org/package=shinycssloaders", "shinycssloaders: ", 
            target="_blank"), 
          HTML("&nbsp;"),
          span("Sali, A. and Attali, D. (2020)."), 
          strong("shinycssloaders: Add CSS Loading Animations to 'shiny' Outputs."), 
          span("R package version 0.3")),
        
        p(a(href = "https://CRAN.R-project.org/package=shinyWidgets", "shinyWidgets: ", 
            target="_blank"), 
          HTML("&nbsp;"),
          span("Perrier, V., Meyer, F. and Granjon, D. (2024)."), 
          strong("shinyWidgets: Custom Inputs Widgets for Shiny."), 
          span("R package version 0.8.7")),
        
        p(a(href = "https://CRAN.R-project.org/package=tidyr", "tidyr: ", 
            target="_blank"), 
          HTML("&nbsp;"),
          span("Wickham, H., Vaughan, D. and Girlich, M. (2024)."), 
          strong("tidyr: Tidy Messy Data."), 
          span("R package version 1.3.1"))
)