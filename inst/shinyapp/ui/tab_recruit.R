tabItem(tabName = "tab_recruit",
        fluidRow(
        box(fileInput("load_data", textOutput("load_data_txt"), accept = c(".xlsx", ".xls")), 
            uiOutput("var_query"),
            textInput("rank_value", textOutput("rank_value_txt"), placeholder = "50"),
            textInput("skip_grams_window_value", textOutput("skip_grams_window_value_txt"), placeholder = "5"),
            textInput("x_max_value", textOutput("x_max_value_txt"), placeholder = "10"),
            width = 3),
        box(sliderInput("alpha", textOutput("alpha_txt"), min = 0, max = 1, value = 0.7, step = 0.1),
            checkboxInput("max_scaling", textOutput("max_scaling_txt"), FALSE), 
            textInput("top_simil", textOutput("top_simil_txt"), placeholder = "3"),
            hr(style = "border-top: 1px solid #000000;"),
            actionButton("go", textOutput("go_txt"), icon("paper-plane"), 
                         style="color: #fff; background-color: #ff333c; border-color: #2e6da4"),        
            width = 3),
        tabBox(
          tabPanel(textOutput("results_txt"),
                   downloadButton("results_downl", textOutput("results_downl_txt")),
                   br(),
                   br(),
                   withSpinner(DTOutput("results_table")),
          ),
          width = 6)
        )
)
