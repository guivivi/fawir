library(dplyr)
tabItem(tabName = "tab_info",
        p(textOutput("info_text1") %>% withSpinner()),
        p("Guillermo Vinué"),
        p("guillermovinue@gmail.com"),
        img(src = "paper.png", height = "550px", width = "900px")
)