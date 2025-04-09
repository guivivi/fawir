library(readxl)
library(plyr)
# Set working directory to source file location.
translation_content <- read_excel("dictionary.xlsx")
translation <- dlply(translation_content, .(key), function(s) key = as.list(s))
save(translation, file = "translation.bin")
