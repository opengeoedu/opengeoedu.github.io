library(tidytext)
library(dplyr)



be_words <- portale$Beschreibung %>% str_split(" ") %>% unlist() %>% as.factor();
head(be_words, 20)
summary(be_words)
