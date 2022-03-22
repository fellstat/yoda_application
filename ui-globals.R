library(tidyverse)
library(stringr)

if(!file.exists("yoda_code")){
  setwd("..")
}

countries <- "application/countries" %>% 
  list.dirs(full.names = FALSE, recursive = FALSE) %>%
  str_replace_all("_"," ")

runs <- dir("runs")