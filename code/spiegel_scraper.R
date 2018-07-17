#!/usr/local/bin/Rscript
library(stringr)
library(magrittr)
library(httr)
setwd("/Users/simonmunzert/GitHub/rscraping-munich-2018/")
url <- "http://www.spiegel.de/schlagzeilen/"
url_out <- GET(url, add_headers(from = "eddie@datacollection.com"))
datetime <- str_replace_all(Sys.time(), "[ :]", "-")
httr::content(url_out, as = "text") %>% write(file = str_c("data/spiegelheadlines/headlines-spiegel-", datetime, ".html"))
