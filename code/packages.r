
# install packages from CRAN
p_needed <- c("rvest", # scraping suite
              "httr", "httpuv", # suites to ease HTTP communication
              "RSelenium", # access Selenium API
              "rtweet", "ROAuth", "ggmap", # access various web APIs
              "robotstxt", # parse robots.txt files
              "readr", # imports spreadsheet data
              "haven", # imports SPSS, Stata and SAS files
              "magrittr", #  for piping
              "dplyr",  # provides data manipulating functions
              "stringr", # for string processing
              "stringi", # for advanced string processing
              "lubridate", # work with dates
              "jsonlite", # parse JSON data
              "devtools", # developer tools
              "networkD3", # tools to process network data
              "ggplot2", # for graphics
              "tidyr", # for tidying data frames
              "reshape2", # reshape data 
              "tm", # text mining suite
              "quanteda", # quantitative text analysis
              "readtext", # import text files for quantitative text analysis
              "glmnet", # lasso and elastic-net generalized linear models 
              "doMC", # multi-core processing
              "xgboost", # extreme gradient boosting
              "topicmodels", # topic modeling with LDA
              "stm" # structural topic modeling
)
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)
