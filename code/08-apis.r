### -----------------------------
### simon munzert
### tapping apis
### -----------------------------

## peparations -------------------

source("packages.r")



## ready-made R bindings to web APIs ---------

## overview
browseURL("http://ropensci.org/")
browseURL("https://github.com/ropensci/opendata")
browseURL("https://cran.r-project.org/web/views/WebTechnologies.html")


## example: IP API

# API documentation
browseURL("http://ip-api.com/")

# ipapi package
#devtools::install_github("hrbrmstr/ipapi")
library(ipapi)

# function call
ip_df <- geolocate(c(NA, "", "10.0.1.1", "72.33.67.89", "www.spiegel.de", "search.twitter.com"), .progress=TRUE)
View(ip_df)



## example: NY Times API

# overview
browseURL("https://cran.r-project.org/web/packages/rtimes/vignettes/rtimes_vignette.html")
browseURL("http://developer.nytimes.com/article_search_v2.json#/README")

# register for key at
browseURL("http://developer.nytimes.com/")

# use API
library(rtimes)
load("/Users/s.munzert/Munzert Dropbox/Simon Munzert/rkeys.RDa")
Sys.setenv(NYTIMES_AS_KEY = nytimes_apikey)

terms <- c("John McCain", "Nancy Pelosi", "Bernie Sanders", "Al Franken", "Marco Rubio", "Paul Ryan", "Elizabeth Warren", "Mitch McConnell", "Tim Kaine", "Dianne Feinstein")

nytimes_hits <- numeric()
for(i in seq_along(terms)) {
  nytimes_hits[i] <-  as_search(q = terms[i], begin_date = "20170103", end_date = '20180601')$meta$hits
  Sys.sleep(runif(1, 1, 2))
}
nytimes_hits_df <- data.frame(name = terms, nytimes_hits, stringsAsFactors = FALSE)
head(nytimes_hits_df)



## accessing APIs from scratch ---------

# most modern APIs use HTTP (HyperText Transfer Protocol) for communication and data transfer between server and client
# R package httr as a good-to-use HTTP client interface
# most web data APIs return data in JSON or XML format
# R packages jsonlite and xml2 good to process JSON or XML-style data

# if you want to tap an existing API, you have to
  # figure out how it works (what requests/actions are possible, what endpoints exist, what )
  # (register to use the API)
  # formulate queries to the API from within R
  # process the incoming data


## example: back to the IP API

# API documentation
browseURL("http://ip-api.com/docs/")

# manual API call, XML data
url <- "http://ip-api.com/xml/"
ip_parsed <- xml2::read_xml(url)
ip_list <- as_list(ip_parsed)
ip_list %>% unlist %>% t %>% as.data.frame(stringsAsFactors = FALSE)

# manual API call, JSON data
url <- "http://ip-api.com/json"
ip_parsed <- jsonlite::fromJSON(url)
as.data.frame(ip_parsed, stringsAsFactors = FALSE)

# modify call
fromJSON("http://ip-api.com/json/72.33.67.89") %>% as.data.frame(stringsAsFactors = FALSE)
fromJSON("http://ip-api.com/json/www.spiegel.de") %>% as.data.frame(stringsAsFactors = FALSE)

# build function
ipapi_grabber <- function(ip = "") {
  dat <- fromJSON(paste0("http://ip-api.com/json/", ip)) %>% as.data.frame(stringsAsFactors = FALSE)
  dat
}
ipapi_grabber("193.17.243.1")


