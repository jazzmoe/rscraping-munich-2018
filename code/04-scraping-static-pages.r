### -----------------------------
## simon munzert
## scraping static pages
### -----------------------------


## load packages -----------------

library(rvest)
library(stringr)


## basic workflow of scraping with rvest  ----------

# see also: https://github.com/hadley/rvest
# convenient package to scrape information from web pages
# builds on other packages, such as xml2 and httr
# provides very intuitive functions to import and process webpages

# 1. specify URL
url <- "https://www.nytimes.com"

# 2. download static HTML behind the URL and parse it
url_parsed <- read_html(url)

# 3. extract specific nodes with XPath
headings_nodes <- html_nodes(url_parsed, xpath = "//*[@class = 'story-heading']")

# 4. extract content from nodes
headings <- html_text(headings_nodes)


# 5. tidy headlines
headings <- str_replace_all(headings, "\\n|\\t|\\r", " ") %>% str_trim()
head(headings)
length(headings)




## extract data from tables --------------

## HTML tables 
  # ... are a special case for scraping because they are already very close to the data structure you want to build up in R
  # ... come with standard tags and are usually easily identifiable

## scraping HTML tables with rvest
url_p <- read_html("https://en.wikipedia.org/wiki/List_of_MPs_elected_in_the_United_Kingdom_general_election,_1992")
tables <- html_table(url_p, header = TRUE, fill = TRUE)
mps <- tables[[4]]
head(mps)
names(mps) <- c("constituency", "mp", "party")
mps <- mps[2:nrow(mps),]
mps <- filter(mps, !str_detect(constituency, fixed("[edit]")))

table(mps$party, str_detect(mps$mp, "^Sir ")) # how many "Sirs" per party?

## note: HTML tables can get quite complex. there are more flexible solutions than html_table() on the market (e.g., package "htmltab") 



### working with SelectorGadget ----------

# to learn about it, visit
vignette("selectorgadget")

# to install it, visit
browseURL("http://selectorgadget.com/")
# and follow the advice below: "drag this link to your bookmark bar: >>SelectorGadget>> (updated August 7, 2013)"

## SelectorGadget is magic. Proof:
browseURL("https://www.nytimes.com")

url <- "https://www.nytimes.com"
xpath <-  '//*[contains(concat( " ", @class, " " ), concat( " ", "story-heading", " " ))]//a'
url_parsed <- read_html(url)
html_nodes(url_parsed, xpath = xpath) %>% html_text()





