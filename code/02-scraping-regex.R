### -----------------------------
## simon munzert
## regular expressions and
## string manipulation
### -----------------------------


source("packages.r")


## string matching ----------

# example
raw.data <- "555-1239Moe Szyslak(636) 555-0113Burns, C. Montgomery555-6542Rev. Timothy Lovejoy555 8904Ned Flanders636-555-3226Simpson, Homer5553642Dr. Julius Hibbert"

name <- unlist(str_extract_all(raw.data, "[[:alpha:]., ]{2,}"))
name

phone <- unlist(str_extract_all(raw.data, "\\(?(\\d{3})?\\)?(-| )?\\d{3}(-| )?\\d{4}"))
phone
data.frame(name = name, phone = phone)


# running example
example.obj <- "1. A small sentence. - 2. Another tiny sentence."

# self match
str_extract(example.obj, "small")
str_extract(example.obj, "banana")

# multiple matches
(out <- str_extract_all(c("text", "manipulation", "basics"), "a")) 

# case sensitivity
str_extract(example.obj, "small")
str_extract(example.obj, "SMALL")
str_extract(example.obj, ignore.case("SMALL")) # wrong
str_extract(example.obj, regex("SMALL", ignore_case = TRUE))

# match empty space
str_extract(example.obj, "mall sent")

# match the beginning of a string
str_extract(example.obj, "^1")
str_extract(example.obj, "^2")

# match the end of a string
str_extract(example.obj, "sentence$")
str_extract(example.obj, "sentence.$")

# pipe operator
unlist(str_extract_all(example.obj, "tiny|sentence"))

# wildcard
str_extract(example.obj, "sm.ll")

# character class
str_extract(example.obj, "sm[abc]ll")

# character class: range
str_extract(example.obj, "sm[a-p]ll")

# character class: additional characters
unlist(str_extract_all(example.obj, "[uvw. ]"))

# pre-defined character classes
unlist(str_extract_all(example.obj, "[:punct:]"))
unlist(str_extract_all(example.obj, "[[:punct:]ABC]"))
unlist(str_extract_all(example.obj, "[^[:alnum:]]"))
# for more character classes, see
?base::regex

# additional shortcuts
unlist(str_extract_all(example.obj, "\\w+"))

# word edges
unlist(str_extract_all(example.obj, "e\\b")) 
unlist(str_extract_all(example.obj, "e\\B"))

# quantifier
str_extract(example.obj, "s[:alpha:][:alpha:][:alpha:]l")
str_extract(example.obj, "s[:alpha:]{3}l")
str_extract(example.obj, "A.+sentence")

# greedy quantification
str_extract(example.obj, "A.+sentence")
str_extract(example.obj, "A.+?sentence")

# quantifier with pattern sequence
unlist(str_extract_all(example.obj, "(.en){1,5}"))
unlist(str_extract_all(example.obj, ".en{1,5}"))

# meta characters
unlist(str_extract_all(example.obj, "\\."))
unlist(str_extract_all(example.obj, fixed(".")))

# meta characters in character classes
unlist(str_extract_all(example.obj, "[1-2]"))
unlist(str_extract_all(example.obj, "[12-]"))

# backreferencing
str_extract(example.obj, "([:alpha:]).+?\\1")
str_extract(example.obj, "(\\b[a-z]+\\b).+?\\1")

# do you think you can master regular expressions now?
browseURL("http://stackoverflow.com/questions/201323/using-a-regular-expression-to-validate-an-email-address/201378#201378") # think again




## string manipulation ----------

example.obj

# locate
str_locate(example.obj, "tiny")

# substring extraction
str_sub(example.obj, start = 35, end = 38)

# replacement
str_sub(example.obj, 35, 38) <- "huge"
str_replace(example.obj, pattern = "huge", replacement = "giant")

# splitting
str_split(example.obj, "-") %>% unlist
str_split_fixed(example.obj, "[:blank:]", 5) %>% as.character()

# manipulate multiple elements; example
(char.vec <- c("this", "and this", "and that"))

# detection
str_detect(char.vec, "this")

# keep strings matching a pattern
str_subset(char.vec, "this") # wrapper around x[str_detect(x, pattern)]

# counting
str_count(char.vec, "a")
str_count(char.vec, "\\w+")
str_length(char.vec)


# a note on the stringi package
# source: [https://goo.gl/XzEQai]

# stringr is built on top of the stringi package. 
# stringr is convenient because it exposes a minimal set of functions, which have been carefully picked to handle the most common string manipulation functions. 
# stringi is designed to be comprehensive. It contains almost every function you might ever need: stringi has 234 functions (compare that to stringr's 42)
# packages work very similarly; translating knowledge is easy (try stri_ instead of str_)
library(stringi)
?stri_count_words
example.obj
stri_count_words(example.obj)
stri_stats_latex(example.obj)
stri_stats_general(example.obj)
stri_escape_unicode("\u00b5")
stri_unescape_unicode("\u00b5")
stri_rand_lipsum(3)
stri_rand_shuffle("hello")
stri_rand_strings(100, 10, pattern = "[munich]")




## character encoding ---------

# some background on character encoding in general
browseURL("https://en.wikipedia.org/wiki/Character_encoding")

# some background on how RStudio handles character encoding
browseURL("https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding")

# tutorial on how to deal with encodings in R (some of the code borrowed below)
browseURL("https://rstudio-pubs-static.s3.amazonaws.com/279354_f552c4c41852439f910ad620763960b6.html")



## how to query your locale and (maybe) change it ---------

# query your current locale
Sys.getlocale()

# query your locale for individual categories
localeCategories <- c("LC_COLLATE","LC_CTYPE","LC_MONETARY","LC_NUMERIC","LC_TIME")
setNames(sapply(localeCategories, Sys.getlocale), localeCategories)

# set your locale
Sys.setlocale("LC_ALL", 'en_US.UTF-8') # Mac users: this could help if Sys.getlocale() returns something else, for instance, "C"
Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252") # Windows users: alternative
Sys.setlocale("LC_TIME", "German") # Windows users on a German locale

# alternatively, retrieve the language of your OS first, then set locale
(LANG <- Sys.getenv("LANG"))
if(nchar(LANG)) Sys.setlocale("LC_ALL", LANG)



## how to declare or convert encodings ---------

# sample from the list of available conversions
(encodings <- length(iconvlist()))
sample(iconvlist(), 10)

# an example string
small.frogs <- "Små grodorna, små grodorna är lustiga att se."
small.frogs

# check encoding
Encoding(small.frogs)

# declare (wrong) encoding - what happens?
Encoding(small.frogs) <- "latin1"
small.frogs

# declare (right) encoding again
Encoding(small.frogs) <- "utf8"
small.frogs # in this case, we could restore the original string

# more on reading or setting encodings for a character vector in R
?Encoding

# translate the encoding
small.frogs.latin1 <- iconv(small.frogs, from = "utf8", to = "latin1")
Encoding(small.frogs.latin1)

# unescape and escape unicode character sequences (code points) with stringi
symbol <- stri_unescape_unicode("\u00b5")
stri_escape_unicode(symbol)

