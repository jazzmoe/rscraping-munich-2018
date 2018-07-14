### -----------------------------
### simon munzert
### text import
### -----------------------------

## peparations -------------------

source("packages.r")


## why quanteda? -------------------------

browseURL("http://docs.quanteda.io/articles/pkgdown/comparison.html")


## overview of quanteda functionality ----

browseURL("http://docs.quanteda.io/reference/")


## getting started -------------------------

# inspect corpus of US Presidents' inaugural speeches (comes with quanteda)
?data_corpus_inaugural
summary(data_corpus_inaugural)
class(data_corpus_inaugural)
data_corpus_inaugural[1]

# create document-feature matrix, removing stop words
inaugural_dfm <- dfm(data_corpus_inaugural, remove = c(stopwords("english")), stem = TRUE)
inaugural_dfm # sparsity: proportion of cells that have zero counts

# look at top features
topfeatures(inaugural_dfm)

# make word cloud
textplot_wordcloud(inaugural_dfm, max_words = 100, random_order = FALSE)



## on quanteda objects -------------------------

# quanteda has three basic types of objects:

# 1. Corpus
# - Saves character strings and variables in a data frame
# - Combines texts with document-level variables

# 2. Tokens
# - Words or sentences
# - More efficient than character strings, but preserves positions of words

# 3. Document-feature matrix (DFM)
# - Represents frequencies of features in documents in a matrix
# - The most efficient structure, but it does not have information on positions of words


## four ways to create a corpus object -------------------------

# 1. from a character vector object
inaugural_george_washington <- data_corpus_inaugural[1:2]
class(inaugural_george_washington)
my_tiny_corpus <- corpus(inaugural_george_washington, metacorpus = list(notes = "From G.W."))
summary(my_tiny_corpus)

# 2. from a VCorpus object from the tm package
data(crude, package = "tm")
crude
my_tm_corpus <- corpus(crude)
summary(my_tm_corpus, 5)

# 3. from a readtext object / raw txt files (also accepts json, csv, html, pdf, and many others)
txt_files <- list.files("../data/inaugural", pattern = "txt$", full.names = TRUE)
inaugural_txts <- readtext(txt_files)
my_txt_corpus <- corpus(inaugural_txts)
summary(my_txt_corpus, 5)

# 4. from a structured file (here: csv)
sz_df <- read_csv("../data/sueddeutsche-articles.csv")
my_sz_corpus <- readtext("../data/sueddeutsche-articles.csv", text_field = "text") %>% corpus
summary(my_sz_corpus, 5)

# check out readtext() help for more functionality
?readtext


## modifying the corpus object ---------------------------------

# add metadata
docvars(data_corpus_inaugural) %>% head()
docvars(data_corpus_inaugural)$FullName <- paste(docvars(data_corpus_inaugural)$FirstName, docvars(data_corpus_inaugural)$President)
summary(data_corpus_inaugural, 5)

# subset corpus
str(data_corpus_inaugural) # structure of corpus object
corpus_inaugural_sub <- corpus_subset(data_corpus_inaugural, Year >= 2000)
summary(corpus_inaugural_sub)

# change units of texts (documents, paragraphs, sentences)
ndoc(data_corpus_inaugural)
corpus_inaugural_sent <- corpus_reshape(data_corpus_inaugural, 'sentences')
ndoc(corpus_inaugural_sent)
corpus_inaugural_sent[1]
