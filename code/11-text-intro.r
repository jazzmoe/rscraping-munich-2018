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
textplot_wordcloud(inaugural_dfm, max.words = 100, random_order = FALSE)


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

# 3. from a readtext object
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

# new corpus
txt_files <- list.files("../data/sotu", pattern = "txt$", full.names = TRUE)
sotu_corpus <- readtext(txt_files) %>% corpus()
summary(sotu_corpus, 5)

# add metadata
sotu_docvars <- read.csv("../data/sotu-metadata.csv", stringsAsFactors = FALSE)
head(sotu_docvars)
docvars(sotu_corpus) <- sotu_docvars
summary(sotu_corpus, 5)

# subset corpus
sotu_corpus$documents$year <- str_extract(sotu_corpus$documents$Date, "[:digit:]{4}") %>% as.numeric
sotu_corpus_sub <- corpus_subset(sotu_corpus, year >= 2008)
summary(sotu_corpus_sub)

# change units of texts (documents, paragraphs, sentences)
ndoc(sotu_corpus)

sotu_corpus_sent <- corpus_reshape(sotu_corpus, 'sentences')
ndoc(sotu_corpus_sent)
sotu_corpus_sent[1]

# extract tags from text
speach_corp <- corpus("Mr. Smith: Text.
                       Mrs. Jones: More text.
                       Mr. Smith: I'm speaking, again.")
speaker_corp <- corpus_segment(speach_corp, pattern = "\\b[A-Z].+\\s[A-Z][a-z]+:", valuetype = "regex")
cbind(texts(speaker_corp), docvars(speaker_corp)) # texts() extracts docs as character data from corpus

