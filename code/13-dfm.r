### -----------------------------
### simon munzert
### introduction to quanteda
### -----------------------------

## peparations -------------------

source("packages.r")


## construct a document-feature matrix (DFM) --------

# dfm() constructs a document-feature matrix (DFM) from a tokens object
irish_toks <- tokens(data_corpus_irishbudget2010, remove_punct = TRUE)
summary(irish_toks)
irish_dfm <- dfm(irish_toks)
irish_dfm

# you can also pass corpus to dfm directly; remove_* arguments are passed to tokens() internally
irish_dfm <- dfm(data_corpus_irishbudget2010, remove_punct = TRUE)
irish_dfm

# dfm has many useful options
irish_dfm <- dfm(data_corpus_irishbudget2010, 
                 tolower = TRUE,
                 remove_punct = TRUE,
                 stem = TRUE,
                 ngrams = 1:3 # passed on to tokens()
                 )
irish_dfm

# dealing with stopwords
stopwords("english")
stopwords("german")
irish_dfm <- dfm(data_corpus_irishbudget2010, 
                 remove_punct = TRUE, 
                 remove=c(stopwords("english"), "£"), 
                 verbose = TRUE)


## do more things with DFMs -------

# check number of documents, features, document names, feature names
ndoc(irish_dfm)
nfeat(irish_dfm)
docnames(irish_dfm)
head(featnames(irish_dfm), 20)

# calculate marginals of df matrix
rowSums(irish_dfm) %>% head(10)
colSums(irish_dfm) %>% head(10)

# identify most frequent features
topfeatures(irish_dfm, 10)

# convert frequency count to a proportion within documents
prop_irish_dfm <- dfm_weight(irish_dfm, scheme  = "prop")
topfeatures(prop_irish_dfm[1,])

# weight frequency count by uniqueness of the features ("term frequency-inverse document frequency", tf-idf); see https://en.wikipedia.org/wiki/Tf%E2%80%93idf 
tfidf_irish_dfm <- dfm_tfidf(irish_dfm)
topfeatures(tfidf_irish_dfm[1,])


## select features from DFM -----------

nfeat(irish_dfm)

# remove stopwords
nostop_irish_dfm <- dfm_select(irish_dfm, stopwords('en'), selection = 'remove') # dfm_remove() works, too
nfeat(nostop_irish_dfm)

# minimum length of features
long_irish_dfm <- dfm_select(irish_dfm, min_nchar = 5)
nfeat(long_irish_dfm)

# minimum feature frequencies
freq_irish_dfm <- dfm_trim(irish_dfm, min_termfreq = 10)
nfeat(freq_irish_dfm)


## group documents -------------------

docvars(irish_dfm)
party_dfm <- dfm_group(irish_dfm, groups = docvars(irish_dfm, "party"))
ndoc(party_dfm)
docvars(party_dfm)



