### -----------------------------
### simon munzert
### introduction to quanteda
### -----------------------------

## peparations -------------------

source("packages.r")


## dealing with tokens ------------------

# data
?data_char_ukimmig2010

# words
immig_corp <- corpus(data_char_ukimmig2010)
toks <- tokens(immig_corp, what = "word")
summary(toks)
toks$Labour[1:100]

# sentences
toks <- tokens(immig_corp, what = "sentence")
toks$Labour[1:10]

# clean up
toks <- tokens(immig_corp, what = "word", 
               remove_numbers = TRUE,
               remove_punct = TRUE,
               remove_hyphens = TRUE)
toks$Labour[1:10]

# retrieve word stems
tokens(toks$Labour[1:10]) %>% tokens_wordstem() %>% unlist
tokens("Die Münchner essen ihre Weißwürste gerne vor Mittag") %>% tokens_wordstem(language = "de") %>% unlist


## keyword-in-contexts ------------------

immig_kw <- kwic(toks, 'immig*')
head(immig_kw, 5)

# expand window
immig_kw <- kwic(toks, 'immig*', window = 7)
head(immig_kw, 5)

# look for multi-word expressions
asylum_kw <- kwic(toks, phrase('asylum seeker*'))
head(asylum_kw)

View(asylum_kw) # View() pretty useful here! Requires xtable to be installed


## select tokens ------------------------

# select
immig_toks <- tokens_select(toks, c('immig*', 'migra*'))
head(immig_toks[[1]], 50)

# remove
nostop_toks <- tokens_select(toks, stopwords('en'), selection = 'remove')
head(nostop_toks[[1]], 50)

# explicit remove function
nostop_toks <- tokens_remove(toks, stopwords('en'))
head(nostop_toks[[1]], 50)

# select with window
window_toks <- tokens_select(toks, c('immig*', 'migra*'), window = 5)
head(window_toks[[1]], 50)


## generate n-grams -------------------

# generate 2-4-grams
ngram <- tokens_ngrams(toks, n = 2:4)
length(toks[[1]])
length(ngram[[1]])
head(ngram[[1]], 50)
tail(ngram[[1]], 50)

# generate skip-grams
skipgram <- tokens_ngrams(toks, n = 2, skip = 1:2)
head(skipgram[[1]], 50)


## compound tokens ---------------------

comp_toks <- tokens_compound(toks, phrase(c('asylum seeker*', 'british citizen*')))
compu_kw <- kwic(comp_toks, c('asylum_seeker*', 'british_citizen*'))
head(compu_kw, 10)

neg_bigram <- tokens_compound(toks, phrase('not *'))
neg_bigram <- tokens_select(neg_bigram, phrase('not_*'))
head(neg_bigram[[1]], 50)

# how to discover popular multi-words expressions in the first place?
textstat_collocations(nostop_toks, min_count = 5)

# tokens_ngrams() is an efficient function, but it returns a large object if multiple values are given to n or skip. Since n-grams inflate the size of objects without adding much information, it is recommended to generate n-grams more selectively using tokens_compound().

