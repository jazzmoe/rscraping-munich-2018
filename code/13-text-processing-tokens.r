### -----------------------------
### simon munzert
### introduction to quanteda
### -----------------------------

## peparations -------------------

source("packages.r")


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


## dealing with tokens ------------------------

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
tokens_wordstem(tokens(toks$Labour[1:10])) %>% unlist
tokens_wordstem(tokens("Die Münchner essen ihre Weißwürste gerne vor Mittag"), language = "de") %>% unlist




## keyword-in-contexts ------------------------

immig_kw <- kwic(toks, 'immig*')
head(immig_kw, 5)

# expand window
immig_kw <- kwic(toks, 'immig*', window = 7)
head(immig_kw, 5)

# look for multi-word expressions
asylum_kw <- kwic(toks, phrase('asylum seeker*'))
head(asylum_kw)

View(asylum_kw) # View() pretty useful here!


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


# look up dictionary -------------------

# retrieve dictionary
newsmap_dict <- dictionary(file = "data/newsmap.yml")
names(newsmap_dict) # level 1
names(newsmap_dict[['AFRICA']]) # level 2
newsmap_dict[['AFRICA']][['NORTH']] # level 3
newsmap_dict[['EUROPE']][['NORTH']] # level 3

# look up regions
region_toks <- tokens_lookup(toks, newsmap_dict, levels = 1)
head(region_toks)

# make document-feature matrix
dfm(region_toks)

# look up countries
country_toks <- tokens_lookup(toks, newsmap_dict, levels = 3)
head(country_toks)
dfm(country_toks)

# keywords in context
kwic(toks, newsmap_dict['AFRICA'], window = 10)

# create your own dictionary
dict <- dictionary(list(refugee = c('refugee*', 'asylum*'),
                        worker = c('worker*', 'employee*')))
print(dict)

dict_toks <- tokens_lookup(toks, dict)
head(dict_toks)

# note: tokens_lookup() ignores multiple matches of dictionary values for the same key with the same token to avoide double counting. For example, if US = c('United States of America', 'United States') is in your dictionary, you get ‘US’ only once for a sequence of tokens 'United' 'States' 'of' 'America'.


# generate n-grams -------------------

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

# tokens_ngrams() is an efficient function, but it returns a large object if multiple values are given to n or skip. Since n-grams inflates the size of objects without adding much information, it is recommended to generate n-grams more selectively using tokens_compound().

