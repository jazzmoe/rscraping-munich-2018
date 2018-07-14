### -----------------------------
### simon munzert
### introduction to quanteda
### -----------------------------

## peparations -------------------

source("packages.r")


## simple frequency analysis ---------------------

# generate corpus / document-feature matrix
tweets <- read.csv("../data/candidate-tweets.csv", stringsAsFactors = F)
# extract month data and subset only data during campaign
tweets$month <- substr(tweets$datetime, 1, 7)
tweets <- tweets[tweets$month>"2015-06",]
# create DFM at the candidate and month level
twcorpus <- corpus(tweets)
tweet_dfm <- dfm(twcorpus, remove_punct = TRUE, select = "#*") # only select hash tags
tweet_dfm
topfeatures(tweet_dfm)

# identify top 3 frequent hashtags
freq <- textstat_frequency(tweet_dfm, n = 3, groups = docvars(tweet_dfm, 'screen_name'))
head(freq, 15)

# dotplot of hashtag frequencies
tweet_dfm %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

# word cloud of 100 most common tags (DON'T do this)
textplot_wordcloud(tweet_dfm, max_words = 100)

# create document-level variable indicating whether Tweet was in English or other language
docvars(twcorpus, "dummy_bernie") <- factor(ifelse(docvars(twcorpus, "screen_name") == "BernieSanders", "Bernie", "Not Bernie"))
tweet_corp_bernie <- dfm(twcorpus, select = "#*", groups = "dummy_bernie")

# wordcloud with group comparison (again: DON'T do this)
textplot_wordcloud(tweet_corp_bernie, comparison = TRUE, max_words = 100)


## assess lexical diversity ---------------------

inaug_toks <- tokens(data_corpus_inaugural)
inaug_dfm <- dfm(inaug_toks, remove = stopwords('en'))
lexdiv <- textstat_lexdiv(inaug_dfm)
tail(lexdiv, 5)

plot(lexdiv$TTR, type = 'l', xaxt = 'n', xlab = NULL, ylab = "TTR")
grid()
axis(1, at = seq_len(nrow(lexdiv)), labels = docvars(inaug_dfm, 'President'))


## similarity/distance -------------------

# We can identify documents that are similar to one another based on the frequency of words, using `similarity`. There's different metrics to compute similarity. Here we explore two of them: [Jaccard distance](https://en.wikipedia.org/wiki/Jaccard_index) and [Cosine distance](https://en.wikipedia.org/wiki/Cosine_similarity).

twdfm <- dfm(twcorpus, groups=c("screen_name"), verbose=TRUE)
docnames(twdfm)
textstat_simil(twdfm, margin="documents", method="jaccard")
textstat_simil(twdfm, margin="documents", method="cosine")

# term similarity

twdfm <- dfm(twcorpus, verbose=TRUE)
# term similarities
simils <- textstat_simil(twdfm, "america", margin="features", method="cosine")
# most similar features
df <- data.frame(
  featname = rownames(simils),
  simil = as.numeric(simils),
  stringsAsFactors=F
)
head(df[order(simils, decreasing=TRUE),], n=5)

# another example
simils <- textstat_simil(twdfm, "immigration", margin="features", method="cosine")
# most similar features
df <- data.frame(
  featname = rownames(simils),
  simil = as.numeric(simils),
  stringsAsFactors=F
)
head(df[order(simils, decreasing=TRUE),], n=5)



## most unique features of documents -------------

# Keyness is a measure of to what extent some features are specific to a (group of) document in comparison to the rest of the corpus, taking into account that some features may be too rare.

twdfm <- dfm(twcorpus, groups=c("screen_name"), remove_punct=TRUE,
             remove=c(stopwords("english"), 'rt', 'u', 's'), verbose=TRUE)
head(textstat_keyness(twdfm, target="realDonaldTrump",
                      measure="chi2"), n=5)
head(textstat_keyness(twdfm, target="HillaryClinton",
                      measure="chi2"), n=5)
head(textstat_keyness(twdfm, target="tedcruz",
                      measure="chi2"), n=5)
head(textstat_keyness(twdfm, target="BernieSanders",
                      measure="chi2"), n=5)


## Lexical dispersion / xray plot -----------------

trump <- paste(
  tweets$text[tweets$screen_name=="realDonaldTrump"], collapse=" ")
textplot_xray(kwic(trump, "hillary"), scale="relative")
textplot_xray(kwic(trump, "crooked"), scale="relative")
textplot_xray(kwic(trump, "fake"), scale="relative")
textplot_xray(kwic(trump, "immigr*"), scale="relative")

clinton <- paste(
  tweets$text[tweets$screen_name=="HillaryClinton"], collapse=" ")
textplot_xray(kwic(clinton, "trump"), scale="relative")
textplot_xray(kwic(clinton, "sanders"), scale="relative")
textplot_xray(kwic(clinton, "gun*"), scale="relative")



## collocation analysis ----------

load("../data/guardianSample.RData")
names(guardianSample)

guardian_corp <- corpus(guardianSample$documents, text_field = "texts")
guardian_toks <- tokens(guardian_corp, remove_punct = TRUE)
guardian_toks[1]

# select tokens starting with capital letters
cap_col <- tokens_select(guardian_toks, '^[A-Z]', valuetype = 'regex', case_insensitive = FALSE, padding = TRUE) 
cap_col[1]

# identify and score multi-word expressions (with at least 10 observations)
cap_col <- textstat_collocations(cap_col, min_count = 10)
head(cap_col, 20)

# use results from analysis to compound meaningful (statistically strongly associated) tokens; this makes them less ambiguous and might improve statistical analyses
comp_toks <- tokens_compound(guardian_toks, cap_col[cap_col$z > 3])
comp_toks[1]


