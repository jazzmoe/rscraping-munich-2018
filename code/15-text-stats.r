### -----------------------------
### simon munzert
### introduction to quanteda
### -----------------------------

## peparations -------------------

source("packages.r")


## simple frequency analysis ---------------------

load("data/tweetSample.RData")
names(tweetSample)

# generate corpus / document-feature matrix
tweet_corp <- corpus(tweetSample, text_field = "text")
tweet_dfm <- dfm(tweet_corp, remove_punct = TRUE, select = "#*") # only select hash tags
tweet_dfm

# identify top 10 frequent hashtags by group (language)
freq <- textstat_frequency(tweet_dfm, n = 10, groups = docvars(tweet_dfm, 'lang'))
head(freq, 20)

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
docvars(tweet_corp, "dummy_english") <- factor(ifelse(docvars(tweet_corp, "lang") == "English", "English", "Not English"))
tweet_corp_language <- dfm(tweet_corp, select = "#*", groups = "dummy_english")

# wordcloud with group comparison (again, DON'T do this)
textplot_wordcloud(tweet_corp_language, comparison = TRUE, max_words = 100)


## lexical diversity ---------------------

inaug_toks <- tokens(data_corpus_inaugural)
inaug_dfm <- dfm(inaug_toks, remove = stopwords('en'))
lexdiv <- textstat_lexdiv(inaug_dfm)
tail(lexdiv, 5)

plot(lexdiv$TTR, type = 'l', xaxt = 'n', xlab = NULL, ylab = "TTR")
grid()
axis(1, at = seq_len(nrow(lexdiv)), labels = docvars(inaug_dfm, 'President'))



## document/feature distance/similarity ----------

?textstat_dist
?textstat_simil

dist <- textstat_dist(inaug_dfm)
as.matrix(dist)[1:10,1:10]

clust <- hclust(dist)
plot(clust, xlab = "Distance", ylab = NULL)



