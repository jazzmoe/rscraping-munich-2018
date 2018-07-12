### -----------------------------
### simon munzert
### introduction to quanteda
### -----------------------------

## peparations -------------------

source("packages.r")

## Exploring large-scale text datasets
tweets <- read.csv("../data/candidate-tweets.csv", stringsAsFactors = F)
# extract month data and subset only data during campaign
tweets$month <- substr(tweets$datetime, 1, 7)
tweets <- tweets[tweets$month>"2015-06",]
# create DFM at the candidate and month level
twcorpus <- corpus(tweets)


### Identifying most unique features of documents

# _Keyness_ is a measure of to what extent some features are specific to a (group of) document in comparison to the rest of the corpus, taking into account that some features may be too rare.

twdfm <- dfm(twcorpus, groups=c("screen_name"), remove_punct=TRUE,
             remove=c(stopwords("english"), 'rt', 'u', 's'), verbose=TRUE)
head(textstat_keyness(twdfm, target="realDonaldTrump",
                      measure="chi2"), n=20)
head(textstat_keyness(twdfm, target="HillaryClinton",
                      measure="chi2"), n=20)
head(textstat_keyness(twdfm, target="tedcruz",
                      measure="chi2"), n=20)
head(textstat_keyness(twdfm, target="BernieSanders",
                      measure="chi2"), n=20)

trump <- corpus_subset(twcorpus, screen_name=="realDonaldTrump")
twdfm <- dfm(trump, remove_punct=TRUE,
             remove=c(stopwords("english"), 'rt', 'u', 's'), verbose=TRUE)
head(textstat_keyness(twdfm, target=docvars(twdfm)$month<"2016-01", 
                      measure="chi2"), n=20)
head(textstat_keyness(twdfm, target=docvars(twdfm)$month>"2016-03", 
                      measure="chi2"), n=20)


### Lexical dispersion / xray plot

trump <- paste(
  tweets$text[tweets$screen_name=="realDonaldTrump"], collapse=" ")
textplot_xray(kwic(trump, "hillary"), scale="relative")
textplot_xray(kwic(trump, "crooked"), scale="relative")
textplot_xray(kwic(trump, "mexic*"), scale="relative")
textplot_xray(kwic(trump, "fake"), scale="relative")
textplot_xray(kwic(trump, "immigr*"), scale="relative")
textplot_xray(kwic(trump, "muslim*"), scale="relative")

clinton <- paste(
  tweets$text[tweets$screen_name=="HillaryClinton"], collapse=" ")
textplot_xray(kwic(clinton, "trump"), scale="relative")
textplot_xray(kwic(clinton, "sanders"), scale="relative")
textplot_xray(kwic(clinton, "gun*"), scale="relative")


### Clustering documents and features

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






