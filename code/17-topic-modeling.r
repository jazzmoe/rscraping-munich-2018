### -----------------------------
### simon munzert
### introduction to quanteda
### -----------------------------

## peparations -------------------

source("packages.r")
library(topicmodels)


## Topic modeling: LDA -------------

# dataset that contains the lead paragraph of around 5,000 articles about the economy published in the New York Times between 1980 and 2014. As before, we will preprocess the text using the standard set of techniques.

# The number of topics in a topic model is somewhat arbitrary, so you need to play with the number of topics to see if you get anything more meaningful. We start here with 30 topics.

# reading data and preparing corpus object
nyt <- read.csv("../data/nytimes.csv", stringsAsFactors = FALSE)
nytcorpus <- corpus(nyt$lead_paragraph)
nytdfm <- dfm(nytcorpus, remove=stopwords("english"), verbose=TRUE,
              remove_punct=TRUE, remove_numbers=TRUE)
cdfm <- dfm_trim(nytdfm, min_docfreq = 2)

# estimate LDA with K topics
K <- 30
lda <- LDA(cdfm, k = K, method = "Gibbs", 
           control = list(verbose=25L, seed = 123, burnin = 100, iter = 500))


# get top `n` terms from the topic model
terms <- get_terms(lda, 15)
terms[,1]
head(terms)

# predict the top `k` topic for each document. 
topics <- get_topics(lda, 1)
head(topics)


# closer inspection of documents linked with topic

# Topic 2
paste(terms[,2], collapse=", ")
sample(nyt$lead_paragraph[topics==2], 2)

# Topic 3
paste(terms[,3], collapse=", ")
sample(nyt$lead_paragraph[topics==3], 2)

# Topic 7
paste(terms[,7], collapse=", ")
sample(nyt$lead_paragraph[topics==7], 2)

# topics don't always make sense

# Topic 4
paste(terms[,4], collapse=", ")
sample(nyt$lead_paragraph[topics==4], 2)


# prevalence of topic over time

# Topic 2
paste(terms[,2], collapse=", ")
sample(nyt$lead_paragraph[topics==2], 1)
# add predicted topic to dataset
nyt$pred_topic <- topics
nyt$year <- substr(nyt$datetime, 1, 4) # extract year
# frequency table with articles about stock market, per year
tab <- table(nyt$year[nyt$pred_topic==2])
plot(tab)


# inspect mixture of topics (gamma matrix; theta on the slides)
round(lda@gamma[1,], 2)

# work with topic probabilities

# Topic 15: financial crisis
paste(terms[,15], collapse=", ")
# add probability to df
nyt$prob_topic <- lda@gamma[,15]
# now aggregate at the year level
agg <- aggregate(nyt$prob_topic, by=list(year=nyt$year), FUN=mean)
# and plot it
plot(agg$year, agg$x, type="l", xlab="Year", ylab="Avg. prob. of article about topic 15",
main="Estimated proportion of articles about the financial crisis")


## Structural topic todeling-------------

library(stm)

# extracting covariates
year <- as.numeric(substr(nyt$datetime, 1, 4))
repub <- ifelse(year %in% c(1981:1992, 2000:2008), 1, 0)
meta <- data.frame(year = year, repub = repub)
head(meta)

# running STM (this takes a while... consider skipping and loading backup output directly)
stm <- stm(documents = cdfm, K = 30, prevalence = ~repub + s(year),
           data = meta, seed = 123)
save(stm, file="../backup/stm-output.Rdata")

# load backup output
load("../data/stm-output.Rdata")

# looking at a few topics
labelTopics(stm, topics = 1)
labelTopics(stm, topics = 4)
labelTopics(stm, topics = 7)
labelTopics(stm, topics = 10)


# estimate effects of features on prevalence of topics
est <- estimateEffect(~repub, stm, uncertainty = "None")
labelTopics(stm, topics = 1)
summary(est, topics = 1)

labelTopics(stm, topics = 4)
summary(est, topics = 4)

# identify most partisan topics (strong association with repub)
names(est)
length(est$parameters)
est$parameters[[1]]

# extract the coefficients for each topic
coef <- se <- rep(NA, 30)
for (i in 1:30){
  coef[i] <- est$parameters[[i]][[1]]$est[2]
  se[i] <- sqrt(est$parameters[[i]][[1]]$vcov[2,2])
}

df <- data.frame(topic = 1:30, coef=coef, se=se)
df <- df[order(df$coef),] # sorting by "partisanship"
head(df[order(df$coef),])
tail(df[order(df$coef),])

# three most "democratic" topics
labelTopics(stm, topics=df$topic[1])
labelTopics(stm, topics=df$topic[5])
labelTopics(stm, topics=df$topic[26])

# three most "republican" topics
labelTopics(stm, topics=df$topic[16])
labelTopics(stm, topics=df$topic[28])
labelTopics(stm, topics=df$topic[11])


