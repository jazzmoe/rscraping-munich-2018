### -----------------------------
### simon munzert
### introduction to quanteda
### -----------------------------

## peparations -------------------

source("packages.r")


## dictionary/sentiment analysis ----------

# Lexicoder Sentiment Dictionary by Young/Soroka
browseURL("https://www.tandfonline.com/doi/abs/10.1080/10584609.2012.671234")
browseURL("http://www.lexicoder.com/")

# data available in quanteda!
lengths(data_dictionary_LSD2015)
as.list(data_dictionary_LSD2015[1])$negative[1:50]
as.list(data_dictionary_LSD2015[3])$neg_positive[1:50]

# load Guardian corpus
load("../data/guardianSample.RData")
names(guardianSample)
guardian_corp <- corpus(guardianSample$documents, text_field = "texts")

# subset corpus
docvars(guardian_corp, 'year') <- year(docvars(guardian_corp, 'date'))
docvars(guardian_corp, 'month') <- month(docvars(guardian_corp, 'date'))
docvars(guardian_corp, 'week') <- week(docvars(guardian_corp, 'date'))

guardian_corp <- corpus_subset(guardian_corp, year >= 2016)
guardian_toks <- tokens(guardian_corp, remove_punct = TRUE)

# check sentiment
lsd_toks <- tokens_lookup(guardian_toks, data_dictionary_LSD2015[1:2])
head(lsd_toks, 2)

# DFM from classified tokens
lsd_dfm <- dfm(lsd_toks)
head(lsd_dfm, 5)

# more targeted sentiment analysis
eu <- c('EU', 'europ*', 'european union')
eu_toks <- tokens_keep(guardian_toks, phrase(eu), window = 10)
eu_toks[2]

# grouped DFM
eu_lsd_dfm <- dfm(eu_toks, dictionary = data_dictionary_LSD2015[1:2]) %>% 
  dfm_group(group = 'week', fill = TRUE) 

# plot absolute frequencies
matplot(eu_lsd_dfm, type = 'l', xaxt = 'n', lty = 1, ylab = 'Frequency')
grid()
axis(1, seq_len(ndoc(eu_lsd_dfm)), ymd("2016-01-01") + weeks(seq_len(ndoc(eu_lsd_dfm)) - 1))
legend('topright', col = 1:2, legend = c('Negative', 'Positive'), lty = 1, bg = 'white')

# plot average sentiment
eu_n <- ntoken(dfm(eu_toks, group = docvars(eu_toks, 'week')))
plot((eu_lsd_dfm[,2] - eu_lsd_dfm[,1]) / eu_n, 
     type = 'l', ylab = 'Sentiment', xlab = '', xaxt = 'n')
axis(1, seq_len(ndoc(eu_lsd_dfm)), ymd("2016-01-01") + weeks(seq_len(ndoc(eu_lsd_dfm)) - 1))
grid()
abline(h = 0, lty = 2)




## case study ----------

# use dictionary method to identicy populist rhetoric in British parties' Facebook posts

# load data
uk <- read.csv("../data/FB-UK-parties.csv", stringsAsFactors = FALSE)
head(uk)
table(uk$party)

# populism dictionary from Rooduijn, Matthijs, and Teun Pauwels (2011) “Measuring Populism: Comparing Two Methods of Content Analysis.” West European Politics 34(6): 1272–83. (see Appendix B for for a dictionary key for the concept populism)

# build dictionary
populist_dict <- dictionary(list(
  populism = c(
    "elit*",
    "consensus*",
    "undemocratic*",
    "referend*",
    "corrupt*",
    "propagand*",
    "politici*",
    "*deceit*",
    "*deceiv*",
    "*betray*",
    "shame*",
    "scandal*",
    "truth*",
    "dishonest*",
    "establishm*",
    "ruling*")))


# create corpus
fbcorpus <- corpus(uk)
fbdfm <- dfm(fbcorpus, groups = "party")

# normalize for document length: turn word counts into proportions
fbdfm <- dfm_weight(fbdfm, scheme="prop")

# find % of words in populism dictionary
pop <- dfm_lookup(fbdfm, dictionary = populist_dict)
pop * 100

# check precision
kwic(fbcorpus, pattern = 'undemocratic') # sounds good
kwic(fbcorpus, pattern = 'ruling*') # probably not

# check recall
kwic(fbcorpus, pattern = 'unaccountable')
kwic(fbcorpus, pattern = 'dodging')

# One cognitive explanation for the success of populist appeals is that it expresses anger, which is likely to affect voters' emotional states.
# Are populist parties more likely to use anger words?
# Let's use the LIWC dictionary to find out!

liwc <- read.csv("../data/liwc-dictionary.csv", stringsAsFactors = FALSE)
anger_words <- liwc$word[liwc$class == "anger"]
sample(anger_words, 10)
anger <- dfm_lookup(fbdfm, dictionary = dictionary(list('anger' = anger_words)))
anger*100


