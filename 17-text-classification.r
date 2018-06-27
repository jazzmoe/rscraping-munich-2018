### -----------------------------
### simon munzert
### introduction to quanteda
### -----------------------------

## peparations -------------------

source("packages.r")


## collocation analysis ----------

load("data/guardianSample.RData")
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



## dictionary/sentiment analysis ----------

# Lexicoder Sentiment Dictionary by Young/Soroka
lengths(data_dictionary_LSD2015)
as.list(data_dictionary_LSD2015[1])$negative[1:50]
as.list(data_dictionary_LSD2015[3])$neg_positive[1:50]

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


