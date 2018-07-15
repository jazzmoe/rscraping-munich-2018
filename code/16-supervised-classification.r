### -----------------------------
### simon munzert
### introduction to quanteda
### -----------------------------

## peparations -------------------

source("packages.r")


## regularized regression ----------

# random sample of nearly 5,000 tweets mentioning the names of the candidates to the 2014 EP elections in the UK. 
# source: Yannis Theocharis, Pablo Barberá, Zoltan Fazekas, and Sebastian Popa, Journal of Communication. (http://onlinelibrary.wiley.com/doi/10.1111/jcom.12259/abstract). 
# focus on variable named `communication`, which indicates whether each tweet was hand-coded as being 
  # engaging (a tweet that tries to engage with the audience of the account) or   
  # broadcasting (just sending a message, without trying to elicit a response)

# import data
tweets <- read.csv("../data/UK-tweets.csv", stringsAsFactors = F)
tweets$engaging <- ifelse(tweets$communication == "engaging", 1, 0)
tweets <- tweets[!is.na(tweets$engaging),]
head(tweets)

# inspect tweets
set.seed(42)
filter(tweets, engaging == 1) %>% sample_n(5) %>% select(text)
filter(tweets, engaging == 0) %>% sample_n(5) %>% select(text)

# substitute handles with @ to avoid overfitting
tweets$text <- str_replace_all(tweets$text, '@[0-9_A-Za-z]+', '@')
tweets$text[1:10]

# further preprocessing
twcorpus <- corpus(tweets$text)
summary(twcorpus, 10)

# keep only tokens that appear in 2 or more tweets
# keep punctuation -- it turns out to be quite informative.
twdfm <- dfm(twcorpus, remove = stopwords("english"), remove_url = TRUE, 
             ngrams = 1:2, verbose = TRUE) 
twdfm <- dfm_trim(twdfm, min_docfreq = 2, verbose = TRUE)

# split into training and test set (80% / 20%)
set.seed(123)
training <- sample(1:nrow(tweets), floor(.80 * nrow(tweets)))
test <- (1:nrow(tweets))[1:nrow(tweets) %in% training == FALSE]


# run regularized regression with glmnet
  # note: other packages available, such as caret or mlr, but glmnet tends to be faster and has cross-validation built in

# ridge regression
parallel::detectCores()
registerDoMC(cores = 3) # parallel computing on multiple cores using the doMC package 
ridge <- cv.glmnet(twdfm[training,], # x matrix
                   tweets$engaging[training],  # y response
                   family = "binomial", # family; here: dichotomous response
                   alpha = 0, # elastic net mixing parameter; alpha = 0 --> ridge, alpha = 1 --> lasso
                   nfolds = 5, # k-folds cross-validation
                   parallel = TRUE, # enable parallel computing
                   intercept = TRUE, # intecept to be fitted?
                   type.measure = "class")
plot(ridge)

## function to compute accuracy
accuracy <- function(ypred, y){
  tab <- table(ypred, y)
  return(sum(diag(tab))/sum(tab))
}
# function to compute precision
precision <- function(ypred, y){
  tab <- table(ypred, y)
  return((tab[2,2])/(tab[2,1]+tab[2,2]))
}
# function to compute recall
recall <- function(ypred, y){
  tab <- table(ypred, y)
  return(tab[2,2]/(tab[1,2]+tab[2,2]))
}

# computing predicted values
preds <- predict(ridge, twdfm[test,], type="class")
table(preds, tweets$engaging[test]) # confusion matrix

# performance metrics
accuracy(preds, tweets$engaging[test])
precision(preds == 1, tweets$engaging[test] == 1)
recall(preds == 1, tweets$engaging[test] == 1)
precision(preds == 0, tweets$engaging[test] == 0)
recall(preds == 0, tweets$engaging[test] == 0)


# from the different values of lambda, let's pick the highest one that is within one standard error of the best one 
best.lambda <- which(ridge$lambda==ridge$lambda.1se)
beta <- ridge$glmnet.fit$beta[,best.lambda]
head(beta)

## identifying predictive features
df <- data.frame(coef = as.numeric(beta),
                 word = names(beta), stringsAsFactors = F)

df <- df[order(df$coef),]
head(df[,c("coef", "word")], n = 30)
paste(df$word[1:30], collapse = ", ")
df <- df[order(df$coef, decreasing = TRUE),]
head(df[,c("coef", "word")], n = 30)
paste(df$word[1:30], collapse = ", ")


# lasso regression
lasso <- cv.glmnet(twdfm[training,], 
                   tweets$engaging[training], 
                   family = "binomial", 
                   alpha = 1, # <- here's the difference
                   nfolds = 5, 
                   parallel = TRUE, 
                   intercept = TRUE,
                   type.measure = "class")

# computing predicted values
preds <- predict(lasso, twdfm[test,], type="class")
# confusion matrix
table(preds, tweets$engaging[test])
# performance metrics (slightly better!)
accuracy(preds, tweets$engaging[test])
precision(preds==1, tweets$engaging[test]==1)
recall(preds==1, tweets$engaging[test]==1)
precision(preds==0, tweets$engaging[test]==0)
recall(preds==0, tweets$engaging[test]==0)

best.lambda <- which(lasso$lambda==lasso$lambda.1se)
beta <- lasso$glmnet.fit$beta[,best.lambda]
head(beta)

## identifying predictive features
df <- data.frame(coef = as.numeric(beta),
                 word = names(beta), stringsAsFactors = F)

df <- df[order(df$coef),]
head(df[,c("coef", "word")], n = 30)
paste(df$word[1:30], collapse = ", ")
df <- df[order(df$coef, decreasing = TRUE),]
head(df[,c("coef", "word")], n = 30)
paste(df$word[1:30], collapse = ", ") # coefficients for some features actually became zero


# elastic net 
enet <- cv.glmnet(twdfm[training,], 
                  tweets$engaging[training], 
                  family = "binomial", 
                  alpha = 0.5, # <- here's the difference
                  nfolds = 5, 
                  parallel = TRUE, 
                  intercept = TRUE,
                  type.measure = "class")
# note: this will not cross-validate across values of alpha

# computing predicted values
preds <- predict(enet, twdfm[test,], type="class")
# confusion matrix
table(preds, tweets$engaging[test])
# performance metrics (slightly better!)
accuracy(preds, tweets$engaging[test])
precision(preds==1, tweets$engaging[test]==1)
recall(preds==1, tweets$engaging[test]==1)
precision(preds==0, tweets$engaging[test]==0)
recall(preds==0, tweets$engaging[test]==0)



## distributed gradient boosting with Xgboost ----------

# If we really want the best performance at a low computational cost, the cutting-edge method many people are using is Distributed Gradient Boosting, based on the same ideas as boosted trees / random forests, implemented as `xgboost`. You can read more about the history of this package [here](https://homes.cs.washington.edu/~tqchen/2016/03/10/story-and-lessons-behind-the-evolution-of-xgboost.html).
# Paper here: http://delivery.acm.org/10.1145/2940000/2939785/p785-chen.pdf 


# converting matrix object
X <- as(twdfm, "dgCMatrix")

# parameters to explore
tryEta <- c(.1, .3, .5)
tryDepths <- c(3, 6)

# placeholders for now
bestEta = NA
bestDepth = NA
bestAcc = 0

for(eta in tryEta){
for(dp in tryDepths){	
bst <- xgb.cv(data = X[training,], 
label =  tweets$engaging[training], 
max.depth = dp,
eta = eta, 
nthread = 4,
nround = 500,
nfold = 5,
print_every_n = 100L,
objective = "binary:logistic")
# cross-validated accuracy
acc <- 1-mean(tail(bst$evaluation_log$test_error_mean))
cat("Results for eta=",eta," and depth=", dp, " : ",
acc," accuracy.\n",sep="")
if(acc>bestAcc){
bestEta=eta
bestAcc=acc
bestDepth=dp
}
}
}

cat("Best model has eta=",bestEta," and depth=", bestDepth, " : ",
bestAcc," accuracy.\n",sep="")


# running best model
rf <- xgboost(data = X[training,], 
label = tweets$engaging[training], 
max.depth = bestDepth,
eta = bestEta, 
nthread = 4,
nround = 1000,
print_every_n=100L,
objective = "binary:logistic")

# out-of-sample accuracy
preds <- predict(rf, X[test,])

cat("\nAccuracy on test set=", round(accuracy(preds>.50, tweets$engaging[test]),3))
cat("\nPrecision(1) on test set=", round(precision(preds>.50, tweets$engaging[test]),3))
cat("\nRecall(1) on test set=", round(recall(preds>.50, tweets$engaging[test]),3))
cat("\nPrecision(0) on test set=", round(precision(preds<.50, tweets$engaging[test]==0),3))
cat("\nRecall(0) on test set=", round(recall(preds<.50, tweets$engaging[test]==0),3))

# What we sacrifice is interpretability (yet again!). We can check feature importance, but it's often hard to tell what's going on exactly. Why? We only see what features "matter", but not why!

# feature importance
labels <- dimnames(X)[[2]]
importance <- xgb.importance(labels, model = rf, data = X, label = tweets$engaging)
importance <- importance[order(importance$Gain, decreasing=TRUE),]
head(importance, n=20)

# adding sign
sums <- list()
for (v in 0:1){
sums[[v+1]] <- colSums(X[tweets[,"engaging"]==v,])
}
sums <- do.call(cbind, sums)
sign <- apply(sums, 1, which.max)

df <- data.frame(
Feature = labels, 
sign = sign-1,
stringsAsFactors=F)
importance <- merge(importance, df, by="Feature")

## best predictors
for (v in 0:1){
cat("\n\n")
cat("value==", v)
importance <- importance[order(importance$Gain, decreasing=TRUE),]
print(head(importance[importance$sign==v,], n=50))
cat("\n")
cat(paste(unique(head(importance$Feature[importance$sign==v], n=50)), collapse=", "))
}







