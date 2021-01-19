# Clustering - Travel reviews
# Loading libraries
library(ggplot2) # Make nice plots
library(factoextra) # Make plots for clusters
library(gridExtra) # Stack several plots
library(knitr) # Create html table
library(dplyr) # Data manipulation
library(tidyr) # Tidy Data

# Loading data
reviews_data <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/00484/tripadvisor_review.csv')

# Data attributes
# Attribute 1 : Unique user id
# Attribute 2 : Average user feedback on art galleries
# Attribute 3 : Average user feedback on dance clubs
# Attribute 4 : Average user feedback on juice bars
# Attribute 5 : Average user feedback on restaurants
# Attribute 6 : Average user feedback on museums
# Attribute 7 : Average user feedback on resorts
# Attribute 8 : Average user feedback on parks/picnic spots
# Attribute 9 : Average user feedback on beaches
# Attribute 10 : Average user feedback on theaters
# Attribute 11 : Average user feedback on religious institutions

# Rename columns according to attributes
new_names <- c('UserId', 'ArtGalleries', 'DanceClubs', 'JuiceBars',
               'Restaurants', 'Museums', 'Resorts', 'ParksPicnics',
               'Beaches', 'Theaters', 'Religious')
colnames(reviews_data) <- new_names

# Get rid of UserId column and scale values even though they all range from 1 to 5.
# Scaling values makes the variables to have mean zero and variance equal to 1.
reviews_data <- scale(reviews_data[,-1])
summary(reviews_data)

## K-means clustering
# Trying k = 2
set.seed(44)
kmReviews <- kmeans(reviews_data, centers = 2)
kmReviews

# Plotting clusters
# Plot the data points according to the first two
# principal components that explain most of the variance
fviz_cluster(kmReviews, data = reviews_data, geom = 'point')

## Select best k
# Run k-means for different k values where each run has 20
# random starts in order to get a more stable result. This allows the
# algorithm to attempt multiple initial configurations and
# report the best one.
k_values <- seq(2, 10, 1)

set.seed(668822544)
withins_sum <- sapply(k_values, function(k){
    kmeans(reviews_data, centers = k, nstart = 25)$tot.withinss
})

# "Elbow" method - Pick the k that has the largest decrease in
# the within clusters sum of squares (wcss).
plot(k_values, withins_sum, type='b', xlab="Number of clusters",
     y="Within groups sum of squares", main="WCSS vs K")

# It seems like a good choice of k is between 3 and 6.
# Trying k-means with these different values since the plot does not show
# a clear minimum
set.seed(668822544)
km3 <- kmeans(reviews_data, centers = 3, nstart = 25)
km4 <- kmeans(reviews_data, centers = 4, nstart = 25)
km5 <- kmeans(reviews_data, centers = 5, nstart = 25)
km6 <- kmeans(reviews_data, centers = 6, nstart = 25)

p3 <- fviz_cluster(km3, reviews_data, geom='point') + ggtitle("3 Clusters")
p4 <- fviz_cluster(km4, reviews_data, geom='point') + ggtitle("4 Clusters")
p5 <- fviz_cluster(km5, reviews_data, geom='point') + ggtitle("5 Clusters")
p6 <- fviz_cluster(km6, reviews_data, geom='point') + ggtitle("6 Clusters")

grid.arrange(p3, p4, p5, p6, ncol = 2)

# Finding with optimal k
# With k=6 there is a higher between cluster sum of squares
# which means that each cluster is very different to each other,
# which is good.
btw_df <- data.frame(k = 3:6, betweenss = sapply(list(km3, km4, km5, km6), function(km) km$betweenss))
kable(btw_df)

# However, by looking at the decrease in wccs, it seems that k=4
# is a reasonable choice. We don't want too many or too few clusters
# for this problem and there aren't that many travel categories (just 10).
kable(data.frame(k=3:10, wcss_decrease=round(abs(diff(withins_sum)), 2)))

# Checking cluster means for scaled data
km4_centers <- as.data.frame(round(km4$centers, 3))
km4_centers <- km4_centers %>% gather(Activity, Value)
km4_centers$Cluster <- rep(paste0("Cluster", " ", 1:4), 10)
km4_centers <- arrange(km4_centers, Activity)

# Interpreting clusters
# Cluster 1: People in this cluster seem to like visiting dance
# clubs as well as restaurants and it's somewhat probable that they are not very religious.
# Cluster 2: It looks like the users in this group are very religious
# ones and they also have preference for cultural activities like going to
# art galleries.
# Cluster 3: This group also likes to go outside, specifically,
# they tend to enjoy going to theaters and beaches, but don't seem get
# along with juice bars or art galleries.
# Cluster 4: This cluster really seems to enjoy juice bars, resorts and
# parks, just like cluster 2, they do not have big preference for
# activicties related to religion.

ggplot(km4_centers, aes(Activity, Value, group=Cluster, fill=Cluster)) +
    geom_bar(stat="identity", position = position_dodge()) +
    theme(axis.text.x = element_text(hjust = 1, angle = 60)) +
    labs(y = "Scaled Mean Rating", fill="") +
    ggtitle("Cluster means")

# REFERENCE
# Renjith, Shini, A. Sreekumar, and M. Jathavedan. 2018.
# Evaluation of Partitioning Clustering Algorithms for Processing
# Social Media Data in Tourism Domain. In 2018 IEEE Recent Advances in Intelligent Computational Systems (RAICS), 12731. IEEE.

## Hierarchical clustering
# Get euclidean distance matrix
distances <- dist(reviews_data, method = "euclidean")

# Perform hierarchical clustering with Wards method, which
# minimizes the variances inside each group.
reviews.hclust <- hclust(distances, method = "ward.D")

# Plot dendrogram
plot(reviews.hclust)

# Cut dendrogram/tree to obtain 4 clusters
reviews.hclusters <- cutree(reviews.hclust, k=4)

# Split data by cluster
spl <- split(as.data.frame(reviews_data), reviews.hclusters)

hclustDf <- as.data.frame(round(sapply(spl, colMeans), 3))
names(hclustDf) <- c("Cluster1", "Cluster2", "Cluster3", "Cluster4")
hclustDf$Activity <- rownames(hclustDf)

# Delete row names
rownames(hclustDf) <- NULL

# Reorder columns
hclustDf <- hclustDf[,c(5, 1:4)]
hclustDf <- gather(hclustDf, Cluster, Value, -Activity) %>% arrange(Activity)

# Interpreting clusters
# Cluster 1: This cluster really seems to enjoy juice bars, dance clubs and
# parks, they like to party, but for some reason don't like beaches.
# Cluster 2: This group also likes to go outside, specifically,
# they tend to enjoy going to art galleries and beaches, but don't seem get
# along with dance clubs or museums (which might be contradictory because of their preference
# for art).
# Cluster 3: It looks like the users in this group are very religious
# and they also have little preference for other activities that are
# not related to religion.
# Cluster 4: People in this cluster seem to like visiting restaurants, resorts
# and also museums.

ggplot(hclustDf, aes(Activity, Value, group=Cluster, fill=Cluster)) +
    geom_bar(stat="identity", position = position_dodge()) +
    theme(axis.text.x = element_text(hjust = 1, angle = 60)) +
    labs(y = "Scaled Mean Rating", fill="") +
    ggtitle("Cluster means")

# Conclusion
# This data exploration showed that the output from both k-means and hierarchical
# clustering or other clustering methods don't necessarily have to agree with
# each other. However the methods used here clearly distinguished the group
# that was more devoted to religion and managed to make each group different
# to each other.

## Text mining - Online News Feeds
# Loading packages
library(tm) # Text mining
library(SnowballC) # Word Stemming
library(caret) # Split data into train and test sets
library(lubridate) # Handle dates
library(tidyverse) # Data manipulation, plotting, etc.
library(glmnet) # Lasso regression
library(adabag) # Boosting for classification trees
library(ipred) # Bagging for regression trees

# Variables description:
# IDLink (numeric): Unique identifier of news items
# Title (string): Title of the news item according to the official media sources
# Headline (string): Headline of the news item according to the official media sources
# Source (string): Original news outlet that published the news item
# Topic (string): Query topic used to obtain the items in the official media sources
# PublishDate (timestamp): Date and time of the news items' publication
# SentimentTitle (numeric): Sentiment score of the text in the news items' title
# SentimentHeadline (numeric): Sentiment score of the text in the news items' headline
# Facebook (numeric): Final value of the news items' popularity according to the social media source Facebook
# GooglePlus (numeric): Final value of the news items' popularity according to the social media source Google+
# LinkedIn (numeric): Final value of the news items' popularity according to the social media source LinkedIn
news_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00432/Data/News_Final.csv",
                      stringsAsFactors = FALSE)
# Remove Id column
news_data <- news_data[, -1]

# Set system to english for current session
Sys.setlocale("LC_ALL", "C")

# Take smaller subset/sample from original data file
set.seed(441110560)
index <- sample(nrow(news_data), nrow(news_data)*0.25, replace = FALSE)
news_data <- news_data[index,]

# Convert Topic to factor
# Don't perform the same for Headline and Title since they are
# required to be characters for later
# Create day of the week feature
news_data <- mutate(news_data,
                    PublishDate = ymd_hms(PublishDate),
                    Topic = as.factor(Topic),
                    Source = as.factor(Source),
                    WeekDay = weekdays(PublishDate))

# Convert WeekDay to factor
news_data$WeekDay <- as.factor(news_data$WeekDay)

# Get year from 2015 and 2016
news_data <- subset(news_data, year(PublishDate) %in% 2015:2016)

# Plot average sentiment title by day of the week
# There seems to be no influence in the day of the week on the news sentiment
news_data %>% ggplot(aes(x = SentimentTitle, group = WeekDay, fill = WeekDay)) +
    geom_density(position = "stack", alpha = 0.4)

news_data %>% ggplot(aes(x = SentimentHeadline, group = WeekDay, fill = WeekDay)) +
    geom_density(position = "stack", alpha = 0.4)

# Helper function makeCorpus to create and perform basic cleaning for corpus
makeCorpus <- function(var){
    # Convert var to corpus
    corpus <- VCorpus(VectorSource(var))
    # Convert documents to lowercase
    corpus = tm_map(corpus, content_transformer(tolower))
    # Remove punctuation
    corpus = tm_map(corpus, removePunctuation)
    # Remove stop words (words like the, I, etc)
    corpus <- tm_map(corpus, removeWords, stopwords('english'))
    # Stem documents (represent words with different endings as the same word)
    corpus <- tm_map(corpus, stemDocument)
    # Return corpus
    corpus
}

# Corpus for title variable
titleCorpus <- makeCorpus(news_data$Title)

# Create word frequency matrix
frequencies <- DocumentTermMatrix(titleCorpus)
frequencies

# Inspect matrix
inspect(frequencies[10:14, 2000:2003])

# Take a look at the most frequent words (that appear at least 500 times)
findFreqTerms(frequencies, lowfreq = 500)

# Not surprisingly, the words that appear more often are related to the news topics
table(news_data$Topic)

# Get rid of sparse terms that don't appear very often
# Keep terms that are present by at least 1% or more in the news titles
sparse.title <- removeSparseTerms(frequencies, 0.99)
sparse.title

# Convert to data frame (df)
sparseTitles <- as.data.frame(as.matrix(sparse.title))

# Change names to convey R standards
names(sparseTitles) <- make.names(names(sparseTitles))

# Add dependent variable and other predictors
sparseTitles$SentimentTitle <- news_data$SentimentTitle

# Since this process will be repeated for the Headline column, two more helper functions
# are created

# getDtm: Create document term matrix (dtm)
getDtm <- function(corpus, sparseness=.99){
    removeSparseTerms(DocumentTermMatrix(corpus), sparseness)
}

# asDf: Convert from dtm to df
asDf <- function(dtm, outcome){
    df <- as.data.frame(as.matrix(dtm))
    # Friendly names
    colnames(df) <- make.names(colnames(df))
    # Append outcome
    df[, c(outcome)] <- news_data[, c(outcome)]

    df
}

# Split data: 60% to train set and the rest for testing
set.seed(1203160122)
train_index.title <- createDataPartition(sparseTitles$SentimentTitle, times = 1, p = 0.6, list = FALSE)
trainTitles <- sparseTitles[train_index.title,]
testTitles <- sparseTitles[-train_index.title,]

# Regression for SentimentTitle columns
## Lasso regression (Apply L1 Penalty for large coefficients)
# Pass x matrix of predictors and y vector because glmnet does not take data frame as input
# alpha = 1 gives Lasso penalty
lasso_fit <- glmnet(x = as.matrix(trainTitles[, -c(which(colnames(trainTitles) == 'SentimentTitle'))]),
                    y = trainTitles$SentimentTitle,
                    alpha = 1)

# Lambda = 0.5
coef(lasso_fit, s = 0.5)

# Lambda = 5
coef(lasso_fit, s = 5)

# Pick the optimal value for lambda with 5-fold cross validation
# Higher values for lambda means higher penalty for coefficientes (which may lead
# to oversmoothing)
# Lower values may lead to overfitting because we would be closer to just
# to the original objective (minimize RMSE).
# Therefore, is useful to find the best tuning parameter.
cv_lasso.title <- cv.glmnet(x = as.matrix(trainTitles[, -c(which(colnames(trainTitles) == 'SentimentTitle'))]),
                            y = trainTitles$SentimentTitle,
                            alpha = 1,
                            nfolds = 5)
plot(cv_lasso.title)

# With this lambda value we may get a higher training error
# but this compensates with the model generalization in the test set
# (better prediction accuracy)
# Best lambda (or s) that minimized error on validation set:
cv_lasso.title$lambda.min

# Make predictions on test set for the selected lambda
preds.title <- predict(lasso_fit,
                       as.matrix(testTitles[, -c(which(colnames(testTitles) == 'SentimentTitle'))]),
                       s = cv_lasso.title$lambda.min)

# Another helper function to calculate RMSE
RMSE <- function(pred, label){
    sqrt(mean((pred - label)^2))
}

# RMSE on train set = 0.1279099
preds.title.train <- predict(lasso_fit,
                             as.matrix(trainTitles[, -c(which(colnames(trainTitles) == 'SentimentTitle'))]),
                             s = cv_lasso.title$lambda.min)
RMSE(as.vector(preds.title.train), trainTitles$SentimentTitle)

# RMSE on testing = 0.1276828
RMSE(as.vector(preds.title), testTitles$SentimentTitle)

## Bagging - Fit a tree for each bootstrap sample, and then
# aggregate the predicted values from all these trees
bagged.title <- bagging(SentimentTitle ~ ., data = trainTitles, nbagg=100)
bagged.title

# RMSE on train set = 0.131872
RMSE(predict(bagged.title), trainTitles$SentimentTitle)

# Predict on test set
bag.pred.title <- predict(bagged.title, newdata = testTitles)
# RMSE on test set = 0.1316223
RMSE(bag.pred.title, testTitles$SentimentTitle)

# Corpus for headlines
headlineCorpus <- makeCorpus(news_data$Headline)
sparse.headline <- getDtm(headlineCorpus)
sparseHeadlines <- asDf(sparse.headline, "SentimentHeadline")

# Split data
set.seed(1203160122)
train_index.headline <- createDataPartition(sparseHeadlines$SentimentHeadline, times = 1, p = 0.6, list = FALSE)
trainHeadlines <- sparseHeadlines[train_index.headline,]
testHeadlines <- sparseHeadlines[-train_index.headline,]

# Regression for SentimentHeadline columns
lasso.headlines <- glmnet(x=as.matrix(trainHeadlines[, -c(which(colnames(trainHeadlines) == "SentimentHeadline"))]), y=trainHeadlines$SentimentHeadline, alpha = 1, numFolds=5)

# RMSE train = 0.1297885
pred.headlines.train <- predict(lasso.headlines,
                          as.matrix(trainHeadlines[, -c(which(colnames(trainHeadlines) == "SentimentHeadline"))]),
                          s = lasso.headlines$lambda.min)
RMSE(pred.headlines.train, trainHeadlines$SentimentHeadline)

# RMSE test = 0.1306057
pred.headlines.test <- predict(lasso.headlines,
                       as.matrix(testHeadlines[, -c(which(colnames(testHeadlines) == 'SentimentHeadline'))]),
                       s = lasso.headlines$lambda.min)
RMSE(pred.headlines.test, testHeadlines$SentimentHeadline)

# Classification for Topic column
# Remove outcomes
sparseTitles$SentimentTitle <- NULL
sparseHeadlines$SentimentHeadline <- NULL

# Change names for dtms for both title and headline
colnames(sparseTitles) <- paste0("T", colnames(sparseTitles))
colnames(sparseHeadlines) <- paste0("H", colnames(sparseHeadlines))

# Join Title and Headline sparseMatrix as features
final_data <- cbind(news_data, sparseTitles, sparseHeadlines)
final_data <- select(final_data, -c(Title, Headline, Source, PublishDate, WeekDay, SentimentTitle, SentimentHeadline))

# Split data
set.seed(882)
train_index <- createDataPartition(final_data$Topic, times = 1, p = 0.6, list = FALSE)
final_data.train <- final_data[train_index,]
final_data.test <- final_data[-train_index,]

## 10 Nearest Neighbor
knn.topic <- knn3(Topic ~., data=final_data.train, k=10)

# Since there is no higher cost involved when predicting false positives
# or false negatives (we care about specifity and sensitivity equally),
# accuracy serves as a good measure of model performance
# Accuracy on train set = 0.89
confusionMatrix(predict(knn.topic, final_data.train, type="class"),
                final_data.train$Topic)

# Test set accuracy = 0.8713657
confusionMatrix(predict(knn.topic, final_data.test, type="class"),
                final_data.test$Topic)$overall["Accuracy"]

## Boosting
# Use 90 trees and bootstrapped samples
boost.topic <- boosting(Topic ~., data=final_data.train, boost=TRUE, mfinal=90)
importanceplot(boost.topic)

# Training Acuraccy = 0.9945668
pred.boost.topic <- predict(boost.topic, final_data.train)
confusionMatrix(factor(pred.boost.topic$class), final_data.train$Topic)$overall["Accuracy"]

# Testing Accuracy = 0.9895934
pred.boost.topic <- predict(boost.topic, final_data.test)
confusionMatrix(factor(pred.boost.topic$class), final_data.test$Topic)$overall["Accuracy"]

# Summary
# In general, there is a relatively high accuracy. Boosting significantly outperformed KNN
# because it takes the average vote of many trees.

# Future Work
# Try other classification models like randomForest, CART, etc., as they might
# defeat boosting.
# Tune parameters for both KNN and boosting and futures models. It seems that
# setting an arbitrary number of neighbors affected the KNN performance on
# new data (overfitting).
# Find a way to plot the ROC Curve and get the AUC for a multiclass classification
# problem like this one.

# REFERENCE
# Nuno Moniz and Luís Torgo (2018), "Multi-Source Social Feedback of Online News Feeds",
# CoRR, abs/1801.07055