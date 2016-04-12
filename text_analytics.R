library(tm)
library(SnowballC)
#Should not be of factor type
tweets <- read.csv("/home/romeo/Desktop/Data Science/Algorithms/tweets.csv"
                   , stringsAsFactors = F)
str(tweets)
#To get negative sentiment
tweets$Negative <- as.factor(tweets$Avg <= -1)
table(tweets$Negative)
#Converting the tweets to corpus for preprocessing
corpus <- Corpus(VectorSource(tweets$Tweet))
corpus
##
corpus[[1]]
##preprocessing
##converting all tweets to lowercase
#First argument is our corpus and the second argument is what we want to do
#Similar to tapply
corpus <- tm_map(corpus, tolower)
#Converting the document corpus to plain text. This should be done after tolower because
#of the change in tm package
corpus = tm_map(corpus, PlainTextDocument)
#removing all punctuations
corpus <- tm_map(corpus, removePunctuation)
##removing stopwords
#Checking the first 10 stopwords
stopwords("english")[1:10]
stopwords("italian")[1:10]
#Removing all enlish stopwrods and also apple as it doesn't help to make any predictions
#in this case
corpus <- tm_map(corpus, removeWords,c("apple", stopwords("english")))
##Stemming our document
corpus <- tm_map(corpus, stemDocument)
####Bag of Words
##Creating document term matrix
frequencies <- DocumentTermMatrix(corpus)
frequencies
###It implies there are 3289 words in our matrix and 1181 documents
##Let's inspect some documents and words using inspect function
inspect(frequencies[1000:1005, 505:515])
##It is sparse matrix - lots of 0s
###Looking at the most frequent words. Lowfreq is the min num of times required for a
#word to appear
####Here out of more than 3000+ words only 56 appears 20+ times. So there might be 
#many words which are pretty useless for our model
findFreqTerms(frequencies, lowfreq = 20)
##Having too many features will also become a computational issue
##So lets removing them
#Here, we keep terms that appear in 0.5% of tweets or more
sparse <- removeSparseTerms(frequencies, 0.995)
sparse
##Now there are only 309 terms
##Now let's convert our sparse matrix into a dataframe to be used for predictive modelling
tweetsSparse <- as.data.frame(as.matrix(sparse))
##To solve the problem of variable names starting with numbers in R
#We can use make.names function to create appropriate variable names
#this should always be done in text analytics
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))
##Adding dependent variable to the dataframe
tweetsSparse$Negative <- tweets$Negative
###WordCloud###
library(wordcloud)
wordcloud(corpus)
###train and test set###
library(caTools)
set.seed(123)
split <- sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse <- subset(tweetsSparse, split == TRUE)
testSparse <- subset(tweetsSparse, split == FALSE)



