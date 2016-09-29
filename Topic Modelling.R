# read in some stopwords:
library(tm)
stop_words <- read.table("C:/Users/vimalromeo.thottumga/Desktop/Cruscotto d'Ascolto/stopwords.txt", header = T)

# pre-processing:
Textframe$Text <- gsub("'", "", Textframe$Text)  # remove apostrophes
Textframe$Text <- gsub("[[:punct:]]", " ", Textframe$Text)  # replace punctuation with space
Textframe$Text <- gsub("[[:cntrl:]]", " ", Textframe$Text)  # replace control characters with space
Textframe$Text <- gsub("^[[:space:]]+", "", Textframe$Text) # remove whitespace at beginning of documents
Textframe$Text <- gsub("[[:space:]]+$", "", Textframe$Text) # remove whitespace at end of documents
Textframe$Text <- tolower(Textframe$Text)  # force to lowercase

# tokenize on space and output as a list:
doc.list <- strsplit(Textframe$Text, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words$stop_word | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)
###
# Compute some statistics related to the data set:
D <- length(documents)  # number of documents 
W <- length(vocab)  # number of terms in the vocab 
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document 
N <- sum(doc.length)  # total number of tokens in the data 
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus 
###
# MCMC and model tuning parameters:
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # time taken 
##
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
##
Cruscolto <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)
##
library(LDAvis)

# create the JSON object to feed the visualization:
json <- createJSON(phi = Cruscolto$phi, 
                   theta = Cruscolto$theta, 
                   doc.length = Cruscolto$doc.length, 
                   vocab = Cruscolto$vocab, 
                   term.frequency = Cruscolto$term.frequency)
##
serVis(json, out.dir = 'vis', open.browser = T)
