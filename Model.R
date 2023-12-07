

# Loading the data
Nlines <- 600000
blogs <- readLines("final/en_US/en_US.blogs.txt",Nlines)
news <- readLines("final/en_US/en_US.news.txt",Nlines,warn=FALSE)
twitter <- readLines("final/en_US/en_US.twitter.txt",Nlines,warn=FALSE)

# Split data into training, validation and test sets
set.seed(100)
idx <- sample(seq(Nlines)); N1 <- round(0.6*Nlines); N2 <- round(0.8*Nlines)
train <- c(blogs[idx[1:N1]],          news[idx[1:N1]],          twitter[idx[1:N1]])
valid <- c(blogs[idx[(N1+1):N2]],     news[idx[(N1+1):N2]],     twitter[idx[(N1+1):N2]])
test  <- c(blogs[idx[(N2+1):Nlines]], news[idx[(N2+1):Nlines]], twitter[idx[(N2+1):Nlines]])

# ngram_tokenize()
ngram_tokenize <- function(data) {
  for(i in 1:length(data)) {
    temp <- data[i]
    temp <- gsub("[.-]"," ", temp) # replace . - with space
    temp <- gsub("[[:punct:]]","",temp) # remove punctuation
    temp <- gsub("[0-9]","",temp) # remove numbers
    data[i] <- tolower(temp) # to lower case
  }
  data <- lapply(data, function(x) unlist(strsplit(x,split=" ")) ) # into words
  data <- lapply(data, function(x) grep("^[a-z]+$",x,value=TRUE) ) # select only english
  
  # Remove profane and remove stop words
  profanity <- readLines("D:/R/Data-Science-Specialization-Capstone-Project/british-swear-words-list_text-file.txt",warn=F)[-c(1:9)]; 
  stopWords <- stopwords('en'); stopWords <- gsub("[[:punct:]]","",stopWords)
  remWords <- c(profanity[-1],stopWords)
  data <- lapply(data, function(x) { x[!(x %in% remWords)] })
}

# ngram_compute()
ngram_compute <- function(data,n,mat = 0,dictionary = character()) {
  data <- ngram_tokenize(data) # Tokenize the data
  # Create n-grams
  if(n>1) { data <- sapply(data,function(x) x[x %in% dictionary]) } # select words in dictionary
  if(n==2) { # Bigrams
    idx2 <- sapply(data, function(x) ifelse(length(x)>1,TRUE,FALSE)) # rows with atleast 2 words
    data <- lapply(data[idx2],function(x) { paste(x[1:(length(x)-1)],x[2:length(x)]) })
  }
  if(n==3) { # Trigrams
    idx3 <- sapply(data, function(x) ifelse(length(x)>2,TRUE,FALSE)) # rows with atleast 3 words
    data <- lapply(data[idx3],function(x) { paste(x[1:(length(x)-2)],x[2:(length(x)-1)],x[3:length(x)]) })
  }
  # Count unique n-grams
  if(mat==1) { # simulates term-document-matrix
    L <- length(data)
    unique_ngrams <- unique(unlist(data))
    ngram <- matrix(0,length(unique_ngrams),L); rownames(ngram) <- unique_ngrams; colnames(ngram) <- 1:L
    for(i in 1:L) { ns <- table(data[[i]]);  ngram[names(ns),i] <- ns }
  }else { # obtains ngram counts
    ngram <- sort(table(unlist(data)),decreasing = TRUE)
  }
  ngram
}

# Model Building
library(quanteda)
M <- 100000
x <- 2
unigram <- ngram_compute(train[1:M],n=1)   # Unigram model
dict <- names(unigram[unigram>x])    # Dictionary
# Bigram model
bigram <- ngram_compute(train[1:M],n=2,dictionary = dict)
# Trigram model
trigram <- ngram_compute(train[1:M],n=3,dictionary = dict)


# Calculate unigram log likelihood
N <- sum(unigram) # Total number of tokens
V <- length(dict) # Number of words in the dictionary
unigram.model <- rbind(as.matrix(unigram[dict]),"UNK" = 0) + 1 # Laplacian smoothing
unigram.model <- cbind(unigram.model,log(unigram.model[,1]/(N+V)) ); colnames(unigram.model) <- c("count","MLE")

# Calculate bigram log likelihood
bigram.x <- bigram[bigram > x]
PQ <- names(bigram.x)
P <- sapply(strsplit(PQ, split = " "), function(x) x[1])
bm.1 <- as.matrix(bigram.x) + 1 # Laplacian smoothing
bm.1 <- cbind(bm.1,log(bm.1[,1]/unigram.model[P,1]) ) # "Word1" "Word2"
bm.2 <- as.matrix(cbind(rep(1,V),log(1/unigram.model[1:V,1]))) # "Word1" "UNK"
rownames(bm.2) <- paste(dict,"UNK")
bm.3 <- as.matrix(cbind(1,unigram.model["UNK",2])); rownames(bm.3) <- "UNK UNK" # "UNK" "UNK"
bigram.model <- rbind(bm.1,bm.2,bm.3); colnames(bigram.model) <- c("count","MLE")

# Calculate trigram log likelihood
trigram.x <- trigram[trigram > x]
PQR <- rownames(trigram.x)
PQ <- sapply(strsplit(PQR, split = " "), function(x) paste(x[1:2],collapse = " "))
trigram.model <- cbind(as.matrix(trigram.x+1), log(as.matrix(trigram.x+1)/bigram.model[PQ,1]) )
colnames(trigram.model) <- c("count","MLE")

# SUnigram, Bigram and Trigram Models
unigram.mle <- sort(unigram.model[,2], decreasing = TRUE)
bigram.mle <- sort(bigram.model[,2], decreasing = TRUE)
trigram.mle <- sort(trigram.model[,2], decreasing = TRUE)
# Save the results
save(unigram.model,bigram.model,trigram.model,
     unigram.mle,bigram.mle,trigram.mle,file="Model.RData")
