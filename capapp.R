# App for Coursera DS Capstone Project
# © Denis Rasulev
# Slovakia, 2016

# load necessary R librabires
library(tm)         # Framework for text mining applications within R
library(NLP)        # Basic classes and methods for Natural Language Processing
library(slam)       # Data structures and algorithms for sparse arrays and matrices
library(ggplot2)    # Implementation of the grammar of graphics in R

# set our working directory
setwd("/Volumes/data/coursera/capstone")

# check if zip file already exists and download if it is missing
if (!file.exists("./data/Coursera-SwiftKey.zip")) {
    download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                  destfile = "./data/Coursera-SwiftKey.zip", method = "libcurl")
}

# check if data file exists and unzip it if necessary
if (!file.exists("./data/en_US/en_US.blogs.txt")) {
    unzip("Coursera-SwiftKey.zip", exdir = "./data/en_US", list = TRUE)
}

# FUNCTIONS DECLARATIONS BLOCK

# function for cleaning
clean <- function(x)
{
    # convert everything to lower case
    x <- tolower(x)

    # remove numbers and control symbols
    x <- gsub("[[:digit:]]", "", x)
    x <- gsub("[[:cntrl:]]", "", x)

    # replace common apostrophed phrases
    x <- gsub("i'm",      "i am", x)
    x <- gsub("i've",     "i have", x)
    x <- gsub("it's",     "it is", x)
    x <- gsub("he's",     "he is", x)
    x <- gsub("isn't",    "is not", x)
    x <- gsub("let's",    "let us", x)
    x <- gsub("she's",    "she is", x)
    x <- gsub("i'll",     "i will", x)
    x <- gsub("you're",   "you are", x)
    x <- gsub("you'll",   "you will", x)
    x <- gsub("she'll",   "she will", x)
    x <- gsub("won't",    "will not", x)
    x <- gsub("we'll",    "we will", x)
    x <- gsub("he'll",    "he will", x)
    x <- gsub("it'll",    "it will", x)
    x <- gsub("can't",    "can not", x)
    x <- gsub("that's",   "that is", x)
    x <- gsub("thats",    "that is", x)
    x <- gsub("don't",    "do not", x)
    x <- gsub("didn't",   "did not", x)
    x <- gsub("wasn't",   "was not", x)
    x <- gsub("weren't",  "were not", x)
    x <- gsub("they'll",  "they will", x)
    x <- gsub("couldn't", "could not", x)
    x <- gsub("there's",  "there is", x)

    # remove web addresses and urls
    x <- gsub(" www(.+) ", "", x)
    x <- gsub(" http(.+) ", "", x)

    # remove punctuations marks
    x <- gsub("[[:punct:]]", "", x)

    # replace misspelled apostrohped phrases, not parts of any word
    x <- gsub("isnt",     "is not", x)
    x <- gsub("youre",    "you are", x)
    x <- gsub("itll",     "it will", x)
    x <- gsub("didnt",    "did not", x)
    x <- gsub("wasnt",    "was not", x)
    x <- gsub("youll",    "you will", x)
    x <- gsub("theyll",   "they will", x)
    x <- gsub("couldnt",  "could not", x)

    # replace misspelled apostrophed phrases, could be part of word
    x <- gsub("\\bim\\b",   "i am", x)
    x <- gsub("\\bive\\b",  "i have", x)
    x <- gsub("\\bdont\\b", "do not", x)
    x <- gsub("\\bcant\\b", "can not", x)
    x <- gsub("\\bwont\\b", "will not", x)
    x <- gsub("\\byouve\\b","you have", x)

    # remove remaining single letters (repeat 5 times)
    x <- gsub("\\b [a-hj-z]\\b", "", x)
    x <- gsub("\\b [a-hj-z]\\b", "", x)
    x <- gsub("\\b [a-hj-z]\\b", "", x)
    x <- gsub("\\b [a-hj-z]\\b", "", x)
    x <- gsub("\\b [a-hj-z]\\b", "", x)

    # remove single letters in the beginning of sentence
    x <- gsub("\\b[a-hj-z]\\b ", "", x)

    # remove leading and trailing spaces
    x <- gsub("^\\s+|\\s+$", "", x)

    return(x)
}

# function for sorting n-grams in decreasing order
sort.freq <- function(x){
    srt <- sort(row_sums(x, na.rm = T), decreasing = TRUE)
    frf <- data.frame(ngram = names(srt), freq = srt, row.names = NULL, check.rows = TRUE,  stringsAsFactors = FALSE)
    return(frf)
}

# functions for tokenization
n1gram <- function(x) unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
n2gram <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
n3gram <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
n4gram <- function(x) unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)

# MAIN PROCESSING BLOCK

# read data
numLines <- 5000
news  <- readLines("./data/en_US/en_US.news.txt",    numLines, encoding = "UTF-8")
blogs <- readLines("./data/en_US/en_US.blogs.txt",   numLines, encoding = "UTF-8")
twits <- readLines("./data/en_US/en_US.twitter.txt", numLines, encoding = "UTF-8")

# combine into single block, clean and save
sample <- c(news, blogs, twits)
write(sample, file = "sample.txt")
sample <- clean(sample)
write(sample, file = "sample_clean.txt")

# create our corpus and clean it
corp <- VCorpus(VectorSource(sample))
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, removeWords, stopwords("english"))

# clear some memory
rm(sample, news, blogs, twits)

# create term document matrix for unigrams, reduce sparsity and save
tdm1 <- TermDocumentMatrix(corp, control = list(tokenize = n1gram))
tdm1 <- removeSparseTerms(tdm1, 0.9999)
frq1 <- sort.freq(tdm1)
saveRDS(frq1, file = "data1.RDS")

# create term document matrix for bigrams, reduce sparsity and save
tdm2 <- TermDocumentMatrix(corp, control = list(tokenize = n2gram))
tdm2 <- removeSparseTerms(tdm2, 0.9999)
frq2 <- sort.freq(tdm2)
saveRDS(frq2, file = "data2.RDS")

# create term document matrix for trigrams, reduce sparsity and save
tdm3 <- TermDocumentMatrix(corp, control = list(tokenize = n3gram))
tdm3 <- removeSparseTerms(tdm3, 0.9999)
frq3 <- sort.freq(tdm3)
saveRDS(frq3, file = "data3.RDS")

# create term document matrix for fourgrams, reduce sparsity and save
tdm4 <- TermDocumentMatrix(corp, control = list(tokenize = n4gram))
tdm4 <- removeSparseTerms(tdm4, 0.9999)
frq4 <- sort.freq(tdm4)
saveRDS(frq4, file = "data4.RDS")

# eof
