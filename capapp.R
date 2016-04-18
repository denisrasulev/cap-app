# App for Coursera DS Capstone
# © Denis Rasulev
# Slovakia, 2016

# load necessary R librabires
library(tm)         # Framework for text mining applications within R
library(NLP)        # Basic classes and methods for Natural Language Processing
library(RWeka)      # R interface to Weka - collection of ML algorithms for data mining tasks
library(ggplot2)    # Implementation of the grammar of graphics in R
library(stringi)    # Character string/text processing in every locale and any native encoding
library(SnowballC)  # R interface to the C libstemmer library that implements Porter's word stemming algorithm

# set our working directory
setwd("/Volumes/data/coursera/capstone")

# we need to setup this way for Weka library to work correctly on Mac
options(mc.cores=1)

# check if zip file already exists and download if it is missing
if (!file.exists("./data/Coursera-SwiftKey.zip")) {
    download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                  destfile = "./data/Coursera-SwiftKey.zip", method = "libcurl")
}

# check if data file exists and unzip it if necessary
if (!file.exists("./data/en_US/en_US.blogs.txt")) {
    unzip("Coursera-SwiftKey.zip", exdir = "./data/en_US", list = TRUE)
}

# news
con  <- file("./data/en_US/en_US.news.txt")
news <- readLines(con, 10000, encoding = "UTF-8")
close(con)

# blogs
con   <- file("./data/en_US/en_US.blogs.txt")
blogs <- readLines(con, 10000, encoding = "UTF-8")
close(con)

# twitter
con   <- file("./data/en_US/en_US.twitter.txt")
twits <- readLines(con, 10000, encoding = "UTF-8")
close(con)

# combine them into one
sample  <- c(news, blogs, twits)

# prepare helper function
clean <- function(x)
{
    x <- tolower(x)
    x <- gsub("[[:digit:]]", "", x)
    x <- gsub("[[:cntrl:]]", "", x)
    x <- gsub("^\\s+|\\s+$", "", x)
    x <- gsub(" [b-hj-z] ", "", x)      # remove all single letters beside a and i
    x <- gsub(" www(.+) ", "", x)       # remove all web urls

    x <- gsub("i'm",      "i am", x)
    x <- gsub("i've",     "i have", x)
    x <- gsub("it's",     "it is", x)
    x <- gsub("he's",     "he is", x)
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
    x <- gsub("theyll",   "they will", x)
    x <- gsub("couldn't", "could not", x)
    x <- gsub("there's",  "there is", x)

    x <- gsub("[[:punct:]]", "", x)

    x <- gsub("\\bim\\b", "i am", x)
    x <- gsub("\\bive\\b","i have", x)
    x <- gsub("dont",     "do not", x)
    x <- gsub("youre",    "you are", x)
    x <- gsub("itll",     "it will", x)
    x <- gsub("cant",     "can not", x)
    x <- gsub("didnt",    "did not", x)
    x <- gsub("wasnt",    "was not", x)
    x <- gsub("wont",     "will not", x)
    x <- gsub("youll",    "you will", x)
    x <- gsub("couldnt",  "could not", x)
    x <- gsub("\\byouve\\b", "you have", x)
    return(x)
}

# and clean our sample from unnecessary information
sample <- clean(sample)

write(sample, file = "sample.txt")

# helper function for convenience
freq <- function(x){
    srt <- sort(rowSums(as.matrix(x)), decreasing = TRUE)
    frf <- data.frame(word = names(srt), freq = srt)
    return(frf)
}

# prepare functions for tokenization
n1gramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
n2gramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
n3gramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

# create our corpus
corp <- VCorpus(VectorSource(sample))

# clean corpus from profanity words
#profanity  <- readLines("badwords.txt")
corp <- tm_map(corp, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")), mc.cores = 6)
corp <- tm_map(corp, stripWhitespace)
#corp <- tm_map(corp, removeWords, profanity)
corp <- tm_map(corp, removeWords, stopwords("english"))

# clean by dictionary
con <- file("words355.txt")
dict <- readLines(con)
close(con)

beg.time <- Sys.time()
corp_dict <- TermDocumentMatrix(corp, list(dictionary = dict))
end.time <- Sys.time()
all.time <- end.time - beg.time
all.time

# create term document matrix for unigrams
beg.time <- Sys.time()
tdm1 <- TermDocumentMatrix(corp, control = list(tokenize = n1gramTokenizer))
end.time <- Sys.time()
all.time <- end.time - beg.time
all.time
tdm1 <- removeSparseTerms(tdm1, 0.9999)
frq1 <- freq(tdm1)
inspect(tdm1)

# plot top 30 of unigrams
ggplot(frq1[1:30,],
        aes(x = reorder(word, freq), y = freq)) +
        geom_bar(stat = "identity", fill = "green", col = "black") +
        theme_bw() +
        coord_flip() +
        theme(axis.title.y = element_blank()) +
        labs(y = "Frequency", title = "Most common Unigrams in the Sample")

# create term document matrix for bigrams
beg.time <- Sys.time()
tdm2 <- TermDocumentMatrix(corp, control = list(tokenize = n2gramTokenizer))
end.time <- Sys.time()
all.time <- end.time - beg.time
all.time
tdm2 <- removeSparseTerms(tdm2, 0.9999)
frq2 <- freq(tdm2)

# plot top 30 of bigrams
ggplot(frq2[1:30,],
        aes(x = reorder(word, freq), y = freq)) +
        geom_bar(stat = "identity", fill = "green", col = "black") +
        theme_bw() +
        coord_flip() +
        theme(axis.title.y = element_blank()) +
        labs(y = "Frequency", title = "Most common Bigrams in the Sample")

# create term document matrix for trigrams
beg.time <- Sys.time()
tdm3 <- TermDocumentMatrix(corp, control = list(tokenize = n3gramTokenizer))
end.time <- Sys.time()
all.time <- end.time - beg.time
all.time
tdm3 <- removeSparseTerms(tdm3, 0.9999)
frq3 <- freq(tdm3)

# plot top 30 of trigrams
ggplot(frq3[1:30,],
        aes(x = reorder(word, freq), y = freq)) +
        geom_bar(stat = "identity", fill = "green", col = "black") +
        theme_bw() +
        coord_flip() +
        theme(axis.title.y = element_blank()) +
        labs(y = "Frequency", title = "Most common Trigrams in the Sample")

test <- as.data.frame(tdm1)

saveRDS(frq1, file = "data1.RDS")
saveRDS(frq2, file = "data2.RDS")
saveRDS(frq3, file = "data3.RDS")
