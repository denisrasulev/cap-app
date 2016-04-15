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

# start.time <- Sys.time()
# ...Relevent codes...
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken

# news
con  <- file("./data/en_US/en_US.news.txt")
news <- readLines(con, 100000, encoding = "UTF-8")
close(con)

# blogs
con   <- file("./data/en_US/en_US.blogs.txt")
blogs <- readLines(con, 100000, encoding = "UTF-8")
close(con)

# twitter
con   <- file("./data/en_US/en_US.twitter.txt")
twits <- readLines(con, 100000, encoding = "UTF-8")
close(con)

# remove variable to free memory
rm(con)

# combine them into one
sample  <- c(news, blogs, twits)

# prepare helper function
clean <- function(x)
{
    x <- tolower(x)
    x <- gsub("[[:digit:]]", "", x)
    x <- gsub("[[:punct:]]", "", x)
    x <- gsub("[[:cntrl:]]", "", x)
    x <- gsub("^\\s+|\\s+$", "", x)
    return(x)
}

# and clean our sample from unnecessary information
sample <- clean(sample)
```

### 6. Create Corpus and N-Grams
At this stage we will do the following:
    - Construct a corpus from the files.
- Tokenization.
- Build basic n-gram model.

```{r corpus}
# helper function for convenience
freq <- function(x){
    srt <- sort(rowSums(as.matrix(x)), decreasing = TRUE)
    frf <- data.frame(word = names(srt), freq = srt)
    return(frf)
}

# prepare functions for tokenization
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))
BeegramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))

# create our corpus
corp <- VCorpus(VectorSource(sample))

# clean corpus from profanity words
profanity  <- readLines("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt")
corp <- tm_map(corp, removeWords, profanity)

# create term document matrix for unigrams
tdm1 <- TermDocumentMatrix(corp, control = list(tokenize = UnigramTokenizer))
tdm1 <- removeSparseTerms(tdm1, 0.99)
frq1 <- freq(tdm1)

# plot top 30 of unigrams
ggplot(frq1[1:30,],
       aes(x = reorder(word, freq), y = freq)) +
    geom_bar(stat = "identity", fill = "green", col = "black") +
    theme_bw() +
    coord_flip() +
    theme(axis.title.y = element_blank()) +
    labs(y = "Frequency", title = "Most common Unigrams in the Sample")

# create term document matrix for bigrams
tdm2 <- TermDocumentMatrix(corp, control = list(tokenize = BeegramTokenizer))
tdm2 <- removeSparseTerms(tdm2, 0.999)
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
tdm3 <- TermDocumentMatrix(corp, control = list(tokenize = TrigramTokenizer))
tdm3 <- removeSparseTerms(tdm3, 0.9999)
frq3 <- freq(tdm3)

# plot top 30 of bigrams
ggplot(frq3[1:30,],
       aes(x = reorder(word, freq), y = freq)) +
    geom_bar(stat = "identity", fill = "green", col = "black") +
    theme_bw() +
    coord_flip() +
    theme(axis.title.y = element_blank()) +
    labs(y = "Frequency", title = "Most common Trigrams in the Sample")
```

### 7. Plans for the Project and App

Now that we have performed some exploratory analysis, we are ready to start building the predictive model(s) and eventually the data product. Below are high-level plan to achieve this goals:

    - Use N-grams to generate tokens of one to four words.
- Find optimal balance between quality (sample size) and speed
- Build predictive model(s) using the tokens.
- Develop data product (Shiny app) to make next word prediction from user input.

For the Shiny, the plan is to create an app with a very simple interface where a user can enter a string of text. Prediction model will then give a list of suggested words to update the next one.
