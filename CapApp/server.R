suppressWarnings(library(shiny))
suppressWarnings(library(stringr))

# load n-gram data files
freq1 <- readRDS("/Volumes/data/coursera/capstone/data1.RDS")
freq2 <- readRDS("/Volumes/data/coursera/capstone/data2.RDS")
freq3 <- readRDS("/Volumes/data/coursera/capstone/data3.RDS")
freq4 <- readRDS("/Volumes/data/coursera/capstone/data4.RDS")

# clean user input
clean <- function(x)
{
    # remove the non-alphabatical characters
    x <- gsub("[[:digit:]]", "", x)
    x <- gsub("[[:punct:]]", "", x)

    # if anything left, return it, otherwise return empty string
    ifelse(nchar(x) > 0, return(x), return(""))
}

# To predict next word
# 1. search 4grams for entry starting with the last three words of user input
# 2. if nothing found, search 3grams for the last two words of user input
# 3. if nothing found, search 2grams for the last word of user input
# 4. if nothing found, use unigram with the highest frequency

predict <- function(x)
{
    # split input string to words spearated by spaces and find its length
    x <- unlist(strsplit(x, split = " "))
    x.len <- length(x)

    was.found <- FALSE
    found <- as.character()

    # 1. search 4grams
    if (x.len >= 3 & !was.found)
    {
        # take last three words from user input
        input <- paste( x[(x.len - 2):x.len], collapse = " " )

        # search 4grams for entry starting with the last three words of user input
        search.string <- paste("^", input, sep = "")
        tmp.df <- freq4[ grep(search.string, freq4$ngram), ]

        # check if anything matched
        if ( nrow(tmp.df) != 0 )
        {
            found <- tmp.df[1,1]
            was.found <- TRUE
        }
        tmp.df <- NULL
    }

    # 2. search 3grams
    if (x.len >= 2 & !was.found)
    {
        # take last two words from user input
        input <- paste( x[(x.len-1):x.len], collapse = " ")

        # search 3grams for entry starting with the last two words of user input
        search.string <- paste("^", input, sep = "")
        tmp.df <- freq3[ grep(search.string, freq3$ngram), ]

        # check if anything matched
        if ( nrow(tmp.df) != 0 )
        {
            found <- tmp.df[1,1]
            was.found <- TRUE
        }
        tmp.df <- NULL
    }

    # 3. search 2grams
    if (x.len >= 1 & !was.found)
    {
        # take last word from user input
        input <- x[x.len]

        # search 2grams for entry starting with the last word of user input
        search.string <- paste("^", input, sep = "")
        tmp.df <- freq2[ grep(search.string, freq2$ngram), ]

        # check if anything matched
        if ( nrow(tmp.df) != 0 )
        {
            found <- tmp.df[1,1]
            was.found <- TRUE
        }
        tmp.df <- NULL;
    }

    # 4. if nothing found in 4,3,2 grams return most frequent unigam
    if (!was.found & x.len > 0)
    {
        found <- freq1$ngram[1]
    }

    final.result <- word(found, -1)

    ifelse(x.len > 0, return(data.frame(final.result)), return(data.frame(final.result = "")))

}

shinyServer(function(input, output) {

    output$prediction <- renderPrint({
        cleared.input <- clean(input$input.string)
        string.df <- predict(cleared.input)
        cat("", as.character(string.df[1,1]))
    })
})
