suppressWarnings(library(tm))
suppressWarnings(library(shiny))
suppressWarnings(library(stringr))

# load One-Gram, Two-Gram, Three-Gram and Four-Gram Data frame files
# This data is already cleansed with N-Grams frequency in decending order
# The data was convert to lower case, punctuations removed, numbers removed,
# white spaces removed, non print characters removed

fDF1 <- readRDS("/Volumes/data/coursera/capstone/data1.RDS")
fDF2 <- readRDS("/Volumes/data/coursera/capstone/data2.RDS")
fDF3 <- readRDS("/Volumes/data/coursera/capstone/data3.RDS")
#load("fDF4.RData");

#-------------------------------------------------
# This function "Clean up" the user input string
# before it is used to predict the next term
#-------------------------------------------------
CleanInputString <- function(inStr)
{
    # First remove the non-alphabatical characters
    inStr <- iconv(inStr, "latin1", "ASCII", sub = " ")
    inStr <- gsub("[^[:alpha:][:space:][:punct:]]", "", inStr);

    # Return the cleaned resulting senytense
    # If the resulting string is empty return empty and string.
    if (nchar(inStr) > 0) {
        return(inStr);
    } else {
        return("");
    }
}

#---------------------------------------
# Description of the Back Off Algorithm
#---------------------------------------
# To predict the next term of the user specified sentence
# 1. first we use a FourGram; the first three words of which are the last three words of the user provided sentence
#    for which we are trying to predict the next word. The FourGram is already sorted from highest to lowest frequency
# 2. If no FourGram is found, we back off to ThreeGram (first two words of ThreeGram last two words of the sentence)
# 3. If no TriGram is found, we back off to TwoGram (first word of TwoGram last word of the sentence)
# 4. If no TwoGram is found, we back off to OneGram (the most common word with highest frequency)
#
PredNextTerm <- function(inStr)
{
    # Split the input string across white spaces and then extract the length
    inStr <- unlist(strsplit(inStr, split=" "));
    inStrLen <- length(inStr);

    nxtTermFound <- FALSE;
    predNxtTerm <- as.character(NULL);

    # 1. First test the Four Gram using the four gram data frame
    # if (inStrLen >= 3 & !nxtTermFound)
    # {
    #     # Assemble the terms of the input string separated by one white space each
    #     inStr1 <- paste(inStr[(inStrLen-2):inStrLen], collapse=" ");
    #
    #     # Subset the Four Gram data frame
    #     searchStr <- paste("^",inStr1, sep = "");
    #     fDF4Temp <- fDF4[grep (searchStr, fDF4$terms), ];
    #
    #     # Check to see if any matching record returned
    #     if ( length(fDF4Temp[, 1]) > 1 )
    #     {
    #         predNxtTerm <- fDF4Temp[1,1];
    #         nxtTermFound <- TRUE;
    #         mesg <<- "Next word is predicted using 4-gram."
    #     }
    #     fDF4Temp <- NULL;
    # }

    # 2. Next test the Three Gram using the three gram data frame
    if (inStrLen >= 2 & !nxtTermFound)
    {
        # Assemble the terms of the input string separated by one white space each
        inStr1 <- paste(inStr[(inStrLen-1):inStrLen], collapse=" ");

        # Subset the Three Gram data frame
        searchStr <- paste("^",inStr1, sep = "");
        fDF3Temp <- fDF3[grep (searchStr, fDF3$word), ];

        # Check to see if any matching record returned
        if ( length(fDF3Temp[, 1]) > 1 )
        {
            predNxtTerm <- fDF3Temp[1,1];
            nxtTermFound <- TRUE;
        }
        fDF3Temp <- NULL;
    }

    # 3. Next test the Two Gram using the three gram data frame
    if (inStrLen >= 1 & !nxtTermFound)
    {
        # Assemble the terms of the input string separated by one white space each
        inStr1 <- inStr[inStrLen];

        # Subset the Two Gram data frame
        searchStr <- paste("^",inStr1, sep = "");
        fDF2Temp <- fDF2[grep (searchStr, fDF2$word), ];

        # Check to see if any matching record returned
        if ( length(fDF2Temp[, 1]) > 1 )
        {
            predNxtTerm <- fDF2Temp[1,1];
            nxtTermFound <- TRUE;
        }
        fDF2Temp <- NULL;
    }

    # 4. If no next term found in Four, Three and Two Grams return the most
    #    frequently used term from the One Gram using the one gram data frame
    if (!nxtTermFound & inStrLen > 0)
    {
        predNxtTerm <- fDF1$word[1];
    }

    nextTerm <- word(predNxtTerm, -1)

    if (inStrLen > 0){
        dfTemp1 <- data.frame(nextTerm)
        return(dfTemp1)
    } else {
        nextTerm <- ""
        dfTemp1 <- data.frame(nextTerm)
        return(dfTemp1)
    }
}

shinyServer(function(input, output) {

    output$prediction <- renderPrint({
        cleared <- CleanInputString(input$inputString)
        strDF <- PredNextTerm(cleared)
        cat("", as.character(strDF[1,1]))
        cat("\n")
    })
}
)
