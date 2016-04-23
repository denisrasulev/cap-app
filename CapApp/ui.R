suppressWarnings(library(shiny))
suppressWarnings(library(shinythemes))

shinyUI(
    fluidPage(

        theme = "bootstrap.css",

        tags$head(
            tags$style(HTML("
                            @import url('//fonts.googleapis.com/css?family=Catamaran');

                            h1 {
                            font-family: 'Catamaran', sans-serif;
                            font-weight: 400;
                            line-height: 1.1;
                            color: #6D7993;
                            align: center;
                            }

                            body {
                            background-color: #EFEFEF;
                            }

                            "))),

        tags$h1("Coursera Data Science Specialization Capstone Project App", align = "center"),
        tags$hr(),
        tags$br(),

        fluidRow(
            column(4, align = "left", offset = 0,
                   tags$p("")
            ),
            column(4, align = "center", offset = 0,
                   textInput("input.string", label = "Please, start typing your sentence here:", value = "", width = "100%"),
                   tags$p("Scientifically predicted next word:"),
                   verbatimTextOutput("prediction")
            ),
            column(4, align = "right", offset = 0,
                   tags$p("")
            )
        ),
        tags$br(),
        tags$hr(),
        tags$br(),

        fluidRow(
            column(12, align = "center", offset = 0,
                    tags$p("\"Simplicity is the ultimate sophistication.\" - Leonardo da Vinci."),
                    tags$p("If you need more information or want to connect with me, please, follow any of the links below."),
                    tags$br(),
                    tags$p(
                        tags$a(href = "https://github.com/denrasulev/CapApp/blob/master/howitworks.Rmd", "How it works"),
                        tags$span("."),
                        tags$a(href = "http://rpubs.com/drasulev/capapp", "App presentation"),
                        tags$span("."),
                        tags$a(href = "https://github.com/denrasulev/CapApp/tree/master", "Code on GitHub")
                    ),

                    tags$p(
                        tags$a(href = "https://www.linkedin.com/in/denisrasulev", "LinkedIn"),
                        tags$span("."),
                        tags$a(href = "https://www.facebook.com/denis.rasulev", "Facebook"),
                        tags$span("."),
                        tags$a(href = "https://twitter.com/drasulev", "Twitter"),
                        tags$span("."),
                        tags$a(href = "https://github.com/denrasulev", "GitHub"),
                        tags$span("."),
                        tags$a(href = "https://www.pinterest.com/denisrasulev", "Pinterest")
                    )
            )
        ),

        tags$hr(),
        tags$footer("© Denis Rasulev 2016", align = "center")
    )
)
