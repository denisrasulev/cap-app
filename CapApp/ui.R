suppressWarnings(library(shiny))

shinyUI(fluidPage(

    fluidRow(
              br(),
              column(width = 4, offset = 1),
              p(strong("Project App")),
              br() ),

    sidebarLayout(

        sidebarPanel(
            p("This is Shiny App"),
            p("\n"),
            p("Some other text")
        ),

        mainPanel(
            textInput("inputString", "Enter your sentence here", value = ""),
            submitButton("Predict"),

            h5("Predicted Next Word"),
            verbatimTextOutput("prediction"),
            textOutput('text')
        )
    )
))
