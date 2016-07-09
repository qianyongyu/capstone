# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Text Prediction with modified Kneser-Ney smoothing"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width = 5,
                 h2("Engine Configuration"),
                 sliderInput("required", label = h3("Number of candidates allowed"),
                             min = 3, max = 10, value = 3),
                 p("This is how many predicted words we can present to the user to choose from.
                   Apparently more choices lead to higher accuracy."),
                 sliderInput("prefix", label = h3("Max prefix letters allowed"),
                             min = 0, max = 5, value = 2),
                 p("When the engine's first guesses are wrong, this allows more guesses based on the first few letters of the actual word.
                   In real world this can save the user some typing."),
                 sliderInput("maxN", label = h3("Max length of history used for prediction"),
                             min = 1, max = 4, value = 3),
                 p("This controls how many previous words the engine can consider for prediction.")
    ),
    
    mainPanel(width = 7,
              h2("Input"),
              p("For best performance please paste whole sentences in the text area below."),
              tags$textarea("", rows = 4, cols = 80, id = "sentence"),
              h2("Summary"),
              textOutput("accuracy"),
              h2("Details"),
              wellPanel(style = "overflow-y:scroll; max-height: 400px",
                        tableOutput("result")
                        )
    )
  )
))
