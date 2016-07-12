# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Text Prediction using N-Gram Model with Modified Kneser-Ney Smoothing"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width = 4,
                 h2("Simulation Configuration"),
                 sliderInput("required", label = h3("Number of candidates allowed"),
                             min = 3, max = 10, value = 3),
                 p("This is how many predicted words we can present to the user to choose from.
                   Apparently more choices lead to higher accuracy."),
                 sliderInput("prefix", label = h3("Max prefix letters allowed"),
                             min = 0, max = 5, value = 2),
                 p("When the prediction engine's first guesses are wrong, this allows additional guesses based on the first few letters of the actual word.
                   In real world this can save the user some typing."),
                 sliderInput("maxN", label = h3("Max length of history used for prediction"),
                             min = 1, max = 4, value = 3),
                 p("This controls how many previous words the prediction engine can consier.")
    ),
    
    mainPanel(width = 8,
              conditionalPanel("$('#guess').text() == ''", 
                tags$h1('Preparing the server, please be patient...', style = 'color:red')
              ),
              h2("Introduction"),
              p("This app demonstrates a text prediction engine using N-gram model with modified Kneser-Ney smoothing. The model is trained on a large corpus from Twitter, news and blogs."),
              p("The prediction engine can be tested in two modes, Single Word Prediction and Swiftkey Simulation."),
              h2("Single Word Prediction"),
              p("In this mode, you can enter a short phrase into the textbox below and press 'Predict'. The prediction engine will display the most likely next word."),
              textInput('phrase', 'Enter test phrase'), 
              p("The next word is ...", span(textOutput('guess', inline = TRUE), style = 'color:red')),
              actionButton('predict', 'Predict'),
              h2("Swiftkey Simulation"),
              p("In this mode, you can enter a complete sentence into the text area and press 'Simulate'."),
              p("The prediction engine tries to predict each word in the sentence and prints the results, which simulates the behavior of an input method like Swiftkey."),
              p("You can also tweak some parameters in the left panel to see how they affect the accuracy."),
              tags$textarea("", rows = 4, cols = 80, id = "sentence"),
              br(),
              actionButton('simulate', 'Simulate'),
              htmlOutput("accuracy"),
              tableOutput("result")
    )
  )
))
