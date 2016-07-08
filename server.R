
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

source("prediction.R")

shinyServer(function(input, output) {
  
 prediction <- reactive({
   if(nchar(input$sentence) > 0) {
      predictSentence(input$sentence, required = input$required, prefix = input$prefix, maxN = input$maxN)
   } else {
     NULL
   }
 })
 
 output$accuracy <- renderText({
   pred <- prediction()
   if(is.null(pred)) {
     "This will show the number of hit/missed words and overall accuracy"
   } else {
     hits <- pred$hits
     misses <- pred$misses
     paste("Hits:", hits, "Misses:", misses, "Accuracy:", paste0(round(hits * 100 / (hits + misses), 2), '%'))
   }
 })
  
 output$result <- renderTable({
   pred <- prediction()
   if(is.null(pred)) {
     data.frame(Word = 'Actual word', Predictions = 'Candidates', Result = 'Hit / Miss')
   } else {
    prediction()$detail
   }
 })

})
