
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

source("prediction.R", local = TRUE)

shinyServer(function(input, output) {
  
 simulation <- eventReactive(input$simulate, {
   predictSentence(input$sentence, required = input$required, prefix = input$prefix, maxN = input$maxN)
 })
 
 output$accuracy <- renderText({
   if(input$simulate > 0) {
     sim <- simulation()
     hits <- sim$hits
     misses <- sim$misses
     paste("<h3>Simulation Result</h3>", 
            "Hits:", hits, "Misses:", misses, "Accuracy:", paste0(round(hits * 100 / (hits + misses), 2), '%'),
           "<h3>Word-by-Word Result</h3>")
   } else {
     'Simulation result will be displayed here.'
   }
 })
  
 output$result <- renderTable({
    simulation()$detail
 })
 
 prediction <- eventReactive(input$predict, {
   cleaned = gsub("[^a-z']+", ' ', tolower(input$phrase))
   words <- strsplit(cleaned, ' ')[[1]]
   m <- pKNm(dict[[1]]$w1, tail(words, 4))
   m[order(-p)][1,]$w1
 })
 
 output$guess <- renderText({
   if(input$predict > 0) {
      prediction()
   } else {
     '???'
   }
 })

})
