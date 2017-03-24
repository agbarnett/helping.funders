# for ORCID
# March 2017
library(shiny)
library(tidyverse)

shinyServer(function(input, output) {
  
  source('orcid.R')
  
  # basic details:
  output$h_text <- renderText({
    results = orcid(orcid.id=input$orcid.id, max.authors=input$max.authors, years.since=input$years.since)
    paste('Researcher = ', results$name, '.\n',
          'Number of papers = ', nrow(results$papers), '.', sep='')
  })
  
  # table of papers:
  output$table <- renderTable({
    results = orcid(orcid.id=input$orcid.id, max.authors=input$max.authors, years.since=input$years.since)
    res = results$papers[, input$variable] # select columns
    #res = DT::datatable(results$papers[, input$variable, drop=FALSE])
    res
  })  


})

