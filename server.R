# for ORCID
# March 2017
library(shiny)
library(tidyverse)

shinyServer(function(input, output) {
  
  source('orcid.R')
  
  # basic details:
  output$h_text <- renderText({
    results = orcid(orcid.id=input$orcid.id, max.authors=input$max.authors, years.since=input$years.since)
    # output or not:
    if(dim(results$papers)[1] == 0){
      paste(results$name, '.\n', sep='')
    }
    if(dim(results$papers)[1] > 0){
      paste('Researcher = ', results$name, '.\n',
            'Number of papers = ', nrow(results$papers), '.', sep='')
    }
  })
  
  # table of papers:
  output$table <- renderTable({
    results = orcid(orcid.id=input$orcid.id, max.authors=input$max.authors, years.since=input$years.since)
    # output or not:
    if(dim(results$papers)[1] == 0){
      res = data.frame(NULL)
    }
    if(dim(results$papers)[1] > 0){
      res = results$papers[, input$variable] # select columns
    }
    res
  })
  
  # report for download; see https://shiny.rstudio.com/articles/generating-reports.html
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$max.authors)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

})
