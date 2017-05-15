# for Google Scholar
# May 2017

shinyServer(function(input, output) {
  
  source('scholar.R')
  
  # reactive function to run chart
  results <- function(){
    results = scholar(scholar.id=input$google.id, max.authors=input$max.authors, years.since=input$years.since, order.by=input$order)
    results
  }
  
  # basic details:
  output$h_text <- renderText({
    # output or not:
    if(dim(results()$papers)[1] == 0){
      paste(results()$name, '.\n', sep='')
    }
    if(dim(results()$papers)[1] > 0){
      paste('Researcher = ', results()$name, '.\n',
            'Affiliation = ', results()$affiliation, '.\n',
            'Number of papers = ', nrow(results()$papers), '.', sep='')
    }
  })
  
  # table of papers:
  output$table <- renderTable({
    # output or not:
    if(dim(results()$papers)[1] == 0){
      res = data.frame(NULL)
    }
    if(dim(results()$papers)[1] > 0){
      res = results()$papers[, input$variable] # select columns
    }
    res
  })
  
  # report for download; see https://shiny.rstudio.com/articles/generating-reports.html
  # and here http://stackoverflow.com/questions/37018983/how-to-make-pdf-download-in-shiny-app-response-to-user-inputs
  output$report <- downloadHandler(
    filename = function(){
      paste("report.pdf", sep='') # could expand
    },
    content = function(file){
      
      tempReport <- file.path('C:/temp/', "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      rmarkdown::render(
          input = tempReport,
          output_file = "built_report.pdf",
          # from output:
          params = list(table = output$table, # output does not work
                        text = results()$name)
        ) 
        readBin(con = "built_report.pdf", 
              what = "raw",
              n = file.info("built_report.pdf")[, "size"]) %>%
        writeBin(con = file)
      }
  )
  
})
