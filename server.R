# server for helping.funders
# May 2017

shinyServer(function(input, output) {
  
  source('orcid.R')
  
  # reactive function to get publication data
  results <- reactive({
    validate(
      need(nchar(input$orcid.id) == 19, 
           paste("ORCID IDs should be 16 numbers separated by three dashes, e.g., 0000-0002-2358-2440", sep=''))
    )
    orcid(orcid.id=input$orcid.id)
  })
  
  # separate function to get filtered data?
  
  # basic details:
  output$h_text <- renderText({
    paste(results()$name, '.\n', sep='')
    res = data.frame(NULL)
    res = results()$papers
    res = subset(res, Year>= input$years.since) # filter by year to give updated number of papers
    # journal articles only
    if(input$journal.only=='Yes'){
      index = grep(pattern='journal', tolower(res$Type)) # search for journal in type
      res = res[index, ]
    }
    paste('Researcher = ', results()$name, '.\n',
            'Number of papers = ', nrow(res), '.', sep='')
    # Add percent of first author papers?
  })
  
  # table of papers:
  output$table <- renderTable({
    res = data.frame(NULL)
    res = results()$papers
    # add authors
    if(input$max.authors==1){res$Authors = results()$authors[,1]}
    if(input$max.authors>1){
      res$Authors = apply(results()$authors[, 1:input$max.authors], 1, paste, collapse= ' and ') # could make collapse another input
    } 
    # filter by year:
    res = subset(res, Year>= input$years.since) 
    # ordering 
    res$Year = as.numeric(res$Year) # for sorting
    if(input$order=='ayear'){res = arrange(res, -Year)} #
    if(input$order=='dyear'){res = arrange(res, Year)} # 
    if(input$order=='journal'){res = arrange(res, Journal, Year)} # 
    res$Year = as.character(res$Year) # looks better as character
    # journal articles only
    if(input$journal.only=='Yes'){
      index = grep(pattern='journal', tolower(res$Type)) # search for journal in type
      res = res[index, ]
    }
    # select columns and return
    res = res[, input$variable] # select columns
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
