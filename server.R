# server for helping.funders
# May 2017

shinyServer(function(input, output) {
  
  source('orcid.R')
  source('paste5.R')
  
  # reactive function to get publication data
  results <- reactive({
    validate(
      need(nchar(input$orcid.id) == 19, 
           paste("ORCID IDs should be 16 numbers separated by three dashes, e.g., 0000-0002-2358-2440", sep=''))
    )
    orcid(orcid.id=input$orcid.id)
  })
  
  # function to get filtered papers (used by basics and table; must be copied into report)
  my.filter = function(){
    res = data.frame(NULL)
    res = results()$papers
    # add authors
    if(input$max.authors==1){res$Authors = results()$authors[,1]}
    if(input$max.authors>1){
      upper.limit = min(c(input$max.authors, ncol(results()$authors)))
      res$Authors = apply(results()$authors[, 1:upper.limit], 1, paste5, collapse=input$spacer) #
    } 
    # add et al
    if(input$max.authors < ncol(results()$authors)){ # don't add if at max author number
      index = results()$authors[, input$max.authors+1] != '' # something in next author
      res$Authors[index] = paste(res$Authors[index], input$spacer, 'et al', sep='')
    }
    # filter by year:
    res = subset(res, Year>= input$years.since) 
    # journal articles only
    if(input$journal.only=='Yes'){
      index = grep(pattern='journal', tolower(res$Type)) # search for journal in type
      res = res[index, ]
    }
    return(res)
  }

  # basic details:
  output$h_text <- renderText({
    papers = my.filter()
    # percent first author
    p.first = round(100* sum(papers$First.author==1) / nrow(papers))
    # output
    paste('Researcher = ', results()$name, '.\n',
            'Number of papers = ', nrow(papers), 
          '. Percent of first authors papers = ', p.first, 
          '%.', sep='')
    # Add percent of first author papers?
  })
  
  # table of papers:
  output$table <- renderTable({
    papers = my.filter()
    # ordering 
    papers$Year = as.numeric(papers$Year) # for sorting
    if(input$order=='ayear'){papers = arrange(papers, -Year)} #
    if(input$order=='dyear'){papers = arrange(papers, Year)} # 
    if(input$order=='journal'){papers = arrange(papers, Journal, Year)} # 
    papers$Year = as.character(papers$Year) # looks better as character
    ## select columns and return
    # column order - to do
    papers = papers[, input$variable] # select columns
    papers
  })
  
  # report for download; see https://shiny.rstudio.com/articles/generating-reports.html
  # and here http://stackoverflow.com/questions/37018983/how-to-make-pdf-download-in-shiny-app-response-to-user-inputs
  output$report <- downloadHandler(
    filename = function(){
      paste("report.docx", sep='') # could expand, e.g., see here: 
    },
    content = function(file){
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      #tempReport <- "C:/temp/report.Rmd"
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params = list(orcid.id = input$orcid.id, 
                    years.since = input$years.since,
                    spacer = input$spacer,
                    journal.only = input$journal.only,
                    order = input$order,
           max.authors = input$max.authors,
           style = input$style)
      
      out = rmarkdown::render(
          input = tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        ) 
      file.rename(out, file)
    }
  )
  
})
