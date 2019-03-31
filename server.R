# server for helping.funders
# March 2018

shinyServer(function(input, output) {
  
  source('orcid.R')
  source('paste5.R')
  
  # reactive function to get publication data for file
  multiple = reactive({
    inFile <- input$orcid.file
    if (is.null(inFile)==TRUE){return(NULL)} # stop here if no file
    if (is.null(inFile)==FALSE){
      multiple.orcids = read.table(inFile$datapath, header=FALSE)
    }
    n.people = nrow(multiple.orcids)
    
    # Loop through people
    big.o = list()
    big.o$papers = big.o$authors = big.o$auth.order = big.o$name = big.o$oa.warning= NULL
    for (k in 1:n.people){
      id.no.spaces = multiple.orcids$V1[k]
      id.no.spaces = id.no.spaces[id.no.spaces!=''] # remove missing IDs
      id.no.spaces = gsub(' $', '', id.no.spaces) # remove trailing space
    
      validate(
        need(nchar(id.no.spaces) == 19, 
             paste("ORCID IDs should be 16 numbers or X's separated by three dashes, e.g., 0000-0001-7564-073X", sep=''))
      )
      withProgress(message = 'Getting data from ORCID/Crossref', 
                 detail = 'This may take a while...', value=0, {
                   o = my.orcid(orcid.id=id.no.spaces)
                   incProgress(1)
                 })
      # concatenate 
      big.o$papers = rbind(big.o$papers, o$papers)
      big.o$authors = rbind(big.o$authors, o$authors) 
      big.o$auth.order = c(big.o$auth.order, o$auth.order)
      big.o$name = c(big.o$name, o$name)
      big.o$oa.warning = c(big.o$oa.warning, o$oa.warning)
    } # end of loop
    
    return(big.o)
  })
  
  # reactive function to get publication data for one person
  single <- reactive({
    inFile <- input$orcid.file
    if (is.null(inFile)==FALSE){return(NULL)} # stop here if there is a file
    
    id.no.spaces = input$orcid.id
    id.no.spaces = id.no.spaces[id.no.spaces!=''] # remove missing IDs
    id.no.spaces = gsub(' $', '', id.no.spaces) # remove trailing space
    
    validate(
      need(nchar(id.no.spaces) == 19, 
           paste("ORCID IDs should be 16 numbers or X's separated by three dashes, e.g., 0000-0001-7564-073X", sep=''))
    )
    withProgress(message = 'Getting data from ORCID/Crossref', 
                 detail = 'This may take a while...', value=0, {
      o = my.orcid(orcid.id=id.no.spaces)
                   incProgress(1)
    })
    return(o)
  })
  
  # results that select either single ORCID input or file
  results <- reactive({
    inFile <- input$orcid.file
    if (is.null(inFile)==TRUE){o = single(); o$type = 'single'} #
    if (is.null(inFile)==FALSE){o = multiple(); o$type = 'multiple'} #
    return(o)
  })
  
  # function to get filtered papers (used by basics and table; must be copied into report)
  my.filter = function(){
    res = data.frame(NULL)
    res = results()$papers
    if(is.null(res)==T){
      res = data.frame(NULL)
      return(res)
    }
    # add authors
    if(input$max.authors==1){res$Authors = results()$authors[,1]}
    if(input$max.authors>1){
      upper.limit = min(c(input$max.authors, ncol(results()$authors)))
      if(nrow(results()$authors) > 1){res$Authors = apply(results()$authors[, 1:upper.limit], 1, paste5, collapse=input$spacer)} #
      if(nrow(results()$authors) == 1 ){res$Authors = paste5(results()$authors, collapse=input$spacer)} #
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
    # filter by keywords
    if(input$keywords != ''){
      keywords = tolower(gsub(',|, ', '\\|', input$keywords))
      index = grep(pattern=keywords, tolower(res$Title)) #
      res = res[index, ]
    }
    return(res)
  }

  # basic details:
  output$h_text <- renderText({
    papers = my.filter()
    # percent open access
    if(sum(results()$oa.warning)==0){
      p.OA = 0
      if(is.null(papers)==FALSE){p.OA = round(100* sum(papers$OA) / nrow(papers))}
    }
    if(any(results()$oa.warning) == TRUE){ # OA routine did not work
      p.OA = '*OA routine did not work, try again*'
    }
    # percent first author
    p.first = 0
    if(is.null(papers)==F){p.first = round(100* sum(papers$First.author==1) / nrow(papers))}
    # output for single results
    if(results()$type == 'single'){
      out.text = paste('Researcher = ', results()$name, '.\n',
            'Number of papers = ', nrow(papers), 
          '. Percent of first authors papers = ', p.first, 
          '%. Percent of open access papers = ', p.OA, 
          '%.', sep='')
    }
    # output for combined (do not show first author stats because does not work)
    if(results()$type == 'multiple'){
      out.text = paste('Combined researchers = ', paste(results()$name, collapse=', '), '.\n',
            'Number of papers = ', nrow(papers), 
            '. Percent of open access papers = ', p.OA[1], 
            '%.', sep='')
    }
    out.text
  })
  
  # table of papers:
  output$table <- renderTable({
    papers = my.filter()
    if(nrow(papers)>0){
    # ordering 
    papers$Year = as.numeric(papers$Year) # for sorting
    if(input$order=='dyear'){papers = arrange(papers, -Year)} #
    if(input$order=='ayear'){papers = arrange(papers, Year)} # 
    if(input$order=='journal'){papers = arrange(papers, Journal, Year)} # 
    papers$Year = as.character(papers$Year) # looks better as character
    ## select columns and return
    # column order - to do
    papers = papers[, input$variable] # select columns
    papers
    }
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
                    orcid.file = input$orcid.file, 
                    years.since = input$years.since,
                    spacer = input$spacer,
                    journal.only = input$journal.only,
                    order = input$order,
                    extra.order = input$extra.order,
                    bullets = input$bullets,
           max.authors = input$max.authors,
           style = input$style,
           flag.OA = input$flag.OA,
           bold.author = input$bold.author)
      
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
