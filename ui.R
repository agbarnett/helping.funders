# Scholar version
# May 2017
library(shiny)

shinyUI(fluidPage(
  
  # Application title
  tags$h2("Reducing the administrative burden on researchers"),
  p("WORK IN PROGRESS. Researchers are often asked by funders to give their publication list, but funders often have different requirements (e.g., all papers versus only those in the last five years) and researchers waste a lot of time formating papers. ",
    "This page takes a ", tags$a(href="https://scholar.google.com.au/", "Google Scholar ID"), ' and outputs a researcher`s papers in multiple potential formats to suit whatever the funder needs. There may be a slight delay whilst the data is extracted from Google Scholar.'),
    p("No funder should ever need to ask a researcher to paste their papers into an application form. ", 
    "If any funders are interested I can set up a batch process just ", tags$a(href='mailto:a.barnett@qut.edu.au', 'e-mail'), ' me. Also please ', tags$a(href='mailto:a.barnett@qut.edu.au', 'e-mail'), ' if you have any suggestions for improvement.'),
  
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "google.id",
                label = "Google Scholar ID (12 characters)",
                value='lhc97roAAAAJ'),
                 
      numericInput(inputId = "years.since",
                              label = "Earliest year of papers",
                              min = 1970,
                              max = 2017,
                              step = 1,
                              value = 2014),
      
      numericInput(inputId = "max.authors",
                   label = "Maximum number of authors",
                   min = 1,
                   max = 50,
                   step = 1,
                   value = 3),
      
      radioButtons(inputId = "order", 
                     label = "Order papers by:",
                     choices = c("Ascending year" = "ayear",
                                 "Descending year" = "dyear",
                                 "Journal name" = "journal"), 
                     selected = 'ayear'),
      
      checkboxGroupInput(inputId = "variable", 
                         label = "What to show:",
                         choices = c("Authors" = "Authors",
                           "Title" = "Title",
                           "Journal" = "Journal",
                           "Year" = "Year"), 
                         selected = c('Title','Journal','Year')),
      
      # download
      downloadButton("report", "Generate report")
      
    ), # end of sidebar panel
    
    mainPanel(
      h3('Researcher'),
      textOutput(outputId = 'h_text'),
      h3('List of papers'),
      tableOutput(outputId = 'table')
    ) # end of main panel
    
)))
