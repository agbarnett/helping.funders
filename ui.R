# ORCID
# March 2017
library(shiny)

shinyUI(fluidPage(
  
  # Application title
  tags$h2("Reducing the administrative burden on researchers"),
  p("Researchers are often asked by funders to give their publication list, but funders often have different requirements (e.g., only papers in last five years) and researchers waste a lot of time formating papers. ",
    "This page takes an ", tags$a(href="https://orcid.org/content/orcid-public-data-file", "ORCID ID"), ' and outputs a researcher`s papers in multiple potential formats to suit whatever the funder needs. ',
    "No funder should ever need to ask a researcher to paste their papers into Word or some other horrible web form. ", 
    "If any funders are interested I can set up a batch process just ", tags$a(href='mailto:a.barnett@qut.edu.au', 'e-mail'), ' me. Also please ', tags$a(href='mailto:a.barnett@qut.edu.au', 'e-mail'), ' if you have any suggestions for improvement.', sep=''),
  
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "orcid.id",
                label = "ORCID ID (16 digits with 3 dashes)",
                value='0000-0001-6339-0374'),
                 
                numericInput(inputId = "years.since",
                              label = "Earliest year of papers",
                              min = 1970,
                              max = 2017,
                              step = 1,
                              value = 2014),
      
      numericInput(inputId = "max.authors",
                   label = "Maximum number of authors",
                   min = 1,
                   max = 100,
                   step = 1,
                   value = 3),
      
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
      textOutput(outputId = 'h_text'),
      h3('List of papers'),
      tableOutput(outputId = 'table')
    ) # end of main panel
    
)))
