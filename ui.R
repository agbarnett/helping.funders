# UI for helping.funders
# May 2017

shinyUI(fluidPage(
  
  # Application title
  tags$h2("Reducing the administrative burden on researchers"),
  p("WORK IN PROGRESS. Researchers are often asked by funders to give their publication list, but funders often have different requirements (e.g., all papers versus only those in the last five years) and researchers waste a lot of time formating papers. ",
    "This page takes a researcher`s ", tags$a(href="https://orcid.org/content/orcid-public-data-file", "ORCID ID"), ' and outputs their papers in alternative formats to suit what the funder wants. 
    It uses ', tags$a(href="https://www.crossref.org/", "crossref"), ' to supplement the ORCID data. It may take a while for the output to appear because of the use of multiple databases.'),

  p("No funder should ever need to ask a researcher to paste their papers into an application form. ", 
    "If any funders are interested I can set up a batch process just ", tags$a(href='mailto:a.barnett@qut.edu.au', 'e-mail'), ' me. Also please ', tags$a(href='mailto:a.barnett@qut.edu.au', 'e-mail'), ' if you have any suggestions for improvement.', sep=''),
  
  p("Please check your papers as I cannot guarantee that lists are correct or complete."),

  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "orcid.id",
                label = "ORCID ID (16 digits with 3 dashes):",
                value='0000-0003-3637-2423'),
                 
      numericInput(inputId = "years.since",
                              label = "Earliest year of papers:",
                              min = 1970,
                              max = 2017,
                              step = 1,
                              value = 2014),
      
      numericInput(inputId = "max.authors",
                   label = "Maximum number of authors:",
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
                         label = "What details to show:",
                         choices = c("Authors" = "Authors",
                           "Title" = "Title",
                           "Journal" = "Journal",
                           "Volume" = "Volume",
                           "Issue" = "Issue",
                           "Page" = "Page",
                           "Year" = "Year",
                           "DOI" = "DOI"), 
                         selected = c('Title','Journal','Year')),
      
      checkboxInput(inputId="additional", label="Show additional options:",
                     value = FALSE, width='100%'),      
      
      conditionalPanel(
        condition = "input.additional==1",
        selectInput(inputId = "journal.only",
                  label = "What papers to include:",
                  choices = c("Journal articles only" = "Yes",
                              "Everything" = "No"),
                  selected = "No")),
      
      conditionalPanel(
        condition = "input.additional==1",
        textInput(inputId = "spacer",
                label = "Space between authors",
                value=', '))
      
      
      # download
      #downloadButton("report", "Generate report (not yet working)")
      
    ), # end of sidebar panel
    
    mainPanel(
      textOutput(outputId = 'h_text'),
      h3('List of papers'),
      tableOutput(outputId = 'table')
    ) # end of main panel
    
)))
