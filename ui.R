# UI for helping.funders
# June 2017

shinyUI(fluidPage(
  
  # Application title
  tags$h2("Reducing the administrative burden on researchers"),
  div(p(HTML(paste0("Researchers are often asked by funders to give their publication list, but funders often have different requirements (e.g., all papers versus only those in the last five years) and researchers waste a lot of time formating papers. ",
                    "This page takes a researcher`s ", tags$a(href="https://orcid.org/content/orcid-public-data-file", "ORCID ID"), ' and outputs their papers in alternative formats to suit what the funder wants. 
                    It uses ', a(href="https://www.crossref.org/", "crossref"), ', ', 
                    a(href="https://cran.r-project.org/web/packages/rentrez/vignettes/rentrez_tutorial.html", "rentrez"), ' and ', 
                    a(href="https://cran.r-project.org/web/packages/roadoi/vignettes/intro.html", "roadoi"), ' to supplement the ORCID data. It may take a while for the output to appear because of the use of multiple databases.')))),
  
  p("Funders should stop asking researchers to paste or type their papers into an application form. ", 
    "If any funders are interested I can set up a batch process just ", tags$a(href='mailto:a.barnett@qut.edu.au', 'e-mail'), ' me. Also please ', tags$a(href='mailto:a.barnett@qut.edu.au', 'e-mail'), ' if you find a bug or have any ideas for improvements. Thanks to Scott Chamberlain for help with R.', sep=''),
  
  strong("If papers are missing or no papers appear then please first check your ", tags$a(href="https://orcid.org/", "ORCID profile"),"as that may need updating."),
  div(p(HTML(paste0("The list will only include public works data on the ORCID record that have a ", a(href="https://en.wikipedia.org/wiki/Digital_object_identifier", "DOI"), ". Please check your papers as I cannot guarantee that lists are correct or complete.")))),
  
  p("You might also like my ", tags$a(href="https://aushsi.shinyapps.io/work-pals/", "other app"), "that creates a network diagram of groups of researchers` papers."),
  
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "orcid.id",
                label = "ORCID ID (16 digits with 3 dashes):",
                value='0000-0003-3637-2423'), # Anisa
      
      numericInput(inputId = "years.since",
                   label = "Earliest year of papers:",
                   min = 1970,
                   max = 2019,
                   step = 1,
                   value = 2015),
      
      numericInput(inputId = "max.authors",
                   label = "Maximum number of authors:",
                   min = 1,
                   max = 50,
                   step = 1,
                   value = 3),
      
      radioButtons(inputId = "order", 
                   label = "Order papers by:",
                   choices = c("Most recent at top" = "dyear",
                               "Most recent at bottom" = "ayear",
                               "Journal name" = "journal"), 
                   selected = 'ayear'),
      
      checkboxGroupInput(inputId = "variable", 
                         label = "What details to show in list:",
                         choices = c("Authors" = "Authors",
                                     "Title" = "Title",
                                     "Journal" = "Journal",
                                     "Volume" = "Volume",
                                     "Issue" = "Issue",
                                     "Pages" = "Pages",
                                     "Year" = "Year",
                                     "DOI" = "DOI",
                                     "Open Access" = "OA"), 
                         selected = c('Title','Journal','Year')),
      
      checkboxInput(inputId="additional", label="Show additional options",
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
                  value=', ')),
      
      conditionalPanel(
        condition = "input.additional==1",
        textInput(inputId = "keywords",
                  label = "Only show papers with these words in the title (separate words using a comma; case insensitive)",
                  value='')),
      
      h3("Report using selections above"),
      
      radioButtons(inputId = "style", 
                   label = "Reference style:",
                   choices = c("APA" = "APA",
                               "Harvard" = "Harvard"), 
                   selected = 'APA'),
      
      radioButtons(inputId = "extra.order", 
                   label = "Split papers by:",
                   choices = c("None" = "None",
                               "Books, papers, conferences (ARC style)" = "ARC",
                               "Author order" = 'split'), 
                   selected = 'None'),
      
      radioButtons(inputId = "bullets", 
                   label = "Use bullets or numbers:",
                   choices = c("Bullets" = "bullets",
                               "Numbers" = 'numbers'), 
                   selected = 'bullets'),
      
      checkboxInput(inputId = "flag.OA",
                    label = "Highlight Open Access papers",
                    TRUE),
      
      checkboxInput(inputId = "bold.author",
                    label = "Bold the author's name",
                    TRUE),
      # report
      downloadButton("report", "Generate Word document")
      
    ), # end of sidebar panel
    
    mainPanel(
      textOutput(outputId = 'h_text'),
      h3('List of papers'),
      tableOutput(outputId = 'table')
    ) # end of main panel
    
  )))