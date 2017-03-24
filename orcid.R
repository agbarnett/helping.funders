# orcid.R
# Take ORCID ID and make a list of papers
# Version for shiny
# March 2017
library(tidyverse)
library(rorcid)

#orcid.id ='0000-0002-2358-2440'

orcid = function(orcid.id, max.authors=3, years.since=2000){

  # a) select person
  bio = orcid_id(orcid = orcid.id, profile='profile') # get basics
  name = paste(bio[[1]]$`orcid-bio`$`personal-details`$`given-names`$value,
               bio[[1]]$`orcid-bio`$`personal-details`$`family-name`$value)
  
  # b) select works
  w = orcid_id(orcid = orcid.id, profile='works') # get works
  d = w[[1]]$works # get tibble
  d$`publication-date.year.value` = as.numeric(d$`publication-date.year.value`)
  
  # c) restrict to selected years; exclude missing years
  d = filter(d, is.na(`publication-date.year.value`) ==F & `publication-date.year.value` >= years.since)
  
  # d) format papers
  # split by type (bibtex or not)
  others = filter(d, `work-citation.work-citation-type` != 'BIBTEX')
  bib = filter(d, `work-citation.work-citation-type` == 'BIBTEX')
  papers = NULL
  for (k in 1:nrow(bib)){ # loop needed
    r = gsub('\\}', '', bib$`work-citation.citation`[k]) # remove closing curly
    r = strsplit(r, split='\\{|,') # now split on opening curly or comma
    journal = as.character(bib$`journal-title.value`[k])
    title = as.character(bib$`work-title.title.value`[k])
    year = as.numeric(bib$`publication-date.year.value`[k])
    a.start = grep('author =', r[[1]])+1 # start index for authors
    a.end = min(length(r[[1]]), a.start+max.authors) # end index for authors; maximum number of aithors
    authors = paste(r[[1]][a.start:a.end], collapse = '') # find authors
    frame = data.frame(Authors=authors, Journal=journal, Title=title, Year=year)
    papers = rbind(papers, frame)
  }
  # ... now for non bibtex
#  for (k in 1:nrow(others)){ # loop needed
#    r = others$`work-citation.citation`[k]
#    journal = as.character(bib$`journal-title.value`[k])
#    title = as.character(bib$`work-title.title.value`[k])
#    year = as.numeric(bib$`publication-date.year.value`[k])
#    a.start = grep('author =', r[[1]])+1 # start index for authors
#    a.end = min(length(r[[1]]), a.start+max.authors) # end index for authors; maximum number of aithors
#    authors = paste(r[[1]][a.start:a.end], collapse = '') # find authors
#    frame = data.frame(Authors=authors, Journal=journal, Title=title, Year=year)
#    papers = rbind(papers, frame)
#  }
  
  papers$Authors = as.character(papers$Authors) ## NOT in there...
  papers$Title = as.character(papers$Title)
  papers$Journal = as.character(papers$Journal)
  
# return
ret = list()
ret$name = name
ret$papers = papers
return(ret)
}

# $`last-modified-date`$value