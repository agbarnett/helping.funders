# orcid.R
# Take ORCID ID and make a list of papers
# Version for shiny
# March 2017
library(tidyverse)
library(rorcid)
library(stringr)

#orcid.id ='0000-0002-2358-2440'

orcid = function(orcid.id='0000-0002-2358-2440', max.authors=3, years.since=2000){
  ret = list() # start with blank output
  
  # 0) verify ORCID and flag problems
  fail = FALSE; problem='' # flag for failure
  dashes = str_count(orcid.id, '-')
  if(dashes>3){fail=T; problem='Too many dashes in ORCID ID'}
  if(dashes<3){fail=T; problem='Too few dashes in ORCID ID'}
  numbers = str_count(orcid.id, '[0-9]')
  if(numbers>16){fail=T; problem='Too many numbers in ORCID ID'}
  if(numbers<16){fail=T; problem='Too few numbers in ORCID ID'}
  if(fail==T){
    ret$name = problem
    ret$papers = data.frame(NULL)
  }
  
  # a) select person
  if(fail==F){
  bio = orcid_id(orcid = orcid.id, profile='profile') # get basics
  name = paste(bio[[1]]$`orcid-bio`$`personal-details`$`given-names`$value,
               bio[[1]]$`orcid-bio`$`personal-details`$`family-name`$value)
  
  # b) select works
  w = orcid_id(orcid = orcid.id, profile='works') # get works
  d = w[[1]]$works # get tibble
  d$`publication-date.year.value` = as.numeric(d$`publication-date.year.value`)
  
  # c) restrict to selected years; include missing years (filtered below)
  d = filter(d, is.na(`publication-date.year.value`) ==T | `publication-date.year.value` >= years.since)
  
  # d) format papers
  # split by type (bibtex or not)
  missing = filter(d, is.na(`work-citation.work-citation-type`))
  others = filter(d, `work-citation.work-citation-type` != 'BIBTEX')
  bib = filter(d, `work-citation.work-citation-type` == 'BIBTEX')
  papers = NULL
  if(nrow(bib)>0){
    for (k in 1:nrow(bib)){ # loop needed
      r = gsub('\\}', '', bib$`work-citation.citation`[k]) # remove closing curly
      r = strsplit(r, split='\\{|,') # now split on opening curly or comma
      journal = as.character(bib$`journal-title.value`[k])
      # remove anything in brackets for journal
      cut.start = gregexpr(pattern='\\(', journal)[[1]][1]
      if(cut.start>0){journal = substr(journal, 1, cut.start-2)}
      title = as.character(bib$`work-title.title.value`[k])
      year = as.numeric(bib$`publication-date.year.value`[k])
      a.start = grep('author =', r[[1]])+1 # start index for authors
      a.end = min(length(r[[1]]), a.start+max.authors) # end index for authors; maximum number of aithors
      authors = paste(r[[1]][a.start:a.end], collapse = '') # find authors
      frame = data.frame(Authors=authors, Journal=journal, Title=title, Year=year)
      papers = rbind(papers, frame)
    }
  }
  # ... now for non bibtex
  if(nrow(others)>0){
    for (k in 1:nrow(others)){ # loop needed
      r = others$`work-citation.citation`[k]
      # authors
      fauthors = others$`work-contributors.contributor`[k][[1]]$`credit-name.value`
      m = min(max.authors, length(fauthors))
      authors = paste(fauthors[1:m], collapse=' and ', sep='')
      # year
      year = others$`publication-date.year.value`[k]
      ## journal
      find.year = gregexpr(year, r)[[1]][1] # just first year
      broken = strsplit(r, split=paste(' ', year, sep=''))[[1]] # break at year
      journal = gsub(paste(fauthors, collapse='|', sep=''), '', broken[1])
      journal = gsub(', |,$', '', journal) # tidy up
      # remove anything in brackets for journal
      cut.start = gregexpr(pattern='\\(', journal)[[1]][1]
      if(cut.start>0){journal = substr(journal, 1, cut.start-2)}
      # title
      title = as.character(others$`work-title.title.value`[k])
      frame = data.frame(Authors=authors, Journal=journal, Title=title, Year=year) # add volume?
      papers = rbind(papers, frame)
    }
  }
  # ... now for missing (crossref?)
  if(nrow(missing)>0){
    for (k in 1:nrow(missing)){ # loop needed
      # authors
      fauthors = missing$`work-contributors.contributor`[k][[1]]$`credit-name.value`
      m = min(max.authors, length(fauthors))
      authors = paste(fauthors[1:m], collapse=' and ', sep='')
      # year
      year = missing$`publication-date.year.value`[k]
      # journal
      journal = as.character(missing$`journal-title.value`[k]) 
      # title
      title = as.character(missing$`work-title.title.value`[k])
      frame = data.frame(Authors=authors, Journal=journal, Title=title, Year=year) # add volume?
      papers = rbind(papers, frame)
    }
  }
  
  # filter on years again because of missing years
  papers = filter(papers, Year >= years.since)
  papers = arrange(papers, -Year) # sort
  
  papers$Authors = as.character(papers$Authors) # 
  papers$Title = as.character(papers$Title)
  papers$Journal = as.character(papers$Journal)
  papers$Year = as.character(papers$Year) # looks better as character
  
  # remove duplicates
  dups = duplicated(papers$Title)
  papers = papers[!dups,]
  
  # return
  ret$name = name
  ret$papers = papers
  }
  
# return
return(ret)
}

# $`last-modified-date`$value