# orcid.R
# Take ORCID ID and make a list of papers
# use rcrossref to get better formatted data
# Version for shiny
# May 2017

# to do, add DOI
# to do, add ordering 

# orcid.id ='0000-0003-3637-2423' # Anisa
# orcid.id ='0000-0002-2358-2440' # ginny 
# orcid.id ='0000-0001-6339-0374' # me
# orcid.id = '0000-0002-2826-0627' # sonya
# orcid.id = '0000-0002-5559-3267' # nick

orcid = function(orcid.id='0000-0002-2358-2440'){
  ret = list() # start with blank output
  
  # a) select person
  bio = orcid_id(orcid = orcid.id, profile='profile') # get basics
  name = paste(bio[[1]]$`orcid-bio`$`personal-details`$`given-names`$value,
               bio[[1]]$`orcid-bio`$`personal-details`$`family-name`$value)
  
  # b) select works
  w = orcid_id(orcid = orcid.id, profile='works') # get works
  d = w[[1]]$works # get tibble
  d = arrange(d, `work-citation.work-citation-type`) # put bibtex higher for later duplicates
  # do not filter on year; do filtering on shiny server page instead as it is faster
  
  # c) removed duplicates
  titles = tolower(d$`work-title.title.value`) # get all the titles
  titles = gsub("\\.|,|:|?|!|\\'", '', titles) # remove simple punctuation to be able to better match titles
  index = duplicated(titles)
  d = d[!index,]
  
  # d1) separate to those with DOIs and those without
  if(nrow(d)>0){
    DOIs = other = NULL
    for (i in 1:nrow(d)){
      to.check = d$`work-external-identifiers.work-external-identifier`[[i]]
      if(is.null(to.check)==T){
#        cat('i=',i,'\n')
        other = rbind(other, d[i,]) # add to other
      }
      if(is.null(to.check)==F){
        doi = subset(to.check, `work-external-identifier-type`=='DOI')$`work-external-identifier-id.value` # extract DOI
        if(length(doi)==0){ # if no DOI available then use PMID
          pmid = subset(to.check, `work-external-identifier-type`=='PMID')$`work-external-identifier-id.value`
          if(length(pmid) > 0){doi = id_converter(pmid, type='pmid')$records$doi} # does not always work
          if(is.null(doi) ==T){ # if still not luck try PMC
            pcmid = subset(to.check, `work-external-identifier-type`=='PMC')$`work-external-identifier-id.value`
            if(length(pcmid) > 0){doi = id_converter(pcmid, type='pmcid')$records$doi}
          }
        }
        if(length(doi)==0){ # if no DOI available then add to 'other' frame
          other = rbind(other, d[i,])
        }
        if(length(doi)!=0){ # if DOI available ...
          DOIs = c(DOIs, doi) # ... concatenate
        }
      }
    }
    
    # d2) now get better formatted data for papers with a DOIs using crossref
    cdata.nonbibtex = cr_works(DOIs)$data
  } # end of if
  
  # d3) separate other papers into bibtex and not
  bib = NULL
  if(is.null(other)==F){
    bib = filter(other, `work-citation.work-citation-type` == "BIBTEX")
    other = filter(other, `work-citation.work-citation-type` != "BIBTEX" | is.na(`work-citation.work-citation-type`))
  }
  
  ### e) format papers with separate matrix for authors ###
  papers = bib.authors = NULL
  # e1) first bibtex papers
  if(is.null(bib) == F){
    bib.authors = matrix(data='', nrow=nrow(bib), ncol=200) # start with huge matrix
    for (k in 1:nrow(bib)){ # loop needed
      # journal
      journal = as.character(bib$`journal-title.value`[k])
      if(is.na(journal)==F){
        cut.start = gregexpr(pattern='\\(', journal)[[1]][1]
        if(cut.start>0){journal = substr(journal, 1, cut.start-2)}
      }
      if(is.na(journal)==T){
        j = bib$`work-citation.citation`[k]
        index = gregexpr(pattern='journal = \\{', j)[[1]][1] # find start of journal
        j = str_sub(j, index+11, nchar(j)) # now take next bit of text
        index = gregexpr(pattern='\\}', j)[[1]][1] # find first closing curly brackets
        journal = str_sub(j, 1, index-1)
      }
      # title
      title = as.character(bib$`work-title.title.value`[k])
      year = as.numeric(bib$`publication-date.year.value`[k])
      # volume
      r = bib$`work-citation.citation`[k]
      index = gregexpr(pattern='volume = \\{', r)[[1]][1] # find volume
      r = str_sub(r, index+10, index+10+20) # now take next short bit of text (maximum 20 for volume number)
      index = gregexpr(pattern='\\}', r)[[1]][1] # find first closing curly brackets
      volume = str_sub(r, 1, index-1)
      # authors (use a separate matrix)
      b.authors = bib$`work-contributors.contributor`[[k]]$`credit-name.value`
      if(is.null(b.authors) == F){bib.authors[k, 1:length(b.authors)] = b.authors}
      if(is.null(b.authors) == T){ # extract from bibtex
        r = bib$`work-citation.citation`[k]
        index = gregexpr(pattern='author = \\{', r)[[1]][1] # find start of authors
        r = str_sub(r, index+10, nchar(r)) # now take next bit of text
        index = gregexpr(pattern='\\}', r)[[1]][1] # find first closing curly brackets
        b.authors = str_sub(r, 1, index-1)
        b.authors = strsplit(b.authors, split = ' and ')[[1]] # split by and
        bib.authors[k, 1:length(b.authors)] = b.authors
      }
      # type
      type = bib$`work-type`[[k]]
      # put it all together
      frame = data.frame(Journal=journal, Title=title, Year=year, Volume=volume, Issue=NA, Pages=NA, Type=type, DOI=NA)
      papers = rbind(papers, frame)
    }
  }
  # e2) ... now for non bibtex from crossref
  authors.crossref = NULL
  if(nrow(cdata.nonbibtex)>0){
    authors.crossref = matrix(data='', nrow=nrow(cdata.nonbibtex), ncol=200) # start with huge matrix
    for (k in 1:nrow(cdata.nonbibtex)){ # loop needed
      # authors, convert from tibble
      fauthors = cdata.nonbibtex$author[[k]]
      if('given' %in% names(fauthors) == F){
        fauthors = filter(fauthors, is.na(name)==F) # not missing
        fauthors = paste(fauthors$name)
      }
      if('given' %in% names(fauthors)){
        fauthors = filter(fauthors, is.na(family)==F) # not missing
        fauthors = select(fauthors, given, family)
        fauthors = paste(fauthors$given, fauthors$family) # does include NA - to fix
      }
      authors.crossref[k, 1:length(fauthors)] = fauthors
      # year (convert dat, to do)
      year = format(as.Date(cdata.nonbibtex$created[k]),'%Y') 
      ## journal
      journal = cdata.nonbibtex$container.title[k] 
      # title
      title = as.character(cdata.nonbibtex$title[k])
      # volume/issue/pages
      volume = cdata.nonbibtex$volume[k]
      issue = cdata.nonbibtex$issue[k]
      pages = cdata.nonbibtex$page[k]
      # doi
      DOI = cdata.nonbibtex$DOI[k]
      # type
      type = cdata.nonbibtex$type[k]
      # put it all together
      frame = data.frame(Journal=journal, Title=title, Year=year, Volume=volume, Issue=issue, Pages=pages, Type=type, DOI=DOI) 
      papers = rbind(papers, frame)
    }
  }
  # e3) ... lastly try others
  # to do
  
  # f) combine authors and remove empty columns
  authors = rbind(bib.authors, authors.crossref)
  fmin = min(which(colSums(authors=='') == nrow(authors))) # find first empty column
  authors = authors[, 1:(fmin-1)]

  # remove duplicates (again, just a safety net, should have been caught earlier)
  dups = duplicated(tolower(papers$Title))
  papers = papers[!dups,]
  authors = authors[!dups,]
  
  ## count first author papers
  # make alternative versions of name
  reverse = paste(bio[[1]]$`orcid-bio`$`personal-details`$`family-name`$value, ', ',
                  substr(bio[[1]]$`orcid-bio`$`personal-details`$`given-names`$value,1,1), '.', sep='')
  simple = paste(substr(bio[[1]]$`orcid-bio`$`personal-details`$`given-names`$value,1,1), '. ', 
        bio[[1]]$`orcid-bio`$`personal-details`$`family-name`$value, sep='')
  s0 = paste(substr(bio[[1]]$`orcid-bio`$`personal-details`$`given-names`$value,1,1), ' ', 
                 bio[[1]]$`orcid-bio`$`personal-details`$`family-name`$value, sep='')
  s1 = paste(substr(bio[[1]]$`orcid-bio`$`personal-details`$`given-names`$value,1,1), '.[A-Z] ', 
                 bio[[1]]$`orcid-bio`$`personal-details`$`family-name`$value, sep='')
  s2 = paste(substr(bio[[1]]$`orcid-bio`$`personal-details`$`given-names`$value,1,1), '. [A-Z] ', 
             bio[[1]]$`orcid-bio`$`personal-details`$`family-name`$value, sep='')
  s3 = paste(substr(bio[[1]]$`orcid-bio`$`personal-details`$`given-names`$value,1,1), '. [A-Z]. ', 
             bio[[1]]$`orcid-bio`$`personal-details`$`family-name`$value, sep='')
  s4 = paste(substr(bio[[1]]$`orcid-bio`$`personal-details`$`given-names`$value,1,1), '.[A-Z]. ', 
             bio[[1]]$`orcid-bio`$`personal-details`$`family-name`$value, sep='')
  s5 = paste(substr(bio[[1]]$`orcid-bio`$`personal-details`$`given-names`$value,1,1), ' [A-Z] ', 
             bio[[1]]$`orcid-bio`$`personal-details`$`family-name`$value, sep='')
  middle  = paste(bio[[1]]$`orcid-bio`$`personal-details`$`given-names`$value, ' [A-Z]. ', 
                  bio[[1]]$`orcid-bio`$`personal-details`$`family-name`$value, sep='')
  name.to.search = tolower(c(name, reverse, simple, s0, s1, s2, s3, s4, s5, middle))
  index = grep(paste(name.to.search, sep='', collapse='|'), tolower(authors[,1]))
  papers$First.author = 0
  papers$First.author[index] = 1

  # for appearances
  papers$Title = as.character(papers$Title)
  papers$Journal = as.character(papers$Journal)
  if(class(papers$Year)=='factor'){
    papers$Year = as.numeric(as.character(papers$Year))
  }
  if(class(papers$Volume)=='factor'){
    papers$Volume = as.character(papers$Volume)
  }
  
  ## need to remove/change special characters like: … and --
  
  # return
  ret$name = name
  ret$papers = papers
  ret$authors = authors # separate matrix so that authors can be selected

# return
return(ret)
}
