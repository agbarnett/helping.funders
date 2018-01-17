# orcid.R
# Take ORCID ID and make a list of papers
# use rcrossref to get better formatted data
# Version for shiny
# May 2017

## Test IDs
# orcid.id = '0000-0001-8369-1238' # Suzanne
# orcid.id = '0000-0003-0152-4394' # Richard
# orcid.id = '0000-0002-7129-0039' # Sue
# orcid.id = '0000-0003-2434-4206' # David Moher
# orcid.id ='0000-0002-2358-2440' # ginny 
# orcid.id ='0000-0001-6339-0374' # me
# orcid.id = '0000-0002-5559-3267' # nick
# orcid.id='0000-0001-7733-287X'
# orcid.id='0000-0001-7564-073X' # Paul

# function for finding details in bibtex
bibtex.search = function(input, pattern, length){
  index = gregexpr(pattern=pattern, input)[[1]][1] # find issue
  if(index>=0){ 
    input = str_sub(input, index+length, nchar(input)) # now take next short bit of text (maximum 20 for volume number)
    index = gregexpr(pattern='\\}', input)[[1]][1] # find first closing curly brackets
    out = str_sub(input, 1, index-1)
    out = gsub("\\{", '', out) # remove any lingering opening curly brackets
  }
  if(index<0){out = NA}
  return(out)
}

# main function
orcid = function(orcid.id='0000-0002-2358-2440'){
  ret = list() # start with blank output

  # a) select person
  bio = orcid_id(orcid = orcid.id, profile='profile') # get basics
  name = paste(bio[[1]]$`orcid-bio`$`personal-details`$`given-names`$value,
               bio[[1]]$`orcid-bio`$`personal-details`$`family-name`$value)
  name = gsub('  ', ' ', name) # remove double space
  name = gsub(' $', '', name) # remove trailing space
  
  # b) select works
  w = orcid_id(orcid = orcid.id, profile='works') # get works
  d = w[[1]]$works # get tibble
  #d = arrange(d, `work-citation.work-citation-type`) # put bibtex higher for later duplicates
  # do not filter on year; do filtering on shiny server page instead as it is faster
  
  if(nrow(d)==0){ # if no papers then end function here
    ret$name = name
    ret$papers = NULL
    ret$authors = NULL
    return(ret)
  }
    
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
          if(length(pmid) > 0){
            pmid = gsub('MEDLINE:','', pmid) # added 17 jan 2018
            doi = id_converter(pmid, type='pmid')$records$doi # does not always work
          } 
          if(is.null(doi) ==T){ # if still not luck try PMC
            pcmid = subset(to.check, `work-external-identifier-type`=='PMC')$`work-external-identifier-id.value`
            if(length(pcmid) > 0){doi = id_converter(pcmid, type='pmcid')$records$doi}
          }
        }
        if(length(doi)==0){ # try again, searching for any DOI, works with Agricola (15 jan 2018)
          if(length(grep('doi.org',to.check$`work-external-identifier-id.value`))>0){
            doi = to.check$`work-external-identifier-id.value` # extract DOI 
            doi = gsub('https://doi.org/', '', doi) # remove http stuff
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
    if(nrow(bib)>0){
    bib.authors = matrix(data='', nrow=nrow(bib), ncol=300) # start with huge matrix
    for (k in 1:nrow(bib)){ # loop needed
      # journal
      if(bib$`work-type`[k]=='DISSERTATION'){journal = 'Dissertation'}
      if(bib$`work-type`[k]!='DISSERTATION'){
        if(is.null(bib$`journal-title.value`) == TRUE){journal = as.character(bib$`journal-title`[k])}
        if(is.null(bib$`journal-title.value`) == FALSE){journal = as.character(bib$`journal-title.value`[k])}
      }
      # removes words in brackets after journal title
#      if(is.na(journal)==F){
 #       cut.start = gregexpr(pattern='\\(', journal)[[1]][1]
#        if(cut.start>0){journal = substr(journal, 1, cut.start-2)}
#      }
      if(is.na(journal)==T){
        journal = bibtex.search(input=bib$`work-citation.citation`[k], pattern='journal = \\{|journal= \\{', length=10)
      }
      # title
      title = as.character(bib$`work-title.title.value`[k])
      year = as.numeric(bib$`publication-date.year.value`[k])
      # volume
      volume = bibtex.search(input=bib$`work-citation.citation`[k], pattern='volume = \\{|volume= \\{', length=9)
      # issue
      issue = bibtex.search(input=bib$`work-citation.citation`[k], pattern='number = \\{|number= \\{|issue = \\{|issue= \\{', length=9)
      # pages
      pages = bibtex.search(input=bib$`work-citation.citation`[k], pattern='pages = \\{|pages= \\{', length=8)
      if(bib$`work-type`[k]=='DISSERTATION'){volume = issue = pages = NA}
      # authors (use a separate matrix)
      b.authors = bib$`work-contributors.contributor`[[k]]$`credit-name.value`
      if(is.null(b.authors) == F){bib.authors[k, 1:length(b.authors)] = b.authors}
      if(is.null(b.authors) == T){ # extract from bibtex
        # if there's a PMID
        pmid.index = gregexpr(pattern='PMID', bib$`work-citation.citation`[k])[[1]][1] # find 
        if(pmid.index >= 0){
          pmid = str_sub(bib$`work-citation.citation`[k], pmid.index+5, pmid.index+5+8-1) # 8 digits long, +5 for "PMID:"
          paper.details <- entrez_summary(db="pubmed", id=pmid)
          b.authors = paper.details$authors$name
          # fill in other details too
          issue = paper.details$issue
          volume = paper.details$volume
          title = paper.details$title
          journal = paper.details$fulljournalname
        }
        if(pmid.index < 0){ # no PMID
          b.authors = bibtex.search(input=bib$`work-citation.citation`[k], pattern='author = \\{|author= \\{', length=9)
          b.authors = strsplit(b.authors, split = ' and ')[[1]] # split by and
        }
        bib.authors[k, 1:length(b.authors)] = b.authors
    }
      # type
      type = bib$`work-type`[[k]]
      # put it all together
      frame = data.frame(Journal=journal, Title=title, Year=year, Volume=volume, Issue=issue, Pages=pages, Type=type, DOI=NA)
      papers = rbind(papers, frame)
    }
    }
    }
  # e2) ... now for non bibtex from crossref
  authors.crossref = NULL
  if(nrow(cdata.nonbibtex)>0){
    authors.crossref = matrix(data='', nrow=nrow(cdata.nonbibtex), ncol=300) # start with huge matrix
    for (k in 1:nrow(cdata.nonbibtex)){ # loop needed
      # authors, convert from tibble
      fauthors = cdata.nonbibtex$author[[k]]
      fam.only = F # flag for family only
      if(is.null(fauthors)==F){
        if('family' %in% names(fauthors) & length(names(fauthors))==1){
          fauthors = fauthors$family
          fam.only = T
        }
      }
      if(fam.only==F & 'given' %in% names(fauthors) == F & is.null(fauthors)==F){
        fauthors = filter(fauthors, is.na(name)==F) # not missing
        fauthors = paste(fauthors$name)
      }
      if(fam.only==F & 'given' %in% names(fauthors) & is.null(fauthors)==F){
        fauthors = filter(fauthors, is.na(family)==F) # not missing
        fauthors = select(fauthors, given, family)
        fauthors = paste(fauthors$given, fauthors$family) # does include NA - to fix
      }
      if(is.null(fauthors)==F){
        if(length(fauthors)>ncol(authors.crossref)){fauthors = fauthors[1:ncol(authors.crossref)]} # truncate where author numbers are huge (jan 2018)
        authors.crossref[k, 1:length(fauthors)] = fauthors
      }
      # year (was based on created, fixed January 2018)
      idates = cdata.nonbibtex$issued[k]
      cdates = cdata.nonbibtex$created[k]
      if(is.na(idates)){idates = cdates} # if missing use created date
      dlengths = nchar(idates)
      idates[dlengths==4] = paste(idates[dlengths==4],'-01-01',sep='') # add years and months as needed
      idates[dlengths==7] = paste(idates[dlengths==7],'-01',sep='')
      year = format(as.Date(idates), '%Y')
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
  ## e3) ... lastly try others
  other.authors = NULL
  exceptions = c('DISSERTATION','REPORT','NEWSLETTER_ARTICLE','DATA_SET') # exceptions to journal
  if(is.null(other) == F){
    if(nrow(other)>0){
      remove = c('manuscript in preparation','manuscript under review')
      other = filter(other, tolower(`journal-title.value`) %in% remove ==F)
      other.authors = matrix(data='', nrow=nrow(other), ncol=300) # start with huge matrix
      for (k in 1:nrow(other)){ # loop needed
        # journal
        if(other$`work-type`[k]=='DATA_SET'){journal = 'Data set'}
        if(other$`work-type`[k]=='DISSERTATION'){journal = 'Dissertation'}
        if(other$`work-type`[k]=='REPORT'){journal = 'Report'}
        if(other$`work-type`[k]=='NEWSLETTER_ARTICLE'){journal = 'Newsletter article'}
        if(other$`work-type`[k]%in%exceptions == F ){journal = as.character(other$`journal-title.value`[k])}
        if(is.na(journal)==T){
          if(is.na(other$`work-citation.citation`[k])==F){ # added to fix bug 15 January 2018
            journal = bibtex.search(input=other$`work-citation.citation`[k], pattern='journal = \\{|journal= \\{', length=10)
          }
        }
        # title bib
        title = as.character(other$`work-title.title.value`[k])
        year = as.numeric(other$`publication-date.year.value`[k])
        # volume
        volume = NA # not clear if I can get anything for these
        # issue
        issue = NA
        # pages
        pages = NA
        if(other$`work-type`[k]=='DISSERTATION'){volume = issue = pages = NA}
        # Fill in authors if type is Vancouver, could add others
        # authors (use a separate matrix)
        b.authors = other$`work-contributors.contributor`[[k]]$`credit-name.value`
        if(is.null(b.authors) == T & is.na(other$`work-citation.work-citation-type`[[k]])==F){
          if(other$`work-citation.work-citation-type`[[k]] == 'FORMATTED_VANCOUVER'){
            b.authors = other$`work-citation.citation`[[k]]
            cut.start = gregexpr(pattern='\\.', b.authors)[[1]][1] # First full stop
            if(cut.start>0){b.authors = substr(b.authors, 1, cut.start-1)} # take start of text as authors
            b.authors = strsplit(b.authors, split=', ')[[1]] # split authors
          }
        }
        if(is.null(b.authors) == F){other.authors[k, 1:length(b.authors)] = b.authors} # update authors
        # type
        type = other$`work-type`[[k]]
        # put it all together
        frame = data.frame(Journal=journal, Title=title, Year=year, Volume=volume, Issue=issue, Pages=pages, Type=type, DOI=NA)
        papers = rbind(papers, frame)
      }
    }
  }
  
  # f) combine authors and remove empty columns
  authors = rbind(bib.authors, authors.crossref, other.authors)
  to.find = which(colSums(authors=='') == nrow(authors))
  if(length(to.find)==0){fmin = ncol(authors)+1 } # all columns full
  if(length(to.find)>0){fmin = min(to.find)} # find first empty column
  authors = authors[, 1:(fmin-1)]
  if(nrow(papers)==1){authors=matrix(authors); authors=t(authors)}

  # remove duplicates (again, just a safety net, should have been caught earlier)
  if(nrow(papers)>1){
    dups = duplicated(tolower(papers$Title))
    papers = papers[!dups,]
    authors = authors[!dups,]
  }
  
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
  s6 = paste(substr(bio[[1]]$`orcid-bio`$`personal-details`$`given-names`$value,1,1), '[A-Z] ', 
             bio[[1]]$`orcid-bio`$`personal-details`$`family-name`$value, sep='')
  middle  = paste(bio[[1]]$`orcid-bio`$`personal-details`$`given-names`$value, ' [A-Z]. ', 
                  bio[[1]]$`orcid-bio`$`personal-details`$`family-name`$value, sep='')
  middle1  = paste(bio[[1]]$`orcid-bio`$`personal-details`$`given-names`$value, ' [A-Z] ', 
                  bio[[1]]$`orcid-bio`$`personal-details`$`family-name`$value, sep='')
  name.to.search = tolower(c(name, reverse, simple, s0, s1, s2, s3, s4, s5, s6, middle, middle1))
  index = grep(paste(name.to.search, sep='', collapse='|'), tolower(authors[,1])) # NEED TO CHANGE TO APPROXIMATE MATCHING, SEE TIERNEY
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
  if(class(papers$Issue)=='factor'){
    papers$Issue = as.character(papers$Issue)
  }
  if(class(papers$Pages)=='factor'){
    papers$Pages = as.character(papers$Pages)
  }
  if(class(papers$DOI)=='factor'){
    papers$DOI = as.character(papers$DOI)
  }
  
  ## need to remove/change special characters like: … and -- from title
  
  # replace NAs is authors with ''
  authors[is.na(authors)==T] = ''
  
  # return
  ret$name = name
  ret$papers = papers
  ret$authors = authors # separate matrix so that authors can be selected

# return
return(ret)
}
