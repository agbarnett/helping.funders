# scholar.R
# Take Google Scholar ID and make a list of papers
# Version for shiny
# May 2017

scholar = function(scholar.id='lhc97roAAAAJ', max.authors=3, years.since=2000, order.by='ayear'){
  ret = list() # start with blank output
  
  # 0) verify scholar ID and flag problems
  fail = FALSE; problem='' # flag for failure
  if(nchar(scholar.id) != 12){fail=T; problem='Google Scholar ID should be 12 characters'}
  if(fail==T){
    ret$name = problem
    ret$papers = data.frame(NULL)
  }
  
  # a) select person
  if(fail==F){
  bio = get_profile(id=scholar.id) # get basics
  name = bio$name
  affiliation = bio$affiliation
  h = bio$h_index # h-index
  
  # b) select works
  papers = get_publications(id=scholar.id) # get works

  # c) restrict to selected years
  papers = filter(papers, is.na(year) ==F & year >= years.since)
  papers = select(papers, -cid, -pubid)
  papers$title = as.character(papers$title)
  papers$journal = as.character(papers$journal)
  papers$author = as.character(papers$author)
  papers$number = as.character(papers$number)
  
  # change names to uppercase first
  first.letter  <- toupper(substring(names(papers), 1, 1))
  other.letters <- substring(names(papers), 2)
  newnames      <- paste(first.letter, other.letters, sep="")
  names(papers) = newnames
  
  # d) format authors
  papers$Authors = ''
  for (k in 1:nrow(papers)){ # loop needed
    fauthors = strsplit(papers$Author[k], split=', ')[[1]]
    m = min(max.authors, length(fauthors))
    papers$Authors[k] = paste(fauthors[1:m], collapse=' and ', sep='')
  }
  
  papers = select(papers, -Author) # use Authors
  # remove duplicates based on title
  dups = duplicated(papers$Title)
  papers = papers[!dups,]
  
  # order by
  if(order.by=='ayear'){papers = arrange(papers, -Year)} #
  if(order.by=='dyear'){papers = arrange(papers, Year)} # 
  if(order.by=='journal'){papers = arrange(papers, Journal, Year)} # 

  # remove if journal is empty
  papers = filter(papers, Journal !='')
  
  # for appearances
  papers$Authors = as.character(papers$Authors) # 
  papers$Title = as.character(papers$Title)
  papers$Journal = as.character(papers$Journal)
  papers$Year = as.character(papers$Year) # looks better as character

  # return
  ret$name = name
  ret$affiliation = affiliation
  ret$papers = papers
  }
  
# return
return(ret)
}
