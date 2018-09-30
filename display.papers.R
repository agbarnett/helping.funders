# display.papers.R
# function to display papers using different formating styles
# Sep 2018

display.papers = function(papers, in.style='APA', star.to.use, flag.OA=TRUE, 
                          bullets=FALSE, counter.start=0){
  
  # start with null file
  to.print = NULL

  for (k in 1:nrow(papers)){
    to.print = paste(to.print, '  \n') # line break
    star = ""
    if(papers$OA[k]==TRUE & flag.OA==TRUE){star = star.to.use} # star open access

    # bullet or number
    if(bullets==TRUE){ to.print = paste(to.print, paste('- ', sep='')) }
    if(bullets==FALSE){ to.print = paste(to.print, paste(k+counter.start, '. ', sep='')) }
    
    if(in.style == 'Harvard'){
      to.print = paste(to.print, paste(star, papers$Authors[k], ", ", papers$Year[k], ", '", papers$Title[k], "', *", papers$Journal[k], '*', sep=''))
      # add volume/issue/doi if not null
      if(is.na(papers$Volume[k])==F){to.print = paste(to.print, paste(', vol. ', papers$Volume[k], sep=''))}  
      if(is.na(papers$Issue[k])==F){to.print = paste(to.print, paste(', no. ', papers$Issue[k], sep=''))}  
      if(is.na(papers$Pages[k])==F){
        pages = papers$Pages[k]
        pages = gsub('--', '-', pages) # try to get en dash?
        to.print = paste(to.print, paste(', pp. ', pages, sep=''))
      }  
      if(is.na(papers$DOI[k])==F){to.print = paste(to.print, paste(', doi:', papers$DOI[k], sep=''))}
    } # end of Harvard

    if(in.style == 'APA'){
      to.print = paste(to.print, paste(star, papers$Authors[k], " (", papers$Year[k], "). ", papers$Title[k], '. *', papers$Journal[k], '*', sep=''))
      # add volume/issue/doi if not null
      if(is.na(papers$Volume[k])==F){to.print = paste(to.print, paste(', **', papers$Volume[k], '**', sep=''))}  
      if(is.na(papers$Pages[k])==F){
        pages = papers$Pages[k]
        pages = gsub('--', '-', pages) # try to get en dash?
        to.print = paste(to.print, paste(', ', pages, sep=''))
      }  
      if(is.na(papers$DOI[k])==F){to.print = paste(to.print, paste(' doi:', papers$DOI[k], sep=''))}
    } # end of APA
    
  } # end of for loop

  return(to.print)

} # end of function


