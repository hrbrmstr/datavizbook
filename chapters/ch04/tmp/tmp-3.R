BulkOrigin <- function(ip.list,host="v4.whois.cymru.com",port=43) {
  
  # setup query to bulk whois server
  cmd <- "begin\nverbose\n" 
  ips <- paste(unlist(ip.list), collapse="\n")
  cmd <- sprintf("%s%s\nend\n",cmd,ips)
  
  # creat the connection and post the query 
  con <- socketConnection(host=host,port=port,blocking=TRUE,open="r+")  
  cat(cmd,file=con)
  response <- readLines(con)
  close(con)
  
  # trim header, split fields and convert results
  response <- response[2:length(response)]
  # split and trim each response line
  response <- laply(response,.fun=function(n) {
    sapply(strsplit(n,"|",fixed=TRUE),trim)
  })
  # put the results into a data frame
  response <- adply(response,c(1))
  # ignore the trailing field
  response <- subset(response, select = -c(X1) )
  # assign meaningful column names
  names(response) = c("AS","IP","BGP.Prefix","CC",
                      "Registry","Allocated","AS.Name")
  # return the result
  return(response)
  
}