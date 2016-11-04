library(rjson)

getAltmetricURL <- function() {
  return("http://api.altmetric.com/v1")
}

queryString <- function(type, required_param, ...) {
  return(paste0(paste(getAltmetricURL(), type, required_param, sep="/"), '?', paste(c(...), collapse='&')))
}

query <- function(...) {
  return(fromJSON(file=queryString(...)))
}

getTimeFrames <- function() {
  return(c('at',
           '1d', '2d', '3d', '4d', '5d', '6d',
           '1w',
           '1m', '3m',
           '1y'))
}

citationsQuery <- function(timeframe, page=NULL, num_results=NULL, cited_in=NULL, doi_prefix=NULL) {
  params <- c(if(!is.null(page)) paste0("page=", page) else NULL,
              if(!is.null(num_results)) paste0("num_results=", num_results) else NULL,
              if(!is.null(cited_in)) ? paste0("cited_in=", cited_in) else NULL,
              if(!is.null(doi_prefix)) ? paste0("doi_prefix=", doi_prefix) else NULL)
  params <- params[!is.null(params)]
  return(query('citations', timeframe, params))
}

mergeQueries <- function(queries) {
  totals <- vector(length = length(queries))
  pages <- vector(length = length(queries))
  num_results <- vector(length = length(queries))

  for (i in 1:length(queries)) {
    query <- queries[[i]]
    info <- query$query
    result <- query$result
    totals[[i]] <- info$total
    pages[[i]] <- info$page
    num_results[[i]] <- info$num_results
  }

  results <- vector(length=sum(num_results), mode = "list")

  offset <- 0
  for (query in queries) {
    for (o in 1:query$query$num_results)
      results[[o+offset]] <- query$results[[o]]
    offset <- offset + query$query$num_results
  }


  return(list(
    query=list(
      total=totals,
      page=pages,
      num_results=num_results),
    results=results))
}

elementsFromArticles <- function(articles, ...) {
  results<-vector(mode="list", length=length(articles))
  elements<-c(...)
  for (i in 1:length(articles))
    results[[i]]<-articles[[i]][elements]
  return(results)
}