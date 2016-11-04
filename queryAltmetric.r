library(rjson)

getAltmetricURL <- function() {
  return("http://api.altmetric.com/v1")
}

queryString <- function(type, required_param, ...) {
  return(paste0(paste(getAltmetricURL(), type, required_param, sep="/"), '?', paste(c(...), collapse='&')))
}

query <- function(..., include_total=FALSE) {
  result = fromJSON(file=queryString(...))
  if (include_total)
    return(list(results=result$results, total=result$query$total))
  return(result$results)
}

getTimeFrames <- function() {
  return(c('at',
           '1d', '2d', '3d', '4d', '5d', '6d',
           '1w',
           '1m', '3m',
           '1y'))
}

citationsQuery <- function(timeframe, page=NULL, num_results=NULL, cited_in=NULL, doi_prefix=NULL, include_total=FALSE) {
  params <- c(if(!is.null(page)) paste0("page=", page) else NULL,
              if(!is.null(num_results)) paste0("num_results=", num_results) else NULL,
              if(!is.null(cited_in)) paste0("cited_in=", cited_in) else NULL,
              if(!is.null(doi_prefix)) paste0("doi_prefix=", doi_prefix) else NULL)
  params <- params[!is.null(params)]
  return(query('citations', timeframe, params, include_total))
}

mergeQueries <- function(queries) {
  return(unlist(queries, recursive=FALSE))
}

elementsFromArticles <- function(articles, ...) {
  elements <- c(...)
  return(sapply(articles, FUN=function(article) article[elements]))
}