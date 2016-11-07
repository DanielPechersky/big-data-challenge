library(rjson)

getAltmetricVersion <- function() {
  return('v1')
}

getAltmetricURL <- function() {
  return(paste0("http://api.altmetric.com/",getAltmetricVersion()))
}

queryString <- function(type, required_param, ...) {
  return(paste0(paste(getAltmetricURL(), type, required_param, sep="/"), '?', paste(c(...), collapse='&')))
}

query <- function(...) {
  return(fromJSON(file=queryString(...)))
}

queryFetchType <- function(type, id, key, ...) {
  return(query(paste0(if(fetch) 'fetch/' else '', type), id, c(..., paste0('key=',key))))
}

queryIDType <- function(type, id, key=NULL, fetch=FALSE, ...) {
  if (fetch) {
    if (is.null(key))
      stop("Key is necessary for fetch type query")
    return(queryFetchType(type, id, key, ...))
  }
  if (!is.null(key))
    key_param <- paste0('key=',key)
  else
    key_param <- NULL
  return(query(type, id, key_param))
}

queryID <- function(altmetric_ID, key=NULL, fetch=FALSE, ...) {
  return(queryIDType('id', altmetric_ID, key, fetch, ...))
}

queryDOI <- function(doi, key=NULL, fetch=FALSE, ...) {
  return(queryIDType('doi', doi, key, fetch, ...))
}

queryPMID <- function(pmid, key=NULL, fetch=FALSE, ...) {
  return(queryIDType('pmid', pmid, key, fetch, ...))
}

queryArXiv <- function(arXiv_ID, key=NULL, fetch=FALSE, ...) {
  return(queryIDType('arxiv', arXiv_ID, key, fetch, ...))
}

queryADS <- function(ADS_bibcode, key=NULL, fetch=FALSE, ...) {
  return(queryIDType('ads', ADS_bibcode, key, fetch, ...))
}

queryURI <- function(uri, key=NULL, fetch=FALSE, ...) {
  return(queryIDType('uri', uri, key, fetch, ...))
}

queryISBN <- function(isbn, key=NULL, fetch=FALSE, ...) {
  return(queryIDType('isbn', isbn, key, fetch, ...))
}

getTimeFrames <- function() {
  return(c('at',
           '1d', '2d', '3d', '4d', '5d', '6d',
           '1w',
           '1m', '3m',
           '1y'))
}

queryCitations <- function(timeframe, page=NULL, num_results=NULL, cited_in=NULL, doi_prefix=NULL, key=NULL, include_total=FALSE) {
  params <- c(
    if(!is.null(page)) paste0("page=", page) else NULL,
    if(!is.null(num_results)) paste0("num_results=", num_results) else NULL,
    if(!is.null(cited_in)) paste0("cited_in=", cited_in) else NULL,
    if(!is.null(doi_prefix)) paste0("doi_prefix=", doi_prefix) else NULL,
    if(!is.null(key)) paste0("key=", key) else NULL)
  params <- params[!is.null(params)]
  result <- query('citations', timeframe, params, include_total)
  if (include_total)
    return(list(results = result$results, total = result$query$total))
  return(result$results)
}

mergeQueries <- function(queries) {
  return(unlist(queries, recursive=FALSE))
}

elementsFromArticles <- function(articles, ...) {
  elements <- c(...)
  return(sapply(articles, FUN=function(article) article[elements]))
}