library('rjson')

getAltmetricVersion <- function() {
  return('v1')
}

getAltmetricURL <- function() {
  return(paste0("http://api.altmetric.com/", getAltmetricVersion()))
}

queryString <- function(type, required_param, ...) {
  return(paste0(paste(getAltmetricURL(), type, required_param, sep="/"), '?', paste(c(...), collapse='&')))
}

query <- function(...) {
  return(fromJSON(file=queryString(...)))
}

queryFetchType <- function(type, id, key, ...) {
  return(query(paste0('fetch/', type), id, c(..., paste0('key=',key))))
}

queryIDType <- function(type, id, key=NULL, fetch=FALSE, ...) {
  if (fetch) {
    if (is.null(key))
      stop("Key is necessary for fetch type query")
    return(queryFetchType(type, id, key, ...))
  }

  return(query(type, id, if(!is.null(key)) paste0('key=',key)))
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

mergeQueries <- function(queries) {
  return(unlist(queries, recursive=FALSE))
}

queryCitations <- function(timeframe, page=NULL, num_results=NULL, cited_in=NULL, doi_prefix=NULL, key=NULL, merge_queries=TRUE, include_total=FALSE) {
  params <- c('citations', timeframe,
    if(!is.null(num_results)) paste0("num_results=", num_results),
    if(!is.null(cited_in)) paste0("cited_in=", cited_in),
    if(!is.null(doi_prefix)) paste0("doi_prefix=", doi_prefix),
    if(!is.null(key)) paste0("key=", key))

  if (is.null(page) || length(page) == 1) {
    c(params, if(!is.null(page)) paste0("page=", page))
    q <- do.call(query, as.list(params))
    results <- q$results
  } else {
    results = vector(mode = "list", length = length(page))
    i <- 1
    for (p in page) {
      q <- do.call(query, as.list(c(params, paste0("page=", p))))
      results[[i]] <- q$results
      i <- i+1
    }
    if (merge_queries)
      results <- mergeQueries(results)
  }

  if (include_total)
    return(list(results=results, total=q$query$total))
  return(results)
}

elementsFromArticles <- function(articles, ...) {
  elements <- c(...)
  return(sapply(articles, '[', elements))
}
