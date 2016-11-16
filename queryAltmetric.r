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

# http://api.altmetric.com/docs/call_fetch.html
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

# http://api.altmetric.com/docs/call_id.html
queryID <- function(altmetric_ID, key=NULL, fetch=FALSE, ...) {
  return(queryIDType('id', altmetric_ID, key, fetch, ...))
}

# http://api.altmetric.com/docs/call_doi.html
queryDOI <- function(doi, key=NULL, fetch=FALSE, ...) {
  return(queryIDType('doi', doi, key, fetch, ...))
}

# http://api.altmetric.com/docs/call_pmid.html
queryPMID <- function(pmid, key=NULL, fetch=FALSE, ...) {
  return(queryIDType('pmid', pmid, key, fetch, ...))
}

# http://api.altmetric.com/docs/call_arxiv.html
queryArXiv <- function(arXiv_ID, key=NULL, fetch=FALSE, ...) {
  return(queryIDType('arxiv', arXiv_ID, key, fetch, ...))
}

# http://api.altmetric.com/docs/call_ads.html
queryADS <- function(ADS_bibcode, key=NULL, fetch=FALSE, ...) {
  return(queryIDType('ads', ADS_bibcode, key, fetch, ...))
}

# http://api.altmetric.com/docs/call_uri.html
queryURI <- function(uri, key=NULL, fetch=FALSE, ...) {
  return(queryIDType('uri', uri, key, fetch, ...))
}

# http://api.altmetric.com/docs/call_isbn.html
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

# http://api.altmetric.com/docs/call_citations.html
queryCitations <- function(timeframe, page=NULL, num_results=NULL, cited_in=NULL, doi_prefix=NULL, key=NULL, include_total=FALSE) {
  params <- c('citations', timeframe,
    if(!is.null(num_results)) paste0("num_results=", num_results),
    if(!is.null(cited_in)) paste0("cited_in=", cited_in),
    if(!is.null(doi_prefix)) paste0("doi_prefix=", doi_prefix),
    if(!is.null(key)) paste0("key=", key))

  if (is.null(page)) {
    query_ <- do.call(query, params)
    results <- query_$results
  } else {
    results = vector(mode = "list", length = length(page))
    for (i in 1:length(page)) {
      query_ <- do.call(query, as.list(c(params, paste0("page=", page[[i]]))))
      results[[i]] <- query_$results
    }
    results <- unlist(results, recursive=FALSE)
  }

  return(
    if (include_total) list(results=results, total=query_$query$total)
    else results)
}

mergeQueries <- function(queries) {
  return(unlist(queries, recursive=FALSE))
}

elementsFromArticles <- function(articles, ...) {
  return(sapply(articles, '[', c(...)))
}
