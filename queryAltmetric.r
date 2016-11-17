library('rjson')

# http://api.altmetric.com/

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

queryFetchType <- function(type, id, key, include_sources=NULL, include_sections=NULL) {
  # http://api.altmetric.com/docs/call_fetch.html
  
  return(query(paste0('fetch/', type), id, c(
    if(!is.null(include_sources)) paste0('include_sources=',include_sources),
    if(!is.null(include_sections)) paste0('include_sections=',include_sections),
    paste0('key=',key))))
}

queryIDType <- function(type, id, key=NULL, fetch=FALSE, include_sources=NULL, include_sections=NULL) {
  if (fetch) {
    if (is.null(key))
      stop("Key is necessary for fetch type query")
    return(queryFetchType(type, id, key, include_sources, include_sections))
  }

  return(query(type, id, if(!is.null(key)) paste0('key=',key)))
}

queryID <- function(altmetric_ID, key=NULL, fetch=FALSE, include_sources=NULL, include_sections=NULL) {
  # http://api.altmetric.com/docs/call_id.html
  
  return(queryIDType('id', altmetric_ID, key, fetch, include_sources=NULL, include_sections=NULL))
}

queryDOI <- function(doi, key=NULL, fetch=FALSE, include_sources=NULL, include_sections=NULL) {
  # http://api.altmetric.com/docs/call_doi.html
  
  return(queryIDType('doi', doi, key, fetch, include_sources=NULL, include_sections=NULL))
}

queryPMID <- function(pmid, key=NULL, fetch=FALSE, include_sources=NULL, include_sections=NULL) {
  # http://api.altmetric.com/docs/call_pmid.html
  
  return(queryIDType('pmid', pmid, key, fetch, include_sources=NULL, include_sections=NULL))
}

queryArXiv <- function(arXiv_ID, key=NULL, fetch=FALSE, include_sources=NULL, include_sections=NULL) {
  # http://api.altmetric.com/docs/call_arxiv.html
  
  return(queryIDType('arxiv', arXiv_ID, key, fetch, include_sources=NULL, include_sections=NULL))
}

queryADS <- function(ADS_bibcode, key=NULL, fetch=FALSE, include_sources=NULL, include_sections=NULL) {
  # http://api.altmetric.com/docs/call_ads.html
  
  return(queryIDType('ads', ADS_bibcode, key, fetch, include_sources=NULL, include_sections=NULL))
}

queryURI <- function(uri, key=NULL, fetch=FALSE, include_sources=NULL, include_sections=NULL) {
  # http://api.altmetric.com/docs/call_uri.html
  
  return(queryIDType('uri', uri, key, fetch, include_sources=NULL, include_sections=NULL))
}

queryISBN <- function(isbn, key=NULL, fetch=FALSE, include_sources=NULL, include_sections=NULL) {
  # http://api.altmetric.com/docs/call_isbn.html
  
  return(queryIDType('isbn', isbn, key, fetch, include_sources=NULL, include_sections=NULL))
}

queryCitations <- function(timeframe, page=NULL, num_results=NULL, cited_in=NULL, doi_prefix=NULL, key=NULL, include_total=FALSE) {
  # http://api.altmetric.com/docs/call_citations.html
  
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
