library(rjson)

# http://api.altmetric.com/

getAltmetricVersion <- function() 'v1'

getAltmetricURL <- function()
  paste0("http://api.altmetric.com/", getAltmetricVersion())

queryString <- function(type, required_param, ...)
  paste0(paste(getAltmetricURL(), type, required_param, sep="/"), '?', paste(c(...), collapse='&'))

query <- function(...) {
  library(rjson)
  return(rjson::fromJSON(file=queryString(...)))
}

queryFetchType <- function(type, id, key, include_sources=NULL, include_sections=NULL)
  # http://api.altmetric.com/docs/call_fetch.html
  query(paste0('fetch/', type), id, c(
    if(!is.null(include_sources)) paste0('include_sources=',include_sources),
    if(!is.null(include_sections)) paste0('include_sections=',include_sections),
    paste0('key=',key)))

queryIDType <- function(type, id, key=NULL, fetch=FALSE, include_sources=NULL, include_sections=NULL) {
  if (fetch) {
    if (is.null(key))
      stop("Key is necessary for fetch type query")
    return(queryFetchType(type, id, key, include_sources, include_sections))
  }

  return(query(type, id, if(!is.null(key)) paste0('key=',key)))
}

queryID <- function(altmetric_ID, key=NULL, fetch=FALSE, include_sources=NULL, include_sections=NULL)
  # http://api.altmetric.com/docs/call_id.html
  queryIDType('id', altmetric_ID, key, fetch, include_sources, include_sections)

queryDOI <- function(doi, key=NULL, fetch=FALSE, include_sources=NULL, include_sections=NULL)
  # http://api.altmetric.com/docs/call_doi.html
  queryIDType('doi', doi, key, fetch, include_sources, include_sections)

queryPMID <- function(pmid, key=NULL, fetch=FALSE, include_sources=NULL, include_sections=NULL)
  # http://api.altmetric.com/docs/call_pmid.html
  queryIDType('pmid', pmid, key, fetch, include_sources, include_sections)

queryArXiv <- function(arXiv_ID, key=NULL, fetch=FALSE, include_sources=NULL, include_sections=NULL)
  # http://api.altmetric.com/docs/call_arxiv.html
  queryIDType('arxiv', arXiv_ID, key, fetch, include_sources, include_sections)

queryADS <- function(ADS_bibcode, key=NULL, fetch=FALSE, include_sources=NULL, include_sections=NULL)
  # http://api.altmetric.com/docs/call_ads.html
  queryIDType('ads', ADS_bibcode, key, fetch, include_sources, include_sections)

queryURI <- function(uri, key=NULL, fetch=FALSE, include_sources=NULL, include_sections=NULL)
  # http://api.altmetric.com/docs/call_uri.html
  queryIDType('uri', uri, key, fetch, include_sources, include_sections)

queryISBN <- function(isbn, key=NULL, fetch=FALSE, include_sources=NULL, include_sections=NULL)
  # http://api.altmetric.com/docs/call_isbn.html
  queryIDType('isbn', isbn, key, fetch, include_sources, include_sections)

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

mergeQueries <- function(queries)
  unlist(queries, recursive=FALSE)

elementsFromArticles <- function(articles, ...)
  sapply(articles, '[', c(...))
