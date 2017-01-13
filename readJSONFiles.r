# https://help.altmetric.com/support/solutions/articles/6000086844-sample-api-response

applyToJSONPaths <- function(cl, path_to_files='.', FUN, ...)
  lapply(dir(path_to_files, pattern='\\.json$', full.names=TRUE, recursive=TRUE),
         FUN, ...)

applyToJSONFiles <- function(cl, path_to_files='.', FUN, ...) {
  library(rjson)
  return(applyToJSONPaths(path_to_files,
         function(path, ...) FUN(rjson::fromJSON(file=path), ...), ...))
}

parApplyToJSONPaths <- function(cl, path_to_files='.', FUN, ...)
  parLapply(cl, dir(path_to_files, pattern='\\.json$', full.names=TRUE, recursive=TRUE),
         FUN, ...)

parApplyToJSONFiles <- function(cl, path_to_files='.', FUN, ...) {
  library(rjson)
  return(applyToJSONPaths(cl, path_to_files,
         function(path, ...) FUN(rjson::fromJSON(file=path), ...), ...))
}

elementsFromJSONFiles <- function(path_to_files='.', ...) {
  attributes <- c(...)
  return(applyToJSONFiles(path_to_files, function(file) file[attributes]))
}

readJSONFiles <- function(path_to_files='.')
  applyToJSONFiles(path_to_files, function(file) file)

elementsFromArticles <- function(articles, ...)
  sapply(articles, '[', c(...))
