library(rjson)

readJSONFiles <- function(path_to_files, ..., debug=FALSE) {
  attributes <- c(...)
  
  readPath <- 
    if (length(attributes) == 0) function(path) fromJSON(file=path) 
  else function(path) fromJSON(file=path)[attributes]
  
  return(lapply(dir(path_to_files, pattern="\\.json$", full.names=TRUE, recursive=TRUE),
                if (debug) function(path) {print(path); return(readPath(path))}
                else readPath))
}
  
elementsFromArticles <- function(articles, ...)
  sapply(articles, '[', c(...))
