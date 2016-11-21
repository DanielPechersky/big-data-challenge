library("rjson")

readJSONFiles <- function(path_to_files, debug=FALSE)
  lapply(dir(path_to_files, full.names=TRUE, recursive=TRUE),
         if(debug) function(path) {print(path); return(fromJSON(file=path))}
         else function(path) fromJSON(file=path))

elementsFromArticles <- function(articles, ...) {
  return(sapply(articles, '[', c(...)))
}