library(rjson)

functionToDebug <- function(f) function(file) {
  print(file)
  return(f(file))
}

applyToJSONFiles <- function(path_to_files, FUN=NULL) {
  return(lapply(dir(path_to_files, pattern="\\.json$", full.names=TRUE, recursive=TRUE),
                FUN(fromJSON(file=path))))
}

elementsFromJSONFiles <- function(path_to_files, ..., debug=FALSE) {
  attributes <- c(...)
  fun <- function(file) file[attributes]
  
  return(applyToJSONFiles(path_to_files,
                          if (debug) functionToDebug(fun)
                          else fun))
}

readJSONFiles <- function(path_to_files, debug=FALSE) {
  fun <- function(file) file
  applyToJSONFiles(if (debug) functionToDebug(fun)
                   else fun)
}

elementsFromArticles <- function(articles, ...)
  sapply(articles, '[', c(...))
