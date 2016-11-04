getPages <- function(npages) {
  data <- vector(mode = "list", length=npages)
  for (i in 1:npages) {
    data[[i]] <- citationsQuery("at", page=i, num_results=100)
  }
  return(data)
}