#' Compute size of prediction sets.
#'
#' @import BiocGenerics
#' @export
cardinality_set = function(predicted_set){
  list_length = BiocGenerics::unlist(BiocGenerics::lapply(predicted_set,length))
  average = as.numeric(mean(list_length))
  average
}
