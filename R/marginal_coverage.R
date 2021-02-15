#' Compute marginal coverage.
#'
#' @import BiocGenerics
#' @export
marginal_coverage = function(y_test, predicted_set){
  nb_rows = length(predicted_set)
  coverage = c()
  for (n in 1:nb_rows){
    coverage = BiocGenerics::append(coverage, ifelse(y_test[n] %in% predicted_set[[n]], 1, 0))
  }
  average_coverage = as.numeric(mean(coverage))
  average_coverage
}
