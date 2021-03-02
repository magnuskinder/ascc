#' Compute marginal coverage for each target value.
#'
#' @import BiocGenerics
#' @export
target_coverage = function(y_test, predicted_set){
  target_values = BiocGenerics::unique(y_test)
  target_average = c()
  for (t in target_values){
    idx_cell = which(y_test %in% t)
    coverage = c()
    for (j in idx_cell){
      coverage = BiocGenerics::append(coverage, ifelse(y_test[j] %in% predicted_set[[j]], 1, 0))
    }
    target_average[[t]] = BiocGenerics::append(target_average[[t]], mean(coverage))
  }
  target_average
}
