#' Build prediction sets with respect to significance level.
#'
#' @import BiocGenerics
#' @import utils
#' @export
predict_sets = function(Y_pred,
                        epsilon,
                        unif){

  test_prob = BiocGenerics::lapply(seq_len(nrow(Y_pred)), function(x) Y_pred[x,])
  sort_prob = BiocGenerics::lapply(test_prob, function(x) BiocGenerics::sort(x, decreasing = TRUE))
  cumsum_prob = BiocGenerics::lapply(sort_prob, function(x) cumsum(x))

  L = BiocGenerics::lapply(cumsum_prob, function(x) x[1:min(which(x>=(1-epsilon)))])
  excess = BiocGenerics::lapply(cumsum_prob, function(x) x[min(which(x>=(1-epsilon)))] - (1-epsilon))
  prob_min = BiocGenerics::lapply(sort_prob, function(x) x[min(which(cumsum(x)>=(1-epsilon)))])

  V = mapply("/", excess, prob_min, SIMPLIFY=FALSE)
  idx_remove = which(V>=unif)

  for (i in idx_remove){
    L[[i]] = utils::head(L[[i]], -1)
  }

  L
}
