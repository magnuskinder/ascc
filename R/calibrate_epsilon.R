#' Calibrate coverage threshold.
#'
#' @import BiocGenerics
#' @export
calibrate_epsilon = function(Y,
                             Y_pred,
                             unif){

  test_prob = BiocGenerics::lapply(seq_len(nrow(Y_pred)), function(x) Y_pred[x,])
  sort_prob = BiocGenerics::lapply(test_prob, function(x) BiocGenerics::sort(x, decreasing = TRUE))
  cumsum_prob = BiocGenerics::lapply(sort_prob, function(x) cumsum(x))

  size_Y = length(Y)
  prob_cum = c()
  prob = c()

  for (i in 1:size_Y){
    target_cal = as.character(Y[i])
    prob_cum = BiocGenerics::append(prob_cum, cumsum_prob[[i]][target_cal])
    prob = BiocGenerics::append(prob, sort_prob[[i]][target_cal])
  }

  epsilon_max = 1 - prob_cum
  epsilon_max = epsilon_max + (prob * unif)
  epsilon_max = BiocGenerics::pmin(epsilon_max,1)
  epsilon_max
}
