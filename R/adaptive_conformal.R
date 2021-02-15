#' Build prediction sets with respect to a significance level.
#'
#' @import stats
#' @import randomForest
#' @import BiocGenerics
#' @import e1071
#' @import FOCI
#' @export

adaptive_conformal = function(x_train,
                              y_train,
                              x_cal,
                              y_cal,
                              x_test,
                              y_test,
                              method = "rf",
                              epsilon=0.1,
                              feature_selection = FALSE,
                              num_features = NULL){

  method_check = c("rf", "svm")
  method = match.arg(method, method_check)


  if (feature_selection == TRUE){
    if (is.null(num_features) == TRUE){
      y_train_foci = match(y_train, unique(y_train))
      foci_result = FOCI::foci(y_train_foci, x_train, stop = TRUE, numCores = 1)
      foci_table = foci_result$selectedVar
      selected_features = c(foci_table$names)
      x_train = x_train[, colnames(x_train) %in% selected_features]
      x_cal = x_cal[, colnames(x_cal) %in% selected_features]
      x_test = x_test[, colnames(x_test) %in% selected_features]

    } else {
      y_train_foci = match(y_train, unique(y_train))
      foci_result = FOCI::foci(y_train_foci, x_train, num_features = num_features, stop = FALSE, numCores = 1)
      foci_table = foci_result$selectedVar
      selected_features = c(foci_table$names)
      x_train = x_train[, colnames(x_train) %in% selected_features]
      x_cal = x_cal[, colnames(x_cal) %in% selected_features]
      x_test = x_test[, colnames(x_test) %in% selected_features]

    }
  }

  if (method == "rf"){
    rf_model = randomForest::randomForest(x = x_train, y = y_train, ntree = 1000, type = 'Classification')
    pred_cal = stats::predict(object = rf_model, x_cal, type = "prob")
    pred_test = stats::predict(object = rf_model, x_test, type = "prob")

  } else if (method == "svm"){
    svm_model = e1071::svm(x = x_train, y = y_train, probability = TRUE)

    pred_cal_table = stats::predict(object = svm_model, x_cal, probability = TRUE)
    pred_cal = attr(pred_cal_table, "probabilities")

    pred_test_table = stats::predict(object = svm_model, x_test, probability = TRUE)
    pred_test = attr(pred_test_table, "probabilities")
  }

  size_cal = length(y_cal)
  uniform_cal = stats::runif(size_cal, min=0, max=1)

  epsilon_max = ascc::calibrate_epsilon(y_cal, pred_cal, uniform_cal)
  scores = epsilon - epsilon_max
  prob_adjustement = (1 - epsilon)*(1+1/size_cal)
  epsilon_correction = stats::quantile(scores, prob_adjustement)

  epsilon_calibrated = epsilon - as.numeric(epsilon_correction)



  size_test = length(y_test)
  uniform_test = stats::runif(size_test, min=0, max=1)
  prediction_set = ascc::predict_sets(pred_test, epsilon_calibrated, uniform_test)
  prediction_set = BiocGenerics::lapply(prediction_set, names)
  prediction_set
}
