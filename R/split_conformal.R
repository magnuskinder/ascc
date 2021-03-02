#' Basic split conformal method for building prediction sets with respect to significance level.
#'
#' @import randomForest
#' @import stats
#' @import BiocGenerics
#' @import e1071
#' @import FOCI
#' @export

split_conformal = function(x_train,
                           y_train,
                           x_cal,
                           y_cal,
                           x_test,
                           y_test,
                           method = "rf",
                           epsilon = 0.1,
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

  nb_rowsCal = nrow(pred_cal)
  significanceList = seq(0.6, 0.9, by = 0.001)
  accuracyCal = c()

  for (r in 1:nb_rowsCal){
    for (s in significanceList){
      ch_s = as.character(s)
      cal_set = names(which(pred_cal[r,]>=s))
      accuracyCal[[ch_s]] = BiocGenerics::append(accuracyCal[[ch_s]], ifelse(y_cal[r] %in% cal_set, 1, 0))
    }
  }

  significance_averages = BiocGenerics::lapply(accuracyCal,mean)

  calibrate_significance = BiocGenerics::which.min(abs((1-epsilon)-BiocGenerics::unlist(significance_averages)))
  threshold = as.numeric(names(calibrate_significance))

  nb_rowsTest = nrow(pred_test)
  prediction_set = vector('list', length = nb_rowsTest)

  for (r in 1:nb_rowsTest){
    prediction_set[[r]] = names(which(pred_test[r,]>=threshold))
  }
  prediction_set
}
