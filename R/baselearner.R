
#' Wrapper for decision tree initialization and prediction
#'
#' This script defines functions for initializing and predicting using decision tree models.
#' 
#' @import rpart
#' 
#' @keywords internal
#'
library(rpart)
library(xgboost)

# Define wrapper for decision tree
dt.init = list(lr=list(), model=list())

#' Predict function for decision tree models
#'
#' This function predicts the response variable using a decision tree model.
#'
#' @param dtree A list containing the decision tree model.
#' @param X The input data for prediction.
#' @return A vector of predicted response values.
#'

dt.predict = function(dtree, X) {
  if (is.null(dtree)) {
    rep(0, nrow(X))
  } else {
    yhat = rep(0, nrow(X))
    for (i in seq_along(dtree$model)) {
      yhat = yhat + dtree$lr[[i]] * predict(dtree$model[[i]], newdata = as.data.frame(X))
    }
    yhat
  }
}

#' Fit function for decision tree models
#'
#' This function fits a decision tree model using the given data and learning rate.
#'
#' @param dtree A list containing the decision tree model.
#' @param X The input data for fitting.
#' @param y The response variable.
#' @param lr The learning rate.
#' @return A list containing the updated decision tree model.
#'
dt.fit = function(dtree, X, y, lr){
  if (is.null(dtree)) {
    dtree = list(lr=list(), model=list())
  }
  dtree$lr = append(dtree$lr, lr)
  ds = as.data.frame(X)
  ds$y = y
  dtree$model[[length(dtree$model)+1]] <- rpart(y~., ds)
  dtree
}

#' Predict function for XGBoost models
#'
#' This function predicts the response variable using an XGBoost model.
#'
#' @param xgb A list containing the XGBoost model.
#' @param X The input data for prediction.
#' @return A vector of predicted response values.
#'

xgb.predict = function(xgb, X) {
  if (is.null(xgb)) {
    rep(0, nrow(X))
  } else {
    yhat = rep(0, nrow(X))
    for (i in seq_along(xgb$model)) {
      yhat = yhat + xgb$lr[[i]] * predict(xgb$model[[i]], newdata = X)
    }
    yhat
  }
}

#' Fit function for XGBoost models
#'
#' This function fits an XGBoost model using the given data, learning rate, lambda, and alpha parameters.
#'
#' @param xgb A list containing the XGBoost model.
#' @param X The input data for fitting.
#' @param y The response variable.
#' @param lr The learning rate.
#' @param lambda L2 Regularization parameter on weights (default: 1).
#' @param alpha L1 regularization parameter on weights (default: 0).
#' @return A list containing the updated XGBoost model.
#'

xgb.fit = function(xgb, X, y, lr, lambda = 1, alpha = 0){
  if (is.null(xgb)) {
    xgb = list(lr=list(), model=list())
  }
  xgb$lr = append(xgb$lr, lr)
  xgb.incr = xgboost(params = list(max_depth = 10,
                                   lambda = lambda, 
                                   alpha = alpha),
                     data = X,
                     label = y,
                     nrounds = 30)
  xgb$model[[length(xgb$model)+1]] <- xgb.incr
  xgb
}