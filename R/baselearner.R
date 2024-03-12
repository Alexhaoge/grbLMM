library(rpart)
library(xgboost)

# Define wrapper for decision tree
dt.init = list(lr=list(), model=list())

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

xgb.fit = function(xgb, X, y, lr, lambda = 1, alpha = 0){
  if (is.null(xgb)) {
    xgb = list(lr=list(), model=list())
  }
  xgb$lr = append(xgb$lr, lr)
  xgb.incr = xgboost(params = list(eta=lr, 
                                   lambda = lambda, 
                                   alpha = alpha),
                     data = X,
                     label = y,
                     nrounds = 1)
  xgb$model[[length(xgb$model)+1]] <- xgb.incr
  xgb
}