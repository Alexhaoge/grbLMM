library(rpart)
library(glmnet)

source("R/grbLMM.R")

# Define wrapper for decision tree
dt.init = list(lr=list(), model=list())
dt.predict = function(dt, X) {
  if (is.null(dt)) {
    rep(0, nrow(X))
  } else {
    yhat = rep(0, nrow(X))
    for (i in seq_along(dt$model)) {
      yhat = yhat + dt$lr[i] * predict(dt$model[i], newdata = as.data.frame(X))
    }
    yhat
  }
}
dt.fit = function(dt, X, y, lr){
  if (is.null(dt)) {
    dt = list(lr=list(), model=list())
  }
  dt$lr = append(dt$lr, lr)
  ds = as.data.frame(X)
  ds$y = y
  dt$model = append(dt$model, rpart(y~., ds))
  dt
}

# Define wrapper for lasso
# lasso.init = NULL
# lasso.predict = NULL
# lasso.predict = function(beta, X) {
#   cbind(1, X) %*% beta
# }
# lasso.fit = function(beta, X, y, lr, family = "Gaussian", intercept = TRUE, lambda = NULL){
#   lasso = glmnet(X, y, family = family, alpha = 1, intercept = intercept, lambda = lambda)

# }

# load data


# Run decision tree model
grbLMM.dt = cv.grbLMM(5, y, X, Z, id, dt.fit, dt.predict, dt.init, beta.keep.all = FALSE)

# Run grbLMM baseline
grbLMM.baseline = cv.grbLMM(5, y, X, Z, id, beta.keep.all = TRUE)