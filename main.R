library(rpart)
library(glmnet)
library(PGEE)

source("R/grbLMM.R")

# Define wrapper for decision tree
dt.init = list(lr=list(), model=list())
dt.predict = function(dt, X) {
  if (is.null(dt)) {
    rep(0, nrow(X))
  } else {
    yhat = rep(0, nrow(X))
    for (i in seq_along(dt$model)) {
      yhat = yhat + dt$lr[[i]] * predict(dt$model[[i]], newdata = as.data.frame(X))
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
# ds = read.delim('../Mu_etal_mouseVRE_FEATURE_TABLE_case_corrected.tsv',
#                 header = F, comment.char = "#")
data("yeastG1")
Z = as.matrix(cbind(1, yeastG1$time))
X = as.matrix(yeastG1[, -c(1, 2, 3)])
y = yeastG1$y
id = yeastG1$id

# Run decision tree model
grbLMM.dt = grbLMM(y, X, Z, id, dt.fit, dt.predict, dt.init, 
                   beta.keep.all = FALSE)
cv.grbLMM.dt = cv.grbLMM(5, y, X, Z, id, 
                         dt.fit, dt.predict, dt.init, 
                         m.stop = 1, beta.keep.all = FALSE)


# Run grbLMM baseline
grbLMM = grbLMM(y, X, Z, id, beta.keep.all = TRUE)
grbLMM.baseline = cv.grbLMM(5, y, X, Z, id, beta.keep.all = TRUE)
