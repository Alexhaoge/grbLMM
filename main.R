library(rpart)
library(parallel)
library(glmnet)
library(PGEE)

source("R/grbLMM.R")
source("R/utils.R")
source("R/baselearner.R")


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

cl = makeCluster(4, outfile="cluster.Rout")
clusterExport(cl, c("dt.fit", "dt.predict", "dt.init", "rpart",
                    "xgboost", "xgb.fit", "xgb.predict"))

# Run decision tree model
set.seed(202403)
cv.grbLMM.dt = cv.grbLMM(y, X, Z, id, 0.25, m.stop = 10,
                         beta.fit = dt.fit, beta.predict = dt.predict, beta.init = dt.init,
                         beta.keep.all = FALSE, cores = 4, cl = cl)
#save(cv.grbLMM.dt, file = "../cv.dt.rda")

# Run grbLMM baseline
# set.seed(202403)
# cv.grbLMM.base = cv.grbLMM(y, X, Z, id, 0.25, beta.keep.all = TRUE, cores = 4, cl = cl)
# save(cv.grbLMM.base, file = "../cv.base.rda")

# Regularization
# set.seed(202403)
# cv.grbLMM.l2 = cv.grbLMM(y, X, Z, id, 0.25, m.stop = 500, ny = 0.01,
#                            beta.fit = xgb.fit, beta.predict = xgb.predict, beta.init = dt.init,
#                            beta.keep.all = FALSE, cores = 4, cl = cl, lambda = 1, alpha = 0)
# save(cv.grbLMM.l2, file = "../cv.l2.rda")
# 
# set.seed(202403)
# cv.grbLMM.l1 = cv.grbLMM(y, X, Z, id, 0.25, m.stop = 500, ny = 0.01,
#                          beta.fit = xgb.fit, beta.predict = xgb.predict, beta.init = dt.init,
#                          beta.keep.all = FALSE, cores = 4, cl = cl, lambda = 0, alpha = 1)
# save(cv.grbLMM.l1, file = "../cv.l1.rda")

stopCluster(cl)