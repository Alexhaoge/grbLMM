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
# set.seed(202403)
# cv.grbLMM.dt = cv.grbLMM(y, X, Z, id, 0.25, m.stop=200,
#                          beta.fit = dt.fit, beta.predict = dt.predict, 
#                          beta.init = list(lr=list(), model=list()),
#                          beta.keep.all = FALSE, ny = 0.01, cores = 4, cl = cl)
# save(cv.grbLMM.dt, file = "../cv.dt.rda")

# # Run grbLMM baseline
# set.seed(202403)
# cv.grbLMM.base = cv.grbLMM(y, X, Z, id, 0.25, m.stop=50, beta.keep.all = TRUE, cores = 4, cl = cl)
# save(cv.grbLMM.base, file = "../cv.base.rda")

# Regularization
set.seed(202403)
cv.grbLMM.l2 = cv.grbLMM(y, X, Z, id, 0.25, m.stop = 200, ny = 0.01,
                         beta.fit = xgb.fit, beta.predict = xgb.predict,
                         beta.init = list(lr=list(), model=list()),
                         beta.keep.all = FALSE, cores = 4, cl = cl,
                         refit = TRUE, lambda = 1, alpha = 0)
save(cv.grbLMM.l2, file = "../cv.l2.rda")

# set.seed(202403)
# cv.grbLMM.el = cv.grbLMM(y, X, Z, id, 0.25, m.stop = 200, ny = 0.01,
#                          beta.fit = xgb.fit, beta.predict = xgb.predict,
#                          beta.init = list(lr=list(), model=list()),
#                          beta.keep.all = FALSE, cores = 4, cl = cl,
#                          refit = TRUE, lambda = 0.5, alpha = 0.5)
# save(cv.grbLMM.el, file = "../cv.el_0.5_0.5.rda")

# set.seed(202403)
# cv.grbLMM.l1 = cv.grbLMM(y, X, Z, id, 0.2, m.stop = 200, ny = 0.01,
#                          beta.fit = xgb.fit, beta.predict = xgb.predict,
#                          beta.init = list(lr=list(), model=list()),
#                          beta.keep.all = FALSE, cores = 4, cl = cl,
#                          refit = TRUE, lambda = 0, alpha = 1)
# save(cv.grbLMM.l1, file = "../cv.l1.rda")


# sim1 = read.csv("sim_data_1.csv")
# Z = matrix(1, ncol = 1, nrow = nrow(sim1))
# X = as.matrix(sim1[, -c(1, 2, 3)])
# y = sim1[, 1]
# id = sim1[, 2]

# set.seed(202403)
# cv.grbLMM.l2 = cv.grbLMM(y, X, Z, id, 0.2, m.stop = 200, ny = 0.1,
#                          beta.fit = xgb.fit, beta.predict = xgb.predict,
#                          beta.init = list(lr=list(), model=list()),
#                          beta.keep.all = FALSE, cores = 4, cl = cl,
#                          refit = TRUE, lambda = 1, alpha = 0)
# save(cv.grbLMM.l2, file = "../sim1.cv.l2.rda")

stopCluster(cl)
