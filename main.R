source("R/grbLMM.R")

# Define wrapper for decision tree
dt.init = NULL
dt.predict = function(dt, X) {

}
dt.fit = function(dt, X, y, lr){

}

# load data


# Run decision tree model
grbLMM.dt = cv.grbLMM(5, y, X, Z, id, dt.fit, dt.predict, dt.init, beta.keep.all = FALSE)

# Run grbLMM baseline
grbLMM.baseline = cv.grbLMM(5, y, X, Z, id, beta.keep.all = TRUE)