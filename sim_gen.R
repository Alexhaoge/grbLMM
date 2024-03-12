library(SimCorMultRes)
set.seed(571)
m = 100
n = 5
p = 1000
id = rep(1:m, each = n)
t = rep(1:n, times = m)
# generate conitnuous y
Xmat = matrix(runif(m * n * p), nrow = m * n, ncol = p)
colnames(Xmat) = paste0("V", 1:p)
pool = seq(0, 1, by = 0.1)
betas = sample(pool, 1000, replace = TRUE)
b0s = rep(rnorm(m), each = n)
b1s = rep(rnorm(m), each = n)
Zmat = cbind(rep(1, m * n), t)
e = rnorm(m * n)
## random intercept
Y_i = rep(-1.5, m * n) + Xmat %*% betas + b0s * Zmat[, 1] + e
sim_1 = as.data.frame(cbind(Y_i, id, t, Xmat))
write_csv(sim_1, "sim_data_1")
## random intercept + slope
Y_it = rep(-1.5, m * n) + Xmat %*% betas + b0s * Zmat[, 1] + b1s * Zmat[, 2] + e
sim_2 = as.data.frame(cbind(Y_it, id, t, Xmat))
write_csv(sim_2, "sim_data_2")
# generate binary y 
formula = as.formula(paste("~ V1 +", paste(colnames(Xmat)[-1], collapse = " + ")))
d_temp = rbin(clsize = n, intercepts = -1.5, betas = betas, xformula = formula, 
                xdata = Xmat, cor.matrix = toeplitz(c(1, rep(0.25, n-1))), link = "logit")
Y2 = d_temp$simdata

