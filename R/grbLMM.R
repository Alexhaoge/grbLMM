library(nlme)
library(parallel)
library(Matrix)

.predict_random = function(Z, id, gamm) {
  q = ncol(Z); n = length(gamm) / q
  gamm_split = split(gamm, rep(1:n, each = q))
  sapply(1:nrow(Z), function(i){
    Z[i, ] %*% gamm_split[[id[i]]]
  })
}

grbLMM = function(y, X, Z, id,
                  beta.fit = NULL, beta.predict = NULL, beta.init = NULL, beta.keep.all = TRUE, 
                  m.stop = 500, ny = .1, cv.dat = NULL, aic = FALSE,
                  ...){
  getV = function(x){solve(x/sigma2 + Qi)}
  getQ = function(x, y){x + tcrossprod(as.numeric(y))}
  cfc = function(x)(length(unique(x)))
  getZ2D = function(x){chol2inv(chol(x + D))}

  ### basic definitions
  id = as.numeric(factor(id, levels = unique(id)))
  N = length(id)
  n = length(unique(id))
  id.t = as.numeric(table(id))
  first = rep(FALSE, N)
  for(i in 1:N){
    first[which.max(id==i)] = TRUE
  }
  p = ncol(X)
  q = ncol(Z)

  ### extract cluster-constant covariates
  ccc = rep(FALSE, p)
  for(r in 1:p){
    if(Reduce('+', lapply(split(X[,r], id), cfc)) == n){ccc[r] = TRUE}
  }
  Xcc = cbind(1, X[first, ccc])
  Xsw = X[,ccc]
  Xcor = list()
  Xcor[[1]] = Xcc%*%solve(crossprod(Xcc))%*%t(Xcc)

  if(q>1){
    for(s in 2:q){
      x = matrix(rep(1, n), n, 1)
      Xcor[[s]] = x%*%solve(crossprod(x))%*%t(x)
    }
  }
  
  p1 = rep(seq(1, q*n, q), q) + rep(0:(q-1), each = n)
  p2 = rep(seq(1, q*n, n), n) + rep(0:(n-1), each = q)
  P1 = sparseMatrix(seq_along(p1), p1)
  P2 = sparseMatrix(seq_along(p2), p2)

  Xcor = bdiag(Xcor)
  Xcor = P2%*%(diag(n*q) - Xcor)%*%P1
  
  ### construct random effects design matrix
  Z0 = Z
  Z = Z2 = QQ = list()
  for(i in 1:n){
    if(id.t[i] == 1){
      Z[[i]] = matrix(Z0[id==i,], 1, q)
    }else{
      Z[[i]] = Z0[id==i,]
    }
    Z2[[i]] = crossprod(Z[[i]])
  }
  # bZ = Z
  Z = bdiag(Z)
  # Z22 = crossprod(Z)

  ##### set starting values
  if (is.null(beta.init)) {
    beta = rep(0, p)
  } else {
    beta = beta.init
  }
  
  ### offset model for intercept and random structure
  if(q==1){
    offset = lme(y ~ 1, random = ~ 1 | id, control = lmeControl(opt = "optim", singular.ok = TRUE, returnObject = TRUE))
  }else{
    offset = lme(y ~ 1, random = ~ Z0[,-1] | id, control = lmeControl(opt = "optim", singular.ok = TRUE, returnObject = TRUE))
  }

  ### extract starting values
  int = offset$coefficients$fixed[1]
  gamm = Xcor%*%as.numeric(t(offset$coefficients$random$id))
  sigma2 = offset$sigma^2
  Q = getVarCov(offset)
  
  ### construct initial hat matrix
  CC = cbind(1, Z)
  G = kronecker(diag(n), Q)
  B = as.matrix(bdiag(diag(1), solve(G)))
  S = list(CC%*%bdiag(1,Xcor)%*%solve(crossprod(CC) + sigma2*B)%*%t(CC))

  ### in case of cv
  clcv = NA
  if(!is.null(cv.dat)){
    idcv = as.numeric(factor(cv.dat$idcv), levels = unique(cv.dat$idcv))
    Ncv = length(idcv)
    ncv = length(unique(idcv))
    idcv.t = as.numeric(table(idcv))
    
    Zcv = list()
    for(i in 1:ncv){
      if(idcv.t[i] == 1){
        z = cv.dat$Zcv[idcv==i,]
        Zcv[[i]] = t(as.matrix(z, 1, q))
      }else{
        Zcv[[i]] = cv.dat$Zcv[idcv==i,]
      }
    }
    
    Zcv = as.matrix(bdiag(Zcv))
  }

  ### prepare baselearners
  BL = HM = list()
  for(r in 1:p){
    x = cbind(1, X[,r])
    BL[[r]] = solve(crossprod(x))%*%t(x)
    HM[[r]] = ny*x%*%BL[[r]]
  }

  ### define storing matrices/vectors
  INT = int
  BETA = beta
  GAMMA = gamm
  SIGMA2 = sigma2
  CLCV = TRCV = AICc = c()
  
  for(m in 1:m.stop){
    ###############################################################
    #### S1 #######################################################
    ###############################################################
    if (is.null(beta.predict)) {
      eta = as.vector(int + X%*%beta + Z%*%gamm)
    } else {
      eta = as.vector(beta.predict(beta, X) + Z%*%gamm)
    }
    u = y - eta

    if (is.null(beta.fit)) {
      fits = matrix(0, 3, p)
      for(r in 1:p) {
        fit = BL[[r]]%*%u
        fits[1,r] = fit[1]
        fits[2,r] = fit[2]
        fits[3,r] = sum((u - cbind(1, X[,r])%*%fit)^2)
      }

      best = which.min(fits[3,])
      int = int + ny*fits[1,best]
      beta[best] = beta[best] + ny*fits[2,best]
    } else {
      beta = beta.fit(beta, X, u, ny, ...)
    }

    ###############################################################
    #### S2 #######################################################
    ###############################################################
    if (is.null(beta.predict)) {
      eta = as.vector(int + X%*%beta + Z%*%gamm)
    } else {
      eta = as.vector(beta.predict(beta, X) + Z%*%gamm)
    }
    u = y - eta

    D = solve(Q/sigma2)
    Z2D = lapply(Z2, getZ2D)
    rfit = ny*Xcor%*%bdiag(Z2D)%*%t(Z)
    gamm = gamm + rfit%*%u

    ###############################################################
    #### S3 #######################################################
    ###############################################################
    if (is.null(beta.predict)) {
      eta = as.vector(int + X%*%beta + Z%*%gamm)
    } else {
      eta = as.vector(beta.predict(beta, X) + Z%*%gamm)
    }
    
    Qi = solve(Q)
    sigma2 = var(y - eta)

    V = lapply(Z2, getV)
    V = mapply(getQ, V, split(gamm, rep(1:n, each = q)), SIMPLIFY = FALSE)
    Q = Reduce("+", V)/n
    ### predictive risk computation
    if(!is.null(cv.dat)){
      # Qcv = diag(Ncv) + Zcv%*%kronecker(diag(ncv), Q/sigma2)%*%t(Zcv)
      random_effect = .predict_random(cv.dat$Zcv, cv.dat$idcv, gamm)
      if (is.null(beta.predict)) {
        clcv = mean((cv.dat$ycv - int - cv.dat$Xcv%*%beta - random_effect)^2)
      } else {
        clcv = mean((cv.dat$ycv - beta.predict(beta, cv.dat$Xcv) - random_effect)^2)
      }
    }
    random_effect_tr = .predict_random(Z0, id, gamm)
    if (is.null(beta.predict)) {
      trcv = mean((y - int - X%*%beta - random_effect_tr)^2)
    } else {
      trcv = mean((y - beta.predict(beta, X) - random_effect_tr)^2)
    }
    if(aic){
      Sma = HM[[best]]
      Smb = Z%*%rfit
      S[[m+1]] = diag(N) - (diag(N) - Smb)%*%(diag(N) - Sma)%*%(diag(N) - S[[m]])
      df = sum(diag(S[[m+1]]))
      s2 = mean((y - eta)^2)
      AICc[m] = log(s2) + (1 + df/N) / (1 - (df + 2)/N)
    }
    if (beta.keep.all) {
      BETA = append(BETA, beta)
    }
    INT = c(INT, int)
    GAMMA = cbind(GAMMA, gamm)
    SIGMA2 = c(SIGMA2, sigma2)
    QQ[[m]] = Q
    CLCV = c(CLCV, clcv)
    TRCV = c(TRCV, trcv)
    if(m%%50 == 0){print(m)}
  }

  if (!beta.keep.all) {
    BETA = beta
  }
  structure(list(int = int, beta = beta, sigma2 = sigma2, gamma = gamm, Q = Q, AICc = AICc,
                 INT = INT, BETA = BETA, SIGMA2 = SIGMA2, GAMMA = t(GAMMA), QQ = QQ, CLCV = CLCV,
                 S = S, eta = eta, TRCV = TRCV))
}

predict.grbLMM <- function(model, X, Z, id, beta.predict = NULL) {
  if (is.null(beta.predict)) {
    res <- X %*% model$beta + model$int
  } else {
    res <- beta.predict(model$beta, X)
  }
  res + .predict_random(Z, id, model$gamma)
}

cv.grbLMM <- function(y, X, Z, id, prop,
                      beta.fit = NULL, beta.predict = NULL, beta.init = NULL,
                      beta.keep.all = TRUE, m.stop = 500, ny = .1,
                      cores = 1, cl = NULL, refit = FALSE,
                      ...) {
  p <- ncol(X)
  q <- ncol(Z)
  
  ### create an equally sized partition
  kfolds <- k_fold_by_prop(cbind(as.data.frame(X), as.data.frame(Z), y, id), prop)
  K <- length(kfolds)

  ### function for each fold
  k.fold <- function(k) {
    cv.dat <- list("ycv" = kfolds[[k]]$y,
                   "Xcv" = as.matrix(kfolds[[k]][, 1:p]),
                   "Zcv" = as.matrix(kfolds[[k]][, (p + 1):(p + q)]),
                   "idcv" = kfolds[[k]]$id)
    train <- do.call(rbind, kfolds[-k])
    
    model <- grbLMM(train$y, as.matrix(train[, 1:p]), as.matrix(train[, (p+1):(p+q)]), train$id,
                    beta.fit = beta.fit, beta.predict = beta.predict, beta.init = beta.init, 
                    beta.keep.all = beta.keep.all, m.stop = m.stop, ny = ny, cv.dat = cv.dat, ...)
    ans <- rbind(model$TRCV, model$CLCV)
    rm(train, cv.dat, model)
    gc()
    ans
  }

  ### execute model on all folds
  if (cores == 1) {
    cv.ls <- lapply(1:K, k.fold)
  } else {
    if (Sys.info()["sysname"] == "Windows") {
      if (is.null(cl))
        stop("Cluster instance must be given by cl on Windows OS if cores > 1")
      clusterExport(cl, 
                    c("lme", "lmeControl", "getVarCov",
                      "sparseMatrix", "crossprod", "tcrossprod", "bdiag", 
                      "kronecker", "chol2inv", "solve", "t",
                      ".predict_random", "grbLMM"))
      cv.ls <- parLapply(cl, 1:K, k.fold)
    } else {
      cv.ls <- mclapply(1:K, k.fold, mc.cores = cores)
    }
  }
  
  ### find best performing m.stop averaged over all folds
  cv.mse.val <- c()
  cv.mse.tr <- c()
  for (i in 1:K){
    cv.mse.tr <- rbind(cv.mse.tr, cv.ls[[i]][1, ])
    cv.mse.val <- rbind(cv.mse.val, cv.ls[[i]][2, ])
  }
  train.risk <- colMeans(cv.mse.tr)
  pred.risk <- colMeans(cv.mse.val)
  m.opt <- which.min(pred.risk)
  
  if (refit) {
    model <- grbLMM(y, X, Z, id,
                   beta.fit = beta.fit, beta.predict = beta.predict, beta.init = beta.init,
                   beta.keep.all = beta.keep.all, ny=ny, m.stop = m.opt, ...)
    model$coef <- list(int=model$int, beta=model$beta, gamma=model$gamma)
  } else {
    model = list()
  }
  
  model$m.opt <- m.opt
  model$pred <- pred.risk
  model$trainrisk <- train.risk
  model$valfolds <- cv.mse.val
  model$trfolds <- cv.mse.tr
  model
}
