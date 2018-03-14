gprior_regression = function(y,X){
  library(mnormt)
  
  n = length(y)
  p = dim(X)[2]
  
  # set g
  g = max(n,p^2)
  b_0 = 0 #solve(t(X)%*%X)%*%t(X)%*%y
  
  # set initial values
  cursam <- NULL
  cursam$beta = rep(0,p)
  cursam$phi = 0.1
  
  # from data
  beta.hat = solve(t(X)%*%X)%*%t(X)%*%y
  XtX = t(X)%*%X
  s2 = sum((y-X%*%beta.hat)^2)
  
  # save MCMC
  I = 1500
  save <- NULL
  save$beta = matrix(NA,p,I)
  save$phi = rep(NA,I)
  
  # MCMC
  for(i in 1:I){
    cursam$phi = rgamma(1, shape = n/2, rate = s2/2 + 0.5*(1/(g+1))*(t(beta.hat-b_0)%*%t(X)%*%X%*%(beta.hat-b_0)) )
    cursam$beta = rmnorm(1,g*(b_0/g + beta.hat)/(g+1), (cursam$phi*g)*solve(XtX)/(g+1) )
    
    #save samples
    save$beta[,i] = cursam$beta
    save$phi[i] = cursam$phi
  }
  
  # burn
  save$beta = save$beta[,-(1:500)]
  save$s2 = save$s2[-(1:500)]
  
  return(list(ps.beta = save$beta, ps.phi = save$phi))
  
}