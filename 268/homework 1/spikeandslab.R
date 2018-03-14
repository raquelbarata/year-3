spikeandslab = function(y,X){
  n = length(y)
  p = dim(X)[2]
  
  # set initial values
  cursam <- NULL
  cursam$beta = rep(0,p)
  cursam$s2 = 0.1
  cursam$gam = rep(0,p)
  cursam$w = rep(0.5,p)
  
  # set hyper priors
  v_0 = 0.1
  v_1 = 10
  a_w = 0.1
  b_w = 0.1
  a_s = 1
  b_s = 0.1
  
  # from data
  beta.hat = solve(t(X)%*%X)%*%t(X)%*%y
  XtX = t(X)%*%X
  
  # save MCMC
  I = 1500
  save <- NULL
  save$beta = matrix(NA,p,I)
  save$s2 = rep(NA,I)
  save$gam = matrix(NA,p,I)
  save$w = matrix(NA,p,I)
  
  # MCMC
  for(i in 1:I){
    cursam$s2 = 1/rgamma(1, shape = n/2+a_s, scale = b_s + 0.5*sum((y-X%*%cursam$beta)^2) )
    cursam$D = (v_0^2)*as.numeric(cursam$gam==0)*diag(p) + (v_1^2)*as.numeric(cursam$gam==1)*diag(p)
    cursam$beta = beta.hat + solve(chol(XtX + solve(cursam$D)))%*%rnorm(p)
    for(j in 1:p){
      cursam$gam[j] = rbinom(1,1,
        cursam$w[j]*dnorm(cursam$beta[j],0,v_1)/(cursam$w[j]*dnorm(cursam$beta[j],0,v_1) + (1-cursam$w[j])*dnorm(cursam$beta[j],0,v_0)))
      cursam$w[j] = rbeta(1,cursam$gam[j]+a_w,1-cursam$gam[j]+b_w)
    }
    #save samples
    save$beta[,i] = cursam$beta
    save$gam[,i] = cursam$gam
    save$w[,i] = cursam$w
    save$s2[i] = cursam$s2
  }
  
  # burn
  save$beta = save$beta[,-(1:500)]
  save$gam = save$gam[,-(1:500)]
  save$w = save$w[,-(1:500)]
  save$s2 = save$s2[-(1:500)]
  
  return(list(ps.beta = save$beta, ps.s2 = save$s2, ps.w = save$w, ps.gam = save$gam))
  
}