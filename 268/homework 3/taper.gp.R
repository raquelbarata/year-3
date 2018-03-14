setwd("~/Google Drive/year 3/268/homework 3")
load("hw3_sim_data.Rda")

library("plotly")
dat = data.frame(s1 = sim_data$s[,1],s2 = sim_data$s[,2],y = sim_data$y)
plot_ly(dat, x=~s1, y=~s2, color = ~y, type="scatter") 

y = sim_data$y
x = sim_data$s
n = length(y)

D21 = as.matrix(dist(x[,1]))^2
D22 = as.matrix(dist(x[,2]))^2
dist = sqrt(D21 + D22) ; D21 = NA ; D22 = NA 

taper.C.fn = function(nu){
C = matrix(0,2000,2000)
C[which(dist<nu)] = ( (1-dist[which(dist<nu)]/nu)^4 )*(1+4*dist[which(dist<nu)]/nu) 
return(C) }

for(nu in c(0.05)){

taper.C = taper.C.fn(nu)

exp.C.fn = function(phi){ exp( -phi*dist )}

H.fn = function(phi){ exp.C.fn(phi)*taper.C } 

# hyperpriors
hyper <- NULL
hyper$a_t = 2
hyper$b_t = .01
hyper$a_s = 2
hyper$b_s = 1
hyper$a_p = 0.5
hyper$b_p = 4.5

# logit functions
logit = function(z){ log((z-hyper$a_p)/(hyper$b_p-z)) }
inv.logit = function(z){ (hyper$b_p*exp(z)+hyper$a_p)/( exp(z)+1 ) }

# log posterior function 
transformed_log_post = function(sig2,tau2,beta,H,inv.S,phi){
  lp =  - 0.5*as.numeric(determinant.matrix( diag(tau2,dim(H)[1])+sig2*H, logarithm = TRUE)$modulus) - 0.5*t(y-beta)%*%inv.S%*%(y-beta) + (-hyper$a_s-1)*log(sig2) - hyper$b_s/sig2 + log(sig2) + (-hyper$a_t-1)*log(tau2) - hyper$b_t/tau2 + log(tau2) + log((hyper$b_p-hyper$a_p)*exp(logit(phi))) - 2*log(exp(logit(phi))+1)
  return(lp)
}

# initialize MCMC
cur_sam <- NULL
cur_sam$sig2 = 1
cur_sam$tau2 = 0.1
cur_sam$phi = 1
cur_sam$beta = 1
cur_sam$H = H.fn(cur_sam$phi)
L = t(chol( (diag(cur_sam$tau2,dim(cur_sam$H)[1])+cur_sam$sig2*cur_sam$H )))
inv.L = forwardsolve(L,diag(1,dim(L)[1])) 
W = forwardsolve(inv.L,rep(1,dim(L)[1]))
z = forwardsolve(inv.L,y)
beta.hat = as.numeric(t(W)%*%z/(t(W)%*%W))
cur_sam$inv.L = inv.L 
cur_sam$inv.S = t(inv.L)%*%inv.L ; L = NA ; inv.L = NA

n_iter = 1000
burn = 200
save_MCMC <- NULL
save_MCMC$sig2 = rep(NA,n_iter)
save_MCMC$tau2 = rep(NA,n_iter)
save_MCMC$phi = rep(NA,n_iter)
save_MCMC$beta = rep(NA,n_iter)

count = 0 

# MCMC
for(i_iter in 1:n_iter){
  if((i_iter%%100)==0)
  {
    print(paste("i.sam=", i_iter))
    print(date())
  }
  
  prop_sig2 = exp(log(cur_sam$sig2) + rnorm(1, 0, 0.5))
  prop_tau2 = exp(log(cur_sam$tau2) + rnorm(1, 0, 0.1))
  prop_phi = inv.logit(logit(cur_sam$phi) + rnorm(1,0,.5) )
  prop_H = H.fn(prop_phi)
  L = t(chol( diag(prop_tau2,dim(prop_H)[1])+prop_sig2*prop_H ))
  inv.L = forwardsolve(L,diag(1,dim(L)[1])) 
  W = forwardsolve(inv.L,rep(1,dim(L)[1]))
  z = forwardsolve(inv.L,y)
  prop_inv.S = t(inv.L)%*%inv.L ; L = NA 
  prop_beta.hat = as.numeric(t(W)%*%y/(t(W)%*%W))
  
  accept = transformed_log_post(prop_sig2,prop_tau2,cur_sam$beta,prop_H,prop_inv.S,prop_phi) - transformed_log_post(cur_sam$sig2,cur_sam$tau2,cur_sam$beta,cur_sam$H,cur_sam$inv.S,cur_sam$phi)
  if(log(runif(1))<=accept){
    cur_sam$sig2 = prop_sig2
    cur_sam$tau2 = prop_tau2
    cur_sam$phi = prop_phi
    cur_sam$H  = prop_H
    cur_sam$inv.S = prop_inv.S
    cur_sam$inv.L = inv.L 
    beta.hat = prop_beta.hat
    count = count + 1
  }
  
  inv.L = NA
  cur_sam$beta = as.numeric(beta.hat + t(rep(1,n))%*%t(cur_sam$inv.L)%*%cur_sam$inv.L%*%rep(1,n)*rnorm(1,0,1))
  
  save_MCMC$sig2[i_iter] = cur_sam$sig2
  save_MCMC$tau2[i_iter] = cur_sam$tau2
  save_MCMC$phi[i_iter] = cur_sam$phi
  save_MCMC$beta[i_iter] = cur_sam$beta
  
}
dat <- NULL
dat$hyper = hyper
dat$save_MCMC = save_MCMC
dat$nu = nu
dat$count = count
setwd("~/Google Drive/year 3/268/homework 3")
save(dat,file=sprintf("taper_GP_nu_%s.Rda",round(nu,2)))
}

