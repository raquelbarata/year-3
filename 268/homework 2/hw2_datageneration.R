# problem 1

library(mnormt)
#sample size
nn = 1:2
all.n = c(50,200)
#beta size
np = 1
all.p = c(20)
#beta covariance
nS = 2
S2 = function(p){Sig = diag(p)
for(r in 1:p){
  for(c in 1:p){
    Sig[r,c] = (0.6)^(abs(r-c))}}
return(Sig)}
#beta values
nB = 1


# set values for 16 different datasets
for(i in nn){
  n = all.n[i]
  
  for(j in np){
    p = all.p[j]
    
    for(k in nS){
      if(k==1){S = diag(1,p)  
      }else{S = S2(p)}
      
      for(l in nB){
        if(l==1){
          B = rep(0,p)
          B[1:5] = 3
        }else{
          B = rep(0,p)
          B[1:5]=5
          B[6:10]=-2
          B[11:15]=0.5}
        
        #generate data
        dat <- NULL
        dat$n = n
        dat$p = p 
        dat$B = B
        dat$S = S
        dat$X = rmnorm(n,rep(0,p),S)
        dat$y = rnorm(n,dat$X%*%dat$B,1)
        
        setwd("~/Google Drive/year 3/268/homework 2")
        name = sprintf("dat_n%s_p%s_S%s_B%s.Rda",n,p,k,l)
        save(dat,file=name)
        
      }
    }
  }
}

# for(i in nn){
#   n = all.n[i]
#   load(sprintf("dat_n%s_p20_S2_B1.Rda",n))
# }


# problem 2
dat$S <- NULL
dat$B <- NULL

#(i)
n = 200
p = 200
k = 100
X = matrix(rnorm((n+k)*p,0,1),(n+k),p)
y = 10*sin(pi*X[,1]*X[,2])+20*(X[,2]-0.5)^2 + 10*X[,4] + rnorm((n+k),0,0.5)
dat$n = n
dat$p = p 
dat$X.test = X[(n+1):(n+k),]
dat$X = X[1:n,]
dat$y.test = y[(n+1):(n+k)]
dat$y = y[1:n]
name = sprintf("dat2i_n%s_p%s.Rda",n,p)
save(dat,file=name)
X = X[,c(1,2,9)] + matrix(rnorm((n+k)*3,0,0.1),(n+k),3)
dat$X.test = X[(n+1):(n+k),]
dat$X = X[1:n,]
name = sprintf("dat2i_noise_n%s_p%s.Rda",n,p)
save(dat,file=name)

#(ii)
n = 500
p = 100
k = 100
X = matrix(rnorm((n+k)*p,0,1),(n+k),p)
y = 10*sin(pi*X[,1]*X[,2])+20*(X[,2]-0.5)^2 + 10*X[,4] + rnorm((n+k),0,0.5)
dat$n = n
dat$p = p 
dat$X.test = X[(n+1):(n+k),]
dat$X = X[1:n,]
dat$y.test = y[(n+1):(n+k)]
dat$y = y[1:n]
name = sprintf("dat2ii_n%s_p%s.Rda",n,p)
save(dat,file=name)
X = X[,c(1,2,9)] + matrix(rnorm((n+k)*3,0,0.1),(n+k),3)
dat$X.test = X[(n+1):(n+k),]
dat$X = X[1:n,]
name = sprintf("dat2ii_noise_n%s_p%s.Rda",n,p)
save(dat,file=name)

#load(sprintf("dat2ii_n%s_p%s.Rda",n,p))
