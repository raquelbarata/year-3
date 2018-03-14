library(mnormt)
#sample size
nn = 1:2
all.n = c(500,400)
#beta size
np = 1:2
all.p = c(100,300)
#beta covariance
nS = 1:2
S2 = function(p){Sig = diag(p)
    for(r in 1:p){
      for(c in 1:p){
        Sig[r,c] = (0.6)^(abs(r-c))}}
    return(Sig)}
#beta values
nB = 1:2


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
          
          setwd("~/Google Drive/year 3/268/homework 1/datasets")
          name = sprintf("dat_n%s_p%s_S%s_B%s.Rda",n,p,k,l)
          save(dat,file=name)
          
          }
        }
    }
}

# load datasets
for(i in nn){
  n = all.n[i]
  
  for(j in np){
    p = all.p[j]
    
    for(k in nS){
      
      for(l in nB){
        load(sprintf("dat_n%s_p%s_S%s_B%s.Rda",n,p,k,l))
        
        
      }
    }
  }
}


#### LASSO
install.packages("glmnet")
library(glmnet)
m.lassoglm <- cv.glmnet(dat$X,dat$y)
m.lassoglm1 <- glmnet(dat$X,dat$y,lambda=m.lassoglm$lambda.min,family="gaussian", alpha=1)
m.ridgeglm1 <- glmnet(dat$X,dat$y,lambda=m.lassoglm$lambda.min,family="gaussian", alpha=0)

par(mar=c(6,6,1,1))
plot(dat$B,col="red",pch=19,xlab="predictor index",ylab="coefficents",cex.axis=1.5,cexlab=2)
lines(m.lassoglm1$bet)
title("LASSO")

par(mar=c(6,6,1,1))
plot(dat$B,col="red",pch=19,xlab="predictor index",ylab="coefficents",cex.axis=1.5,cexlab=2)
lines(m.ridgeglm1$bet)
title("Ridge Regression")

library(mnormt)
newX = rmnorm(50,rep(0,p),S)
true.y = rnorm(50,newX%*%dat$B,1)
