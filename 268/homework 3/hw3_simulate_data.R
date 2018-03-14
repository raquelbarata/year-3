s = matrix(runif(2*2000,0,1),2000,2)
plot(s[,1],s[,2])

exp.cov = function(sig2,phi,s1,s2){ sig2*exp( -phi*sqrt(sum((s1 - s2)^2)) )}

sig2 = 1
phi = 2

C = matrix(NA,2000,2000)
for(i in 1:dim(s)[1]){
  for(j in 1:dim(s)[1]){
    C[i,j] = exp.cov(sig2,phi,s[i,],s[j,])
  }
}

w_0 = t(chol(C))%*%rnorm(2000,0,1)
beta = 1
tau2_0 = 0.1
y = beta + w_0 + rnorm(2000,0,tau2_0)

library("plotly")
dat = data.frame(s1 = s[,1],s2 = s[,2],y = y)
plot_ly(dat, x=~s1, y=~s2, color = ~y, type="scatter") 

sim_data <- NULL
sim_data$s = s
sim_data$sig2 = sig2
sim_data$phi = phi
sim_data$C = C
sim_data$w_0 = w_0
sim_data$beta = beta
sim_data$tau2_0 = tau2_0
sim_data$y = y 

setwd("~/Google Drive/year 3/268/homework 3")
save(sim_data,file="hw3_sim_data.Rda")



