setwd("~/Google Drive/year 3/268/homework 3")
load("hw3_sim_data.Rda")
library("spBayes")

y = sim_data$y
coords = sim_data$s
n = length(y)

# priors
mod.pp = TRUE
KNOTS = matrix(runif(2*200,0,1),200,2)
m = dim(KNOTS)[1]
n.samples <- 2000
n.report <- 1000
verbose <- TRUE
starting <- list("beta =1","phi"=1, "sigma.sq"=1, "tau.sq"=0.1)
tuning <- list("phi"=0.05, "sigma.sq"=0.05, "tau.sq"=0.05)
priors.1 <- list("beta.Flat", "phi.Unif"=c(0.5, 4.5), "sigma.sq.IG"=c(2, 1),
                 "tau.sq.IG"=c(2, 0.1))
cov.model <- "exponential"
burn.in <- 0.5*n.samples

# m.1 <- spLM(y~1, coords=coords, starting=starting,
#             tuning=tuning, priors=priors.1, cov.model=cov.model,
#             n.samples=n.samples, verbose=verbose, n.report=n.report,
#             modified.pp=mod.pp,knots=KNOTS)
# m.1 <- spRecover(m.1, start=burn.in, verbose=FALSE)
# 
# setwd("~/Google Drive/year 3/268/homework 3")
# save(m.1,file="mod_GP_m_1.Rda")

load("mod_GP_m_1.Rda")

post.samples = as.mcmc(cbind(m.1$p.beta.recover.samples,m.1$p.theta.recover.samples))

# pred.X = matrix(1,200,1)
# pred.coords = coords
# insample.preds = spPredict(m.1, pred.coords=pred.coords, pred.covars = m.1$X,
#                            start=burn.in, thin = 1)$p.y.predictive.samples
# save(insample.preds,file="mod_GP_insample.preds.Rda")

load("mod_GP_insample.preds.Rda")
post.means = apply(insample.preds,1,median)
post.vars = apply(insample.preds,1,var)

pplc = sum((y-post.means)^2) + sum(post.vars)

pred.coords = expand.grid(lon=seq(0,1,0.1),lat=seq(0,1,0.1))
l = dim(pred.coords)[1]

 # surface.preds = spPredict(m.1, pred.coords=pred.coords, pred.covars=pred.X ,
 #                           start=burn.in, thin = 1)$p.y.predictive.samples
 # save(surface.preds,file="mod_GP_surface.preds.Rda")

load("mod_GP_surface.preds.Rda")
post.means = apply(surface.preds,1,mean)
post.vars = apply(surface.preds,1,var)

par(mfrow=c(1,1))
par(mar=c(4,4,1,1))
pdf("modPP_surfacemeans.pdf")
image.plot(matrix(post.means,length(unique(pred.coords[,1])),length(unique(pred.coords[,2])),byrow=FALSE),
           x=unique(pred.coords[,1]),y=unique(pred.coords[,2]),horizontal=FALSE,
           xlab="posterior mean",ylab="")
map('state', region = c('virginia', 'tennessee', 'north carolina', 'kentucky'),add=TRUE,lwd=2)
dev.off()
pdf("modPP_surfacevars.pdf")
image.plot(matrix(post.vars,length(unique(pred.coords[,1])),length(unique(pred.coords[,2])),byrow=FALSE),
           x=unique(pred.coords[,1]),y=unique(pred.coords[,2]),horizontal=FALSE,
           xlab="posterior var",ylab="")
map('state', region = c('virginia', 'tennessee', 'north carolina', 'kentucky'),add=TRUE,lwd=2)
dev.off()
