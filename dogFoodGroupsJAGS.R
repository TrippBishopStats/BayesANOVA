rm(list=ls())

data('dogFood',package='syllogi')
dogFood$type <- relevel(dogFood$type, ref='Our dog food')
dogFood <- dogFood[with(dogFood, order(type)),]
dogFood$type <- factor(dogFood$type, labels=c('our','top','2nd','3rd','4th'))
dogFood

library(rjags)
library(jagsUI)
library(ggmcmc)

## Bayesian model, this is JAGS code
mod <- "
model{
    for(i in 1:n){
        y1[i] ~ dnorm(mu1,tau) ## our
        y2[i] ~ dnorm(mu2,tau)
        y3[i] ~ dnorm(mu3,tau)
        y4[i] ~ dnorm(mu4,tau)
        y5[i] ~ dnorm(mu5,tau)

    }

    mu1 ~ dnorm(0, 0.001)
    mu2 ~ dnorm(0, 0.001)
    mu3 ~ dnorm(0, 0.001)
    mu4 ~ dnorm(0, 0.001)
    mu5 ~ dnorm(0, 0.001)
    sigma ~ dgamma(0.01,0.01)
    tau <- 1/sigma^2

    ## derived parameters
    dOurVsBest <- mu1 - mu2 ## ours - top
    dOurVsOther  <- mu1 - (mu2 + mu3 + mu4 + mu5)/4 ## ours - all others
}
"

## The data and the prior distribution parameter values
dataList <- list(n = 5) ## the number of dogs in each group, NOT the number of groups.
for(i in 1:length(unique(dogFood$type))){
    dataList[[paste0('y', i)]]  <- subset(dogFood,type==unique(type)[i])$gain
}

dataList

## we want a sample from the posterior for these parameters
params <- c('mu1','mu2','mu3','mu4','mu5','dOurVsBest','dOurVsOther','sigma')

## initial values for the MCMC
inits <- NULL

## Run the Bayesian model using MCMC
output <- jags(data = dataList,
               inits = inits,
               parameters.to.save = params,
               model.file = textConnection(mod),
               n.chains = 3,
               n.adapt = 5000,
               n.iter = 11000,
               n.burnin = 1000,
               n.thin = 1,
               parallel = FALSE,
               DIC = FALSE)

output
## traceplot(output)
## autocorr.plot(output$samples,lag.max=40, ask=FALSE)

## make the output into a nicer format
post <- ggs(output$samples)

dOurVsBest <- subset(post,Parameter=='dOurVsBest')$value
dOurVsOther <- subset(post,Parameter=='dOurVsOther')$value

hist(dOurVsBest)
mean(dOurVsBest>0)

hist(dOurVsOther)
mean(dOurVsOther>0)


