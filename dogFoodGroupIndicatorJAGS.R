rm(list=ls())

data('dogFood',package='syllogi')
dogFood$type <- relevel(dogFood$type, ref='Our dog food')
dogFood <- dogFood[with(dogFood, order(type)),]
dogFood$type <- factor(dogFood$type, labels=c('our','top','2nd','3rd','4th'))
dogFood

library(rjags)
library(jagsUI)
library(ggmcmc)

dataList <- list(y = with(dogFood, gain),
                 group=with(dogFood, as.numeric(type))
                 )

mod <- "
model{
    for(i in 1:length(y)){
        y[i] ~ dnorm(mu[group[i]], tau)
    }

    for(j in 1:max(group)){
        mu[j] ~ dnorm(0, 0.001)
    }

    tau ~ dgamma(0.01, 0.01)
    sigma2 <- 1/tau

   dOurVsBest <- mu[1] - mu[2] ## ours - top
   dOurVsOther <- mu[1] - (mu[2] + mu[3] + mu[4] + mu[5])/4 ## ours - all others

}"

## Run the Bayesian model using MCMC
output <- jags(data = dataList,
               inits = NULL,
               parameters.to.save = c('mu','sigma2', 'dOurVsBest', 'dOurVsOther'),
               model.file = textConnection(mod),
               n.chains = 3,
               n.adapt = 5000,
               n.iter = 31000,
               n.burnin = 1000,
               n.thin = 3,
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