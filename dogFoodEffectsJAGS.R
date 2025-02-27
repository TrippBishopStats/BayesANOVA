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
        y[i] ~ dnorm(mu[i],tau)
        ## make our brand the reference category
        mu[i] = beta0 + beta1*I2[i] + beta2*I3[i] + beta3*I4[i] + beta4*I5[i]
    }

    beta0 ~ dnorm(0, 0.001)
    beta1 ~ dnorm(0, 0.001)
    beta2 ~ dnorm(0, 0.001)
    beta3 ~ dnorm(0, 0.001)
    beta4 ~ dnorm(0, 0.001)
    sigma ~ dgamma(0.01,0.01)
    tau <- 1/sigma^2

    ## derived parameters
    dOurVsBest <- -1*beta1 ## ours - top
    dOurVsOthers  <- -1*(beta1 +beta2 + beta3 + beta4)/4 ## ours - all others

}
"

## nice way to create the indicator variables
indicatorVars <- model.matrix(~ type,data=dogFood)

## The data and the prior distribution parameter values
dataList <- list(n = nrow(dogFood),
                 y = dogFood$gain,
                 I2 = indicatorVars[,2],
                 I3 = indicatorVars[,3],
                 I4 = indicatorVars[,4],
                 I5 = indicatorVars[,5]
                 )

## we want a sample from the posterior for these parameters
params <- c('beta0','beta1','beta2','beta3','beta4',
            'dOurVsBest','dOurVsOthers','sigma')

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
## traceplot(output)
## autocorr.plot(output$samples,lag.max=40, ask=FALSE)
output

## make the output into a nicer format
post <- ggs(output$samples)

dOurVsBest <- subset(post,Parameter=='dOurVsBest')$value
dOurVsOthers <- subset(post,Parameter=='dOurVsOthers')$value

hist(dOurVsBest)
mean(dOurVsBest>0)

hist(dOurVsOthers)
mean(dOurVsOthers>0)


