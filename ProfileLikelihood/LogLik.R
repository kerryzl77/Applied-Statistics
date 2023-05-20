# setwd('/Users/liuzikai/Desktop/3508/Quiz 2 data-20230214')
load('netdata.RData')
load('prussiandeaths.RData')

nll <- function(theta, y)  {
  lambda <- exp(theta)
  ll <- sum(log(dpois(y, lambda)))
  return(-ll)
}

start = log(mean(prussiandeaths))
mle <- optim(start, nll, y=prussiandeaths, method='Brent', lower=-10, upper=10, hessian=TRUE)
exp(mle$par)
sd.MLE <- as.vector(sqrt(solve(mle$hessian))) 
sd.MLE
# se <- sqrt(diag(solve(mle$hessian)))
# se == 0.09053574 == sd.MLE
exp.theta <- mle$par
CI.theta <- mle$par + c(-1, 1)*qnorm(0.975)*sd.MLE
CI.theta
exp(CI.theta)

# ProfLik <- function(theta,x){
#   ll <- sum(- theta + x * log(theta) - lgamma(x + 1))
#   return(ll)
# }
# loglike <- lapply(exp.theta,ProfLik, x=prussiandeaths)

ChiLow <- qchisq(0.05, df=3, lower.tail=TRUE)
ChiUp <- qchisq(0.05, df=3, lower.tail=FALSE)
ChiUp/2

# Use the kickloglik.png, 
# find the relevant quantile qchisq(0.05, df=1, lower.tail=FALSE)/2
# And find the two intersection between the plot and the quantile line