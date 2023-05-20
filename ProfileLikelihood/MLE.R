# setwd('/Users/liuzikai/Desktop/3508/Quiz 2 data-20230214')
load('netdata.RData')
library(nnet)
data <- netdata
netdata

negloglik <- function(theta, data) {
  lp <- theta[1] + theta[2] * data$size.class
  p <- plogis(lp)
  -sum(dbinom(data$Secondary.net, data$Main.net, p, log=TRUE))
}

theta <- c(0, 0)
fit <- optim(theta, negloglik, data=data,hessian = TRUE)
theta_hat <- fit$par
lp <- theta_hat[1] + theta_hat[2] * 2
p <- plogis(lp)
p

Var.theta <- solve(fit$hessian)
X <- cbind(rep(1, nrow(data)), data$size.class)
Var.logitp <- X%*%Var.theta%*%t(X)
se <- sqrt(diag(Var.logitp))
ci <- plogis(lp + c(-1, 1)*qnorm(0.975)*se[2])
ci




