# setwd('/Users/liuzikai/Desktop/3508/Quiz 2 data-20230214')
# load('netdata.RData')
load('prussiandeaths.RData')

theta.range <- seq(mle$par - 3 * sd.MLE, mle$par + 3 * sd.MLE, length.out = 101)

# Calculate the negative log-likelihood function for each value of the parameter
nlls <- rep(0, length(theta.range))
for (i in seq_along(theta.range)) {
  nlls[i] <- nll(theta.range[i], y = prussiandeaths)
}

# Find the critical value for the 95% confidence level
crit.val <- qchisq(0.95, df = 1)
max.val <- -min(nlls)
cut.off <- max.val - crit.val

# Calculate the profile likelihood function
pll <- nlls - min(nlls)

# Find the indices of the upper and lower limits of the profile likelihood interval
lower.idx <- max(which(pll <= crit.val))
upper.idx <- min(which(pll > crit.val))

# Find the upper and lower limits of the profile likelihood interval
lower.limit <- (theta.range[lower.idx])
upper.limit <- (theta.range[upper.idx])

# Print the profile likelihood interval
cat("Profile likelihood interval: [", round(lower.limit, 3), ",", round(upper.limit, 3), "]\n")

plot((theta.range), -pll, type = 'l', xlab = expression(log(theta)), ylab = 'log-likelihood')
abline(h = cut.off, col = 'firebrick3')
