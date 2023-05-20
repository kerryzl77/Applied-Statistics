myr <- c(0,4,8,12,20,24,28,32,36,48,52,56,60,64,68,72,76,80,84,88,92,96,
         + 100,104,108)
mtimes <- c(11.0,11.0,10.8,10.8,10.8,10.6,10.8,10.3,10.3,10.3,10.4,10.5,
            + 10.2,10.0,9.95,10.14,10.06,10.25,9.99,9.92,9.96,9.84,9.87,9.85,9.69)
fyr <- c(28,32,36,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,108)
ftimes <- c(12.2,11.9,11.5,11.9,11.5,11.5,11.0,11.4,11.08,11.07,11.08,
            + 11.06,10.97,10.54,10.82,10.94,10.75,10.93,10.78)
plot(myr,mtimes,ylim=c(9,13),
     main="Winning times, men (x) and women (+), Olympic 100m",
     xlab="Year (0=1900)",ylab="time (secs)",pch=4)
points(fyr,ftimes,pch=3)

# mregr and fregr - single model， different slopes and intercepts for men and women
mregr <- lm(mtimes ~ myr)
summary(mregr)
anova(mregr)
# Residuals 0.4004 / 23 df

mpred <- predict.lm(mregr)
lines(myr,mpred,lty=2)

# ---------
fregr <- lm(ftimes ~ fyr)
summary(fregr)
anova(fregr)
# Residuals 0.7452 / 17 df

fpred <- predict.lm(fregr)
lines(fyr,fpred,lty=2)
times <- c(mtimes,ftimes)
yr <- c(myr,fyr)
mn <- length(mtimes)
fn <- length(ftimes)
gend <- c(rep("m",mn),rep("f",fn))
gend <- factor(gend)

# ---------
# bregr fits a model with the same slope for men and women but with different intercepts
bregr <- lm(times ~ yr + gend)
summary(bregr)
anova(bregr)
# Residuals 1.3408 / 41 df

# ---------
newbm <- data.frame(gend=c(rep("m",mn+fn)))
predm2 <- predict.lm(bregr,newbm)
lines(yr,predm2)
newbf <- data.frame(gend=c(rep("f",mn+fn)))
predf2 <- predict.lm(bregr,newbf)
lines(yr,predf2)
nexto <- data.frame(yr=c(112,112),gend=c("m","f"))
predict.lm(bregr,nexto,interval=c("prediction"))

# i-------------------------------
# F = [(RSS_reduced - RSS_full) / (df_reduced - df_full)] / (RSS_full / df_full)
((1.3408 - 1.1456) / (41 - 40))/(1.1456/40)
# F = 6.815642
# qf(0.95,1,40) = 4.084746
# qf(0.99,1,40) = 7.3141
# Moderate evidence that model 1 is better 
# -------------------------------
resf <- residuals(fregr)
qqnorm(resf)
qqline(resf)
resm <- residuals(mregr)
qqnorm(resm)
qqline(resm)
ks.test(resf, "pnorm", mean = mean(resf), sd = sd(resf))
ks.test(resm, "pnorm", mean = mean(resm), sd = sd(resm))
# Both test statistics > 0.1

# ii----------------------
# Create separate data frames for men and women
men_data <- data.frame(year = myr, time = mtimes, gender = "male")
women_data <- data.frame(year = fyr, time = ftimes, gender = "female")
# Combine the two data frames into a single data frame
combined_data <- rbind(men_data, women_data)
# Display the combined data frame
combined_data

n1 = dim(men_data)[1] 
p1 = n-anova(mregr)["Residuals","Df"] 
8/(n1-2*p1) 
# 0.3809524
plot(mregr)

n2 = dim(women_data)[1] 
p2 = n-anova(fregr)["Residuals","Df"] 
8/(n2-2*p2) 
# 0.5333333
plot(fregr)

# Generally Okay 

# iii----------------------
# predict the winning time in 2012 base on bregr model 
# fit       lwr       upr
# 1  9.598898  9.212266  9.985529
# 2 10.690599 10.308047 11.073150

# iv----------------------
interaction_model <- lm(times ~ yr * gender, data = combined_data)
anova(interaction_model)
summary(interaction_model)
# yr = -0.016093
summary(fregr)
summary(mregr)
# fyr = 0.016093
# myr = yr + yr:gender = -0.011076
# Interaction model fitting different slope for fe/maele just like 2 seperate models

#######参考答案#######
bregrI = lm(times ~ (yr+gend)^2)
anova(bregrI)

# table(combined_data[,c("gender","year")])
# model.matrix(time ~ (year + gender)^2, data = combined_data)
### Check the design matrix of the model to verify that this is the model that is fitted ###
model.matrix(times ~ (yr+gend)^2)
##############

# v----------------------
anova(interaction_model) 
# anova(bregrI)
# yr:gender = 0.01268 *  significant at 0.05 level 

# v----------------------
plot(interaction_model)
# constant variance okay 
# Normal may bot be satisfied 
resinter <- residuals(interaction_model)
ks.test(resinter, "pnorm", mean = mean(resinter), sd = sd(resinter))
# p-value = 0.2957

#######参考答案 - Cooks Distance #######
n = length(yr);n
p = n-anova(bregrI)["Residuals","Df"];p
Cookdist = 8/(n-2*p);Cookdist
Leverage = 2*p/n;Leverage

lm.influence(bregrI)$hat 
sort(cooks.distance(bregrI))
# Obs 26 suspicious 
##############

####################################################################################
# ---------------------------------------------------
setwd('/Users/liuzikai/Desktop/3508')
load('nuclear.RData')
head(nuclear)
# predicting the cost of construction of further LWR plants
nuclearfit = lm(cost~.,data=nuclear) 
summary(nuclearfit)
# Perform stepwise regression
best_model <- step(nuclearfit)
# Display the summary of the best model
summary(best_model)
# library(MASS)
# stepAIC(nuclearfit, direction = "both", trace = 0)

# iii----------------------
library(olsrr)
ols_vif_tol(best_model)
# partial residual plot 
# All less than 10 - collinearity okay 
plot(best_model)
# Assumption satisfied 

n = dim(nuclear)[1] 
p = n-anova(best_model)["Residuals","Df"] 
8/(n-2*p) # 0.5714286
2*p/n # 0.5625
sort(cooks.distance(best_model))


# iv----------------------
new_plant <- data.frame(date = 90, t1 = 20, t2 = 50, cap = 100, pr = 0, ne = 0, ct = 0, bw = 0, cum.n = 20, pt = 0)
cost_prediction <- predict(best_model, newdata = new_plant, interval = "prediction", level = 0.95)
cost_prediction

# v----------------------
# Date up to 71.08 (max_cost 6.78) but assumption satisfied -- may be reasonable 

# ----------------------
# nuclearfit_2 <- lm(cost ~ date + t2 + cap + cum.n+ (pr + ne + ct  + pt)^2,data=nuclear)
# best_model_2 <- step(nuclearfit_2, direction = "both", trace = 0)
# summary(best_model_2)
# cost_prediction_2 <- predict(best_model_2, newdata = new_plant, interval = "confidence", level = 0.95)
# cost_prediction_2
# 
# summary(nuclearfit_2)
# ols_vif_tol(nuclearfit_2)
# best_model_coeffs <- (coef(best_model))
# date + t2 + cap + pr + ne + ct + cum.n + pt

#######参考答案 - #######
fit <- lm(cost~date+t1+t2+cap+pr+ne+ct+bw+cum.n+pr,data=nuclear)
main=step(fit)  # stepwise model selection
fitI = lm(cost ~ (date+t2+cap+pr+ne+ct+cum.n)^2,data=nuclear)
bestI = step(fitI)
summary(bestI)
AIC(main,bestI)

