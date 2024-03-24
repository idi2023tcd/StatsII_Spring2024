#######################
# Tutorial 9: Poisson #
#######################

library(tidyverse)
library(ggplot2)

long_data <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Long.txt", header=T)
head(long_data)
dim(long_data) # Rows/Observations=915; Columns/Variables=6
names(long_data) # "fem"  "ment" "phd"  "mar"  "kid5" "art"
str(long_data)
# wrangle
long_data <- within(long_data, {
  fem <- as.logical(fem)
  mar <- as.logical(mar)
})

# EDA
str(long_data)
summary(long_data)

with(long_data,
     list(mean(art), var(art))) # do we meet assumptions for Poisson?
     
# a) Examine distribution of response variable
hist(long_data$art)
##
# 2nd Method:
# Plot histogram
hist(long_data$art, col = "lightblue", main = "Histogram of 'art' Variable")

# Add density line curve
lines(density(long_data$art), col = "red", lwd = 2)
##

ggplot(long_data, aes(ment, art, color = fem)) +
  geom_jitter(alpha = 0.5)

# OLS?
mod.lm <- lm(art ~ ., data = long_data)
summary(mod.lm)

mod2.lm <- lm(art ~ fem * ., data = long_data) # interaction effects with gender?
summary(mod2.lm)

# Do we meet assumptions?
plot(predict(mod2.lm), abs(resid(mod2.lm)), xlab = "Predicted", ylab = "Absolute Residuals")

sresid <- rstandard(mod2.lm) # distribution of standardised residuals
hist(sresid, main = "")

par(mfrow = c(2, 2))
plot(mod2.lm) # R's built-in plots for linear regression model assessment

# b) Poisson regression
mod.ps <- glm(art ~ ., data = long_data, family = poisson)
summary(mod.ps)

# interpreting outputs
cfs <- coef(mod.ps)

# predicted no. of articles for a married male PhD researcher with 1 child at 2-rated 
# institute whose PhD supervisor published 5 articles.
exp(cfs[1] + cfs[2]*0 + cfs[3]*5 + cfs[4]*2 + cfs[5]*1 + cfs[6]*1)

pred <- data.frame(fem = FALSE,
                   ment = 5,
                   phd = 2,
                   mar = TRUE,
                   kid5 = 1)

# check with predict() function
predict(mod.ps, newdata = pred, type = "response")

# plot predictions vs count
ggplot(data = NULL, aes(x = mod.ps$fitted.values, y = long_data$art)) +
  geom_jitter(alpha = 0.5) +
  geom_abline(color = "blue") + 
  theme_bw()+
  theme(panel.grid=element_blank()) #
  #geom_smooth(method = "loess", color = "red")

# calculate pseudo R squared
1 - (mod.ps$deviance/mod.ps$null.deviance) # R_square=0.1007119 that could be explained amount the variables

# calculate RMSE
sqrt(mean((mod.ps$model$art - mod.ps$fitted.values)^2)) # Answer: 1.838292

# Add an interaction?
mod2.ps <- glm(art ~ fem * ., data = long_data, family = poisson)
summary(mod2.ps)


1 - (mod2.ps$deviance/mod2.ps$null.deviance) # pseudo R2 =0.1041892
sqrt(mean((mod2.ps$model$art - mod2.ps$fitted.values)^2)) # RMSE = 1.837679

# c) Over-dispersion?
install.packages("AER")
library(AER)

dispersiontest(mod.ps)
# Answer:
#	Overdispersion test

#data:  mod.ps
#z = 5.7825, p-value = 3.681e-09
#alternative hypothesis: true dispersion is greater than 1
#sample estimates:
#  dispersion 
#1.82454  This value should be less than 1
##
mod2.ps <- glm(art ~ fem*., data=long_data, family=quasipoisson)
summary(mod2.ps)
######
install.packages("pscl")
library(pscl)

mod.zip <- zeroinfl(art ~ ., data = long_data, dist = "poisson")
summary(mod.zip)
###############
AIC (mod.ps, mod2.ps)
## Answer:
#         df      AIC
#mod.ps   6     3314.113
#mod2.ps 11       NA
######
AIC(mod2.ps, mod.zip)
## Answer:
#        df      AIC
#mod2.ps 11       NA
#mod.zip 12 3233.546
#####
exp(coef(mod.ps))
####
exp(0.304619)
