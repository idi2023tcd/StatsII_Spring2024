# PROBLEM SET 3
#APPLIED STATISTICAL ANALYSIS II
#STUDENT FULL NAME: IDI AMIN DA SILVA
# STUDENT NUMBER: 23372225

#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
#
install.packages(c("nnet", "MASS", "mlogit"))
library("nnet")
library("MASS")
library("mlogit") # mlogit package is used to perform Multinomial Logistic Regression Model 
#
install.packages("AER")
library("AER")
#
install.packages("pscl")
library("pscl")
######################
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS", "mlogit"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################
### Answer of Problem 1/Question 1
# load data
#gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)
##
gdp_data <- read.csv("C:/NewGithubFolder/StatsII_Spring2024/datasets/gdpChange.csv", stringsAsFactors = F)# To Load dataset into R
head(gdp_data) # This line of the code provide the first 6 observations in the dataset
dim(gdp_data) # Rows/Observations=3721; Columns/Variables=12
names(gdp_data)# "X" ;"COUNTRY"; "CTYNAME"; "YEAR"; "GDPW"; "OIL"; "REG" ;"EDT";"GDPWlag";"GDPWdiff";"GDPWdifflag" ;"GDPWdifflag2" 
str(gdp_data) # # Check the structure of the dataset
#####################
# NOTE:Before I start answering/Solving the Question 1 of the Problem Set III parts (1) and (2) I need to do some data
# WRANGLING:
##
##
# Create a new column with categorical variable based on continuous/numerical variable GDPWdiff
gdp_data$GDPWdiff_category <- ifelse(gdp_data$GDPWdiff < 0, "negative",
                                     ifelse(gdp_data$GDPWdiff == 0, "no change", "positive"))
head(gdp_data) # # View the first few rows to verify the transformation
#
table(gdp_data$GDPWdiff_category)
#Answer:
#  negative  no change   positive 
#   1105        16        2600
##
###
## Transform the REG (Regime) from binary (0;1) into the Categorical 0=Non-Democracy; 1=Democracy
gdp_data$REG <- factor(gdp_data$REG, levels=c(0, 1), labels=c("Non-Democracy", "Democracy"))
table(gdp_data$REG)
#### Answer/Output
#Non-Democracy     Democracy 
#   2227             1494 
########
#Transform the variable OIL from binary to Categorical Variable:
gdp_data$OIL <- factor(gdp_data$OIL, levels=c(0, 1), labels=c("Not Exceed 50%","otherwise"))
table(gdp_data$OIL)
######
#Not Exceed 50%    Otherwise 
#     3374            347
#####
ftable(xtabs(~REG + GDPWdiff_category + OIL, data=gdp_data))
# Answer:
#                           OIL Not Exceed 50%  otherwise
#REG           GDPWdiff_category                             
#Non-Democracy negative            641           93
#              no change           14            0
#              positive           1284          195

#Democracy     negative           332            39
#no change                         2             0
#positive                         1074           47
######
gdp_data$GDPWdiff_category <- factor(gdp_data$GDPWdiff_category, levels=c("negative","no change","positive"), 
                                     labels=c("negative","no change","positive"))
####################
#################################
#PROBLEM SET III. Question 1: Part 1.
# Fitting an unordered multinomial logit with as the output and setting a reference category "no change".
##
gdp_data$GDPWdiff_category <- relevel(gdp_data$GDPWdiff_category , ref="no change")
# Run the Model:
multinom_model_unordered <- multinom(GDPWdiff_category ~ REG + OIL, data = gdp_data)
summary(multinom_model_unordered) # # Summary of the model
##
## Answer/Output
#Call:
#  multinom(formula = GDPWdiff_category ~ REG + OIL, data = gdp_data)

#Coefficients:
#           (Intercept) REGDemocracy OILotherwise
#negative    3.805370     1.379282     4.783968
#positive    4.533759     1.769007     4.576321

#Std. Errors:
#           (Intercept) REGDemocracy OILotherwise
#negative   0.2706832    0.7686958     6.885366
#positive   0.2692006    0.7670366     6.885097

#Residual Deviance: 4678.77 
#AIC: 4690.77 
################################################################
# Multinomial Regression: Estimation:
#(i) For GDPWdiff_negative and the reference category is DGPWdiff_nochange:

# ln(GDPWdiif_negative/DGPWdiff_nochange)=3.805370 + 1.379282*REGDemocracy + 4.783968*OILotherwise
#(GDPWdiif_negative/DGPWdiff_nochange) = exp(3.805370 + 1.379282*REGDemocracy + 4.783968*OILotherwise)
#
#(ii) For GDPWdiff_positive and the reference category is DGPWdiff_nochange:

# ln(GDPWdiif_positive/DGPWdiff_nochange)=4.533759 + 1.769007*REGDemocracy + 4.576321*OILotherwise
#(GDPWdiif_positivetive/DGPWdiff_nochange) = exp(4.533759 + 1.769007*REGDemocracy + 4.576321*OILotherwise)

exp(coef(multinom_model_unordered)) # Convert the coefficients to odds ratio 
# Answer/Output:
#             (Intercept) REGDemocracy OILotherwise
#negative    44.94186     3.972047    119.57794
#positive    93.10789     5.865024     97.15632
#####
# Calculate the p-values:
z <- summary(multinom_model_unordered)$coefficients/summary(multinom_model_unordered)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1))*2)
## Answer:
#               (Intercept) REGDemocracy OILotherwise
#negative           0       0.07276308    0.4871792
#positive           0       0.02109459    0.5062612
############
#I can use predicted probabilities to help me interpret my output/variables estimated
pp <- data.frame(fitted(multinom_model_unordered))
head(data.frame(GDPWdiff_category=gdp_data$GDPWdiff_category, 
                negative=pp$negative,
                nochange=pp$no.change,
                positive=pp$positive))

## Answer/Output
##
#GDPWdiff_category  negative     nochange  positive
#          positive 0.3726529 6.934296e-05 0.6272778
#          negative 0.3726529 6.934296e-05 0.6272778
#          positive 0.3726529 6.934296e-05 0.6272778
#          positive 0.3726529 6.934296e-05 0.6272778
#          positive 0.3726529 6.934296e-05 0.6272778
#          positive 0.3726529 6.934296e-05 0.6272778
####################################################################################
#PROBLEM SET III. Question 1: Part 2.
#Construct and interpret an ordered multinomial logit with GDPWdiff as the outcome variable, including the estimated cutoff points
# and coefficients.
###########################################################
# Convert GDPWdiff to a factor with ordered levels
#gdp_data$GDPWdiff_category <- factor(gdp_data$GDPWdiff_category, levels = c("negative", "no change", "positive"), 
#                                     ordered = TRUE)
#
# Fit the ordered multinomial logistic regression model:

multinom_model_ordered <- polr(GDPWdiff_category ~ REG + OIL, data = gdp_data, Hess = TRUE)
summary(multinom_model_ordered) # # Summary of the model
### Answer/Output
#
#Call:
#  polr(formula = GDPWdiff_category ~ REG + OIL, data = gdp_data, 
#       Hess = TRUE)
#
#Coefficients:
#                     Value   Std. Error  t value
#REGDemocracy       0.3985    0.07518    5.300
#OILNot Exceed 50%  0.1987    0.11572    1.717
#
#Intercepts:
#                     Value   Std. Error    t value
#negative|no change -0.5325   0.1097        -4.8544
#no change|positive -0.5118    0.1097       -4.6671
#
#Residual Deviance: 4687.689 
#AIC: 4695.689
#######################
## Calculating the p-value
ctable1 <- coef(summary(multinom_model_ordered)) # Extract coefficient summary
p <- 2 * (1 - pnorm(abs(ctable1[, "t value"]))) # Calculate the p-value
ctable1 <- cbind(ctable1, "p-value" = p) ## Combine coefficient summary and p-values
print(ctable1) # Print the results 
## Answer:
#                     Value      Std. Error   t value      p-value
#REGDemocracy        0.3984828  0.07518478    5.300046   1.157735e-07
#OILNot Exceed 50%   0.1987196  0.11571711    1.717288   8.592653e-02
#negative|no change -0.5324600  0.10968546   -4.854426   1.207358e-06
#no change|positive -0.5117652  0.10965270   -4.667147   3.054110e-06
#####
# Calculating  95% confidence intervals:
(ci <- confint(multinom_model_ordered))
## Answer:
#                          2.5 %       97.5 %
#REGDemocracy           0.25165482    0.5464341
#OILNot Exceed 50%      -0.03019571   0.4237548
####
# Converting to odds ratio:
exp(cbind(OR=coef(multinom_model_ordered), ci))
## Answer:
#                        OR        2.5 %       97.5 %
#REGDemocracy         1.489563    1.2861520   1.727083
#OILNot Exceed 50%    1.219840    0.9702556   1.527687
###############################################################################################################################
###############################################################################################################################
# Problem 2
##################### Answer of Problem Set III;  Question 2

# load data
#mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")
###
mexico_elections <- read.csv("C:/NewGithubFolder/StatsII_Spring2024/datasets/MexicoMuniData.csv")
#
head(mexico_elections)
names(mexico_elections)#"MunicipCode" ; "pan.vote.09"; "marginality.06"; "PAN.governor.06"; "PAN.visits.06"; "competitive.district"
dim(mexico_elections) # Rows/Observations=2407; Columns/Variables=6
str(mexico_elections)
################################################################################################
########################################################################################
##############
# Outcome variable: "PAN.visits.06"
###
# Predictors Variables of the interest:
# "competitive.district: 1=close/swing district; 0="safe seat") " 
#PAN.governor.06"
#"marginality.06"
#"PAN.governor.06"
####
table(mexico_elections$PAN.visits.06)
# Answer:
#
#   0    1     2    3    4    5   35 
# 2272  102   17   12    1    2    1 
###############
table(mexico_elections$"marginality.06")
# (a) Answers:
poisson_reg.model <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + 
                           PAN.governor.06 , data=mexico_elections, family=poisson)
summary(poisson_reg.model)
# Answer:
#Call:
#glm(formula = PAN.visits.06 ~ competitive.district + marginality.06 + 
#      PAN.governor.06, family = poisson, data = mexico_elections)
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          -3.81023    0.22209 -17.156   <2e-16 ***
#  competitive.district -0.08135    0.17069  -0.477   0.6336    
#marginality.06       -2.08014    0.11734 -17.728   <2e-16 ***
#  PAN.governor.06      -0.31158    0.16673  -1.869   0.0617 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for poisson family taken to be 1)

#Null deviance: 1473.87  on 2406  degrees of freedom
#Residual deviance:  991.25  on 2403  degrees of freedom
#AIC: 1299.2

#Number of Fisher Scoring iterations: 7
###
### SLIDE 20 # Over-dispersion test in R
# Check equal variance assumption
##
install.packages("AER")
library("AER")
#
dispersiontest(poisson_reg.model)
## Answer:
#Overdispersion test

#data:  poisson_reg.model
#z = 1.0668, p-value = 0.143
#alternative hypothesis: true dispersion is greater than 1
#sample estimates:
#  dispersion 
#2.09834 
########
#Slide 22  Zip Model in R
# R contributed package "pscl" contains the function zeroinfl:
install.packages("pscl")
library("pscl")
###
zeroinfl_poisson_1 <- zeroinfl(PAN.visits.06 ~ competitive.district + marginality.06 +
                                 PAN.governor.06 , data=mexico_elections, dist="poisson")
summary(zeroinfl_poisson_1)
## Answer:
#Call:
#zeroinfl(formula = PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = mexico_elections, 
#         dist = "poisson")

#Pearson residuals:
#  Min       1Q   Median       3Q      Max
#-0.95323 -0.24006 -0.12842 -0.06045 37.56115

#Count model coefficients (poisson with log link):
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)           -1.9145     0.4982  -3.843 0.000122 ***
#  competitive.district   0.4024     0.3119   1.290 0.197028
#marginality.06        -1.2398     0.2610  -4.750 2.03e-06 ***
#  PAN.governor.06       -0.4703     0.2707  -1.737 0.082341 .

#Zero-inflation model coefficients (binomial with logit link):
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)            1.2719     0.6753   1.883  0.05966 .
#competitive.district   0.9000     0.5106   1.763  0.07794 .
#marginality.06         0.8716     0.3021   2.885  0.00392 **
#   PAN.governor.06   -0.1749     0.4119  -0.425  0.67106
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Number of iterations in BFGS optimization: 28
# Log-likelihood: -600.4 on 8 Df
####
#(b)
exp(coef(zeroinfl_poisson_1))
## Answer:
#count_(Intercept) count_competitive.district       count_marginality.06      count_PAN.governor.06 
#0.1474155                  1.4953556                  0.2894367                  0.6247883 
#zero_(Intercept)  zero_competitive.district        zero_marginality.06       zero_PAN.governor.06 
#3.5675098                  2.4596673                  2.3906532                  0.8395139 
####
#exp(coef(poisson_reg.model))

# Answer
#(Intercept) competitive.district       marginality.06      PAN.governor.06 
#0.02214298           0.92186932           0.12491227           0.73228985 
######
##(c)
# Coefficients from the Poisson regression model
coefficients <- coef(zeroinfl_poisson_1)

# Calculating the linear predictor (eta) using the coefficients and values
Est_mean.visitors <- data.frame(competitive.district=1, marginality.06=0, PAN.governor.06=1)
exp(predict(zeroinfl_poisson_1, Est_mean.visitors)) # Answer: 1.016598
