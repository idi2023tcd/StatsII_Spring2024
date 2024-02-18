#####################
########################## PROBLEM SET 2 # 
#### APPLIED STATISTICAL ANALYSIS II
######
# Student Name: Idi Amin Da Silva
# Student Number: 233 722 25
##########
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
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################
#
# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))
####
pset2_data<-climateSupport
head(pset2_data)
#########################
# In the 'choice' column/variable contains logical values Not supported and Supported
# I Convert 'Supported' to 1 and 'Not supported' to 0, also I convert the countries and sanctions variables too.

pset2_data$choice <- ifelse(pset2_data$choice == "Supported", 1, 0)
pset2_data$countries <- factor(pset2_data$countries, ordered=FALSE)
pset2_data$sanctions <- factor(pset2_data$sanctions, ordered=FALSE)
#####
head(pset2_data)
View(pset2_data)
dim(pset2_data) # Rows/Observations= 8500; Columns/Variables=3
names(pset2_data) # "choice" ;   "countries" ; "sanctions"
##
with(pset2_data, table(pset2_data$choice)) # This line of the code provide the frequency of Not supported and Supported 
#  0         1 
# 4264    4236 
#####################
##
## Fit logistic regression model
fit_myFull_logit_supporting_policy <- glm (choice ~ countries + sanctions, family=binomial(link="logit"),
                                           data=pset2_data)
# Summary of the logistic regression model
summary(fit_myFull_logit_supporting_policy)
####
###Answer of Summary Output 
###
#Call:
#  glm(formula = choice ~ countries + sanctions, family = binomial(link = "logit"), 
#      data = pset2_data)
#
#Coefficients:
#                           Estimate      Std. Error  z value   Pr(>|z|)    
#(Intercept)                -0.27266      0.05360     -5.087    3.64e-07 ***
#  countries80 of 192       0.33636       0.05380     6.252     4.05e-10 ***
#  countries160 of 192      0.64835       0.05388     12.033    < 2e-16 ***
#  sanctions5%              0.19186       0.06216     3.086     0.00203 ** 
#  sanctions15%             -0.13325      0.06208     -2.146    0.03183 *  
#  sanctions20%             -0.30356      0.06209     -4.889    1.01e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 11783  on 8499  degrees of freedom
#Residual deviance: 11568  on 8494  degrees of freedom
#AIC: 11580
#
#Number of Fisher Scoring iterations: 4

#################################################################################
###### INTERPRETING THE MODEL PARAMETERS ###
# Let's recall the regression coefficients:

coef(fit_myFull_logit_supporting_policy)
#
# Answer/Output:
#
# (Intercept)  countries80 of 192   countries160 of 192     sanctions5%        sanctions15%        sanctions20% 
#-0.2726631           0.3363609           0.6483497           0.1918553          -0.1332475          -0.3035641 
###

# In a logistic regression, the response being modeled is the log(odds) that Y=1. The regression coefficients gives the
#change in log(odds) in the response for a unit change in the predictor variables, holding all other predictor variables
#constant.Since log(odds) are difficult to interpret, I have decided to exponentiate them (i.e., estimated parameters) 
#to put the results on odd scale:

exp(coef(fit_myFull_logit_supporting_policy))

## Answer:
#(Intercept)  countries80 of 192  countries160 of 192     sanctions5%        sanctions15%        sanctions20% 
#0.7613492        1.3998442           1.9123823           1.2114952           0.8752484           0.7381826 
##################################################################################################################
####
## I would like to explore the dataset in more detail and I am interested in finding the 95% confidence interval for
# estimated coefficients/parameters and coefficients:

# An option for making a data.frame of confidence intervals and coefficients
confMod1 <- data.frame(cbind(lower = exp(confint(fit_myFull_logit_supporting_policy)[,1]), # Calculate lower conf.int 
                            coefs = exp(coef(fit_myFull_logit_supporting_policy)), # The estimated coefficients
                            upper = exp(confint(fit_myFull_logit_supporting_policy)[,2]))) # Calculate upper conf.int
confMod1

### Answer:
#                           lower           coefs           upper
#(Intercept)                0.6853331       0.7613492       0.8455914
#countries80 of 192         1.2598455       1.3998442       1.5556530
#countries160 of 192        1.7209643       1.9123823       2.1257297
#sanctions5%                1.0725857       1.2114952       1.3685566
#sanctions15%               0.7749409       0.8752484       0.9884554
#sanctions20%               0.6535281       0.7381826       0.8336468
####################################
#
#### Now I am interested in Calculating the Global Null Hypothesis and p-value.
# Perform likelihood ratio test
#
my_lr_test <- anova(fit_myFull_logit_supporting_policy, test = "Chisq")# This line of the code assesses the significance 
# of the predictor variables(countries and sanctions) in explaining the response variable (choice) in logistic
# regression model.
my_lr_test
# Answer/Output
#Analysis of Deviance Table

#Model: binomial, link: logit

#Response: choice

#Terms added sequentially (first to last)


#             Df    Deviance    Resid. Df   Resid. Dev      Pr(>Chi)    
#NULL               8499        11783              
#countries    2     146.724     8497        11637           < 2.2e-16 ***
#sanctions    3     68.426      8494        11568           9.272e-15 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
####################################################################
# Extract p-value for the global null hypothesis

my_global_null_p_value <- my_lr_test$"Pr(>Chi)"
print(my_global_null_p_value) # Print the p-value
## 
## Answer: 1.378383e-32 9.271817e-15
####################################################################
#####################################################################################################
#### Question 2:
#Plotting an interactive graph by using the packages: effects and plotly
###
##### (2a)
##
# The coefficient for "sanctions" 15% is -0.1332475 and the coefficient for "sanctions" 5% is 0.1918553
#####
# Calculate the odds ratio for sanctions 15% compared to sanctions 5%

# First method
#logit(p/1-p)=-0.2726631 + 0.3363609*countries_[80 of 192] +0.6483497*countries_[160 of 192] + 0.1918553*sanctions_5%
# -0.1332475*sanctions_15% - 0.3035641*sanctions_20% # The general equation of logistic regression equation
##
# To answer question (2a) we need to extract from general equation the following:
##
#logit(p_5%/(1-p_5%))= -0.2726631 + 0.6483497*countries_[160 of 192] + 0.1918553*sanctions_5% # Transform r.h.s into Rcode
my_5 <- -0.2726631 + 0.6483497*1 + 0.1918553*1
my_5 # Answer: 0.5675419
##########
#logit(p_15%/(1-p_15%))=-0.2726631 + 0.6483497*countries_[160 of 192] - 0.1332475*sanctions_15% 
my_15 <- -0.2726631 + 0.6483497 - 0.1332475
my_15 # Answer:  0.2424391
####
#Now I am going to calculate the difference between my_15 and my_15
diff_my_15_and_my_5 <- my_15 - my_5
diff_my_15_and_my_5 # Answer: -0.3251028
####
exp(diff_my_15_and_my_5) # 0.7224531
#
or
exp(my_15 - my_5) #  0.7224531

#############################################
# (2b)
#logit(p_80/(1-p_80)) <-  -0.2726631 + 0.3363609*countries_[80 of 192]
my_80 <- -0.2726631 + 0.3363609*1
my_80 # Answer:  0.0636978
prob_80 <- 1/(1 + exp(-my_80))
prob_80 # 0.5159191 or 51.59191%
######################
# Obtain effects of predictors

install.packages("effects")
library("effects")
#
install.packages("ggplot2")
library("ggplot2")
#
install.packages("tidyverse")
library("tidyverse")
#
install.packages("plotly")
library("plotly")
##################
effects_plot <- allEffects(fit_myFull_logit_supporting_policy)
summary(effects_plot)
# Answer/Output:
#
#model: choice ~ countries + sanctions

#countries effect
#countries
#20 of 192  80 of 192     160 of 192 
#0.4172845  0.5006076     0.5779635 

#Lower 95 Percent Confidence Limits
#countries
#20 of 192  80 of 192     160 of 192 
#0.3992755  0.4819909     0.5596200 

#Upper 95 Percent Confidence Limits
#countries
#20 of 192  80 of 192     160 of 192 
#0.4355169  0.5192225     0.5960945 

#sanctions effect
#sanctions
#None         5%          15%       20% 
#0.5136380  0.5612953   0.4803396  0.4380700 

#Lower 95 Percent Confidence Limits
#sanctions
#None        5%       15%       20% 
#0.4921582 0.5399365 0.4588899 0.4169742 

#Upper 95 Percent Confidence Limits
#sanctions
#None         5%           15%       20% 
#0.5350674  0.5824294   0.5018621  0.4593921 
#######################################################################################################
###################################################################################################

############################################################################################
#(2c)
fit_interaction_logit <- glm(choice ~ countries * sanctions, family = binomial(link = "logit"), data = pset2_data)
summary(fit_interaction_logit)
########
#### Answer/Output:
#Call:
#  glm(formula = choice ~ countries * sanctions, family = binomial(link = "logit"), 
#      data = pset2_data)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                      -0.27469    0.07534  -3.646 0.000267 ***
#  countries80 of 192                0.37562    0.10627   3.535 0.000408 ***
#  countries160 of 192               0.61266    0.10801   5.672 1.41e-08 ***
#  sanctions5%                       0.12179    0.10518   1.158 0.246909    
#sanctions15%                     -0.09687    0.10822  -0.895 0.370723    
#sanctions20%                     -0.25260    0.10806  -2.338 0.019412 *  
#  countries80 of 192:sanctions5%    0.09471    0.15232   0.622 0.534071    
#countries160 of 192:sanctions5%   0.13009    0.15103   0.861 0.389063    
#countries80 of 192:sanctions15%  -0.05229    0.15167  -0.345 0.730262    
#countries160 of 192:sanctions15% -0.05165    0.15267  -0.338 0.735136    
#countries80 of 192:sanctions20%  -0.19721    0.15104  -1.306 0.191675    
#countries160 of 192:sanctions20%  0.05688    0.15367   0.370 0.711279    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 11783  on 8499  degrees of freedom
#Residual deviance: 11562  on 8488  degrees of freedom
#AIC: 11586

#Number of Fisher Scoring iterations: 4
######################################################################################################
# Reduced Model for logistics Regression Model:
logit.fit.reduced <- step(fit_interaction_logit)
### Answer/Output
#Start:  AIC=11585.97
#choice ~ countries * sanctions

#Df Deviance   AIC
#- countries:sanctions  6    11568 11580
#<none>                      11562 11586
#
#Step:  AIC=11580.26
#choice ~ countries + sanctions
#
#Df Deviance   AIC
#<none>            11568 11580
#- sanctions  3    11637 11643
#- countries  2    11715 11723
#########################################################
summary(logit.fit.reduced)
#### Answer/Output:
#> summary(logit.fit.reduced)

#Call:
#  glm(formula = choice ~ countries + sanctions, family = binomial(link = "logit"), 
#      data = pset2_data)
#
#Coefficients:
#                     Estimate S td. Error  z value    Pr(>|z|)    
#(Intercept)          -0.27266    0.05360  -5.087     3.64e-07 ***
#  countries80 of 192   0.33636    0.05380   6.252    4.05e-10 ***
#  countries160 of 192  0.64835    0.05388  12.033    < 2e-16 ***
#  sanctions5%          0.19186    0.06216   3.086    0.00203 ** 
#  sanctions15%        -0.13325    0.06208  -2.146    0.03183 *  
#  sanctions20%        -0.30356    0.06209  -4.889    1.01e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 11783  on 8499  degrees of freedom
#Residual deviance: 11568  on 8494  degrees of freedom
#AIC: 11580

#Number of Fisher Scoring iterations: 4

############################################################################################
##### Extra work: Data Visualization - Plotting an interactive graphic using packages effects and plotly
#####
# Load the necessary libraries
install.packages("effects")
install.packages("plotly")
library(effects)
library(plotly)
##
# Convert effects plot to a ggplot object
ggplot_obj <- plot(effects_plot)

# Convert ggplot object to plotly object
plotly_obj <- ggplotly(ggplot_obj)

# Display the interactive plot
plotly_obj
####
#######

