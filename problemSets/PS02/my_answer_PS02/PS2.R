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
View(pset2_data)
dim(pset2_data) # Rows/Observations= 8500; Columns/Variables=3
names(pset2_data) # "choice" ;   "countries" ; "sanctions"
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
#glm(formula = choice ~ countries + sanctions, family = binomial(link = "logit"), 
#    data = pset2_data)

#Coefficients:
#               Estimate      Std. Error    z value     Pr(>|z|)    
#(Intercept)    -0.005665     0.021971      -0.258      0.796517    
#countries.L     0.458452     0.038101      12.033     < 2e-16 ***
#countries.Q    -0.009950     0.038056      -0.261       0.793741    
#sanctions.L    -0.276332     0.043925      -6.291      3.15e-10 ***
#sanctions.Q    -0.181086     0.043963      -4.119      3.80e-05 ***
#sanctions.C    0.150207      0.043992      3.414       0.000639 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 11783  on 8499  degrees of freedom
#Residual deviance: 11568  on 8494  degrees of freedom
#AIC: 11580
#
#Number of Fisher Scoring iterations: 4
#################################################################
#From the p-values for regression coefficients(the last column), I have noticed that countries.Q("coefficient for quadratic 
#line") may not make a significant contribution to the equation (I can't reject the hypothesis that the parameters are zero).
##
#I will fit the second equation/model without countries.Q and test if the reduced model fits the data as well:
##
fit_Reduced <- glm(choice~ countries.L + sanction.L + sanction.Q + sanction.C, family=binomial(link="logit"), 
                   data=pset2_data )
summary(fit_Reduced)
#######
table(pset2_data$countries)
#
# Now let me fit the 2nd equation
#### Now I am interested in Calculating the Global Null Hypothesis and p-value.
# Perform likelihood ratio test

my_lr_test <- anova(fit_myFull_logit_supporting_policy, test = "Chisq")# This line of the code assesses the significance of the
#predictor variables(countries and sanctions) in explaining the response variable (choice) in logistic regression model.
my_lr_test
# Answer/Output
##
#Analysis of Deviance Table

#Model: binomial, link: logit

#Response: choice

#Terms added sequentially (first to last)


#             Df    Deviance    Resid. Df   Resid. Dev      Pr(>Chi)    
#NULL                 8499         11783              
#countries    2       146.724      8497      11637          < 2.2e-16 ***
#  sanctions  3       68.426       8494      11568          9.272e-15 ***
#  ---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
####################################################################
# Extract p-value for the global null hypothesis

my_global_null_p_value <- my_lr_test$"Pr(>Chi)"
print(my_global_null_p_value) # # Print the p-value
## 
## Answer: 1.378383e-32 9.271817e-15
########################
# Plotting the logistic regression model
# I have decided to use the 'effects' package for visualizing the effects of predictors
##
install.packages("effects")
library(effects)

# Plot the effects of the 'countries' variable
plot(allEffects(fit_myFull_logit_supporting_policy))
##################
#