##################################
# Problem set 4
# Student Name: Idi Amin Da Silva
# Student Number: 23372225
##################################

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
#getwd() # "C:/NewGithubFolder/StatsII_Spring2024/tutorials"
###
lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)
#################
install.packages("eha")
library("eha")
eha::child
data(child) # Load the child dataset
View(child)
dim(child) # Rows/Observations=26574; Columns/Variables=10
names(child) #"id"; "m.id";  "sex";  "socBranch";  "birthdate" ; "enter"  ;"exit" ; "event" ; "illeg" ; "m.age"
####
table(child$sex)
#  male     female 
# 13676     12898
###
# Fit Cox Proportional Hazard model
##
#The `child` dataset from the `eha` package is a dataset of 26,574 children born in 
# Skellefte?, Sweden, 1850-1884. Children are followed for fifteen years or until death or 
# outmigration.
###################
# I am interested in  Run a Cox Proportional Hazard regression on the data, using an additive model with 
##`m.age` and `sex` as explanatory variables. Also I will Run a test to assess the quality of the
## model. How can I interpret the coefficients?  and Plot the model.
##

# Fit Cox Proportional Hazard model
cox_model <- coxph(Surv(enter, exit, event) ~ m.age + sex, data = child)

summary(cox_model) ## Print the summary of the model
## Answer : Table Output:
#Call:
#coxph(formula = Surv(enter, exit, event) ~ m.age + sex, data = child)

#n= 26574, number of events= 5616 

#coef exp(coef)  se(coef)      z Pr(>|z|)    
#m.age      0.007617  1.007646  0.002128  3.580 0.000344 ***
#  sexfemale -0.082215  0.921074  0.026743 -3.074 0.002110 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#exp(coef) exp(-coef) lower .95 upper .95
#m.age        1.0076     0.9924     1.003    1.0119
#sexfemale    0.9211     1.0857     0.874    0.9706

#Concordance= 0.519  (se = 0.004 )
#Likelihood ratio test= 22.52  on 2 df,   p=1e-05
#Wald test            = 22.52  on 2 df,   p=1e-05
#Score (logrank) test = 22.53  on 2 df,   p=1e-05
#
#sex     1 488321 2.23165   0.1352
###
stargazer(cox_model)


