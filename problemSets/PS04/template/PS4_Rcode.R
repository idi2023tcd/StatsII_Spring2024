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
getwd() # "C:/NewGithubFolder/StatsII_Spring2024/tutorials"
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

# Fit Cox Proportional Hazard model
cox_model <- coxph(Surv(age, delta) ~ age_mother + gender, data = child)

# Print the summary of the model
summary(cox_model)

