##############
# Tutorial 5 Ordered and Multinomial Logistic Regression #
##############
###################
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
###########
library(MASS)
library(nnet)
library(ggplot2)
###########################

#### LOAD THE workingmum dataset

workingMoms <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/WorkingMoms.txt", header=T)
head(workingMoms)
dim(workingMoms)# Rows/Columns=2293; Columns/Variables=7
names(workingMoms) # "age";   "education";  "prestige";   "gender";    "year"  ;    "race" ;     "attitude" 
#########
# EDA
summary(workingMoms)
ftable(xtabs(~ gender + year + attitude, data = workingMoms))

# do some wrangling
workingMoms$attitude <- factor(workingMoms$attitude, 
                               levels = c("SD", "D", "A", "SA"),
                               labels = c("Strongly Disagree",
                                          "Disagree",
                                          "Agree",
                                          "Strongly Agree"))
workingMoms$gender <- as.factor(workingMoms$gender)
workingMoms$race <- factor(workingMoms$race,
                           levels = c(0,1),
                           labels = c("Non-white", "White"))
workingMoms$year <- factor(workingMoms$year,
                           levels = c("Year1977", "Year1989"),
                           labels = c("1977", "1989"))

ftable(xtabs(~ gender + year + attitude, data = workingMoms))

ggplot(workingMoms, aes(x=attitude, y=prestige, fill=attitude)) +
  geom_boxplot() +
  stat_boxplot(geom="errorbar", width=0.5)+
  geom_point()+
  geom_jitter(alpha = 0.3) +
  scale_x_discrete(labels=function(x){sub("\\s", "\n", x)}) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme_bw()+
  facet_grid(gender ~ year)

# a) Perform an ordered (proportional odds) logistic regression

ord.log <- polr(attitude ~ ., data = workingMoms, Hess = TRUE)
summary(ord.log)

# Calculate a p value
ctable <- coef(summary(ord.log))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
##
# My Method of calculating p-value:
ctable2 <- coef(summary(ord.log)) # Extract coefficient summary
p <- 2 * (1 - pnorm(abs(ctable2[, "t value"]))) # Calculate the p-value
ctable2 <- cbind(ctable2, "p-value" = p) ## Combine coefficient summary and p-values
print(ctable2) # Print the results 
##
# Calculate confidence intervals
(ci <- confint(ord.log))

# convert to odds ratio
exp(cbind(OR = coef(ord.log), ci))

# How do we interpret these coefficients?

# b) fit a multinomial logit model
# set a reference level for the outcome
workingMoms$attitude <- relevel(workingMoms$attitude, ref = "Strongly Disagree")

# run model
mult.log <- multinom(attitude ~ ., data = workingMoms)
summary(mult.log)
exp(coef(mult.log))

# get p values
z <- summary(mult.log)$coefficients/summary(mult.log)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)

# how do we interpret these coefficients?

# we can use predicted probabilities to help our interpretation
pp <- data.frame(fitted(mult.log))
head(data.frame(attitude = workingMoms$attitude,
                SD = pp$Strongly.Disagree,
                D = pp$Disagree,
                A = pp$Agree,
                SA = pp$Strongly.Agree))

# c) Consider gender as an interaction
mult.log.int <- multinom(attitude ~ gender * ., data = workingMoms)
summary(mult.log.int)

z.int <- summary(mult.log.int)$coefficients/summary(mult.log.int)$standard.errors
(p.int <- (1 - pnorm(abs(z.int), 0, 1)) * 2)

pp.int <- data.frame(fitted(mult.log))
head(data.frame(attitude = workingMoms$attitude,
                SD = pp.int$Strongly.Disagree,
                D = pp.int$Disagree,
                A = pp.int$Agree,
                SA = pp.int$Strongly.Agree))

