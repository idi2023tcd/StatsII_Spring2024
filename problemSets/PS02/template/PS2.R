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
#########################
# In the 'choice' column/variable contains logical values Not supported and Supported
# I Convert 'Supported' to 1 and 'Not supported' to 0
pset2_data$choice <- ifelse(pset2_data$choice == "Supported", 1, 0)
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

# Coefficients:
#                    Estimate   Std. Error    z value     Pr(>|z|)    
#(Intercept)        -0.005665   0.021971      -0.258      0.796517    
#countries.L         0.458452   0.038101      12.033      < 2e-16 ***
#  countries.Q      -0.009950   0.038056      -0.261      0.793741    
#sanctions.L        -0.276332   0.043925      -6.291      3.15e-10 ***
#  sanctions.Q      -0.181086   0.043963      -4.119      3.80e-05 ***
#  sanctions.C       0.150207   0.043992      3.414       0.000639 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 11783  on 8499  degrees of freedom
#Residual deviance: 11568  on 8494  degrees of freedom
#AIC: 11580

#Number of Fisher Scoring iterations: 4
#################################################################################
#####
# In a logistic regression, the response being modeled is the log(odds) that Y=1. The regression coefficients gives the
#change in log(odds) in the response for a unit change in the predictor variables, holding all other predictor variables
#constant.Since log(odds) are difficult to interpret, I have decided to exponentiate them (i.e., estimated parameters) 
#to put the results on odd scale:

exp(coef(fit_myFull_logit_supporting_policy))
## Answer:
# (Intercept)     countries.L    countries.Q   sanctions.L   sanctions.Q   sanctions.C 
# 0.9943507        1.5816245      0.9900994     0.7585609     0.8343637     1.1620743 
#######################################################################
## I would like to explore the dataset in more detail and I am interested in finding the 95% confidence interval for
# estimated coefficients/parameters and coefficients:

# An option for making a data.frame of confidence intervals and coefficients
confMod1 <- data.frame(cbind(lower = exp(confint(fit_myFull_logit_supporting_policy)[,1]), 
                            coefs = exp(coef(fit_myFull_logit_supporting_policy)), 
                            upper = exp(confint(fit_myFull_logit_supporting_policy)[,2])))
confMod1

### Answer:
#                 lower         coefs        upper
#(Intercept)    0.9524387     0.9943507     1.0381058
#countries.L    1.4679656     1.5816245     1.7044456
#countries.Q    0.9189295     0.9900994     1.0667733
#sanctions.L    0.6959419     0.7585609     0.8267142
#sanctions.Q     0.7654570    0.8343637     0.9094241
#sanctions.C    1.0661324     1.1620743     1.2667989
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
####################################################################
#####################################################################################################
#### Question 2:
#Plotting an interactive graph by using the packages: effects and plotly
###
##### (2a)
##
# The coefficient for "sanctions" 15% is 0.4803396 and the coefficient for "sanctions" 5% is 0.5612953
#####
# Calculate the odds ratio for sanctions 15% compared to sanctions 5%

odds_ratio_15_5 <- exp(0.4803396 - 0.5612953)
odds_ratio_15_5 # 0.9222345
###########################
######################
# Obtain effects of predictors

effects_plot <- allEffects(fit_myFull_logit_supporting_policy)
summary(effects_plot)
# Answer/Output:
#
#model: choice ~ countries + sanctions

#countries effect
#countries
#20 of 192      80 of 192     160 of 192 
#0.4172845      0.5006076     0.5779635 

#Lower 95 Percent Confidence Limits
#countries
#20 of 192      80 of 192     160 of 192 
#0.3992755      0.4819909     0.5596200 

#Upper 95 Percent Confidence Limits
#countries
#20 of 192    80 of 192     160 of 192 
#0.4355169    0.5192225     0.5960945 

#sanctions effect
#sanctions
# None        5%            15%         20% 
#0.5136380  0.5612953   0.4803396     0.4380700 

#Lower 95 Percent Confidence Limits
#sanctions
# None             5%         15%         20% 
#0.4921582    0.5399365     0.4588899   0.4169742 

#Upper 95 Percent Confidence Limits
#sanctions
# None          5%        15%         20% 
#0.5350674  0.5824294   0.5018621   0.4593921
#######################################################################################################
###################################################################################################
#(2b)
## Given values
countries_value <- 0.5006076  # Estimated probability for 80 of 192 countries
sanctions_value <- 0.5136380   # Estimated probability for no sanctions

# Calculate the estimated probability
probability <- countries_value * sanctions_value

# Output the result
probability # Answer:  0.2571311
############################################################################################
#(2c)
fit_interaction_logit <- glm(choice ~ countries * sanctions, family = binomial(link = "logit"), data = pset2_data)
summary(fit_interaction_logit)
########
#### Answer/Output:
#Call:
#glm(formula = choice ~ countries * sanctions, family = binomial(link = "logit"), 
#    data = pset2_data)

#Coefficients:
#                         Estimate    Std. Error    z value     Pr(>|z|)    
#(Intercept)             -0.003809      0.022006    -0.173      0.862583    
#countries.L              0.457140      0.038078    12.005      < 2e-16 ***
#  countries.Q             -0.011167    0.038152    -0.293      0.769750    
#sanctions.L             -0.274221      0.043953    -6.239      4.41e-10 ***
#  sanctions.Q             -0.182289    0.044011    -4.142      3.45e-05 ***
#  sanctions.C              0.153245    0.044069    3.477       0.000506 ***
#  countries.L:sanctions.L -0.001754    0.076700    -0.023      0.981755    
#countries.Q:sanctions.L  0.133840      0.075554    1.771       0.076484 .  
#countries.L:sanctions.Q -0.007622      0.076156    -0.100      0.920278    
#countries.Q:sanctions.Q  0.093425      0.076303     1.224      0.220806    
#countries.L:sanctions.C  0.095197      0.075608    1.259       0.208001    
#countries.Q:sanctions.C  0.010449      0.077046    0.136       0.892123    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 11783  on 8499  degrees of freedom
#Residual deviance: 11562  on 8488  degrees of freedom
#AIC: 11586
#
#Number of Fisher Scoring iterations: 4
####################################################################################################
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

