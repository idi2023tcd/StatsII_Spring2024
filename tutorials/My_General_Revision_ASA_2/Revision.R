#
#My Exercises- APPLIED STATISTICS II INCLUDING ALL LECTURES AND TUTORIALS REVISIONS

View(mtcars)
dim(mtcars) # rows/observations: 32; columns/variables: 11
names(mtcars)# "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear" "carb"
getwd()
#######
ggplot(mtcars, aes(x=mpg, y=disp))+
  geom_point()+
  geom_smooth(method="lm")

pdf("test.pdf")
ggplot(mtcars, aes(x=mpg, y=disp))+
  geom_point()+
  geom_smooth(method="lm")
dev.off()
##################################
#### APPLIED STATISTICAL ANALYSIS II. WEEK 3: MAXIMUM LIKELIHOOD ESTIMATION #####
####
# SLIDE 16 ## POISSON EXAMPLE IN R ###
##
# POISSON LIKELIHOOD AND log-likelihood function.
llhfunc <- function(input_data, input_params, do.log=TRUE){
  
  #data
  d<- rep(input_data, length(input_params))
  #10 x values for given parameters (lambda for poisson)
  q.vec <-rep(length(y.vals), length(input_params))
  print(q.vec)
  
  #Create vector to feed into dpois
  p.vec <-rep(input_params, q.vec)
  d.mat <-matrix(dpois(d, p.vec, log=do.log), ncol=length(input_params))
  print(d.mat)
  
  if(do.log==TRUE){
    #function along columns (so, add)
      apply(d.mat, 2, sum)
    
  }
  else{
    # or multiply
    apply(d.mat, 2, prod)
  }
    
}
######
# SLIDE 17 # POISSON EXAMPLE IN R
##
y.vals <- c(1, 3, 1, 5, 2, 6, 8, 11, 0, 0)
llhfunc (y.vals, c(4, 30))
###
# Answer:
#[1] 10 10
#      [,1]             [,2]
#[#1,] -2.613706       -26.59880
#[2,] -1.632876       -21.58817
#[3,] -2.613706       -26.59880
#[4,] -1.856020       -17.78150
#[5,] -1.920558       -23.89075
#[6,] -2.261485       -16.17207
#[7,] -3.514248       -13.39502
#[8,] -6.253070       -10.08914
#[9,] -4.000000       -30.00000
#[10,] -4.000000      -30.00000
#[1]  -30.66567       -216.11426
#########
# Slide 18 # Poisson Example in R
#Use R code function for optimizing, par=starting values control=list(fnscale=-1) indicates a maximization, 
#bfgs=Quasi-Newton algorithm.
##
mle <- optim(par=1, fn=llhfunc, X=y.vals, control=list(fnscale=-1), method="BFGS")

#Make a pretty graph of log and non-log versions
ruler <- seq(from=0.01, to=20, by=0.01)
poison.ll <-llhfunc(y.vals, ruler)
poison.l <- llhfunc(y.vals, ruler, do.log=FALSE)
plot(ruler, poison.l, col="purple", type="l", xaxt="n")
text(mean(ruler), mean(poison.l), "POISSON LIKELIHOOD FUNCTION")
plot(ruler, poison.ll, col="red", type="l")
text(mean(ruler), mean(poison.ll)/2, "POISON LOG-LIKELIHOOD FUNCTION")
#########################################
###########
### Slide 36 Ex: OLS and MLE
##
beta <-2.7
sigma <- sqrt(1.3)
ex_data <- data.frame(x=runif(200, 1 ,10))
ex_data$y <- 0 + beta*ex_data$x + rnorm(200, 0, sigma)
#pdf("../graphics/normal_mle_ex.pdf", width=9)
plot(ex_data$x, ex_data$y, xlab="X", ylab="Y", main="Normal MLE")
##################
# Slide 39  Ex:OLS and MLE
##
# We code this as a function in R with theta=(beta, sigma)
##
linear.link <- function(theta, y, X){
  n <- nrow(X)
  k <- ncol(X)
  beta <- theta[1:k]
  sigma2 <- theta[k+1]^2
  e <- y - X %*% beta
  logL <- -0.5*n*log(2*pi)- 0.5*n*log(sigma) - ((t(e)%*%e)/(2*sigma2))
  return -logL
}
####
#Slide 40 Ex: Normal MLE
##
#This function, at different values of beta and sigma, creates surface.
##
surface <- list()
k <- 0
for(beta in seq(0, 5, 0.1)){
  for(sigma in seq(0.1, 5, 0.1)){
    k <- k+1
    logL <- linear.link(theta=c(0, beta, sigma), y=ex_data$y, X=cbind(1, ex_data$X))
    surface [[k]]<- data.frame(beta=beta, sigma=sigma, logL=-logL)
  }
}
surface <- do.call(rbind, surface)
library(lattice)
wireframe(logL ~ beta*sigma, surface, shade=TRUE)
##################################################
####################################################### This is the Correct Code
linear.link <- function(theta, y, X) {
  n <- nrow(X)
  k <- ncol(X)
  beta <- theta[1:k]
  sigma2 <- theta[k + 1]^2
  e <- y - X %*% beta
  logL <- -0.5 * n * log(2 * pi) - 0.5 * n * log(sigma2) - (t(e) %*% e) / (2 * sigma2)
  return(-logL)  # Corrected: Return the negative log-likelihood
}

# Generate surface data
surface <- list()
k <- 0
for (beta in seq(0, 5, 0.1)) {
  for (sigma in seq(0.1, 5, 0.1)) {
    k <- k + 1
    logL <- linear.link(theta = c(0, beta, sigma), y = ex_data$y, X = cbind(1, ex_data$X))
    surface[[k]] <- data.frame(beta = beta, sigma = sigma, logL = -logL)  # Corrected: Return the negative log-likelihood
  }
}
surface <- do.call(rbind, surface)

# Plot the wireframe
library(lattice)
wireframe(logL ~ beta * sigma, surface, shade = TRUE, col=2:3)
###################################################
####
## Slide 42 EX: Normal MLE in R ###
##
#We can find parameters that specify this point with R's built-in optimization commands.
##
linear.MLE <- optim (fn=linear.link, par=c(1, 1, 1), hessian=TRUE, y=ex_data$y, X=cbind(1, ex_data$X), method="BFGS")
linear.MLE$par # Answer: 13.90362 1888.12020    1.00000
######
#This is reasonably close to uncovering true parameters, beta=2.7; sigma=sqrt(1.3)=1.14017543
#################################################################################
####
# Slide 43 : Ex: Comparing MLE to lm() in R
#
# Ordinary Least Square (OLS) is equivalent to Maximum Likelihood (ML) for a linear model, so it makes sense that lm() 
#would give us same answer.
# Note that sigma^2 is used in determining the standard errors.
############################################################

summary(lm(y ~ x, data=ex_data))
# Answer:
#Call:
#lm(formula = y ~ x, data = ex_data)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.3779 -0.6902 -0.0224  0.6281  3.0919 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.19011    0.16532   -1.15    0.252    
# x            2.75191    0.02772   99.29   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.093 on 198 degrees of freedom
#Multiple R-squared:  0.9803,	Adjusted R-squared:  0.9802 
##F-statistic:  9858 on 1 and 198 DF,  p-value: < 2.2e-16
##########################################################
######################################################################################
### WEEK 4: 
# To be continued

#########################
## SLIDE 31. Logit Example: Trump vs Age #####
###
trump_data <- read.csv("C:/NewGithubFolder/StatsII_Spring2024/datasets/Trump_select.csv")
head(trump_data)
dim(trump_data) # Rows/Observations=742; Columns/Variables=11
names(trump_data)# "age" ;"age_factor" ;"race" ;"educ"; "income" ;"ideology_factor";"gender_factor" ;"SelectTrump";"condition"; "openResponse"; "codeCorrect" 
################
# Let's fit a logit model for selecting new story about Trump (SelectTrump) with age. 
# Run logit model with y=SelectTrump - is a binary variable (0; 1) and x=age 
######################
#
logit_base <- glm(SelectTrump ~ age, data=trump_data, family=binomial(link="logit"))
summary(logit_base)
### Answer:
#Call:
#glm(formula = SelectTrump ~ age, family = binomial(link = "logit"), 
#    data = trump_data)
#
#Coefficients:
#              Estimate       Std. Error    z value     Pr(>|z|)    
#(Intercept)    -1.662736     0.237405      -7.004      2.49e-12 ***
#  age          0.018008      0.004739      3.800       0.000145 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 910.55  on 741  degrees of freedom
#Residual deviance: 895.89  on 740  degrees of freedom
#AIC: 899.89
#
#Number of Fisher Scoring iterations: 4
####### To be continued
######################
# Book Title: Data Analysis and graphics with R and Tidyverse R in Action Third Edition ## Robert I. Kabacoff
###
#Page 487 19.5 Making Graphs Interactive ###
##
#Listing 19.16 Converting a ggplot2 graph to an interactive plotly graph
###############################################################################################################
################################################################################
####### Book Title: DATA ANALYSIS AND GRAPHICS WIT R AND TIDYVERSE R IN ACTION 3RD 3DITION ## AUTHOR: ROBERT I. KABACOFF
####
# PAGE 320 # 13.2 LOGISTIC REGRESSION 
##
# Logistic regression is useful when you are predicting a binary outcomes from a set of continuous and/or categorical predictor 
#variables.To demonstrate this, let's explore the data on infidelity contained in the data frame Affairs, provided with the 
#AER package. Be sure to download and install the package (using install.packages("AER")) before first use. 
#####
# The infidelity data, known as Fair's Affairs, is based on a cross-sectional survey conducted by Psychology Today in 1969 and 
#is described in Greene(2003) and Fair(1978).
####
# It contain 9 variables collected on 601 participants and includes how often the respondent engaged in extramarital sexual 
#intercourse during the past year, as well as their gender, age, years marries, whether they had children, their
#religiousness (on a 5-point scale from 1=anti to 5=very), education, occupation (Hollingshead 7-point classification with 
#reverse numbering), and a numeric self-rating of their marriage (from 1=very unhappy to 5=very happy).
###
install.packages("AER")
library("AER")
##
data(Affairs, package="AER")
summary(Affairs)
##
View(Affairs)
dim(Affairs) # Row/observations=601, columns/variables=9
names(Affairs) # "affairs", "gender", "age" , "yearsmarried", "children", "religiousness", "education","occupation" ,"rating"   
###
# 1. Descriptive statistics:
#
summary(Affairs)
## Answer:
#    affairs          gender         age         yearsmarried    children  religiousness     education       occupation   
#Min.   : 0.000   female:315   Min.   :17.50   Min.   : 0.125   no :171   Min.   :1.000   Min.   : 9.00   Min.   :1.000  
#1st Qu.: 0.000   male  :286   1st Qu.:27.00   1st Qu.: 4.000   yes:430   1st Qu.:2.000   1st Qu.:14.00   1st Qu.:3.000  
#Median : 0.000                Median :32.00   Median : 7.000             Median :3.000   Median :16.00   Median :5.000  
#Mean   : 1.456                Mean   :32.49   Mean   : 8.178             Mean   :3.116   Mean   :16.17   Mean   :4.195  
#3rd Qu.: 0.000                3rd Qu.:37.00   3rd Qu.:15.000             3rd Qu.:4.000   3rd Qu.:18.00   3rd Qu.:6.000  
#Max.   :12.000                Max.   :57.00   Max.   :15.000             Max.   :5.000   Max.   :20.00   Max.   :7.000  
#rating     
#Min.   :1.000  
#1st Qu.:3.000  
#Median :4.000  
#Mean   :3.932  
#3rd Qu.:5.000  
#Max.   :5.000  
#######
table(Affairs$affairs)
## Answer:
# 0   1   2   3   7  12 
#451  34  17  19  42  38 
###
round(prop.table(table(Affairs$affairs)), digits=2)*100 # Values in percentage
# Answer:
# 0       1     2     3   7   12 
# 75%     6%    3%   3%   7%   6%
####
#From these statistics, you can see that 52% of the respondents were female, 72% had children, and the median age for the
#sample was 32 years. With regard to the response variable, 75% of th respondents reported not engaging in an infidelity in the
#year (451/601=0.75=75%). The largest number of encounters reported was [ 12 or (6%), look at the summary table for affairs].
####
#The number of the indiscretions was recorded, your interest here is in the binary 
#outcome(had and affair/didn't have an affairs).
##
#You can transform affairs into a dichotomous factor called ynaffairs with the following code
####
Affairs$ynaffairs <- ifelse(Affairs$affairs > 0, 1, 0)
Affairs$ynaffairs <- factor(Affairs$ynaffairs, levels=c(0,1), labels=c("No", "Yes"))
table(Affairs$ynaffairs)
### Answer:
#   No      Yes 
#   451     150 
###############
# This dichotomous factor can now be used as the outcome variable in a logistic regression model:
##
fit.full <- glm(ynaffairs ~ gender + age + yearsmarried + children + religiousness + education + occupation + rating, 
                data=Affairs, family=binomial(link="logit"))
summary(fit.full)
############# Answer/Output
##
#Call:
#glm(formula = ynaffairs ~ gender + age + yearsmarried + children + 
#      religiousness + education + occupation + rating, family = binomial(link = "logit"), 
#    data = Affairs)

#Coefficients:
#                 Estimate     Std. Error    z value    Pr(>|z|)    
#(Intercept)         1.37726      0.88776      1.551      0.120807    
#gendermale          0.28029      0.23909      1.172      0.241083    
#age                -0.04426      0.01825     -2.425      0.015301 *  
#  yearsmarried      0.09477      0.03221      2.942      0.003262 ** 
#  childrenyes       0.39767      0.29151      1.364      0.172508    
#religiousness      -0.32472      0.08975     -3.618      0.000297 ***
#  education         0.02105      0.05051      0.417      0.676851    
#occupation          0.03092      0.07178      0.431      0.666630    
#rating             -0.46845      0.09091      -5.153      2.56e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 675.38  on 600  degrees of freedom
#Residual deviance: 609.51  on 592  degrees of freedom
#AIC: 627.51

#Number of Fisher Scoring iterations: 4
######################################
#PAGE 321 # FROM THE P-VALUES FOR THE REGRESSION COEFFICIENTS (the last column), you can see that gender, presence of children
#education, and occupation may not make a significant contribution to the equation(you can't reject the hypothesis that the
# parameters are 0).
#Let's fit a 2nd equation without them and test whether this reduced model fits the data as well:
##
fit.reduced <- glm(ynaffairs ~ age + yearsmarried + religiousness + rating, family = binomial(link = "logit"),
                   data = Affairs)
summary(fit.reduced)
#
########## Answer:
#Call:
#glm(formula = ynaffairs ~ age + yearsmarried + religiousness + 
#      rating, family = binomial(link = "logit"), data = Affairs)

#Coefficients:
#                Estimate Std. Error    z value    Pr(>|z|)    
#(Intercept)    1.93083    0.61032      3.164      0.001558 ** 
#  age           -0.03527    0.01736    -2.032    0.042127 *  
#  yearsmarried   0.10062    0.02921    3.445    0.000571 ***
#  religiousness -0.32902    0.08945    -3.678    0.000235 ***
#  rating        -0.46136    0.08884    -5.193    2.06e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 675.38  on 600  degrees of freedom
#Residual deviance: 615.36  on 596  degrees of freedom
#AIC: 625.36
#
#Number of Fisher Scoring iterations: 4
###
#Each regression coefficient in the reduced model is statistically significant(p<0.05)
              
# TO BE CONTINUED FROM HERE          
##############################################################################################
######################################################
####### Book Title: DATA ANALYSIS AND GRAPHICS WIT R AND TIDYVERSE R IN ACTION 3RD 3DITION ## AUTHOR: ROBERT I. KABACOFF
####
# PAGE 412 # 17.2 LOGISTIC REGRESSION 
#####################
#LOGISTICS REGRESSION-is a type of generalized linear model that is often used to predict a binary outcome from set of
#numeric variables(see section 13.2 for details). 
#The glm() function in the base R installation is used for fitting the model.
#Categorical predictors (factors) are automatically replaced with a set of dummy coded variables. 
# All the predictors in the Wisconsin Breast Cancer data are 
#numeric, so dummy coding is unnecessary. The next listing provides regression analysis of the data.
############################################################################
# Load the dataset from the UCI Machine Learning Repository
# Install the "mlbench" package if you haven't already
install.packages("mlbench")

# Load the "mlbench" package
library(mlbench)

# Load the Wisconsin Breast Cancer dataset
data("BreastCancer")

# Assign the dataset to a variable named "train"
train <- BreastCancer
train$Class <- ifelse(train$Class == "benign", 1, 0)
# Now you can work with the "train" dataset in RStudio
View(train)
head(train)
dim(train) # Rows=699; Columns=11
names(train)#"Id"; "Cl.thickness"; "Cell.size"; "Cell.shape"; "Marg.adhesion"; "Epith.c.size";"Bare.nuclei" ;"Bl.cromatin"; "Normal.nucleoli" ;"Mitoses";"Class" 
##
# Fit the logistic regression by choosing the class as response/binary variable
##
fit.logit <- glm(Class ~., family=binomial(link="logit"), data=train)
summary(fit.logit)
###################################################################################################################
###############################################################################################################
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
###
# Assuming 'choice' column contains logical values Not supported and Supported
# Convert 'Supported' to 1 and 'Not supported' to 0
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
####################################################################################

#Fit_myFull_logit_supporting_policy <- glm (choice ~ countries + sanctions, family=binomial(link="logit"), data=pset2_data)
#summary(Fit_myFull_logit_supporting_policy)

##############################
## My Method:
fit_myFull_logit_supporting_policy <- glm (choice ~ countries + sanctions, family=binomial(link="logit"), data=pset2_data)
summary(fit_myFull_logit_supporting_policy)
####
### Answer/ Output 
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
################################################################################################
########################################################################################################
#2nd Method: for plotting an interactive graph
###
# Load the necessary libraries
install.packages("effects")
install.packages("plotly")
library(effects)
library(plotly)

# Fit logistic regression model (replace with your actual model)
fit_myFull_logit_supporting_policy <- glm(choice ~ countries + sanctions, 
                                          family = binomial(link = "logit"), 
                                          data = pset2_data)

# Obtain effects of predictors
effects_plot <- allEffects(fit_myFull_logit_supporting_policy)
summary(effects_plot)
####
# Convert effects plot to a ggplot object
ggplot_obj <- plot(effects_plot)

# Convert ggplot object to plotly object
plotly_obj <- ggplotly(ggplot_obj)

# Display the interactive plot
plotly_obj
##################################################
## 2nd Method:
##
# Fit logistic regression model
fit_myFull_logit_supporting_policy <- glm(choice ~ countries + sanctions,
                                          family = binomial(link = "logit"),
                                          data = pset2_data)

# Summary of the logistic regression model
summary(fit_myFull_logit_supporting_policy)
###
# Answer of Summary Output:

#Call:
#glm(formula = choice ~ countries + sanctions, family = binomial(link = "logit"), 
#    data = pset2_data)

#Coefficients:
#                 Estimate      Std. Error      z value      Pr(>|z|)    
#(Intercept)      -0.005665     0.021971        -0.258      0.796517    
#countries.L       0.458452     0.038101        12.033      < 2e-16 ***
#  countries.Q     -0.009950    0.038056        -0.261       0.793741    
#sanctions.L      -0.276332     0.043925        -6.291      3.15e-10 ***
#  sanctions.Q    -0.181086     0.043963        -4.119      3.80e-05 ***
#  sanctions.C      0.150207    0.043992          3.414     0.000639 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 11783  on 8499  degrees of freedom
#Residual deviance: 11568  on 8494  degrees of freedom
#AIC: 11580
#
#Number of Fisher Scoring iterations: 4
######################################################################
# MY METHOD BASED IN TUTORIAL 3
# Compute likelihood ratio-test of the omnibus hypothesis that none of the explanatory variables influence 'choice'

nullMod1 <- glm(choice ~ 1, family=binomial(link="logit"), data=pset2_data)
summary(nullMod1)
### Answer:
#Call:
#glm(formula = choice ~ 1, family = binomial(link = "logit"), 
#    data = pset2_data)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept) -0.006588   0.021693  -0.304    0.761

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 11783  on 8499  degrees of freedom
#Residual deviance: 11783  on 8499  degrees of freedom
#AIC: 11785

#Number of Fisher Scoring iterations: 3
##############################################################################
# RUN THE anova() test on the model compared to the null model (nullMod1)

anova(nullMod1, fit_myFull_logit_supporting_policy, test="Chisq")
anova(nullMod1, fit_myFull_logit_supporting_policy, test="LRT")

#####
#### Now I am interested in Calculating the Global Null Hypothesis and p-value.
# Perform likelihood ratio test

my_lr_test <- anova(fit_myFull_logit_supporting_policy, test = "Chisq")
print(my_lr_test)
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
##########################################################################
#######
# Extract p-value for the global null hypothesis

my_global_null_p_value <- my_lr_test$"Pr(>Chi)"
print(my_global_null_p_value) 
#####
# Plotting the logistic regression model
# You can use the 'effects' package for visualizing the effects of predictors
install.packages("effects")
library(effects)

# Plot the effects of the 'countries' variable
plot(allEffects(fit_myFull_logit_supporting_policy))
#################################################################################
##################################################################
# Question 2
###
#(2a)
#(2b)
## Given values
countries_value <- 0.5006076  # Estimated probability for 80 of 192 countries
sanctions_value <- 0.5136380   # Estimated probability for no sanctions

# Calculate the estimated probability
probability <- countries_value * sanctions_value

# Output the result
probability # Answer:  0.2571311
