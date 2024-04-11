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
####### Book Title: DATA ANALYSIS AND GRAPHICS WITH R AND TIDYVERSE R IN ACTION 3RD 3DITION ## AUTHOR: ROBERT I. KABACOFF
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
#outcome(had an affair/didn't have an affairs).
##
#You can transform affairs into a dichotomous factor called ynaffairs with the following code
####
Affairs$ynaffairs <- ifelse(Affairs$affairs > 0, 1, 0)
Affairs$ynaffairs <- factor(Affairs$ynaffairs, levels=c(0,1), labels=c("No", "Yes"))
table(Affairs$ynaffairs)
### Answer:
#   No      Yes 
#   451     150 
##############################
head(Affairs)
###
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
## 1st. Method: fit.reduced

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
####
# 2nd Method for fit.reduced - This method will reduced the full.fit by choosing only those variables that have significant p-vales.
fit.reduced_2 <- step(fit.full)
summary(fit.reduced_2 )

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
# First method: Plot diagnostics to check for model adequacy:
##
plot(predict(fit_myFull_logit_supporting_policy, type="response"),
     residuals(fit_myFull_logit_supporting_policy, type="deviance"))

##############
######## Second method: Plot diagnostics to check for model adequacy:
###
plot(hatvalues(fit_myFull_logit_supporting_policy))
plot(rstudent(fit_myFull_logit_supporting_policy))
plot(cooks.distance(fit_myFull_logit_supporting_policy))
#######################
## Third method: Plot diagnostics to check for model adequacy:
install.packages("car")
library("car")
##
influencePlot(fit_myFull_logit_supporting_policy) # This is my preferable method:
## Answer:
#       StudRes          Hat           CookD
#193 -1.288055     0.0007387185    0.0001592043
#201  1.071259     0.0007387185    0.0000954958
#265  1.430250     0.0006782996    0.0002014243
#737  1.430250    0.0006782996     0.0002014243

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
##################################################################################################
###############################################################################################################
### MY EXAMPLE: WHY WHEN I USE THE FOLLOWING FORMULA:
#Could you please tell why I run the code in RStudio using the formula:  my_fit_lm <- lm( y ~ x1 + x2 + x3, data=my_data) , 
#summary(my_fit_lm) and
#my_fit_glm <- glm( y ~ x1 + x2 + x3, family=gaussian(link="identity"), data=my_fit_glm), summary(my_fit_glm)). 
#Both showing the same output after running the R code in the RStudio. Please tell why this happening? Provide a concrete 
#example.
####
# Generate some example data
set.seed(123)
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
y <- 2*x1 + 3*x2 - 1.5*x3 + rnorm(100)

# Create a data frame
my_data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
head(my_data)
## Answer:
#         y          x1          x2         x3
#1 -7.265629 -0.56047565 -0.71040656  2.1988103
#2 -2.411012 -0.23017749  0.25688371  1.3124130
#3  1.836520  1.55870831 -0.24669188 -0.2651451
#4 -2.768915  0.07050839 -0.34754260  0.5431941
#5 -2.411930  0.12928774 -0.95161857 -0.4143399
#6  4.340596  1.71506499 -0.04502772 -0.4762469
######
#
# Fit linear regression models using lm() and glm()
my_fit_lm <- lm(y ~ x1 + x2 + x3, data = my_data)
my_fit_glm <- glm(y ~ x1 + x2 + x3, family = gaussian(link = "identity"), data = my_data)

# Display summaries
summary(my_fit_lm)
### Answer:
#Call:
#lm(formula = y ~ x1 + x2 + x3, data = my_data)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2.49138 -0.65392  0.05664  0.67033  2.53210 
#
#Coefficients:
#              Estimate  Std. Error    t value    Pr(>|t|)    
#(Intercept) -0.01933    0.10734       -0.18      0.858    
#x1           1.94455    0.11688       16.64     <2e-16 ***
#  x2           3.04622    0.10946     27.83     <2e-16 ***
#  x3          -1.55739    0.11223     -13.88    <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.052 on 96 degrees of freedom
#Multiple R-squared:  0.9284,	Adjusted R-squared:  0.9262 
#F-statistic: 415.2 on 3 and 96 DF,  p-value: < 2.2e-16
###
summary(my_fit_glm)
# Answer:
#Call:
#glm(formula = y ~ x1 + x2 + x3, family = gaussian(link = "identity"), 
#    data = my_data)

#Coefficients:
#              Estimate  Std. Error   t value     Pr(>|t|)    
#(Intercept) -0.01933    0.10734      -0.18       0.858    
#x1           1.94455    0.11688      16.64      <2e-16 ***
#  x2           3.04622    0.10946    27.83      <2e-16 ***
#  x3          -1.55739    0.11223    -13.88     <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 1.105679)
#
#Null deviance: 1483.22  on 99  degrees of freedom
#Residual deviance:  106.15  on 96  degrees of freedom
#AIC: 299.75
#
#Number of Fisher Scoring iterations: 2
########################################################################################################################
##################################################################################################################
### LECTURE - WEEK 8 : MULTINOMINAL LOGISTIC REGRESSION ### 
###
install.packages("nnet")
library("nnet")
####
diocese_data=read.csv("C:/NewGithubFolder/StatsII_Spring2024/datasets/diocese_data.csv", stringsAsFactors=F)
diocese_data
dim(diocese_data) # rows/observations=2808; columns/variables=50
names(diocese_data)
###
# Run the base MULTINOMINAL logit
multinom_model1 <- multinom(state3 ~ cathpct_lag + hogCatholic_lag + propenpt_lag + country, data=diocese_data)
summary(multinom_model1)
#######################################################################################################################
###
######
## My Revision on Time Series
install.packages(c("xts", "forecast", "tseries", "directlabels"))
library("xts") 
library("forecast") 
library("tseries")
library("directlabels")
######
# Page 358 # 15.1 Creating a time-series object in R 
###
## Time-Series Objects in R
##
install.packages(c("tsibble", "timeSeries", "irts", "tis"))
library("tsibble")
library("timeSeries")
library("irts")
library("tis")
##############################
## Create a time series and plot it

profit <-c(2300, 7500, 3250, 1500, 980, 323, 890, 2890, 7890, 5330,9000,4500,6201, 1500, 4321, 5675, 1998, 1456,8965,2789,6789,
           6422,8970, 3489)
length(profit)
date<-seq(from=as.Date("2020/3/1"), to=as.Date("2022/2/1"), by="month")
length(date)
##
my_data.xts<-xts(profit,date)
class(my_data.xts)
#####
# 1st method: Default plotting using autoplot() function
autoplot(my_data.xts)
################################################################################################################################
############################################################################################################################
### PROBLEM SET III/ APPLIED STATISTICAL ANALYSIS II
##
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
install.packages(c("dfidx", "Rtools"))
library("dfidx")
library("Rtools")
library("nnet")
library("MASS")
library("mlogit") # mlogit package is used to perform Multinomial Logistic Regression Model 
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
##
gdp_data <- read.csv("C:/NewGithubFolder/StatsII_Spring2024/datasets/gdpChange.csv", stringsAsFactors = F)# To Load dataset into R
head(gdp_data) # This line of the code provide the first 6 observations in the dataset
dim(gdp_data) # Rows/Observations=3721; Columns/Variables=12
names(gdp_data)# "X" ;"COUNTRY"; "CTYNAME"; "YEAR"; "GDPW"; "OIL"; "REG" ;"EDT";"GDPWlag";"GDPWdiff";"GDPWdifflag" ;"GDPWdifflag2" 
str(gdp_data) # # Check the structure of the dataset
#####################
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
#
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
#################################################################################################################################
#########################################
########################################################################################################################

################################################################################################################################
# PROBLEM SET III. QUESTION 2. Method: Using Jeffrey Student Slides
##################### Answer of Problem Set III;  Question 2
###########################################################

# load data
#mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")
###
mexico_elections <- read.csv("C:/NewGithubFolder/StatsII_Spring2024/datasets/MexicoMuniData.csv")
#
head(mexico_elections)
names(mexico_elections)#"MunicipCode" ; "pan.vote.09"; "marginality.06"; "PAN.governor.06"; "PAN.visits.06"; "competitive.district"
dim(mexico_elections) # Rows/Observations=2407; Columns/Variables=6
str(mexico_elections)
##
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
zeroinfl(formula = PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = mexico_elections, 
         dist = "poisson")

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
#   PAN.governor.06       -0.1749     0.4119  -0.425  0.67106
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Number of iterations in BFGS optimization: 28
# Log-likelihood: -600.4 on 8 Df
####
#(b)
exp(coef(poisson_reg.model))
# Answer
#(Intercept) competitive.district       marginality.06      PAN.governor.06 
#0.02214298           0.92186932           0.12491227           0.73228985 
#########
#(c)
# Coefficients from the Poisson regression model
coefficients <- coef(poisson_reg.model)

# Values for the hypothetical district
competitive_district <- 1  # competitive.district = 1
marginality <- 0           # marginality.06 = 0
pan_governor <- 1          # PAN.governor.06 = 1

# Calculating the linear predictor (eta) using the coefficients and values
eta <- coefficients["(Intercept)"] +
  coefficients["competitive.district"] * competitive_district +
  coefficients["marginality.06"] * marginality +
  coefficients["PAN.governor.06"] * pan_governor

eta
# Answer:
#(Intercept) 
#-4.203166
#
# Calculating the predicted mean using the Poisson link function
predicted_mean <- exp(eta)
predicted_mean # # Output the result
#(Intercept) 
#0.01494818 
############################################################################################################################
###########################################################################################################################
#################################################################################################################################
#PROBLEM SET III. Question 1: Part 1.
##2nd Method: Using Jeffrey Student Slides
###
##
gdp_data <- read.csv("C:/NewGithubFolder/StatsII_Spring2024/datasets/gdpChange.csv", stringsAsFactors = F)# To Load dataset into R
head(gdp_data) # This line of the code provide the first 6 observations in the dataset
dim(gdp_data) # Rows/Observations=3721; Columns/Variables=12
names(gdp_data)# "X" ;"COUNTRY"; "CTYNAME"; "YEAR"; "GDPW"; "OIL"; "REG" ;"EDT";"GDPWlag";"GDPWdiff";"GDPWdifflag" ;"GDPWdifflag2" 
str(gdp_data) # # Check the structure of the dataset
#####################
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
###
## Transform the REG (Regime) from binary (0;1) into the Categorical 0=Non-Democracy; 1=Democracy
gdp_data$REG <- factor(gdp_data$REG, levels=c(0, 1), labels=c("Non-Democracy", "Democracy"))
table(gdp_data$REG)
#### Answer/Output
#Non-Democracy     Democracy 
#   2227             1494 
########
#Transform the variable OIL from binary to Categorical Variable:
gdp_data$OIL <- ifelse(gdp_data$OIL ==1, "Exceed 50%", "Not Exceed 50%")
table(gdp_data$OIL)
######
#Exceed 50%    Not Exceed 50% 
# 374            3347
#####
ftable(xtabs(~REG + GDPWdiff_category + OIL, data=gdp_data))
# Answer:
#                                  OIL Exceed 50%  Not Exceed 50%
#  REG         GDPWdiff_category                              
#Non-Democracy 
#              negative             93              641
#              no change            0                14
#              positive             195             1284

#Democracy
#             negative              39              332
#             no change             0                2
#             positive              47              1074
####################
#################################
# Question 1: Part 1. Fitting a unordered multinomial logit with as the output and setting a reference category "no change"
#
# Run the base multinomial logit
Multinom_Model1 <- multinom(GDPWdiff_category ~ REG + OIL, data = gdp_data)
# weights:  12 (6 variable)
#initial  value 4087.936326 
#iter  10 value 2353.703961
#iter  20 value 2339.597837
#final  value 2339.365153 
#converged
####
summary(Multinom_Model1)
## Answer/Output
#Call:
#multinom(formula = GDPWdiff_category ~ REG + OIL, data = gdp_data)

#Coefficients:
#           (Intercept) REGDemocracy   OILNot Exceed 50%
#no change -10.8788883   -1.3536726         7.0777686
#positive    0.5208708    0.3898426         0.2074934

#Std. Errors:
#           (Intercept) REGDemocracy    OILNot Exceed 50%
#no change  21.6001369   0.75883270        21.6016306
#positive    0.1096375   0.07552351         0.1158108

#Residual Deviance: 4678.73 
#AIC: 4690.73 
#####################################################################################################################
#########################################################################################################################
# Lecture WEEK 9: Count Data - Poisson Regression ##
##
# R code:
elephant <- read.csv("C:/NewGithubFolder/StatsII_Spring2024/datasets/elephants.csv")
head(elephant)
names(elephant) # "Age"     "Matings"
dim(elephant) # Rows/Observations=42; Columns/Variables=2
str(elephant)
####
# Perform the scatterplot:
ggplot(elephant, aes(x=Age, y=Matings))+
  geom_point(size=2)+
  geom_jitter()+
  xlab("Elephant Age")+
  ylab(("Elephant Matings"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ggtitle("Scatterplot Elephant Age Vs Elephant Mates")
#####
# Plot the Scatterplot with fitting linear regression line:
##
ggplot(elephant, aes(x=Age, y=Matings))+
  geom_point(size=2)+
  geom_jitter()+
  geom_smooth(method="lm", formula=y~x, se=TRUE)+
  xlab("Elephant Age")+
  ylab(("Elephant Matings"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ggtitle("Scatterplot Elephant Age Vs Elephant Mates")
######
# Slide 10 # Zoology Example: Poisson/Count Regression Model
##
# 1st. Method:
elephant_poisson <- glm( Matings ~ Age, data=elephant, family=poisson)
summary(elephant_poisson )
### Answer/Output
#Call:
#glm(formula = Matings ~ Age, family = poisson, data = elephant)

#Coefficients:
#              Estimate     Std. Error   z value    Pr(>|z|)    
#(Intercept) -1.58201       0.54462       -2.905    0.00368 ** 
#  Age          0.06869     0.01375       4.997     5.81e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for poisson family taken to be 1)
#
#Null deviance: 75.372  on 40  degrees of freedom
#Residual deviance: 51.012  on 39  degrees of freedom
#AIC: 156.46

#Number of Fisher Scoring iterations: 5
############
# Predicted Equation: ln(lambda_i)= -1.58201 + 0.06869* X_1i
###
exp(coef(elephant_poisson))
## Answer:
# (Intercept)         Age 
# 0.2055619       1.0711071 
##############################################
#To Calculate the log likelihood and  BIC :
##
# Calculate log-likelihood
log_likelihood <- logLik(elephant_poisson) # Answer: 'log Lik.' -76.2289 (df=2)

# Number of parameters (including intercept)
num_parameters <- length(coef(elephant_poisson)) # Answer: 2

# Calculate BIC
bic <- -2 * log_likelihood + num_parameters * log(nrow(elephant))# Answer: 'log Lik.' 159.8849 (df=2)

# Output BIC and log-likelihood
cat("BIC:", bic, "\n")
cat("Log-Likelihood:", log_likelihood, "\n")
## Answer:
#BIC: 159.8849 
# Log-Likelihood: -76.2289 
############
# Slide 11 # Example : Poisson Regression Curve
###
# Get coefficients
coeffs <- coefficients(elephant_poisson)

# Sort x values
xvalues <- sort(elephant$Age)

# Calculate means
means <- exp(coeffs[1] + coeffs[2]*xvalues)

# Create scatterplot
plot(elephant$Age, elephant$Matings, xlab = "Age", ylab = "Matings", main = "Scatterplot with Curve and Dots")

# Add curve
lines(xvalues, means, lty = 2, col = "red")
##################
#
lambda30 <- exp(coeff[1] + coeff[2]*2) # Fix this code later
lambda30
####
#Slide 17: Getting Fitted Values in R
###
predicted_values<- cbind(predict(elephant_poisson, data.frame(Age=seq(25, 55, 5)), type="response", se.fit=TRUE),
                          data.frame(Age=seq(25, 55, 5)))
##
# Create lower and upper bounds for CIs
predicted_values$lowerBound <- predicted_values$fit - 1.96*predicted_values$se.fit
# Answer: 0.6627098 1.1196546 1.7844112 2.6025544 3.4061032 4.1438035 4.7681451
#
predicted_values$upperBound <- predicted_values$fit + 1.96*predicted_values$se.fit
# Answer: 1.627079  2.108541  2.766773  3.813810  5.639833  8.609364 13.211562
#
### SLIDE 20 # Over-dispersion test in R
# Check equal variance assumption
##
install.packages("AER")
library("AER")
#
dispersiontest(elephant_poisson)
# Answer:
#Overdispersion test

#data:  elephant_poisson
#z = 0.49631, p-value = 0.3098
#alternative hypothesis: true dispersion is greater than 1
#sample estimates:
#  dispersion 
#1.107951 
#####
# Slide 22  Zip Model in R
# R contributed package "pscl" contains the function zeroinfl:
install.packages("pscl")
library("pscl")
###
zeroinfl_poisson <- zeroinfl(Matings ~ Age, data=elephant, distribution="poisson")
summary(zeroinfl_poisson)
## Answer:
#Call:
#zeroinfl(formula = Matings ~ Age, data = elephant, distribution = "poisson")

#Pearson residuals:
#  Min       1Q   Median       3Q      Max 
#-1.98529 -0.80156 -0.07509  0.66683  2.54805 
#
#Count model coefficients (poisson with log link):
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.44743    0.55141  -2.625  0.00867 ** 
#  Age          0.06556    0.01392   4.709 2.49e-06 ***
#  
#  Zero-inflation model coefficients (binomial with logit link):
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)   301.13     463.48   0.650    0.516
#Age           -10.98      16.85  -0.652    0.515
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#
#Number of iterations in BFGS optimization: 568 
#Log-likelihood: -74.9 on 4 Df

##################################################################################################################################
##################################################################################################################################
#################################################### ##########################################################################
### REPLICATION PROJECT
`dataset (1)` <- readRDS("C:/Users/idiam/Downloads/dataset (1).rds")
`dataset (1)`
names(`dataset (1)`)
##########################################################################################################################
##########################################################################################################################
library(foreign)
library(dplyr)
library(tidyverse)
library(stargazer)
library(lubridate)
library(ggplot2)
library(xtable)
library(list)
library(data.table)
library(lmtest)
library(interplot)
library(pwr)
library(nnet)
library(scales)

#Loading data
pc <- read.csv("C:/NewGithubFolder/StatsII_Spring2024/replication/Ghana_replication/pettycorruption_data.csv")
head(pc)
dim(pc) # Rows/Observations=1268; Columns/Variables=43
names(pc)
str(pc)

#Recoding variables
# # Audio treatment variable
pc <- mutate(pc,
             audio_treat= ifelse(rand_number6<=0.25, 1, # female control 
                                 ifelse(rand_number6 >0.25 & rand_number6 <=0.5, 2, # female treatment
                                        ifelse(rand_number6 >0.5 & rand_number6 <=0.75, 3, # male control
                                               ifelse(rand_number6 >0.75, 4, NA))))) # male treatment
###
table(pc$gender)
##Answer:
#  1    2     3   4   5   6   7 
#703  241   106   94  42  27  55 

#Gender/experience variables
gender_group <- group_by(pc, audio_treat) 

is.numeric(pc$audio_treat)

#g_exper <- summarize(gender_group, count=n(), mean= mean(gender, na.rm=T), group_by = gender_group$audio_treat) 

g_exper <- aggregate(gender ~ audio_treat, data = gender_group, FUN = mean)

pc$audio_treat<-as.factor(pc$audio_treat)
treats<-as.data.frame(model.matrix( ~ audio_treat - 1, data=pc))
names(treats)<-c('female_nonet', 'female_net', 'male_nonet', 'male_net')
pc<-cbind(pc, treats)

########
######
# # Gender
pc$female<-ifelse(as.numeric(pc$audio_treat)<3,1,0)
pc$male <- ifelse(pc$male_net==1 | pc$male_nonet==1, 1, 0)
# # Experience
pc$network<-ifelse(as.numeric(pc$audio_treat)%in%c(2,4),1,0)
pc$nonetwork<-ifelse(pc$network==0,1,0)
#Experience paying bribes
pc$pay_bribe<-ifelse(is.na(pc$pay_bribe),0,pc$pay_bribe)
# As share of income
pc$pressure_share<-pc$financial_pressure/1200*100

# # # # # # # # # # #
# # # Experiment 1 - Analyses
# # # # # # # # # # #
mean(pc$corrupt) #5.92
reg1 <- lm(corrupt ~ female, data=pc)
reg2 <- lm(corrupt ~ nonetwork, data=pc)

reg1.1 <- lm(corrupt ~ female_nonet, data=pc[pc$female_nonet==1| pc$male_nonet==1,])
reg2.1 <- lm(corrupt ~ female_net, data=pc[pc$female_net==1| pc$male_net==1,])

# # # Experiment 2 - Analyses
#Reported in text
summary(pc$pressure_share)
t.test(pc$pressure_share[pc$john==1], pc$pressure_share[pc$john==0])

# # # # # # # # 
# # Figure 1 - The causal effects of (a) gender and (b) years of experience on petty corruption
# # # # # # # #
cfs<-c(coef(reg1)[2],coef(reg2)[2])
ses<-c(sqrt(vcov(reg1)[2,2]), sqrt(vcov(reg2)[2,2]))

dev.off()
ys<-c(-1,-2)
par(mar=c(0,0,5,0))
plot(c(-1,.25),c(-3,-.45), type = "n", axes=FALSE, xlab="", ylab="")
axis(3,c(-.6,-.4,-.2,0,.2))
segments(0,0,0,-2.5, lty=2)
text(-.63,-1,'Woman\n(baseline = Man)', cex=1,adj=1)
text(-.63,-2,'Average Experience\n(baseline = High Experience)', cex=1, adj=1)
points(cfs, ys, col=c("Gray35"), cex=1.4, pch=20)
segments(cfs-1.96*ses, ys, cfs+1.96*ses, col=c("Gray35"))
segments(cfs-1.65*ses, ys, cfs+1.65*ses, col=c("Gray35"),lwd=3)
mtext("Marginal effects of Gender/Experience", outer=F, line=3, cex=1, adj=.72)

# # # # # # # # 
# # Figure 2 - The causal effects of gender among bureaucrats with different levels of experience
# # # # # # # #
#UPDATE FIGURE 

cfs<-c(coef(reg1.1)[2],coef(reg2.1)[2])
ses<-c(sqrt(vcov(reg1.1)[2,2]), sqrt(vcov(reg2.1)[2,2]))

ys<-c(-1,-2)
par(mar=c(0,0,5,2))
plot(c(-1.3,.25),c(-3,-.5), type = "n", axes=FALSE, xlab="", ylab="")
axis(3,c(-.6,-.4,-.2,0,.2))
segments(0,0,0,-2.5, lty=2)
text(-.6,-1,'Woman avg. experience\n(baseline = Man avg. experience)', cex=1,adj=1)
text(-.6,-2,'Woman high experience\n(baseline = Man high experience)', cex=1, adj=1)
points(cfs, ys, col=c("Gray35"), cex=1.4, pch=20)
segments(cfs-1.96*ses, ys, cfs+1.96*ses, col=c("Gray35"))
segments(cfs-1.65*ses, ys, cfs+1.65*ses, col=c("Gray35"),lwd=3)
mtext("Marginal effects of Gender", outer=F, line=3, cex=1, adj=.79)


# # # # # # # # 
# # TABLE 0A.3 -- DESCRIPTIVES
# # # # # # # #
#Note: table produced by hand based with the data described below

## FULL SAMPLE
mean(pc$age) #Mean age
1-mean(pc$sex) #% Female
sum(prop.table(table(pc$education))[4:9]) # Primary completed or higher
prop.table(table(pc$married)) #Married
prop.table(table(pc$christ)) #%Christian

#Main Ethnic Groups
eth<-pc[,22:42]
names(eth)<-c('Ada','Akuapem','Akyem','Anlo','Ashanti',
              'Brong','Chamba','Dagara','Dagomba','Dangme',
              'Ewe','Fante','Frafra','Ga','Gonja',
              'Hausa','Kotokoli','Krobo','Kwahu','Mamprusi','Ghanaian')
eth$Akan<-ifelse(rowSums(eth[,c(2,3,5,6,12,19)])>0,1,0) #Akuapem, Akyem, Ashanti, Brong, Fante, Kwahu
eth$Ewe<-ifelse(rowSums(eth[,c(4,11)])>0,1,0) #Anlo, Ewe
eth$Northern<-ifelse(rowSums(eth[,c(7,8,9,13,15,16,20)])>0,1,0) #Chamba, Dagara, Dagomba,Frafra,Gonja,Hausa,Mamprusi 
eth$GaDangme<-ifelse(rowSums(eth[,c(1,10,14,18)])>0,1,0) # Ada,Dangme, Ga, Krobo
eth$Others<-ifelse(rowSums(eth[,c(21,17)])>0,1,0)

eth2<-eth[,c('Akan','Ewe','Northern','GaDangme','Others')]
sort(prop.table(colSums(eth2)), decreasing = T)

#Merging ethnic group categories
pc<-cbind(pc,eth2)


# # SUBSET: For central ghana subgroup
pc.cg<-pc[pc$region==2,]
mean(pc.cg$age) #Mean age
1-mean(pc.cg$sex) #% Female
sum(prop.table(table(pc.cg$education))[4:9]) # Primary completed or higher
prop.table(table(pc.cg$married)) 
prop.table(table(pc.cg$christ)) #%Christian
#Main Ethnic Groups
eth.cg<-pc.cg[,22:42]
names(eth.cg)<-c('Ada','Akuapem','Akyem','Anlo','Ashanti',
                 'Brong','Chamba','Dagara','Dagomba','Dangme',
                 'Ewe','Fante','Frafra','Ga','Gonja',
                 'Hausa','Kotokoli','Krobo','Kwahu','Mamprusi',
                 'Ghanaian')
sort(prop.table(colSums(eth.cg)))

eth.cg$Akan<-ifelse(rowSums(eth.cg[,c(2,3,5,6,12,19)])>0,1,0) #Akuapem, Akyem, Ashanti, Brong, Fante, Kwahu
eth.cg$Ewe<-ifelse(rowSums(eth.cg[,c(4,11)])>0,1,0) #Anlo, Ewe
eth.cg$Northern<-ifelse(rowSums(eth.cg[,c(7,8,9,13,15,16,20)])>0,1,0) #Chamba, Dagara, Dagomba,Frafra,Gonja,Hausa,Mamprusi 
eth.cg$GaDangme<-ifelse(rowSums(eth.cg[,c(1,10,14,18)])>0,1,0) # Ada,Dangme, Ga, Krobo
eth.cg$Others<-ifelse(rowSums(eth.cg[,c(21,17)])>0,1,0)

eth.cg2<-eth.cg[,c('Akan','Ewe','Northern','GaDangme','Others')]
sort(prop.table(colSums(eth.cg2)), decreasing = T)

# # SUBSET: For eastern ghana subgroup
pc.eg<-pc[pc$region==3,]
mean(pc.eg$age) #Mean age
1-mean(pc.eg$sex) #% Female
sum(prop.table(table(pc.eg$education))[4:9]) # Primary completed or higher
prop.table(table(pc.eg$married)) 
prop.table(table(pc.eg$christ)) #%Christian
#Main Ethnic Groups
eth.eg<-pc.eg[,22:42]
names(eth.eg)<-c('Ada','Akuapem','Akyem','Anlo','Ashanti',
                 'Brong','Chamba','Dagara','Dagomba','Dangme',
                 'Ewe','Fante','Frafra','Ga','Gonja',
                 'Hausa','Kotokoli','Krobo','Kwahu','Mamprusi',
                 'Ghanaian')
sort(prop.table(colSums(eth.eg)))

eth.eg$Akan<-ifelse(rowSums(eth.eg[,c(2,3,5,6,12,19)])>0,1,0) #Akuapem, Akyem, Ashanti, Brong, Fante, Kwahu
eth.eg$Ewe<-ifelse(rowSums(eth.eg[,c(4,11)])>0,1,0) #Anlo, Ewe
eth.eg$Northern<-ifelse(rowSums(eth.eg[,c(7,8,9,13,15,16,20)])>0,1,0) #Chamba, Dagara, Dagomba,Frafra,Gonja,Hausa,Mamprusi 
eth.eg$GaDangme<-ifelse(rowSums(eth.eg[,c(1,10,14,18)])>0,1,0) # Ada,Dangme, Ga, Krobo
eth.eg$Others<-ifelse(rowSums(eth.eg[,c(21,17)])>0,1,0)

eth.eg2<-eth.eg[,c('Akan','Ewe','Northern','GaDangme','Others')]
sort(prop.table(colSums(eth.eg2)), decreasing = T)

# # SUBSET: For greater accra subgroup
pc.ga<-pc[pc$region==1,]
mean(pc.ga$age) #Mean age
1-mean(pc.ga$sex) #% Female
sum(prop.table(table(pc.ga$education))[4:9]) # Primary completed or higher
prop.table(table(pc.ga$married)) 
prop.table(table(pc.ga$christ)) #%Christian
#Main Ethnic Groups
eth.ga<-pc.ga[,22:42]
names(eth.ga)<-c('Ada','Akuapem','Akyem','Anlo','Ashanti',
                 'Brong','Chamba','Dagara','Dagomba','Dangme',
                 'Ewe','Fante','Frafra','Ga','Gonja',
                 'Hausa','Kotokoli','Krobo','Kwahu','Mamprusi',
                 'Ghanaian')
eth.ga$Akan<-ifelse(rowSums(eth.ga[,c(2,3,5,6,12,19)])>0,1,0) #Akuapem, Akyem, Ashanti, Brong, Fante, Kwahu
eth.ga$Ewe<-ifelse(rowSums(eth.ga[,c(4,11)])>0,1,0) #Anlo, Ewe
eth.ga$Northern<-ifelse(rowSums(eth.ga[,c(7,8,9,13,15,16,20)])>0,1,0) #Chamba, Dagara, Dagomba,Frafra,Gonja,Hausa,Mamprusi 
eth.ga$GaDangme<-ifelse(rowSums(eth.ga[,c(1,10,14,18)])>0,1,0) # Ada,Dangme, Ga, Krobo
eth.ga$Others<-ifelse(rowSums(eth.ga[,c(21,17)])>0,1,0)

eth.ga2<-eth.ga[,c('Akan','Ewe','Northern','GaDangme','Others')]
sort(prop.table(colSums(eth.ga2)), decreasing = T)
##
sum(sort(prop.table(colSums(eth.ga2)), decreasing = T))

# # # # # # # # 
# # Figure 0A.1 -- Distributions of DVs
# # # # # # # #
dev.off()
#####
par(mar=c(4,4,2,2))
h <- hist(pc$corrupt, breaks=7, plot=FALSE)
h$counts = h$counts / sum(h$counts)
plot(h, ylim=c(0,1), xlab='Likelihood of bribe solicitation', main='')
lines(density(pc$corrupt), col='red', lwd=2)  # Add density line
round(prop.table(table(pc$corrupt)) * 100, 2)
###
summary(pc$corrupt)

dev.off()
par(mar=c(4,4,2,2))
h <- hist(pc$pressure_share, breaks=10,plot=F)
h$counts=h$counts/sum(h$counts)
plot(h,ylim=c(0,.5),xlab='Share of salary expected for family support',main='',
     xlim=c(0,100))
##
summary(pc$pressure_share)


# # # # # # # # 
# # Table 0A.4 -- Balance Tests
# # # # # # # #
age<-c(summary(pc$age[pc$male_net==1])[4],
       summary(pc$age[pc$female_net==1])[4],
       summary(pc$age[pc$male_nonet==1])[4],
       summary(pc$age[pc$female_nonet==1])[4],
       summary(aov(age~audio_treat,pc))[[1]][["Pr(>F)"]][1])

pc$sex_female<-abs(1-pc$sex)
female<-c(summary(pc$sex_female[pc$male_net==1])[4]*100,
          summary(pc$sex_female[pc$female_net==1])[4]*100,
          summary(pc$sex_female[pc$male_nonet==1])[4]*100,
          summary(pc$sex_female[pc$female_nonet==1])[4]*100,
          summary(aov(sex_female~audio_treat,pc))[[1]][["Pr(>F)"]][1])

pc$primary_more<-ifelse(pc$education>3,1,0)
educ<-c(summary(pc$primary_more[pc$male_net==1])[4]*100,
        summary(pc$primary_more[pc$female_net==1])[4]*100,
        summary(pc$primary_more[pc$male_nonet==1])[4]*100,
        summary(pc$primary_more[pc$female_nonet==1])[4]*100,
        summary(aov(primary_more~audio_treat,pc))[[1]][["Pr(>F)"]][1])

married<-c(summary(pc$married[pc$male_net==1])[4]*100,
           summary(pc$married[pc$female_net==1])[4]*100,
           summary(pc$married[pc$male_nonet==1])[4]*100,
           summary(pc$married[pc$female_nonet==1])[4]*100,
           summary(aov(married~audio_treat,pc))[[1]][["Pr(>F)"]][1])

christ<-c(summary(pc$christ[pc$male_net==1])[4]*100,
          summary(pc$christ[pc$female_net==1])[4]*100,
          summary(pc$christ[pc$male_nonet==1])[4]*100,
          summary(pc$christ[pc$female_nonet==1])[4]*100,
          summary(aov(christ~audio_treat,pc))[[1]][["Pr(>F)"]][1])

gaccra<-c(summary(pc$gaccra[pc$male_net==1])[4]*100,
          summary(pc$gaccra[pc$female_net==1])[4]*100,
          summary(pc$gaccra[pc$male_nonet==1])[4]*100,
          summary(pc$gaccra[pc$female_nonet==1])[4]*100,
          summary(aov(gaccra~audio_treat,pc))[[1]][["Pr(>F)"]][1])

pc$akan<-eth$Akan
akan<-c(summary(pc$akan[pc$male_net==1])[4]*100,
        summary(pc$akan[pc$female_net==1])[4]*100,
        summary(pc$akan[pc$male_nonet==1])[4]*100,
        summary(pc$akan[pc$female_nonet==1])[4]*100,
        summary(aov(akan~audio_treat,pc))[[1]][["Pr(>F)"]][1])

bals<-as.data.frame(rbind(age,female,educ,married,christ, 
                          gaccra, akan))
names(bals)<-c('Male $\\uparrow$ Experience','Female $\\uparrow$ Experience','Male Av. Experience', 'Female Av. Experience','$p$-value')
xtable(bals,digits = c(1,1,1,1,1,2))

# # Likelihood ratio tests
#Male v. Female
cases<-pc[,c('age','sex_female','primary_more','married','christ', 
             'gaccra', 'akan')]
m1<-glm(female~age+sex_female+primary_more+married+christ+gaccra+akan,family="binomial",pc)
m1n<-glm(female~1,family="binomial",pc[complete.cases(cases),])
lrtest(m1n,m1)

#Average v. High Experience
m2<-glm(network~age+sex_female+primary_more+married+christ+gaccra+akan,family="binomial",pc)
m2n<-glm(network~1,family="binomial",pc[complete.cases(cases),])
lrtest(m2n,m2)


# # # # # # # # 
# # Table 0A.5 -- Regression models used to build Figure 1
# # # # # # # #
stargazer(reg1,reg2,no.space = T, digits=3, 
          covariate.labels = c('Female bureaucrat','Experienced bureaucrat'))


# # # # # # # # 
# # Table 0A.6 -- Regression models used to build Figure 2
# # # # # # # #
stargazer(reg1.1, reg2.1,no.space = T, digits=3, 
          covariate.labels = c('Female bureaucrat with average experience','Female bureaucrat with high experience'))


# # # # # # # # 
# # Table 0A.7 -- Main results (Fig 1) by direct experience with petty corruption
# # # # # # # #
reg1exp <- lm(corrupt ~ female, data=pc[pc$pay_bribe==1,])
reg2exp <- lm(corrupt ~ nonetwork, data=pc[pc$pay_bribe==1,])

stargazer(reg1exp,reg2exp)

# # # # # # # # 
# # Table 0A.8 -- Main results (Fig 2) by direct experience with petty corruption
# # # # # # # #
reg1.1exp <- lm(corrupt ~ female_nonet, data=pc[pc$female_nonet==1  & pc$pay_bribe==1| pc$male_nonet==1 & pc$pay_bribe==1,])
reg2.1exp <- lm(corrupt ~ female_net, data=pc[pc$female_net==1  & pc$pay_bribe==1| pc$male_net==1 & pc$pay_bribe==1,])

stargazer(reg1.1exp,reg2.1exp)


# # # # # # # # 
# # Table 0A.9 -- Main results by ethnicity
# # # # # # # #
reg_akan <- lm(corrupt ~ Akan, data=pc)
reg_akan_gender <- lm(corrupt ~ Akan*female, data=pc)
reg_akan_network <- lm(corrupt ~ Akan*nonetwork, data=pc)

stargazer(reg_akan,reg_akan_gender,reg_akan_network,no.space = T, digits=2)



# # # # # # # # 
# # Table 0A.10 -- Family support expectations by gender and ethnic group
# # # # # # # #
press1<-lm(pressure_share ~ john, pc)
press2<-lm(pressure_share ~ john*Akan, pc)

stargazer(press1,press2,no.space = T,digits=2)


#Power analysis (discussed in footnote 9)
#Audio experiment
#N = 1268
pwr.t.test(n = 1268/2, d = , 
           sig.level = .05, power = .8, type = "two.sample")
#Effect size * SD of the outcome
sd(pc$corrupt)*.157
###############################################################################################################
######################################################################################
######  MY CONTRIBUTION TO RCODE:
# Data visualization:
##
## Boxplot of Region versus age and sex
ggplot(pc, aes(x = factor(region), y = age, fill = factor(region))) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  scale_x_discrete(labels = c("1" = "Greater Accra", "2" = "Central Ghana", "3" = "Eastern Ghana")) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ factor(sex, labels = c("Female", "Male")))
####
##
# Calculate percentage of each education level
education_counts <- pc %>%
  count(education) %>%
  mutate(percentage = n / sum(n) * 100)

# Plot bar plot with percentage values and labels
ggplot(education_counts, aes(x = factor(education), y = percentage, fill = factor(education))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage), "%")), vjust = -0.5) +  # Add percentage labels
  labs(x = "Education", y = "Percentage") +  # Label axes
  scale_fill_brewer(palette = "Set3") +  # Choose color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed
####
# Plot histogram with density curve and percentage on y-axis
ggplot(pc, aes(x = age)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "white", fill = "steelblue") +
  geom_density(colour = "red", size = 1) +
  labs(title = "Age Histogram with Density Curve", y = "Percent", x = "Age") +
  scale_y_continuous(labels = scales::percent) +  # Corrected scale_y_continuous
  theme_bw() +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ factor(sex, labels = c("Female", "Male")))
###################
########
# Multidimensional tables - Three ways contingency table
# Recode variables
pc$married <- factor(ifelse(pc$married == 0, "Single", "Married"))
pc$christ <- factor(ifelse(pc$christ == 0, "Non Christian", "Christian"))
pc$region <- factor(ifelse(pc$region == 1, "Greater Accra", ifelse(pc$region == 2, "Central Ghana", "Eastern Ghana")))

# Create three-way contingency table
mytable <- xtabs(~ sex + region + married + christ, data = pc)
mytable
## Answer:
#, , married = Married, christ = Christian

#region
#sex Central Ghana Eastern Ghana Greater Accra
#0           111           130           112
#1           122           107            80

#, , married = Single, christ = Christian

#region
#sex Central Ghana Eastern Ghana Greater Accra
#0            74            68            91
#1            61            75           108

#, , married = Married, christ = Non Christian

#region
#sex Central Ghana Eastern Ghana Greater Accra
#0            11             5            19
#1            15            14            19

#, , married = Single, christ = Non Christian

#region
#sex Central Ghana Eastern Ghana Greater Accra
#0             4             3             9
#1             3             7            20
##############################################################################################
# Statistical Regression:
##
# (a) Performing an ordered Multinomial logistic regression.
# The research question: 
#How do factors such as sex(female, male), religion(non-Christian, Christian) and number of the children
# influence the likelihood of bureaucrats employees residing and working in region other than Greater Accra soliciting a
# bribe for general public when need to interact with State public and private service (i.e., renewal of driving licence)
##
# 1st. I am going to do some wrangling
#
# Convert region variable into factor
pc$region <- factor(pc$region, levels = c(1, 2, 3), labels = c("Greater Accra", "Central Ghana", "Eastern Ghana"))
table(pc$region)
# Convert sex variable into factor
pc$sex <- factor(pc$sex, levels = c(0, 1), labels = c("Woman", "Man"))
table(pc$sex)
# Convert christ variable into factor
pc$christ <- factor(pc$christ, levels = c(0, 1), labels = c("Non-Christian", "Christian"))
table(pc$christ)
###
# Convert married variable into factor
pc$married <- factor(pc$married, levels = c(0, 1), labels = c("Single", "Married"))
table(pc$married)
######
library(dplyr)

# Rename the variable 'today' to 'Year'
pc <- pc %>%
  rename(Year = today)
###
head(pc)
str(pc)
##
ftable(xtabs(~sex + region + christ + married, data=pc))
##

##########################
# Performing an ordered Multinomial logistic regression where the response variable is region and reference category is
# "Greater Accra"
pc$region <- relevel(pc$region, ref="Greater Accra")
#
## (a) Perform an ordered (proportional odds) logistic regression
#
multinom_ordered_replic <-  polr(region ~ sex + married + children + christ, data = pc, Hess = TRUE)
summary(multinom_ordered_replic)
##
### Answer:
#Call:
#polr(formula = region ~ sex + married + children + christ, data = pc, 
#     Hess = TRUE)

#Coefficients:
#                 Value    Std. Error   t value
#sexMan          0.03533    0.10399     0.3397
#marriedMarried  0.38971    0.11342     3.4358
#children        0.01656    0.01594     1.0392
#christChristian 0.71835    0.17909     4.0112

#Intercepts:
#                            Value    Std. Error  t value
#Greater Accra|Central Ghana 0.3664   0.1971     1.8587 
#Central Ghana|Eastern Ghana 1.7058   0.2028     8.4106 

#Residual Deviance: 2749.378 
#AIC: 2761.378 
########################################################################################
# My Method of calculating p-value:

ctable3 <- coef(summary(multinom_ordered_replic)) # Extract coefficient summary
p <- 2 * (1 - pnorm(abs(ctable3[, "t value"]))) # Calculate the p-value
ctable3 <- cbind(ctable3, "p-value" = p) ## Combine coefficient summary and p-values
print(ctable3) # Print the results 
##### Answer:
#                                 Value Std. Error   t value      p-value
#sexMan                      0.03532781 0.10399245 0.3397151 7.340711e-01
#marriedMarried              0.38970933 0.11342459 3.4358452 5.907084e-04
#children                    0.01656015 0.01593508 1.0392261 2.986996e-01
#christChristian             0.71835215 0.17908647 4.0112029 6.041017e-05
#Greater Accra|Central Ghana 0.36640712 0.19712765 1.8587302 6.306539e-02
#Central Ghana|Eastern Ghana 1.70581934 0.20281673 8.4106443 0.000000e+00
##########
####
# Calculate confidence intervals
(ci <- confint(multinom_ordered_replic))
## Answer:
###
#                      2.5 %    97.5 %
#sexMan          -0.16848314 0.2392410
#marriedMarried   0.16709534 0.6119731
#children        -0.01255856 0.0500013
#christChristian  0.37007053 1.0731235
#########
###
head(pc$Year)
tail(pc$Year)
###
# convert to odds ratio
exp(cbind(OR = coef(multinom_ordered_replic), ci))
### Answer:
#                   OR        2.5 %       97.5 %
#sexMan          1.035959   0.8449455   1.270285
#marriedMarried  1.476552   1.1818669   1.844066
#children        1.016698   0.9875200   1.051272
#christChristian 2.051051   1.4478367   2.924500
###########
# Performing an un-ordered Multinomial logistic regression where the response variable is region and reference category is
# "Greater Accra"
##
# Run the model:

multinom_unordered_replic <-  multinom(region ~ sex + married + children + christ, data = pc)
## Answer:
## weights:  18 (10 variable)
#initial  value 1393.040382 
#iter  10 value 1372.307421
#final  value 1369.869609 
#converged
########
##
summary(multinom_unordered_replic)
############
# Answer:
#Call:
#multinom(formula = region ~ sex + married + children + christ, 
#         data = pc)
#
#Coefficients:
#                 (Intercept) sexMan     marriedMarried   children   christChristian
#Central Ghana   -1.223695    0.08455734  0.5397034      0.03295569   0.7281511
#Eastern Ghana   -1.269220    0.06643425  0.4744520      0.02588328   0.8771846

#Std. Errors:
#                 (Intercept)    sexMan    marriedMarried  children    christChristian
#Central Ghana   0.2545010      0.1391078   0.1520061      0.02288734   0.2282220
#Eastern Ghana   0.2610204      0.1382507   0.1509181      0.02326092   0.2365248
#
#Residual Deviance: 2739.739 
#AIC: 2759.739 
#############################################
# 
exp(coef(multinom_unordered_replic))
## Answer:
#              (Intercept)   sexMan  marriedMarried  children  christChristian
#Central Ghana   0.2941414   1.088235  1.715498      1.033505     2.071248
#Eastern Ghana   0.2810507   1.068691  1.607133      1.026221     2.404122

# get p values
z <- summary(multinom_unordered_replic)$coefficients/summary(multinom_unordered_replic)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)
### Answer:
#               (Intercept)    sexMan marriedMarried  children christChristian
#Central Ghana 1.522869e-06 0.5432839   0.0003844455 0.1498931    0.0014200775
#Eastern Ghana 1.158934e-06 0.6308473   0.0016678561 0.2658216    0.0002083777

