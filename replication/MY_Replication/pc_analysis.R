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

