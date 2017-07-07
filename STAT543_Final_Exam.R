###############################################################################################
#Skyler Kuhn
#STAT 543: Final Exam
#Final Exam
###############################################################################################
#Problem 1.) ANOVA question
#Background: In  a  certain  study,  researchers  investigated  dietary  intake  of Vitamin D among 
## a random  sample  of 140 healthy women ages  25-66 (Split into four different groups: A-D). 

#Reading in the dataset
vitaminD.data <- read.csv(file.choose(), header = TRUE)
##Output:
#     Group VitaminD
# 1       A     1821
# 2       A     2585
# 3       A     2671
# ......
# 34      B     1845
# 35      B      990
# 36      B     1689
# ......
# 58      C     1424
# 59      C     1213
# 60      C      918
# ......
# 87      D      961
# 88      D     1939
# 89      D      944
# ......
# 140     D     1752
##About the groups:
# Group A: 25-35 years old
# Group B: 36-45 years old
# Group C: 45-55 years old
# Group D: 56-66 years old

#Creating table for each group (A, B, C, D):
vitgroupA.data <- vitaminD.data[vitaminD.data$Group == "A", ]
vitgroupB.data <- vitaminD.data[vitaminD.data$Group == "B", ]
vitgroupC.data <- vitaminD.data[vitaminD.data$Group == "C", ]
vitgroupD.data <- vitaminD.data[vitaminD.data$Group == "D", ]
###############################################
#####GroupA data: 

#About: Group A referred to subjects aged 25-35 years old
library(car)

#Finding the sample size:
nrow(vitgroupA.data)
##Output:
# 33  (greater than 30)

#Checking the distribution of Data:
hist(vitgroupA.data$VitaminD,   
     main = "Histogram of GroupA data", #changes the Legend Header/title #comma is neeeded
     xlab = "Vitamin D Dietary Intake",   #changes the X-axis label  
     col = "lightblue")
#data is not normally distributed
qqPlot(vitgroupA.data$VitaminD)
##data is not normally distributed

#Determining the Variance (looking at sd similarty and bartlett test results):
sd(vitgroupA.data$VitaminD) #Output: 479.1529
sd(vitgroupB.data$VitaminD) #Output: 439.7865 
sd(vitgroupC.data$VitaminD) #Output: 403.8845
sd(vitgroupD.data$VitaminD) #Output: 356.1865
bartlett.test(vitaminD.data$VitaminD ~ vitaminD.data$Group) 
# p-value = 0.2798, Result is highly indicative (much greater than 0.05), it suggests equal variances
###############################################
#####GroupB data: 
#About: Group B referred to subjects aged 36-45 years old
library(car)

#Finding the sample size:
nrow(vitgroupB.data)
##Output:
# 24 (less than 30)

#Checking the distribution of Data:
hist(vitgroupB.data$VitaminD,   
     main = "Histogram of GroupB data", # changes the Legend Header/title #comma is neeeded
     xlab = "Vitamin D Dietary Intake",   #changes the X-axis label  
     col = "lightblue")
#data is slightly normally distributed
qqPlot(vitgroupB.data$VitaminD)
##data is normally distributed

#Determining the Variance (looking at sd similarty and bartlett test results):
sd(vitgroupA.data$VitaminD) #Output: 479.1529
sd(vitgroupB.data$VitaminD) #Output: 439.7865 
sd(vitgroupC.data$VitaminD) #Output: 403.8845
sd(vitgroupD.data$VitaminD) #Output: 356.1865
bartlett.test(vitaminD.data$VitaminD ~ vitaminD.data$Group) 
# p-value = 0.2798, Result is highly indicative (much greater than 0.05), it suggests equal variances
###############################################
#####GroupC data: 
#About: Group C referred to subjects aged 45-55 years old
library(car)

#Finding the sample size:
nrow(vitgroupC.data)
##Output:
# 29 (less than 30)

#Checking the distribution of Data:
hist(vitgroupC.data$VitaminD,   
     main = "Histogram of GroupC data", # changes the Legend Header/title #comma is neeeded
     xlab = "Vitamin D Dietary Intake",   #changes the X-axis label  
     col = "lightblue")
#data is normally distributed
qqPlot(vitgroupC.data$VitaminD)
##data is normally distributed

#Determining the Variance (looking at sd similarty and bartlett test results):
sd(vitgroupA.data$VitaminD) #Output: 479.1529
sd(vitgroupB.data$VitaminD) #Output: 439.7865 
sd(vitgroupC.data$VitaminD) #Output: 403.8845
sd(vitgroupD.data$VitaminD) #Output: 356.1865
bartlett.test(vitaminD.data$VitaminD ~ vitaminD.data$Group) 
# p-value = 0.2798, Result is highly indicative (much greater than 0.05), it suggests equal variances
###############################################
#####GroupD data: 
#About: Group D referred to subjects aged 56-66 years old
library(car)

#Finding the sample size:
nrow(vitgroupD.data)
##Output:
# 54 (greater than 30)

#Checking the distribution of Data:
hist(vitgroupD.data$VitaminD,   
     main = "Histogram of GroupD data", # changes the Legend Header/title #comma is neeeded
     xlab = "Vitamin D Dietary Intake",   #changes the X-axis label  
     col = "lightblue")
#data is normally distributed
qqPlot(vitgroupD.data$VitaminD)
##data is normally distributed

#Determining the Variance (looking at sd similarty and bartlett test results):
sd(vitgroupA.data$VitaminD) #Output: 479.1529
sd(vitgroupB.data$VitaminD) #Output: 439.7865 
sd(vitgroupC.data$VitaminD) #Output: 403.8845
sd(vitgroupD.data$VitaminD) #Output: 356.1865
bartlett.test(vitaminD.data$VitaminD ~ vitaminD.data$Group) 
# p-value = 0.2798, Result is highly indicative (much greater than 0.05), it suggests equal variances
###############################################

########DATA SUMMARY:
#Since the sample size/normality is equal for each group, we summarize with:
##sample size, mean standard deviation and 95%CI

##Group A Data Summary:
nrow(vitgroupA.data)            #sample size = 33
mean(vitgroupA.data$VitaminD)   #mean = 1482.152
sd(vitgroupA.data$VitaminD)     #standard deviation = 479.1529
t.test(vitgroupA.data$VitaminD, alternative = "two.sided", var.equal = TRUE) 
#95% CI: (1312.251, 1652.052)

##Group B Data Summary:
nrow(vitgroupB.data)            #sample size = 24
mean(vitgroupB.data$VitaminD)   #mean = 1508.167
sd(vitgroupB.data$VitaminD)     #standard deviation = 439.7865
t.test(vitgroupB.data$VitaminD, alternative = "two.sided", var.equal = TRUE) 
#95% CI: (1322.461, 1693.872)

##Group C Data Summary:
nrow(vitgroupC.data)            #sample size = 29
mean(vitgroupC.data$VitaminD)   #mean = 1254.483
sd(vitgroupC.data$VitaminD)     #standard deviation = 403.8845
t.test(vitgroupC.data$VitaminD, alternative = "two.sided", var.equal = TRUE) 
#95% CI: (1100.853, 1408.112)

##Group D Data Summary:
nrow(vitgroupD.data)            #sample size = 54
mean(vitgroupD.data$VitaminD)   #mean = 971.2037
sd(vitgroupD.data$VitaminD)     #standard deviation = 356.1865
t.test(vitgroupD.data$VitaminD, alternative = "two.sided", var.equal = TRUE) 
#95% CI: (873.9835, 1068.4239)

#########Equal Variance ANOVA TEST:
vitaminD.data.ANOVA <- aov(vitaminD.data$VitaminD ~ vitaminD.data$Group)
##Ouput:
# Call:
#   aov(formula = vitaminD.data$VitaminD ~ vitaminD.data$Group)
# Terms:
#   vitaminD.data$Group Residuals
# Sum of Squares              7567827  23086764
# Deg. of Freedom                   3       136
# Residual standard error: 412.0141
# Estimated effects may be unbalanced
summary(vitaminD.data.ANOVA)
##Output: 
#                       Df   Sum Sq   Mean Sq   F value   Pr(>F)    
# vitaminD.data$Group   3    7567827  2522609   14.86     2.01e-08 ***  #####p-value less than 0.05 (reject Ho)
#   Residuals           136 23086764  169756                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##Becasue the F-test gave us a p-value less than 0.05, we will reject the Null Hypothesis in favor of the alternative
#Now than we know one of the samples means are different, we will use a multiple comparision test to find out which one it is.  

#Tukey Test (multiple comparsion test, to find the different sample):
TukeyHSD(vitaminD.data.ANOVA, ordered = TRUE)   #diff show the difference of the means, report the standard error (not the sd here)
##Output:
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# factor levels have been ordered
# Fit: aov(formula = vitaminD.data$VitaminD ~ vitaminD.data$Group)
# $`vitaminD.data$Group`
#        diff        lwr      upr     p adj
# C-D 283.27905   36.55643 530.0017 0.0174253   ##Significant (< 0.05)!!
# A-D 510.94781  274.15332 747.7423 0.0000006   ##Significant (< 0.05)!!
# B-D 536.96296  274.05084 799.8751 0.0000026   ##Significant (< 0.05)!!
# A-C 227.66876  -45.10695 500.4445 0.1366191
# B-C 253.68391  -42.04865 549.4165 0.1200334
# B-A  26.01515 -261.48671 313.5170 0.9953899
##There is a significant difference between groups A, B, C vs. group D.

##About the groups:
# Group A: 25-35 years old
# Group B: 36-45 years old
# Group C: 45-55 years old
# Group D: 56-66 years old

#Finding the standard error:
nexp <- 24
ncotrol <- 33
sdexp <- sd(vitgroupB.data$VitaminD)
sdcontrol <- sd(vitgroupA.data$VitaminD)
sp <- ((nexp - 1) * sdexp * sdexp + (ncotrol - 1) * sdcontrol * sdcontrol) / (nexp + ncotrol - 2)
sep <- sqrt(sp*(1/nexp+1/ncotrol))
sep

#Finding the 95% CI on the difference:
t.test(vitgroupB.data$VitaminD,vitgroupA.data$VitaminD,
       alternative = "two.sided", conf.level = 0.95, var.equal = TRUE)

#(end of question 1)
###############################################################################################
###############################################################################################
#Problem 2.)
##Background: A  certain  pesticide  is  known  to alter  the  structure  or  function  of the thyroid 
#gland,  which could  possibly  lead  to  abnormal  metabolic  functioning, including abnormal 
#weight gain. A study sought to examine and assess the effects of exposure to such pesticide on 
#weight gain. 110 rats were exposed to specific doses of the pesticide for 15 days. The rats were
#randomly assigned to either high or low exposure groups. Body weight was measured in grams (g) 
#at baseline (Day1) and at the end of the study (Day 15).

#Reading in the dataset
ratsThyroid.data <- read.csv(file.choose(), header = TRUE)
##Output:
#     BWDay1 BWDay15  Exposure X  X.1
# 1   114.07  143.99      low NA  NA
# 2    95.06  127.59      low NA  NA
# 3    82.60  110.11      low NA  NA
# 4   106.53  119.75      low NA  NA
# ......
# 49  103.82  135.26      low NA  NA
# 50   89.86  125.00     high NA  NA
# 51  100.84  126.60     high NA  NA
# 52  109.21  137.34     high NA  NA
# 53  116.31  145.14     high NA  NA
# ......
# 110  79.29  116.98     high NA  NA
ratsThyroid.data <- ratsThyroid.data[,1:3] #deleting the "non-sense" x and x.1 columns
##Output:
#     BWDay1 BWDay15 Exposure
# 1   114.07  143.99      low
# 2    95.06  127.59      low
# 3    82.60  110.11      low
# .....  
# ..... ..
# 110  79.29  116.98     high

## About Groups:
# BWDay1: Body Weight on Day 1 of the Study
# BWDay15: Body Weight on Day 15 of the Study
# Exposure: "Low" or "High" dose exposure to the pesticide  

###############################################
# Question 1: Ignoring the exposure status, is there a relationship 
## between the Day 1 and Day 15 body weight measures? What is the strength and
### direction of the relationship? Is it possible to illustrate the relationship
#### graphically? If yes, show how.

#Comparing Day1 and Day15, ignoring the exposure level
#Ho: ρ = 0, there is no association.
#Ha: ρ ≠ 0, there is an association.

#Sample Size of both groups (BWDay1 and BwDay15):
nrow(ratsThyroid.data)  # 110 rats 

#Seeing if there is an Assoication with a scatterplots:
plot(ratsThyroid.data$BWDay1,ratsThyroid.data$BWDay15,
     xlab = "Body Weight on Day 1",
     ylab = "Body Weight on Day 15",
     main = "BWDay15 versus Day1")
#positive correlation

########Hypothesis Testing:
#Test of Correlation between BWDay1 and BWDay15:
cor.test(ratsThyroid.data$BWDay1,ratsThyroid.data$BWDay15,
         alternative = "two.sided",
         method = "pearson")
##Output:
# Pearson's product-moment correlation
# data:  ratsThyroid.data$BWDay1 and ratsThyroid.data$BWDay15
# t = 12.25, df = 108, p-value < 2.2e-16    #####p-value is less than 0.05, reject the null#####
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.6711525 0.8311335
# sample estimates:
#       cor 
# 0.7625644 
##Reject the null in favor of the alternative hypothesis##

#########Data Summary:
#BWDay1:
nrow(ratsThyroid.data)         #110
mean(ratsThyroid.data$BWDay1)  #97.54045
sd(ratsThyroid.data$BWDay1)    #10.22193
t.test(ratsThyroid.data$BWDay1, conf.level = 0.95) #(95.60878, 99.47213)
#BWDay15:
nrow(ratsThyroid.data)         #110
mean(ratsThyroid.data$BWDay15)  #127.2486
sd(ratsThyroid.data$BWDay15)    #10.51291
t.test(ratsThyroid.data$BWDay15, conf.level = 0.95) #(125.2620, 129.2353)

#(end of question 1 of problem 2)

###############################################
###############################################
# Question 2: Is there a difference in weight between the two exposure groups 
## (low and high) at Day 1? Justify? 

#Is there a difference between Day1 weights between exposure groups' means (low vs. high)
#Ho: u1 - u2 = 0, there is no difference.
#Ha: u1 - u2 ≠ 0, there is a difference. 

#Creating table for each exposure group (low and high):
lowExpgroup.data <- ratsThyroid.data[ratsThyroid.data$Exposure == "low", ]
##Output:
#     BWDay1 BWDay15 Exposure
# 1  114.07  143.99      low
# 2   95.06  127.59      low
# 3   82.60  110.11      low
# ......
nrow(lowExpgroup.data) #Sample size: 49 (greater than 30)
highExpgroup.data <- ratsThyroid.data[ratsThyroid.data$Exposure == "high", ]
##Output:
#      BWDay1 BWDay15 Exposure
# 50   89.86  125.00     high
# 51  100.84  126.60     high
# 52  109.21  137.34     high
# ......
nrow(highExpgroup.data) #Sample size: 61 (greater than 30)

#####Checking the distribution of Data for each group:
hist(lowExpgroup.data$BWDay1,   
     main = "Histogram of Low exposure Day1 data", # changes the Legend Header/title #comma is neeeded
     xlab = "Body Weight",   #changes the X-axis label  
     col = "lightblue")
#data is normally distributed
qqPlot(lowExpgroup.data$BWDay1)
##data is normally distributed

hist(highExpgroup.data$BWDay1,   
     main = "Histogram of High exposure Day1 data", # changes the Legend Header/title #comma is neeeded
     xlab = "Body Weight",   #changes the X-axis label  
     col = "lightblue")
#data is normally distributed
qqPlot(highExpgroup.data$BWDay1)
##data is normally distributed

#####Checking the variance between the two samples:
var.test(lowExpgroup.data$BWDay1,highExpgroup.data$BWDay1)
##Output:
#F test to compare two variances
#p-value = 0.3975. Result is highly indicative (much greater than 0.05), it suggests equal variances.
sd(lowExpgroup.data$BWDay1)   #9.573986
sd(highExpgroup.data$BWDay1)  #10.77789
#similar standard deviations, also suggests equal variance
#Weighing the results of both methods, it is clear that the variance between both samples is SIMILAR or EQUAL

#####Normally distributed data (T-test): summarize with sample sizes, means, standard deviations and 95%CIs
#and summarize the difference with obseverved differnce, standard error, and 95%CI.
#Performing a T-test (data is normally distributed)
t.test(lowExpgroup.data$BWDay1, highExpgroup.data$BWDay1, alternative = "two.sided", conf.level = 0.95, var.equal = TRUE)
##Output:
# Two Sample t-test
# data:  lowExpgroup.data$BWDay1 and highExpgroup.data$BWDay1
# t = 0.43223, df = 108, p-value = 0.6664 #####p-vaule > 0.05, fail to reject the null#####
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -3.050753  4.752292
# sample estimates:
#   mean of x mean of y 
# 98.01224  97.16148 
##Fail to reject the null##

#########SUMMARIZING THE DATA:
#Sample Sizes:
nrow(lowExpgroup.data) #Sample size: 49 (greater than 30)
nrow(highExpgroup.data) #Sample size: 61 (greater than 30)

#Finding the standard deviations of the two samples:
##Standard Deviation of low exposure group:
sd(lowExpgroup.data$BWDay1)
#Output:
#  9.573986
##Standard Deviation of high exposure group:
sd(highExpgroup.data$BWDay1)
#Output:
# 10.77789

#Finding the 95% CIs of the two samples:
##95% CI of low exposure group:
t.test(lowExpgroup.data$BWDay1, conf.level = 0.95)
#Output:
# (95.26228, 100.76221)
##95% CI of high exposure group:
t.test(highExpgroup.data$BWDay1, conf.level = 0.95)
#Output:
# (94.40113, 99.92182)

#Finding the obsevered difference and standard error:
diff <- mean(lowExpgroup.data$BWDay1) - mean(highExpgroup.data$BWDay1)
nexp <- 49
ncotrol <- 61
sdexp <- sd(lowExpgroup.data$BWDay1)
sdcontrol <- sd(highExpgroup.data$BWDay1)
sp <- ((nexp - 1) * sdexp * sdexp + (ncotrol - 1) * sdcontrol * sdcontrol) / (nexp + ncotrol - 2)
sep <- sqrt(sp*(1/nexp+1/ncotrol)) #pooled standard error, used if variance is equal 
##Output: diff (difference of sampled means)
# 0.8507695
##Output: standard error, pooled (for equal variance)
# 1.968305
# (end of question 2 of problem 2)
diff2 <- lowExpgroup.data$BWDay1 - highExpgroup.data$BWDay1

###############################################
# Question 3: Is there a diference in weight between the two exposure groups 
## (low and high) at Day 15? Justify?

#Is there a difference between Day15 weights between exposure groups' means (low vs. high)
#Ho: u1 - u2 = 0, there is no difference.
#Ha: u1 - u2 ≠ 0, there is a difference. 

#####Checking the distribution of Data for each group:
hist(lowExpgroup.data$BWDay15,   
     main = "Histogram of Low exposure Day15 data", # changes the Legend Header/title #comma is neeeded
     xlab = "Body Weight",   #changes the X-axis label  
     col = "lightblue")
#data is normally distributed
qqPlot(lowExpgroup.data$BWDay15)
##data is normally distributed

hist(highExpgroup.data$BWDay15,   
     main = "Histogram of High exposure Day1 data", # changes the Legend Header/title #comma is neeeded
     xlab = "Body Weight",   #changes the X-axis label  
     col = "lightblue")
#data is normally distributed
qqPlot(highExpgroup.data$BWDay15)
##data is normally distributed

######Checking the variance between the two samples:
var.test(lowExpgroup.data$BWDay15,highExpgroup.data$BWDay15)
##Output:
#F test to compare two variances
#p-value = 0.791. Result is highly indicative (much greater than 0.05), it suggests equal variances.
sd(lowExpgroup.data$BWDay15)   #10.32063
sd(highExpgroup.data$BWDay15)  #10.71949
#similar standard deviations, also suggests equal variance
#Weighing the results of both methods, it is clear that the variance between both samples is SIMILAR or EQUAL

#####Normally distributed data (T-test): summarize with sample sizes, means, standard deviations and 95%CIs
#and summarize the difference with obseverved differnce, standard error, and 95%CI.
#Performing a T-test (data is normally distributed)
t.test(lowExpgroup.data$BWDay15, highExpgroup.data$BWDay15, mu = 0, alternative = "two.sided", conf.level = 0.95, var.equal = TRUE)
##Output:
# Two Sample t-test
# data:  lowExpgroup.data$BWDay15 and highExpgroup.data$BWDay15
# t = -0.597, df = 108, p-value = 0.5518 #####p-value > 0.05, fail to reject Ho#####
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -5.217019  2.801863
# sample estimates:
#   mean of x mean of y 
# 126.5790  127.7866
##Fail to reject the null##

#########SUMMARIZING THE DATA:
#Sample Sizes:
nrow(lowExpgroup.data) #Sample size: 49 (greater than 30)
nrow(highExpgroup.data) #Sample size: 61 (greater than 30)

#Finding the standard deviations of the two samples:
##Standard Deviation of low exposure group:
sd(lowExpgroup.data$BWDay15)
#Output:
#  10.32063
##Standard Deviation of high exposure group:
sd(highExpgroup.data$BWDay15)
#Output:
# 10.71949

#Finding the 95% CIs of the two samples:
##95% CI of low exposure group:
t.test(lowExpgroup.data$BWDay15, conf.level = 0.95)
#Output:
# (123.6145, 129.5434)
##95% CI of high exposure group:
t.test(highExpgroup.data$BWDay15, conf.level = 0.95)
#Output:
# (125.0412, 130.5319)

t.test(lowExpgroup.data$BWDay15, highExpgroup.data$BWDay15, conf.level = 0.95)

#Finding the obsevered difference and standard error:
diff <- mean(lowExpgroup.data$BWDay15) - mean(highExpgroup.data$BWDay15)
nexp <- 49
ncotrol <- 61
sdexp <- sd(lowExpgroup.data$BWDay15)
sdcontrol <- sd(highExpgroup.data$BWDay15)
sp <- ((nexp - 1) * sdexp * sdexp + (ncotrol - 1) * sdcontrol * sdcontrol) / (nexp + ncotrol - 2)
sep <- sqrt(sp*(1/nexp+1/ncotrol)) #pooled standard error, used if variance is equal 
##Output: diff (difference of sampled means)
# -1.207578
##Output: standard error, pooled (for equal variance)
# 2.02275
t.test(lowExpgroup.data$BWDay15, highExpgroup.data$BWDay15, conf.level = 0.95)
#difference 95%CI: (-5.201851  2.786696)

# (end of question 3 of problem 2)

###############################################
###############################################
# Question 4: As a result of low exposure, is there a change between the Day 1 and 
## Day 15 body weights? Justify.

#Is there a difference between Day1 and Day15 of the low exposure group (PAIRED data):
#Ho: u1 - u2 = 0, there is no difference. #Paired 
#Ha: u1 - u2 ≠ 0, there is a difference.  #Paired 

#Already created table (question 2 of problem 2) for each exposure group (low and high):
lowExpgroup.data <- ratsThyroid.data[ratsThyroid.data$Exposure == "low", ]
##Output:
#     BWDay1 BWDay15 Exposure
# 1  114.07  143.99      low
# 2   95.06  127.59      low
# 3   82.60  110.11      low
# ......
nrow(lowExpgroup.data) #Sample size: 49 (greater than 30)

#####Checking the distribution of Data for each group:
#(This isn't really nesscary because both sample sizes are > 30)
hist(lowExpgroup.data$BWDay1,   
     main = "Histogram of Low exposure Day1 data", # changes the Legend Header/title #comma is neeeded
     xlab = "Body Weight",   #changes the X-axis label  
     col = "lightblue")
#data is normally distributed
qqPlot(lowExpgroup.data$BWDay1)
##data is normally distributed

hist(lowExpgroup.data$BWDay15,   
     main = "Histogram of Low exposure Day15 data", # changes the Legend Header/title #comma is neeeded
     xlab = "Body Weight",   #changes the X-axis label  
     col = "lightblue")
#data is normally distributed
qqPlot(lowExpgroup.data$BWDay15)
##data is normally distributed

#No need to check for variance because data is paired!

#####Normally distributed paired data (paired T-test): summarize with sample sizes, means, standard deviations and 95%CIs
#and summarize the difference with obseverved differnce, standard error, and 95%CI.
#Performing a paired T-test (data is normally distributed)
t.test(lowExpgroup.data$BWDay1, lowExpgroup.data$BWDay15, mu = 0, alternative = "two.sided", paired = TRUE, conf.level = 0.95)
##Output:
# Paired t-test
# data:  lowExpgroup.data$BWDay1 and lowExpgroup.data$BWDay15
# t = -28.221, df = 48, p-value < 2.2e-16       ####p-value is < 0.05, reject the Ho####
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -30.60198 -26.53149
# sample estimates:
#   mean of the differences 
# -28.56673 
## Reject the Null in favor of the null hypothesis!#####

#########SUMMARIZING THE DATA:
#Sample Sizes:
nrow(lowExpgroup.data) #Sample size: 49 (greater than 30)

#Finding the standard deviations of the two samples:
##Standard Deviation of low exposure group:
sd(lowExpgroup.data$BWDay1)
#Output:
#  9.573986
##Standard Deviation of high exposure group:
sd(lowExpgroup.data$BWDay15)
#Output:
# 10.32063

#Finding the Mean
mean(lowExpgroup.data$BWDay1)  #98.01224
mean(lowExpgroup.data$BWDay15) #126.579
#Finding the 95% CIs of the two samples:
##95% CI of low exposure day1 group:
t.test(lowExpgroup.data$BWDay1, conf.level = 0.95)
#Output:
# (95.26228, 100.76221)
##95% CI of low exposure day15 group:
t.test(lowExpgroup.data$BWDay15, conf.level = 0.95)
#Output:
# (123.6145, 129.5434)

#####Finding the difference data (Post - Pre):
##Difference Dataset:
diffLowexposure.data <- lowExpgroup.data$BWDay15 - lowExpgroup.data$BWDay1
##Mean (Diff):
mean(diffLowexposure.data) # 28.56673
##Samle Size: 
diffLowexposure.data      # 49
##Standard Deviation:
sd(diffLowexposure.data) # 7.085669
##95% CI (diff)
t.test(diffLowexposure.data, conf.level = 0.95) 
#Output (95% CI): (26.53149, 30.60198)
# (end of question 4 of problem 2)

###############################################
# Question 5: As a result of high exposure, is there a change between the Day 1 and 
## Day 15 body weights? Justify.

#Is there a difference between Day1 and Day15 of the high exposure group (PAIRED data):
#Ho: u1 - u2 = 0, there is no difference. #Paired 
#Ha: u1 - u2 ≠ 0, there is a difference.  #Paired 

highExpgroup.data <- ratsThyroid.data[ratsThyroid.data$Exposure == "high", ]
##Output:
#      BWDay1 BWDay15 Exposure
# 50   89.86  125.00     high
# 51  100.84  126.60     high
# 52  109.21  137.34     high
# ......
nrow(highExpgroup.data) #Sample size: 61 (greater than 30)

#####Checking the distribution of Data for each group:
#(This isn't really nesscary because both sample sizes are > 30)
hist(highExpgroup.data$BWDay1,   
     main = "Histogram of Low exposure Day1 data", # changes the Legend Header/title #comma is neeeded
     xlab = "Body Weight",   #changes the X-axis label  
     col = "lightblue")
#data is normally distributed
qqPlot(highExpgroup.data$BWDay1)
##data is normally distributed

hist(highExpgroup.data$BWDay15,   
     main = "Histogram of Low exposure Day15 data", # changes the Legend Header/title #comma is neeeded
     xlab = "Body Weight",   #changes the X-axis label  
     col = "lightblue")
#data is normally distributed
qqPlot(highExpgroup.data$BWDay15)
##data is normally distributed

#No need to check for variance because data is paired!

#####Normally distributed paired data (paired T-test): summarize with sample sizes, means, standard deviations and 95%CIs
#and summarize the difference with obseverved differnce, standard error, and 95%CI.
#Performing a paired T-test (data is normally distributed)
t.test(highExpgroup.data$BWDay1, highExpgroup.data$BWDay15, mu = 0, alternative = "two.sided", paired = TRUE, conf.level = 0.95)
##Output:
# Paired t-test
# data:  highExpgroup.data$BWDay1 and highExpgroup.data$BWDay15
# t = -33.567, df = 60, p-value < 2.2e-16  ####p-value is < 0.05, reject the Ho####
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -32.45006 -28.80011
# sample estimates:
#   mean of the differences 
# -30.62508 
##Reject the Null hypothesis in favor of the alternative!##

#########SUMMARIZING THE DATA:
#Sample Sizes:
nrow(highExpgroup.data) #Sample size: 61 (greater than 30)

#Finding the standard deviations of the two samples:
##Standard Deviation of Day1 high exposure group:
sd(highExpgroup.data$BWDay1)
#Output:
#  10.77789
##Standard Deviation of Day15 high exposure group:
sd(highExpgroup.data$BWDay15)
#Output:
# 10.71949

#Mean 
mean(highExpgroup.data$BWDay1)  #97.16148
mean(highExpgroup.data$BWDay15) #127.7866

#Finding the 95% CIs of the two samples:
##95% CI of high exposure day1 group:
t.test(highExpgroup.data$BWDay1, conf.level = 0.95)
#Output:
# (94.40113, 99.92182)
##95% CI of high exposure day15 group:
t.test(highExpgroup.data$BWDay15, conf.level = 0.95)
#Output:
# (125.0412, 130.5319)

#####Finding the difference data (Post - Pre):
##Difference Dataset:
diffHighexposure.data <- highExpgroup.data$BWDay15 - highExpgroup.data$BWDay1
##Mean (Diff):
mean(diffHighexposure.data) # 30.62508
##Samle Size: 
diffHighexposure.data      # 61
##Standard Deviation:
sd(diffHighexposure.data) # 7.125694
##95% CI (diff)
t.test(diffHighexposure.data, conf.level = 0.95) 
#Output (95% CI): (28.80011 32.45006)
#(end of question 5 of problem 2)

###############################################
###############################################
# Question 6: At the significance level alpha = 0.05, does the level of exposure have
## an effect on the amount of change in body weight from Day1 to Day 15? Justify.

#Difference dataset (Post - Pre):
diffLowexposure.data <- lowExpgroup.data$BWDay15 - lowExpgroup.data$BWDay1    #Sample Size: 49
diffHighexposure.data <- highExpgroup.data$BWDay15 - highExpgroup.data$BWDay1 #Sample Size: 61

#Is there a difference between change in weight from Day1 to Day 15 between exposure groups' means (low vs. high)
#Ho: u1 - u2 = 0, there is no difference.
#Ha: u1 - u2 ≠ 0, there is a difference. 

#####Checking the distribution of Data for each group:
hist(diffLowexposure.data,   
     main = "Histogram of Change in Weight of Low Exposure group", # changes the Legend Header/title #comma is neeeded
     xlab = "Change in Body Weight (Day15 -Day1)",   #changes the X-axis label  
     col = "lightblue")
#data is normally distributed
qqPlot(diffLowexposure.data)
##data is normally distributed

hist(diffHighexposure.data,   
     main = "Histogram of Change in Weight of High Exposure group", # changes the Legend Header/title #comma is neeeded
     xlab = "Change in Body Weight (Day15 -Day1)",   #changes the X-axis label  
     col = "lightblue")
#data is normally distributed
qqPlot(diffHighexposure.data)
##data is normally distributed

#####Checking the variance between the two samples:
var.test(diffLowexposure.data,diffHighexposure.data)
##Output:
#F test to compare two variances
#p-value = p-value = 0.9755. Result is highly indicative (much greater than 0.05), it suggests equal variances.
sd(diffLowexposure.data)   #7.085669
sd(diffHighexposure.data)  #7.125694
#similar standard deviations, also suggests equal variance
#Weighing the results of both methods, it is clear that the variance between both samples is SIMILAR or EQUAL

#####Normally distributed data (T-test): summarize with sample sizes, means, standard deviations and 95%CIs
#and summarize the difference with obseverved differnce, standard error, and 95%CI.
#Performing a T-test (data is normally distributed)
t.test(diffLowexposure.data, diffHighexposure.data, alternative = "two.sided", mu = 0, conf.level = 0.95, var.equal = TRUE)
##Output:
# Two Sample t-test
# data:  diffLowexposure.data and diffHighexposure.data
# t = -1.5095, df = 108, p-value = 0.1341    ####p-value is > 0.05, fail to reject the Ho####
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -4.7611749  0.6444804
# sample estimates:
#   mean of x mean of y 
# 28.56673  30.62508
##Fail to Reject the Null hypothesis!## 

#########SUMMARIZING THE DATA:
#Sample Sizes:
diffLowexposure.data #Sample size: 49 (greater than 30)
diffHighexposure.data #Sample size: 61 (greater than 30)

#Finding the standard deviations of the two samples:
##Standard Deviation of low exposure group:
sd(diffLowexposure.data)
#Output:
#  7.085669
##Standard Deviation of high exposure group:
sd(diffHighexposure.data)
#Output:
# 7.125694

#Finding the 95% CIs of the two samples:
##95% CI of low exposure group:
t.test(diffLowexposure.data, conf.level = 0.95)
#Output:
# (26.53149, 30.60198)
##95% CI of high exposure group:
t.test(diffHighexposure.data, conf.level = 0.95)
#Output:
# (28.80011, 32.45006)

#Finding the obsevered difference and standard error:
diff <- mean(diffLowexposure.data) - mean(diffHighexposure.data)
nexp <- 49
ncotrol <- 61
sdexp <- sd(diffLowexposure.data)
sdcontrol <- sd(diffHighexposure.data)
sp <- ((nexp - 1) * sdexp * sdexp + (ncotrol - 1) * sdcontrol * sdcontrol) / (nexp + ncotrol - 2)
sep <- sqrt(sp*(1/nexp+1/ncotrol)) #pooled standard error, used if variance is equal 
##Output: diff (difference of sampled means)
# -2.058347
##Output: standard error, pooled (for equal variance)
# 1.363568
# (end of question 6 of problem 2)
t.test(diffLowexposure.data, diffHighexposure.data, alternative = "two.sided", conf.level = 0.95)
###############################################
# Question 7: Do the data support the hypothesis that high exposure to the pesticide 
## increase body weight in rats? Based on your conclusions form the research.


# (end of question 7 of problem 2)