###############################################################################################
#Skyler Kuhn
#STAT 543: Assignment 7
#HW5: Analysis of Variance (ANOVA) and Correlation
###############################################################################################
#Problem 1.) ANOVA question

#Reading in the dataset
thrombocytopaenia.data <- read.csv(file.choose(), header = TRUE)
##Output: 
#     NECGroup Platelet
# 1          A     1.97
# 2          A     0.85
# 3          A     1.79
# ...
# 68         B     1.38
# 69         B     1.86
# 70         B     2.26
# ...
# 98         C     1.87
# 99         C     1.90
# 100        C     2.43
# ...
# 152        D     1.77
# 153        D     1.68
# 154        D     1.46
# ...
# 178        D     2.36
##About the groups:
# Group 0 referred to infants with no gangrene and is labeled as Group A
# Group 1 referred to subjects in whom gangrene was limited to a single intestinal segment, Group 1 is labeled as Group B
# Group 2 referred to patients with two or more intestinal segments of gangrene, Group2 is labeled as Group C and
# Group 3 referred to patients with the majority of small and large bowel involved, Group 3 is labeled as GroupD.

table(thrombocytopaenia.data)

#Creating table for each group (A, B, C, D):
groupA.data <- thrombocytopaenia.data[thrombocytopaenia.data$NECGroup == "A", ]
groupB.data <- thrombocytopaenia.data[thrombocytopaenia.data$NECGroup == "B", ]
groupC.data <- thrombocytopaenia.data[thrombocytopaenia.data$NECGroup == "C", ]
groupD.data <- thrombocytopaenia.data[thrombocytopaenia.data$NECGroup == "D", ]
###############################################
#####GroupA data: 
#About: Group 0 referred to infants with no gangrene and is labeled as Group A
library(car)

#Finding the sample size:
nrow(groupA.data)
##Output:
# 67  (greater than 30)

#Checking the distribution of Data:
hist(groupA.data$Platelet,   
     main = "Histogram of groupA data", # changes the Legend Header/title #comma is neeeded
     xlab = "log10 platelet counts",   #changes the X-axis label  
     col = "lightblue")
#data is normally distributed
qqPlot(groupA.data$Platelet)
##data is normally distributed

#Determining the Variance (looking at sd similarty and bartlett test results):
sd(groupA.data$Platelet) #Output: 0.3971375
sd(groupB.data$Platelet) #Output: 0.4383884
sd(groupC.data$Platelet) #Output: 0.4635171
sd(groupD.data$Platelet) #Output: 0.3706927
bartlett.test(thrombocytopaenia.data$Platelet ~ thrombocytopaenia.data$NECGroup) 
# p-value = 0.5077, Result is highly indicative (much greater than 0.05), it suggests equal variances
###############################################
#####GroupB data: 
#About: Group 1 referred to subjects in whom gangrene was limited to a single intestinal segment, Group 1 is labeled as Group B

library(car)

#Finding the sample size:
nrow(groupB.data)
##Output:
#30  (sample size is equal to 30?)

#Checking the distribution of Data:
hist(groupB.data$Platelet,   
     main = "Histogram of groupB data", # changes the Legend Header/title #comma is neeeded
     xlab = "log10 platelet counts",   #changes the X-axis label  
     col = "lightblue")
#data is normally distributed
qqPlot(groupB.data$Platelet)
##data is normally distributed

#Determining the Variance (looking at sd similarty and bartlett test results):
sd(groupA.data$Platelet) #Output: 0.3971375
sd(groupB.data$Platelet) #Output: 0.4383884
sd(groupC.data$Platelet) #Output: 0.4635171
sd(groupD.data$Platelet) #Output: 0.3706927
#all standard deviations are silimar, equal variance
bartlett.test(thrombocytopaenia.data$Platelet ~ thrombocytopaenia.data$NECGroup) 
# p-value = 0.5077, Result is highly indicative (much greater than 0.05), it suggests equal variances
###############################################
#####GroupC data: 
#About: Group 2 referred to patients with two or more intestinal segments of gangrene, Group2 is labeled as Group C

library(car)

#Finding the sample size:
nrow(groupC.data)
##Output:
#54 (sample size is equal to 30)

#Checking the distribution of Data:
hist(groupC.data$Platelet,   
     main = "Histogram of groupC data", # changes the Legend Header/title #comma is neeeded
     xlab = "log10 platelet counts",   #changes the X-axis label  
     col = "lightblue")
#data is normally distributed
qqPlot(groupC.data$Platelet)
##data is normally distributed

#Determining the Variance (looking at sd similarty and bartlett test results):
sd(groupA.data$Platelet) #Output: 0.3971375
sd(groupB.data$Platelet) #Output: 0.4383884
sd(groupC.data$Platelet) #Output: 0.4635171
sd(groupD.data$Platelet) #Output: 0.3706927
#all standard deviations are silimar, equal variance
bartlett.test(thrombocytopaenia.data$Platelet ~ thrombocytopaenia.data$NECGroup) 
# p-value = 0.5077, Result is highly indicative (much greater than 0.05), it suggests equal variances
################################################
#####GroupD data: 
#About: Group 3 referred to patients with the majority of small and large bowel involved, Group 3 is labeled as GroupD.

library(car)

#Finding the sample size:
nrow(groupD.data)
##Output:
#27 (sample size is less than to 30!!!!)

#Checking the distribution of Data:
hist(groupD.data$Platelet,   
     main = "Histogram of groupD data", # changes the Legend Header/title #comma is neeeded
     xlab = "log10 platelet counts",   #changes the X-axis label  
     col = "lightblue")
#data is normally distributed
qqPlot(groupD.data$Platelet)
##data is normally distributed

#Determining the Variance (looking at sd similarty):
sd(groupA.data$Platelet) #Output: 0.3971375
sd(groupB.data$Platelet) #Output: 0.4383884
sd(groupC.data$Platelet) #Output: 0.4635171
sd(groupD.data$Platelet) #Output: 0.3706927
#all standard deviations are silimar, equal variance
bartlett.test(thrombocytopaenia.data$Platelet ~ thrombocytopaenia.data$NECGroup) 
# p-value = 0.5077, Result is highly indicative (much greater than 0.05), it suggests equal variances
###############################################

########DATA SUMMARY:
#Since the sample size/normality is equal for each group, we summarize with:
##sample size, mean standard deviation and 95%CI

#Group A Data Summary:
nrow(groupA.data)            #sample size = 67
mean(groupA.data$Platelet)   #mean = 2.182985
sd(groupA.data$Platelet)     #standard deviation = 0.3971375
t.test(groupA.data$Platelet, alternative = "two.sided", var.equal = TRUE) 
#95% CI: (2.086116, 2.279855)

#Group B Data Summary:
nrow(groupB.data)            #sample size = 30
mean(groupB.data$Platelet)   #mean = 2.081333
sd(groupB.data$Platelet)     #standard deviation = 0.4383884
t.test(groupB.data$Platelet, alternative = "two.sided", var.equal = TRUE) 
#95% CI: (1.917636, 2.245030)

#Group C Data Summary:
nrow(groupC.data)            #sample size = 54
mean(groupC.data$Platelet)   #mean = 2.004815 
sd(groupC.data$Platelet)     #standard deviation = 0.4635171
t.test(groupC.data$Platelet, alternative = "two.sided", var.equal = TRUE) 
#95% CI: (1.878299, 2.131331)

#Group D Data Summary:
nrow(groupD.data)            #sample size = 27
mean(groupD.data$Platelet)   #mean = 1.828519
sd(groupD.data$Platelet)     #standard deviation = 0.3706927
t.test(groupD.data$Platelet, alternative = "two.sided", var.equal = TRUE) 
#95% CI: (1.681877, 1.975160)

#########Equal Variance ANOVA TEST:
thrombocytopaenia.data.ANOVA <- aov(thrombocytopaenia.data$Platelet ~ thrombocytopaenia.data$NECGroup)
#Ouput:
# Call:
#aov(formula = thrombocytopaenia.data$Platelet ~ thrombocytopaenia.data$NECGroup)
# Terms:
#   thrombocytopaenia.data$NECGroup Residuals
# Sum of Squares                         2.637573 30.942439
# Deg. of Freedom                               3       174
# Residual standard error: 0.4216991
# Estimated effects may be unbalanced
summary(thrombocytopaenia.data.ANOVA)
##Output: 
#                                   Df Sum Sq  Mean Sq  F value  Pr(>F)   
# thrombocytopaenia.data$NECGroup   3  2.638   0.8792   4.944     0.00255 **   #####p-value less than 0.05 (reject Ho)
# Residuals                       174 30.942  0.1778 
##Becasue the F-test gave us a p-value less than 0.05, we will reject the Null Hypothesis in favor of the alternative
#Now than we know one of the samples means are different, we will use a multiple comparision test to find out which one it is.  

#Tukey Test (multiple comparsion test, to find the different sample):
TukeyHSD(thrombocytopaenia.data.ANOVA, ordered = TRUE)   #diff show the difference of the means, report the standard error (not the sd here)
##Output:
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# Fit: aov(formula = thrombocytopaenia.data$Platelet ~ thrombocytopaenia.data$NECGroup)
# $`thrombocytopaenia.data$NECGroup`
#         diff         lwr       upr     p adj
# C-D 0.17629630 -0.08153854 0.4341311 0.2895170
# B-D 0.25281481 -0.03736872 0.5429984 0.1115784
# A-D 0.35446656  0.10510902 0.6038241 0.0017074    #Significant (< 0.05)!!
# B-C 0.07651852 -0.17257360 0.3256106 0.8557834
# A-C 0.17817026 -0.02187868 0.3782192 0.0996042
# A-B 0.10165174 -0.13865486 0.3419583 0.6916619

###############################################################################################
#Problem 2.) Correlation question: perform analysis to determine if there is evidience to indiciate that the age (years)
##is correlated with any of the threee outcome variables. alpha = 0.05 

#Ho: ρ = 0, there is no association.
#Ha: ρ ≠ 0, there is an association.

#Reading in the dataset
hyperchildren.data <- read.csv(file.choose(), header = TRUE)
##Output:
#    Subject AGE  ATT  HYP  SOC
# 1        1   9 -1.2 -1.2  0.0     #negative HYP (hyperactivity) score indicates an improvement in hyperactivity
# 2        2   9  0.0  0.0  1.0     #postive ATT (attitude) or SOC (social behavior) score indicates an improvement in hyperactivity
# 3        3  13 -0.4  0.0  0.2
# 4        4   6 -0.4 -0.2  1.2
# 5        5   9  1.0 -0.8  0.2
# 6        6   8  0.8  0.2  0.4
# 7        7   8 -0.6 -0.2  0.6
# 8        8   9 -1.2 -0.8 -0.6
# 9        9   7  0.0  0.2  0.8
# 10      10  12  0.4 -0.8  0.4
#Size:
nrow(hyperchildren.data) #Sample Size: 31
mean(hyperchildren.data$AGE)
mean(hyperchildren.data$HYP)
mean(hyperchildren.data$SOC)

#Seeing if there is an Assoication (3 outcomes) with a scatterplots:
plot(hyperchildren.data$AGE, hyperchildren.data$ATT,
     xlab = "Age",
     ylab = "ATT",
     main = "Attitude versus Age")
#slight positive correlation
plot(hyperchildren.data$AGE, hyperchildren.data$HYP,
     xlab = "Age",
     ylab = "HYP",
     main = "Hyperactivity versus Age")
#graph indicates a very slight correlation
plot(hyperchildren.data$AGE, hyperchildren.data$SOC,
     xlab = "Age",
     ylab = "SOC",
     main = "Social Behavior versus Age")
#graph indcates no correlation 

############## Hypothesis Testing:
#Test of Correlation between ages the three outcome variables (attention, hyperactivity, and social behavior):
#Ho: ρ = 0, there is no association.
#Ha: ρ ≠ 0, there is an association.

#Testing Attitude versus Age:
cor.test(hyperchildren.data$AGE,hyperchildren.data$ATT,
         alternative = "two.sided",
         method = "pearson")
##Output:
#Pearson's product-moment correlation
# data:  hyperchildren.data$AGE and hyperchildren.data$ATT
# t = 2.3436, df = 29, p-value = 0.02616     #####p-value is less than 0.05, reject the null#####
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.05207162 0.66005750
# sample estimates:
#       cor 
# 0.3990489 
##Reject the null in favor of the alternative hypothesis#####

#Testing Hyperactivity versus Age:
cor.test(hyperchildren.data$AGE,hyperchildren.data$HYP,
         alternative = "two.sided",
         method = "pearson")
##Output:
# Pearson's product-moment correlation
# data:  hyperchildren.data$AGE and hyperchildren.data$HYP
# t = 0.054905, df = 29, p-value = 0.9566     #####p-value is greater than 0.05, fail to reject the null#####
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.3453927  0.3632231
# sample estimates:
#        cor 
# 0.01019513 
##Fail to reject the Null hypothesis##

#Testing Social Behavior versus Age:
cor.test(hyperchildren.data$AGE,hyperchildren.data$SOC,
         alternative = "two.sided",
         method = "pearson")
##Output:
# Pearson's product-moment correlation
# data:  hyperchildren.data$AGE and hyperchildren.data$SOC
# t = -1.0147, df = 29, p-value = 0.3187     #####p-value is greater than 0.05, fail to reject the null#####
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.5062822  0.1810605
# sample estimates:
#        cor 
# -0.1851589 
##Fail to reject the Null hypothesis##
