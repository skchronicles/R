###############################################################################################
#Skyler Kuhn
#STAT 543: Assignment 5
#HW5: One-Sample Means
###############################################################################################
#Problem 1.)

#Creating a table for the data:
eyeDoc.data <- c(62,62,68,48,51,60,51,57,
                 57,41,62,50,53,34,62,61)
##Output:
# > eyeDoc.data
# [1] 62 62 68 48 51 60 51 57 57 41 62 50 53 34 62 61

#Creating a histogram to look at distribution:
hist(eyeDoc.data,   
     main = "Histogram of eyeDoc data", # changes the Legend Header/title #comma is neeeded
     xlab = "Age (Years)",   #changes the X-axis label  
     col = "green")
#The data are not normally distributed; we will perform a wilcox test.

#Creating a QQplot to look at distribution:
# install.packages("car")    #does not work!
# library(car)
# qqplot(eyeDoc.data,
#        ylab = "Age (Years)",
#        main = "QQ-Plot of eyeDoc data")

#Creating a QQplot to look at distribution:
qqnorm(eyeDoc.data,
       ylab = "Age (Years)",
       main = "QQ-Plot of eyeDoc data"
       )
qqline(eyeDoc.data)
#The data are not normally distributed; we will perform a wilcox test.

##Not Normally distributed data (Wilcox-test): summarize with sample size, median, IQR
#Performing a Wilcox Test (data is not normally distributed):
wilcox.test(eyeDoc.data,
            mu = 60,
            alternative = "less")
##Output:
# Wilcoxon signed rank test with continuity correction
# data:  eyeDoc.data
# V = 24, p-value = 0.02163   ####Reject the Ho#####
# alternative hypothesis: true location is less than 60
####Reject the Ho#####

#Finding the median:
median(eyeDoc.data)
##Output:
#57

#Finding the IQR:
summary(eyeDoc.data)
##Output:
# Min.    1st Qu.  Median   Mean    3rd Qu.  Max. 
# 34.00   50.75    57.00    54.94   62.00    68.00 

###############################################################################################
#Problem 2.)

#Creating a table for the data:
maxvolValue.data <- c(132,33,91,108,67,169,54,203,190,133,
                      96,30,187,21,63,166,84,110,157,138)
#Output:
# > maxvolValue.data
# [1] 132  33  91 108  67 169  54 203 190 133  96  30 187  21  63 166  84 110 157 138

hist(maxvolValue.data,   
     main = "Histogram of Max Voluntary Value Data", # changes the Legend Header/title #comma is neeeded
     xlab = "Maximum Rate (liters/min)",   #changes the X-axis label  
     col = "lightblue")
#The data are normally distributed; we will perform a T-test.

#Creating a QQplot to look at distribution:
qqnorm(maxvolValue.data,
       ylab = "Maximum Rate (liters/min)",
       main = "QQ-Plot of Maximum Voluntary Value data"
)
qqline(maxvolValue.data)
#The data are normally distributed; we will perform a T-test!

##Normally distributed data (T-test): summarize with sample size, mean, standard deviation and 95%CI
#Performing a T-test (data is normally distributed)
t.test(maxvolValue.data,
       mu = 110,
       alternative = "two.sided",
       conf.level = 0.99)
##Output:
# One Sample t-test
# data:  maxvolValue.data
# t = 0.12709, df = 19, p-value = 0.9002    ####pvalue, fail to reject####
# alternative hypothesis: true mean is not equal to 110
# 95 percent confidence interval:
#   85.24933 137.95067
# sample estimates:
# mean of x 
# 111.6 
####fail to reject Ho####

#Finding the 95%CI:
t.test(maxvolValue.data,
       conf.level = 0.99)
##Output:
# 99 percent confidence interval:
#   75.58151 147.61849

#Finding the SD:
sd(maxvolValue.data)
##Output:
# 56.30313

