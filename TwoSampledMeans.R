###############################################################################################
#Skyler Kuhn
#STAT 543: Assignment 6
#HW5: Two-Sample Means
###############################################################################################
#Problem 1.)

#Creating two tables for the data:
sensoryexp.data <- c(10.2,9.5,10.1,10.0,9.8,10.9,11.4,10.8,9.7,10.4)
sensorycontrol.data <- c(11.0,11.2,10.1,11.4,11.7,11.2,10.8,11.6,10.9,10.9)

#Creating a histogram of sensoryexp.data to look at distribution:
hist(sensoryexp.data,   
     main = "Histogram of sensoryexp Data", # changes the Legend Header/title #comma is neeeded
     xlab = "Alpha-Wave frequency level",   #changes the X-axis label  
     col = "green")

#Creating a QQPlot of sensoryexp.data to look at distribution:
qqnorm(sensoryexp.data,
       ylab = "Alpha-Wave frequency level",
       main = "QQ-Plot of sensoryexp data"
)
qqline(sensoryexp.data)
#The data are normally distributed.

#Creating a histogram of sensorycontrol.data to look at distribution:
hist(sensorycontrol.data,   
     main = "Histogram of sensorycontrol Data", # changes the Legend Header/title #comma is neeeded
     xlab = "Alpha-Wave frequency level",   #changes the X-axis label  
     col = "green")

#Creating a QQPlot of sensorycontrol.data to look at distribution:
qqnorm(sensorycontrol.data,
       ylab = "Alpha-Wave frequency level",
       main = "QQ-Plot of sensorycontrol data"
)
qqline(sensorycontrol.data)
#The data are normally distributed.

#Checking the variance between the two samples:
var.test(sensoryexp.data,sensorycontrol.data)
##Output: p-value = 0.4433. Result is highly indicative (much greater than 0.05), it suggests equal variances.
sd(sensoryexp.data)
##Output:0.5977736, 
sd(sensorycontrol.data)
##Output: 0.4589844
##The two sd are similar which suggest variances are similar
#Weighing the results of both methods, it is clear that the variance between both samples is SIMILAR or EQUAL. 

##Normally distributed data (T-test): summarize with sample sizes, means, standard deviations and 95%CIs
#and summarize the difference with obseverved differnce, standard error, and 95%CI.
#Performing a T-test (data is normally distributed)
t.test(sensoryexp.data, sensorycontrol.data, alternative = "two.sided", conf.level = 0.95, var.equal = TRUE)
#Output:
# Two Sample t-test
# data:  sensoryexp.data and sensorycontrol.data
# t = -3.3567, df = 18, p-value = 0.003512    ######reject the Ho########
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.3007075 -0.2992925
# sample estimates:
#   mean of x mean of y 
# 10.28     11.08 
######reject the Ho########

#########SUMMARIZING THE DATA:
#Finding the standard deviations of the two samples:
##Standard Deviation of sensoryexp:
sd(sensoryexp.data)
#Output:
# 0.5977736
##Standard Deviation of sensorycontrol:
sd(sensorycontrol.data)
#Output:
# 0.4589844

#Finding the 95% CIs of the two samples:
##95% CI of sensoryexp:
t.test(sensoryexp.data, conf.level = 0.95)
#Output:
# (9.852378, 10.707622)
##95% CI of sensorycontrol:
t.test(sensorycontrol.data, conf.level = 0.95)
#Output:
# (10.75166, 11.40834)

#Finding the obsevered difference and standard error:
diff <- mean(sensoryexp.data) - mean(sensorycontrol.data)
nexp <- 10
ncotrol <- 10
sdexp <- sd(sensoryexp.data)
sdcontrol <- sd(sensorycontrol.data)
sp <- ((nexp - 1) * sdexp * sdexp + (ncotrol - 1) * sdcontrol * sdcontrol) / (nexp + ncotrol - 2)
sep <- sqrt(sp*(1/nexp+1/ncotrol)) #pooled standard error, used if variance is equal 
##Output: diff (difference of sampled means)
# -0.8
##Output: standard error, pooled (for equal variance)
#0.2383275

###############################################################################################
#Problem 2.)

#Creating two tables for the data:
strainA.data <- c(54,99,105,46,70,87,55,58,139,91)
strainB.data <- c(93,91,93,150,80,104,128,83,88,95,94,97)

#Creating a histogram of strainA.data to look at distribution:
hist(strainA.data,   
     main = "Histogram of strainA data", # changes the Legend Header/title #comma is neeeded
     xlab = "plasma glucose level",   #changes the X-axis label  
     col = "lightblue")
#The data are not normally distributed.

#Creating a QQPlot of strainA.data to look at distribution:
qqnorm(strainA.data,
       ylab = "plasma glucose level",
       main = "QQ-Plot of strainA data"
)
qqline(strainA.data)
#The data are not normally distributed.

#Creating a histogram of strainB.data to look at distribution:
hist(strainB.data,   
     main = "Histogram of strainB data", # changes the Legend Header/title #comma is neeeded
     xlab = "plasma glucose level",   #changes the X-axis label  
     col = "lightblue")
#The data are not normally distributed.

#Creating a QQPlot of strainB.data to look at distribution:
qqnorm(strainB.data,
       ylab = "plasma glucose level",
       main = "QQ-Plot of strainB data"
)
qqline(strainB.data)
#The data are not normally distributed.

#Checking the variance between the two samples:
var.test(strainA.data,strainB.data)
##Output: p-value = 0.2336, suggests similar variances (equal)
sd(strainA.data)
##Output:29.20502 
sd(strainB.data)
##Output: 19.95601
##The difference between both sd's (about 9 sds). Similar results, but not highly indicative.
#Both methods suggest the variance is similar or EQUAL.

##Not Normally distributed data-- CLT does not apply. Use Wilcoxon sum rank test: summarize groups with sample sizes, medians, IQRs.
#Performing a Wilcox Test (data is not normally distributed):
wilcox.test(strainA.data, strainB.data, mu=0, alternative = "two.sided")
##Output:
# Wilcoxon rank sum test with continuity correction
# data:  strainA.data and strainB.data
# W = 35.5, p-value = 0.1133    ########pvalue, fail to reject########
# alternative hypothesis: true location shift is not equal to 0
####Fail to reject the Ho#####

#########SUMMARIZING THE DATA:
#Finding the median of the two samples:
##StrainA median:
median(strainA.data)
#Output:
# 78.5
##StrainB median:
median(strainB.data)
#Output:
# 93.5

#Finding the IQRs of the two samples:
##StrainA IQR:
summary(strainA.data)
#Output:
# Min.    1st Qu.  Median  Mean   3rd Qu.  Max. 
# 46.00   55.75    78.50   80.40  97.00    139.00 
##StrainB IQR:
summary(strainB.data)
#Output:
# Min.    1st Qu. Median  Mean   3rd Qu.  Max. 
# 80.00   90.25   93.50   99.67  98.75    150.00 

library(car)
qqPlot(strainA.data) #it works now :}
