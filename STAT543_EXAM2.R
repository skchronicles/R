###############################################################################################
#Skyler Kuhn
#STAT 543
#Exam II
###############################################################################################
#Problem 1.) Ho: μ = μo, Ha: μ < μo, μo = 30, alpha = 0.05, n = 50

#Creating two tables for the data:
sickleDeath.data <- c(15.5,2.0,45.1,1.7,0.8,1.1,18.2,9.7,28.1,18.2,
                      27.6,45.0,1.0,66.4,2.0,67.4,2.5,61.7,16.2,31.7,
                      6.9,13.5,1.9,31.2,9.0,2.6,29.7,13.5,2.6,14.4,
                      20.7,30.9,36.6,1.1,23.6,0.9,7.6,23.5,6.3,40.2,
                      23.7,4.8,33.2,27.1,36.7,3.2,38.0,3.5,21.8,2.4)

#Creating a histogram to look at distribution (not needed bc sample size is > 30, but doing it anyway):
hist(sickleDeath.data,   
     main = "Histogram of sickleDeath.data", # changes the Legend Header/title #comma is neeeded
     xlab = "Age of Death (Years)",   #changes the X-axis label  
     col = "lightblue")
library(car)
qqPlot(sickleDeath.data,
       ylab = "Age of Death (Years)",
       main = "QQ-Plot of sickleDeath data")
#Again, these plots are not needed because we will assume the CLT holds true (the sample size is > 30).
##I created these plots purely out of curiosity.

##Normally distributed data (T-test): summarize with sample size, mean, standard deviation and 95%CI
#Performing a T-test (data is normally distributed)
t.test(sickleDeath.data,
       mu = 30,
       alternative = "less",
       conf.level = 0.95)
##Output:
# One Sample t-test
# data:  sickleDeath.data
# t = -4.183, df = 49, p-value = 5.933e-05   -->  ######p-value < 0.05, Reject the Ho######
# alternative hypothesis: true mean is less than 30
# 95 percent confidence interval:
#   -Inf 23.68444
# sample estimates:
#   mean of x 
# 19.46 
######p-value < 0.05, Reject the Ho######

#########SUMMARIZING THE DATA (Sample size, mean, standard deviation and 95%CI):
#Finding the mean:
mean(sickleDeath.data)
##Output: 
# 19.46

#Finding the Standard Deviation:
sd(sickleDeath.data)
##Output: 
# 17.81711

#Finding the 95% CI:
t.test(sickleDeath.data,
       conf.level = 0.95)
##Output: 
#95 percent confidence interval:
# (14.39643, 24.52357)

###############################################################################################
#Problem 2.) 

##Question 1: Can we conclude that the probability of treatment success 
#was significantly different in the two treatment groups (bup-nx, clonidine)?

#Ho: P1-P2 = 0 vs. Ha: P1-P2 ≠ 0

#Reading in the 
opiateDetox.data <- read.csv(file.choose(), header = TRUE)
head(opiateDetox.data)
##Output:
# SubID Arm Success Gender Race Marital.Status Education Employment Age
# 1 02_002261   1       2      2    1              2         3          1   1
# 2 02_002601   1       2      1    1              2         3          1   1
# 3 02_004074   2       2      2    2              2         3          2   2
# 4 02_004099   1       1      1    1              2         3          3   1
# 5 02_004104   1       1      1    2              2         2          1   1
# 6 02_005169   2       2      2    2              2         1          2   1
opiateData.sampleSize <- nrow(opiateDetox.data) #sample size
##Output: (Sample size)
# 229
table(opiateDetox.data$Success)  #total number of successes (1 = sucsess)/ failures (2 = failure)
##Output:
# 1   2 
# 48  181 

#Creating a Dataset of Successful bup-nx treatments
bupnxSuccess.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 1, ]  
#$Arm == 1, means bup-nx treatmnet, $Success == 1, means successful treatmnet
nrow(bupnxSuccess.data) #finding the number of successful bup-nx treatments
##Output:
#44

#Creating a Dataset of failed bup-nx treatments
bupnxfailure.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 2, ]  
#$Arm == 1, means bup-nx treatmnet, $Success == 2, means not uccessful treatmnet
nrow(bupnxfailure.data) #finding the number of not successful bup-nx treatments
##Output:
#112

#Creating a Dataset of Successful clonidine treatments
clonidineSuccess.data <- opiateDetox.data[opiateDetox.data$Arm == 2 & opiateDetox.data$Success == 1, ] 
#$Arm == 2, means clonidipine treatmnet, $Success == 1, means successful treatmnet
nrow(clonidineSuccess.data) #finding the number of successful clonidine treatments
##Output:
#4

#Creating a Dataset of failed clonidine treatments
clonidinefailure.data <- opiateDetox.data[opiateDetox.data$Arm == 2 & opiateDetox.data$Success == 2, ] 
#$Arm == 2, means clonidipine treatmnet, $Success == 2, means not successful treatmnet
nrow(clonidinefailure.data) #finding the number of not successful clonidine treatments
##Output:
#69


#Creating a table to hold a success of the two groups:
bothTreatmentSuccess.data <- matrix(c(44,4,112,69), nrow = 2, ncol = 2)
##Output:
#       [,1] [,2]
# [1,]   44  112  #number of successful(44)/failed(112) bup-nx treatments
# [2,]    4   69  #number of succesful(4)/failed(69) clonidine treatments 

#Finding expected vaules
expTreatmentVal <- chisq.test(bothTreatmentSuccess.data, correct = FALSE)
expTreatmentVal$expected
##Output of expected values (of bothTreatmentSuccess.data)
#     [,1]      [,2]
# [1,] 32.69869 123.30131  #expected number of successful/failed bup-nx treatments
# [2,] 15.30131  57.69869  #expected number of successful/failed clonidine treatments

#Finding the Test Stastistic, p-value, phat1, phat2, and CI (difference between the two proportions):
prop.test(c(44,4),c(156,73), alternative = "two.sided", correct = FALSE)
##Output:
# 2-sample test for equality of proportions without continuity correction
# data:  c(44, 4) out of c(156, 73)
# X-squared = 15.502, df = 1, p-value = 8.24e-05    #####p-value < 0.05, reject the Ho######
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   0.1394393 0.3150742
# sample estimates:
#   prop 1     prop 2 
# 0.28205128 0.05479452 
#####p-value < 0.05, reject the Ho######

#Finding the CI for bup-nx sample
phat <- 0.28
CVforCI <-1.96
n <- 156
se <- sqrt(phat*(1 - phat)/n)
ci_1 <- phat + (CVforCI * se)
ci_2 <- phat - (CVforCI * se)
ci_1
ci_2
#Ouput: CI for Sample1
#(0.2095406, 0.3504594)
confidence.int(phat = 0.28, po=0.28, cv=1.96, n=156)

#Finding the CI for clonidine sample
phat <- 0.055
CVforCI <-1.96
n <- 73
se <- sqrt(phat*(1 - phat)/n)
ci_1 <- phat + (CVforCI * se)
ci_2 <- phat - (CVforCI * se)
ci_1
ci_2
#Ouput: CI for Sample2
#(0.00270118, 0.1072988)
confidence.int(phat = 0.055, po=0.055, cv=1.96, n=73)
#################################
#Problem 2.)
#Question2: In addition, within only the bup-nx treatment group, are there significant relationships 
## between treatment success and each of the following variables: 
## age, gender, race, marital status, level of education, and employment history? 


head(opiateDetox.data)
##Output:
# SubID Arm Success Gender Race Marital.Status Education Employment Age
# 1 02_002261   1       2      2    1              2         3          1   1
# 2 02_002601   1       2      1    1              2         3          1   1
# 3 02_004074   2       2      2    2              2         3          2   2
# 4 02_004099   1       1      1    1              2         3          3   1
# 5 02_004104   1       1      1    2              2         2          1   1
# 6 02_005169   2       2      2    2              2         1          2   1

##Creating a Dataset of Successful/Failed bup-nx treatments for each variable:
table(opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 1) #44 successes
table(opiateDetox.data$Arm == 2 & opiateDetox.data$Success == 1) #4 sucess

###Successful bup-nx treatments (GENDER):#########
maleGenderBupnxSuccess.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 1 & opiateDetox.data$Gender== 1, ]  
#$Arm == 1 means bup-nx treatmnet, $Success == 1 means successful treatmnet, $Gender == 1 means male
nrow(maleGenderBupnxSuccess.data) #finding the number of successful male bup-nx treatments
##Output:
#31
femaleGenderBupnxSuccess.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 1 & opiateDetox.data$Gender== 2, ]  
#$Arm == 1 means bup-nx treatmnet, $Success == 1 means successful treatmnet, $Gender == 2 means female
nrow(femaleGenderBupnxSuccess.data) #finding the number of successful male bup-nx treatments
##Output:
#13
###Failed bup-nx treatments (GENDER):#########
maleGenderBupnxFail.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 2 & opiateDetox.data$Gender== 1, ]  
#$Arm == 1 means bup-nx treatmnet, $Success == 2 means not successful treatmnet, $Gender == 1 means male
nrow(maleGenderBupnxFail.data) #finding the number of successful male bup-nx treatments
##Output:
#83
femaleGenderBupnxFail.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 2 & opiateDetox.data$Gender== 2, ]  
#$Arm == 1 means bup-nx treatmnet, $Success == 2 means not successful treatmnet, $Gender == 2 means female
nrow(femaleGenderBupnxFail.data) #finding the number of successful male bup-nx treatments
##Output:
#29
genderSuccessOutlook.data <- matrix(c(31,13,83,29), nrow = 2, ncol = 2)
##Output:
#       [,1] [,2]
# [1,]   31   83  #number of successes/fails for males
# [2,]   13   29  #number of successes/fails for females
chisq.test(genderSuccessOutlook.data, correct = FALSE)
##Output:
# Pearson's Chi-squared test with Yates' continuity correction
# data:  genderSuccessOutlook.data
# X-squared = 0.21421, df = 1, p-value = 0.6435 (fail to reject null)
####Fail to reject the Null!#####
prop.test(genderSuccessOutlook.data, conf.level = 0.95)



###Successful bup-nx treatments (AGE):#########
younger40BupnxSuccess.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 1 & opiateDetox.data$Age ==1, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 1 means successful treatmnet, $Age ==1 means < 40 yo
nrow(younger40BupnxSuccess.data)
##Output:
#28
older40BupnxSuccess.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 1 & opiateDetox.data$Age ==2, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 1 means successful treatmnet, $Age ==2 means > 40 yo
nrow(older40BupnxSuccess.data)
##Output:
#16
###Failed bup-nx treatments (AGE):#########
younger40Bupnxfailed.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 2 & opiateDetox.data$Age ==1, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 2 means not successful treatmnet, $Age ==1 means < 40 yo
nrow(younger40Bupnxfailed.data)
##Output:
#51
older40Bupnxfailed.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 2 & opiateDetox.data$Age ==2, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 2 means not successful treatmnet, $Age ==2 means > 40 yo
nrow(older40Bupnxfailed.data)
##Output:
#61
ageSuccessOutlook.data <- matrix(c(28,16,51,61), nrow = 2, ncol = 2)
##Output:
#       [,1] [,2]
# [1,]   28   51  #number of successes/fails for ages < 40
# [2,]   16   61  #number of successes/fails for ages > 40
chisq.test(ageSuccessOutlook.data, correct = FALSE)
##Output:
# Pearson's Chi-squared test with Yates' continuity correction
# data:  ageSuccessOutlook.data
# X-squared = 4.1406, df = 1, p-value = 0.04187 (reject the null)
####Reject the Null!#####
prop.test(ageSuccessOutlook.data, conf.level = 0.95)



##Successful bup-nx treatments (RACE):#########
whiteBupnxSuccess.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 1 & opiateDetox.data$Race == 1, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 1 means successful treatmnet, $Race == 1 means white 
nrow(whiteBupnxSuccess.data)
##Output:
#20
not_whiteBupnxSuccess.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 1 & opiateDetox.data$Race == 2, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 1 means successful treatmnet, $Race == 2 means not white 
nrow(not_whiteBupnxSuccess.data)
##Output:
#24
###Failed bup-nx treatments (RACE):#########
whiteBupnxfailed.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 2 & opiateDetox.data$Race == 1, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 2 means not successful treatmnet, $Race == 1 means white 
nrow(whiteBupnxfailed.data)
##Output:
#42
not_whiteBupnxfailed.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 2 & opiateDetox.data$Race == 2, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 2 means not successful treatmnet, $Race == 2 means not white 
nrow(not_whiteBupnxfailed.data)
##Output:
#70
raceSuccessOutlook.data <- matrix(c(20,24,42,70), nrow = 2, ncol = 2)
##Output:
#       [,1] [,2]
# [1,]   20   42  #number of successes/fails for white
# [2,]   24   70  #number of successes/fails for non-white
chisq.test(raceSuccessOutlook.data, correct = FALSE)
##Output:
# Pearson's Chi-squared test with Yates' continuity correction
# data:  raceSuccessOutlook.data
# X-squared = 0.83465, df = 1, p-value = 0.3609 (fail to reject the null)
####Fail to reject the Null!#####
prop.test(raceSuccessOutlook.data, conf.level = 0.95)



##Successful bup-nx treatments (Martial Status):
marriedBupnxSuccess.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 1 & opiateDetox.data$Marital.Status == 1, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 1 means successful treatmnet, $Marital.Status == 1 means married 
nrow(marriedBupnxSuccess.data)
##Output:
#10
not_marriedBupnxSuccess.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 1 & opiateDetox.data$Marital.Status == 2, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 1 means successful treatmnet, $Marital.Status == 2 means not married 
nrow(not_marriedBupnxSuccess.data)
##Output:
#34
###Failed bup-nx treatments (MArtial Status):#########
marriedBupnxfailed.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 2 & opiateDetox.data$Marital.Status == 1, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 2 means not successful treatmnet, $Marital.Status == 1 means married 
nrow(marriedBupnxfailed.data)
##Output:
#19
not_marriedBupnxfailed.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 2 & opiateDetox.data$Marital.Status == 2, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 2 means not successful treatmnet, $Marital.Status == 2 means not married 
nrow(not_marriedBupnxfailed.data)
##Output:
#93
maritalSuccessOutlook.data <- matrix(c(10,34,19,93), nrow = 2, ncol = 2)
##Output:
#       [,1] [,2]
# [1,]   10   19  #number of successes/fails for married
# [2,]   34   93  #number of successes/fails for non-married
chisq.test(maritalSuccessOutlook.data, correct = FALSE)
##Output:
# Pearson's Chi-squared test with Yates' continuity correction
# data:  maritalSuccessOutlook.data
# X-squared = 0.69325, df = 1, p-value = 0.4051 (fail to reject)
####Fail to reject the Null!#####
prop.test(maritalSuccessOutlook.data, conf.level = 0.95)



##Successful bup-nx treatments (Education):#########
lessHighSchoolBupnxSuccess.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 1 & opiateDetox.data$Education == 1, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 1 means successful treatmnet, $Education == 1 means less than High School Education
nrow(lessHighSchoolBupnxSuccess.data)
##Output:
#10
HighSchoolBupnxSuccess.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 1 & opiateDetox.data$Education == 2, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 1 means successful treatmnet, $Education == 1 means High School Education
nrow(HighSchoolBupnxSuccess.data)
##Output:
#15
greaterHighSchoolBupnxSuccess.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 1 & opiateDetox.data$Education == 3, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 1 means successful treatmnet, $Education == 3 means greater than High School Education
nrow(greaterHighSchoolBupnxSuccess.data)
##Output:
#19
###Failed bup-nx treatments (Education):#########
lessHighSchoolBupnxfailed.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 2 & opiateDetox.data$Education == 1, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 2 means not successful treatmnet, $Education == 1 means less than High School Education
nrow(lessHighSchoolBupnxfailed.data)
##Output:
#30
HighSchoolBupnxfailed.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 2 & opiateDetox.data$Education == 2, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 2 means not successful treatmnet, $Education == 1 means High School Education
nrow(HighSchoolBupnxfailed.data)
##Output:
#44
greaterHighSchoolBupnxfailed.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 2 & opiateDetox.data$Education == 3, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 2 means not successful treatmnet, $Education == 3 means greater than High School Education
nrow(greaterHighSchoolBupnxfailed.data)
##Output:
#38
educationSuccessOutlook.data <- matrix(c(10,15,19,30,44,38), nrow = 3, ncol = 2)
##Output:
#       [,1] [,2]
# [1,]   10   30    #number of success/fails for < highschool edu 
# [2,]   15   44    #number of success/fails for highschool edu 
# [3,]   19   38    #number of success/fails for > highschool edu 
chisq.test(educationSuccessOutlook.data, correct = FALSE)
# Pearson's Chi-squared test
# data:  educationSuccessOutlook.data
# X-squared = 1.1686, df = 2, p-value = 0.5575 (fail to reject the null)
####Fail to reject the Null!#####
prop.test(educationSuccessOutlook.data, conf.level = 0.95)




###Successful bup-nx treatments (Empolyment):#########
empolyedBupnxSuccess.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 1 & opiateDetox.data$Employment == 1, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 1 means successful treatmnet, $Empolyment == 1 means employed
nrow(empolyedBupnxSuccess.data)
##Output:
#24
unempolyedBupnxSuccess.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 1 & opiateDetox.data$Employment == 2, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 1 means successful treatmnet, $Empolyment == 2 means unemployed
nrow(unempolyedBupnxSuccess.data)
##Output:
#9
otherEmpolyedBupnxSuccess.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 1 & opiateDetox.data$Employment == 3, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 1 means successful treatmnet, $Empolyment == 3 means other employment
nrow(otherEmpolyedBupnxSuccess.data)
##Output:
#11
###Failed bup-nx treatments (Empolyment):#########
empolyedBupnxfailed.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 2 & opiateDetox.data$Employment == 1, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 2 means not successful treatmnet, $Empolyment == 1 means employed
nrow(empolyedBupnxfailed.data)
##Output:
#60
unempolyedBupnxfailed.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 2 & opiateDetox.data$Employment == 2, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 2 means not successful treatmnet, $Empolyment == 2 means unemployed
nrow(unempolyedBupnxfailed.data)
##Output:
#24
otherEmpolyedBupnxfailed.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Success == 2 & opiateDetox.data$Employment == 3, ]
#$Arm == 1 means bup-nx treatmnet, $Success == 2 means not successful treatmnet, $Empolyment == 3 means other employment
nrow(otherEmpolyedBupnxfailed.data)
##Output:
#28
empolymentSuccessOutlook.data <- matrix(c(24,9,11,60,24,28), nrow = 3, ncol = 2)
# ##Output:
#       [,1] [,2]
# [1,]   24   60   #number of successes/fails for employed
# [2,]    9   24   #number of successes/fails for unemployed
# [3,]   11   28   #number of successes/fails for other employed
chisq.test(empolymentSuccessOutlook.data, correct = FALSE)
# Pearson's Chi-squared test
# data:  empolymentSuccessOutlook.data
# X-squared = 0.019734, df = 2, p-value = 0.9902 (fail to reject null)
####Fail to reject the Null!#####
prop.test(educationSuccessOutlook.data, conf.level = 0.95)


##########Sample Sizes and Demographic Group sizes (broken down):
##Age:
younger40BupnxSize.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Age ==1, ]
#$Arm == 1 means bup-nx treatmnet,  $Age ==1 means < 40 yo
nrow(younger40BupnxSize.data)
##Output:
#79
younger40ClonSize.data <- opiateDetox.data[opiateDetox.data$Arm == 2 & opiateDetox.data$Age ==1, ]
nrow(younger40ClonSize.data)
##Output:
#28
older40BupnxSize.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Age ==2, ]
nrow(older40BupnxSize.data)
##Output:
#77
older40ClonSize.data <- opiateDetox.data[opiateDetox.data$Arm == 2 & opiateDetox.data$Age ==2, ]
nrow(older40ClonSize.data)
##Output:
#45

##Gender:
maleGenderBupnxSize.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Gender== 1, ]  
#$Arm == 1 means bup-nx treatmnet, $Gender == 1 means male
nrow(maleGenderBupnxSize.data) 
##Output:
#114
femaleGenderBupnxSize.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Gender== 2, ]  
#$Arm == 1 means bup-nx treatmnet, $Gender == 2 means female
nrow(femaleGenderBupnxSize.data) 
##Output:
#42
maleGenderClonSize.data <- opiateDetox.data[opiateDetox.data$Arm == 2 & opiateDetox.data$Gender== 1, ]  
nrow(maleGenderClonSize.data) 
##Output:
#51
femaleGenderClonSize.data <- opiateDetox.data[opiateDetox.data$Arm == 2 & opiateDetox.data$Gender== 2, ]  
nrow(femaleGenderClonSize.data) 
##Output:
#22

###Race:
whiteBupnxSize.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Race == 1, ]
#$Arm == 1 means bup-nx treatmnet, $Race == 1 means white 
nrow(whiteBupnxSize.data)
##Output:
#62
not_whiteBupnxSize.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Race == 2, ]
#$Arm == 1 means bup-nx treatmnet, $Race == 2 means not white 
nrow(not_whiteBupnxSize.data)
##Output:
#94
whiteClonSize.data <- opiateDetox.data[opiateDetox.data$Arm == 2 & opiateDetox.data$Race == 1, ]
# $Race == 1 means white 
nrow(whiteClonSize.data)
##Output:
#29
not_whiteClonSize.data <- opiateDetox.data[opiateDetox.data$Arm == 2 & opiateDetox.data$Race == 2, ]
#$Race == 2 means not white 
nrow(not_whiteClonSize.data)
##Output:
#44

###Marital Status
marriedBupnxSize.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Marital.Status == 1, ]
nrow(marriedBupnxSize.data)
##Output:
#29
not_marriedBupnxSize.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Marital.Status == 2, ]
nrow(not_marriedBupnxSize.data)
##Output:
#127
marriedClonSize.data <- opiateDetox.data[opiateDetox.data$Arm == 2 & opiateDetox.data$Marital.Status == 1, ]
nrow(marriedClonSize.data)
##Output:
#7
not_marriedClonSize.data <- opiateDetox.data[opiateDetox.data$Arm == 2 & opiateDetox.data$Marital.Status == 2, ]
nrow(not_marriedClonSize.data)
##Output:
#66


###Level of Education:
##Successful bup-nx treatments (Education):#########
lessHighSchoolBupnxSize.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Education == 1, ]
nrow(lessHighSchoolBupnxSize.data)
##Output:
#40
HighSchoolBupnxSize.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Education == 2, ]
nrow(HighSchoolBupnxSize.data)
##Output:
#59
greaterHighSchoolBupnxSize.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Education == 3, ]
nrow(greaterHighSchoolBupnxSize.data)
##Output:
#57
lessHighSchoolClonSize.data <- opiateDetox.data[opiateDetox.data$Arm == 2 & opiateDetox.data$Education == 1, ]
nrow(lessHighSchoolClonSize.data)
##Output:
#21
HighSchoolClonSize.data <- opiateDetox.data[opiateDetox.data$Arm == 2 & opiateDetox.data$Education == 2, ]
nrow(HighSchoolClonSize.data)
##Output:
#29
greaterHighSchoolClonSize.data <- opiateDetox.data[opiateDetox.data$Arm == 2 & opiateDetox.data$Education == 3, ]
nrow(greaterHighSchoolClonSize.data)
##Output:
#23

###Employment:
empolyedBupnxSize.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Employment == 1, ]
nrow(empolyedBupnxSize.data)
##Output:
#84
unempolyedBupnxSize.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Employment == 2, ]
nrow(unempolyedBupnxSize.data)
##Output:
#33
otherEmpolyedBupnxSize.data <- opiateDetox.data[opiateDetox.data$Arm == 1 & opiateDetox.data$Employment == 3, ]
nrow(otherEmpolyedBupnxSize.data)
##Output:
#39
empolyedClonSize.data <- opiateDetox.data[opiateDetox.data$Arm == 2 & opiateDetox.data$Employment == 1, ]
nrow(empolyedClonSize.data)
##Output:
#40
unempolyedClonSize.data <- opiateDetox.data[opiateDetox.data$Arm == 2 & opiateDetox.data$Employment == 2, ]
nrow(unempolyedClonSize.data)
##Output:
#12
otherEmpolyedClonSize.data <- opiateDetox.data[opiateDetox.data$Arm == 2 & opiateDetox.data$Employment == 3, ]
nrow(otherEmpolyedClonSize.data)
##Output:
#21


####################################################
#Other
#Finding the CIs for Demographics

#Age: below 40
phat <- 0.354
CVforCI <-1.96
n <- 79
se <- sqrt(phat*(1 - phat)/n)
ci_1 <- phat + (CVforCI * se)
ci_2 <- phat - (CVforCI * se)
ci_1
ci_2
# > ci_1
# [1] 0.4594534
# > ci_2
# [1] 0.2485466
confidence.int(phat = 0.354, po = 0.354, cv = 1.96, n = 79)
#0.2485466 0.4594534

#Age: above 40
confidence.int(phat = 0.208, po = 0.208, cv = 1.96, n = 77)
#0.1173422 0.2986578

#Gender: male
confidence.int(phat = 0.272, po = 0.272, cv = 1.96, n = 114)
#0.1903128 0.3536872

#Gender: female
confidence.int(phat = 0.310, po = 0.310, cv = 1.96, n = 42)
#0.1701261 0.4498739

#Race: white
confidence.int(phat = 0.323, po = 0.323, cv = 1.96, n = 62)
#0.2065992 0.4394008

#Race: Not white
confidence.int(phat = 0.255, po = 0.255, cv = 1.96, n =94)
#0.1668869 0.3431131

#Marital Status: Married
confidence.int(phat = 0.345, po = 0.345, cv = 1.96, n =29)
#0.1719836 0.5180164

#Marital Status: Not Married
confidence.int(phat = 0.268, po = 0.268, cv = 1.96, n =127)
# 0.1909669 0.3450331

#Education: Less than HS
confidence.int(phat = 0.250, po = 0.250, cv = 1.96, n =40)
#0.115808 0.384192

#Education: HS
confidence.int(phat = 0.250, po = 0.250, cv = 1.96, n =59)
#

#Education: Greater than HS
confidence.int(phat = 0.333, po = 0.333, cv = 1.96, n =57)
#

#Employment:empolyed
confidence.int(phat = 0.286, po = 0.286, cv = 1.96, n =84)
#0.1893619 0.3826381

#Employment: unemployed
confidence.int(phat = 0.273, po = 0.273, cv = 1.96, n =33)
#0.1209986 0.4250014

#Employment: other
confidence.int(phat = 0.280, po = 0.280, cv = 1.96, n =39)
#0.1390812 0.4209188




