###############################################################################################
#Skyler Kuhn
#STAT 543
#HW3: Two-Sampled Proportions 
###############################################################################################

###1.) Ho: P1-P2 = 0 vs. Ha: P1-P2 ≠ 0, x1=249, x2=134, n1=529, n2=326 ,α=0.05 

#Creating a table to hold the values:
kidney_table.data <- matrix(c(249,134,280,192), nrow = 2, ncol = 2)
##Output: kidney_table.data
#[1,]  249  280
#[2,]  134  192

expKidneyVal <- chisq.test(kidney_table.data, correct = FALSE)
##Output: expKidenyVal
#Pearson's Chi-squared test with Yates' continuity correction
# data:  kidney_table.data
#X-squared = 2.9028, df = 1, p-value = 0.08843
#z = 1.704

#Finding the expected values for Sample1 and Sample2:
expKidneyVal$expected
# Outcome: expKidenyVal$expected
# [1,] 236.9673 292.0327 #Expected Values for sample1
# [2,] 146.0327 179.9673 #Expected Values for sample2

#Finding the Test Stastistic, p-value, phat1, phat2, and CI (difference between the two proportions)
prop.test(c(249,134),c(529,326), alternative = "two.sided", correct = FALSE)
##Output: prop.test(c(249,134),c(529,326), alternative = "two.sided", correct = FALSE)
# 2-sample test for equality of proportions without continuity correction
# data:  c(249, 134) out of c(529, 326)
# X-squared = 2.9028, df = 1, p-value = 0.08843
# alternative hypothesis: two.sided
# 95 percent confidence interval:    #CI (Difference between the two proportions)
#   -0.008621297  0.127934273
# sample estimates:
#   prop 1    prop 2 
# 0.4706994 0.4110429 #phat1 and phat2

#Finding the CI for Sample1
phat <- 0.4707
CVforCI <-1.96
n <- 529
se <- sqrt(phat*(1 - phat)/n)
ci_1 <- phat + (CVforCI * se)
ci_2 <- phat - (CVforCI * se)
ci_1
ci_2
#Ouput: CI for Sample1
# (0.4281645, 0.5132355)

#Finding the CI for Sample2
phat <- 0.4110
CVforCI <-1.96
n <- 326
se <- sqrt(phat*(1 - phat)/n)
ci_1 <- phat + (CVforCI * se)
ci_2 <- phat - (CVforCI * se)
ci_1
ci_2
#Ouput: CI for Sample2
# (0.3575896, 0.4644104)

###############################################################################################
###2.) Ho: P1-P2 = 0 vs. Ha: P1-P2 ≠ 0, x1=21, x2=48, n1=150, n2=200,α=0.05 

#Creating a table to hold the values:
genderObesity_table.data <- matrix(c(21,48,129,152), nrow = 2, ncol = 2)
##Output: genderObesity_table.data
# [1,]   21  129
# [2,]   48  152

expGenderObesityVal <- chisq.test(genderObesity_table.data, correct = FALSE)
##Output: expGenderObesityVal
# Pearson's Chi-squared test with Yates' continuity correction
# data:  genderObesity_table.data
# X-squared = 5.4154, df = 1, p-value = 0.01996
# z = 2.327

#Finding the expected values for sample1 and sample2:
expGenderObesityVal$expected
##Output: expGenderObesityVal$expected
# [1,] 29.57143 120.4286  #Expected Values for sample1
# [2,] 39.42857 160.5714  #Expected Values for sample2

#Finding the Test Stastistic, p-value, phat1, phat2, and CI (difference between the two proportions):
prop.test(c(21,48),c(150,200), alternative = "two.sided", correct = FALSE)
##Output: prop.test(c(21,48),c(150,200), alternative = "two.sided", correct = FALSE)
# 2-sample test for equality of proportions without continuity correction
# data:  c(21, 48) out of c(150, 200)
# X-squared = 5.4154, df = 1, p-value = 0.01996
# alternative hypothesis: two.sided
# 95 percent confidence interval:    #CI (Difference between the two proportions)
#   -0.18115923 -0.01884077
# sample estimates:
#   prop 1 prop 2 
# 0.14   0.24    #phat1 and phat2

#Finding the CI for Sample1
phat <- 0.14
CVforCI <-1.96
n <- 150
se <- sqrt(phat*(1 - phat)/n)
ci_1 <- phat + (CVforCI * se)
ci_2 <- phat - (CVforCI * se)
ci_1
ci_2
#Ouput: CI for Sample1
#(0.08447051,0.1955295)

#Finding the CI for Sample2
phat <- 0.24
CVforCI <-1.96
n <- 200
se <- sqrt(phat*(1 - phat)/n)
ci_1 <- phat + (CVforCI * se)
ci_2 <- phat - (CVforCI * se)
ci_1
ci_2
#Ouput: CI for Sample2
#(0.1808093, 0.2991907)

###############################################################################################





