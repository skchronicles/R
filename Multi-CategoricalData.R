###############################################################################################
#Skyler Kuhn
#STAT 543: Assignment 4
#HW4: Multi-Categorical Data
###############################################################################################

#Problem 1.) Ho: Py1 = Py2 = Py3 = Py4 vs. Ha: Pyi ≠ Pyj (for at least one combination), α=0.05 
#Depending on the practice type, whether doctors perfromed the baseline test (measured in y/n)

#Creating a table for the data:
baseline_table.data <- matrix(c(294,98,50,203,921,2862,3064,2652), nrow = 4, ncol = 2)
##Output:
# > baseline_table.data
# [,1] [,2]
# [1,]  294  921
# [2,]   98 2862
# [3,]   50 3064
# [4,]  203 2652

#Total number of physicians sampled
Sumbaseline <- sum(294,98,50,203,921,2862,3064,2652)
#Output:
# 10144

#Getting the row proportions for each outcome (column, yes/no):
prop.table(baseline_table.data,1) # where the number “1” indicates “row percentages”, 2 would be used to find column %
##Output:
# > prop.table(baseline_table.data,1)
# [,1]      [,2]
# [1,] 0.24197531 0.7580247
# [2,] 0.03310811 0.9668919
# [3,] 0.01605652 0.9839435
# [4,] 0.07110333 0.9288967

#Chi-square Test of data
expBaselineVal <- chisq.test(baseline_table.data)
##Output:
# > expBaselineVal
# Pearson's Chi-squared test
# data:  baseline_table.data
# X-squared = 816.41, df = 3, p-value < 2.2e-16
#pvaule ~ 0, reject the Ho

#Finding the expected values:
expBaselineVal$expected
##Output:
# > expBaselineVal$expected
# [,1]     [,2]
# [1,]  77.25503 1137.745
# [2,] 188.20978 2771.790
# [3,] 198.00177 2915.998
# [4,] 181.53342 2673.467

###############################################################################################
#Problem 2.) Ho:  vs. Ha: , α=0.05 
#Testing to see if there is a relationship between subjects level of education and their attitude 
## of smoking in public places.

#Creating a table for the data:
smoking_table.data <- matrix(c(5,15,15,44,100,40,23,30,10,3,5,10), nrow = 3, ncol = 4)
##Output:
# > smoking_table.data
# [,1] [,2] [,3] [,4]
# [1,]    5   44   23    3
# [2,]   15  100   30    5
# [3,]   15   40   10   10
 
#Total number of people sampled
sum(smoking_table.data)
##Output: 
# 300

#Getting the row proportions for each outcome (policy favored):
prop.table(smoking_table.data,1) # where the number “1” indicates “row percentages”, 2 would be used to find column %
##Output:
# [,1]      [,2]      [,3]       [,4]
# [1,] 0.06666667 0.5866667 0.3066667 0.04000000
# [2,] 0.10000000 0.6666667 0.2000000 0.03333333
# [3,] 0.20000000 0.5333333 0.1333333 0.13333333

#Chi-square Test of data
expSmokingVal <- chisq.test(smoking_table.data) #getting a warning message because one of the expected values is less than 5.
##Output:
# > expSmokingVal
# Pearson's Chi-squared test
# data:  smoking_table.data
# X-squared = 22.502, df = 6, p-value = 0.0009817
# pvalue is less than 0.05, reject the Ho

#Finding the expected values:
expSmokingVal$expected
##Output:
# > expSmokingVal$expected
# [,1] [,2]  [,3] [,4]
# [1,]  8.75   46 15.75  4.5
# [2,] 17.50   92 31.50  9.0
# [3,]  8.75   46 15.75  4.5
## I was getting this error message: ***"Warning message: In chisq.test(smoking_table.data): 
### Chi-squared approximation may be incorrect"***, because two of the expected values are less than '5'.

























