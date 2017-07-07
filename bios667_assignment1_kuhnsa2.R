#############################################################################################################################
#Skyler Kuhn
#BIOS 667: Data Mining and Machine Learning 
#Assignment 1
##########################################################
#1. Write some R expressions, without using  c(list  of  entries), that return  the  
##  following vectors and matrix:
##########################################################
#1a. (1, 2, 3, 4, 5, 6, 7, 8, 7, 6, 5, 4, 3, 2, 1)
numbersVect <- append(1:8, 7:1)
numbersVect 
#Ouput: [1] 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
rnorm(20,0,1)
#1b. (1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5)
numbersVect2 <- rep(1:5, times=1:5) 
numbersVect2
#Output: [1] 1 2 2 3 3 3 4 4 4 4 5 5 5 5 5

#1c.
# 0 1 1
# 1 0 1
# 1 1 0
numbersMatr <- matrix(1,nrow=3,ncol=3) - diag(1,3)
numbersMatr
#Output: 
#       [,1] [,2] [,3]
# [1,]    0    1    1
# [2,]    1    0    1
# [3,]    1    1    0

##########################################################
#2a.Supposing that youare given x, write an R function that returns y.
myfunction <- function(x) {
  if (x <= 0)
  {
    x1 <- -(x^3)
    return(x1)
  } 
  else if (x > 0 & x <= 1)
  {
    x2 <- x^2
    return(x2)
  }
  else
  {
    x3 <-sqrt(x)
    return(x3)
  }
}

#2b.Plot your function from part (a) versus x when x ranges from [-2, 2]. 
myvalues <- seq(-2,2,0.01) #Creates a vector of values ranging from -2 to 2, increamenting by 0.01
myvalues
passedvalues <- c()
for (i in myvalues){
  passedvalues <- append(passedvalues, myfunction(i))
  #passedvalues <- c()
}
passedvalues
plot(x=myvalues, y=passedvalues,
     xlab = "x", ylab = "f(x)",
     col="blue", pch=1, cex=.25)

##########################################################
#3. Let h(x,n) = 1 + x^1 + x^2 + ..... + x^3 . 
## Write an R function to calculate h(x,n). What is the result when x=0.3 and n=55? 
expfunction <- function(x,n){
  sum1 <- 0
  for (i in 0:n){
    sum1 = sum1 + (x^i)
  }
  return(sum1)
}

expPassedVal <- expfunction(0.3,55)
expPassedVal
#Output: [1] 1.428571

##########################################################
#4.Generate 100,000 random Weibull(shape = 2, scale = 3) variates/samples 
## (sample  A)  directly  using  R.  Then,  generate  100,000  similar  Weibull  
## variates/samples   (say,   sample   B),   with   the   same   shape   and   scale   
## parameters,  using  the  Distribution  Function  trick.  Draw  their  histograms,  
## aligning  them  sidewise,  with  proper  labels.  Compare  the  mean  and  
## Standard Deviations of samples A and B, and comment

a <- rweibull(100000, shape = 2, scale = 3)
b <- (3) * (-log(1-runif(100000, 0,1)))^(1/2)
#Creating the Sidewise graph of Hist A & B using ggplot2:
require(ggplot2)
require(reshape2)
df_AB <- data.frame(a = rweibull(100000, shape = 2, scale = 3),
                    b = rweibull(100000, shape = 2, scale = 3))
histoAB <- ggplot(melt(df_AB), aes(value, fill = variable)) + geom_histogram(position = "dodge")
histoAB <- histoAB + ggtitle("Two histograms of 100,000 random Weibull samples")
histoAB <- histoAB + theme(plot.title = element_text(size=17, face="bold", margin = margin(10, 0, 10, 0)))
histoAB
#Mean of A & B:
mean_of_a <- mean(a)
mean_of_a
#Output: 2.661412
mean_of_b <- mean(b)
mean_of_b 
#Output: 2.661713
#Standard Deviation of A & B:
sd_of_a <- sd(a)
sd_of_a
#Output: 1.394241
sd_of_b <- sd(b)
sd_of_b
#Output: 1.385083
#Comment: The output of the two Weibull distribution functions are very similar. This can further be seen
##        after plotting the function's outputed frequrencies as histograms. This is even further supported
###       after comparing the means and sd of a and b.

##########################################################
#5a. How many rows and how many columns are in the dataset?
library(MASS) #imports the MASS library (contains the Boston dataset)
data(Boston)  #loading the Boston dataset
head(Boston)  #looking at the first ten rows
dim(Boston)
#Output: 506 rows x 14 columns 

#5b. How  many  towns  and  which  one(s) [report row number(s)] have a tax over 700?
taxOver700 <- Boston[Boston$tax > 700,]
taxOver700
#Output: 489, 490, 491,492, 493
# crim zn indus chas   nox    rm  age    dis rad tax ptratio  black lstat medv
# 489 0.15086  0 27.74    0 0.609 5.454 92.7 1.8209   4 711    20.1 395.09 18.06 15.2
# 490 0.18337  0 27.74    0 0.609 5.414 98.3 1.7554   4 711    20.1 344.05 23.97  7.0
# 491 0.20746  0 27.74    0 0.609 5.093 98.0 1.8226   4 711    20.1 318.43 29.68  8.1
# 492 0.10574  0 27.74    0 0.609 5.983 98.8 1.8681   4 711    20.1 390.11 18.07 13.6
# 493 0.11132  0 27.74    0 0.609 5.983 83.5 2.1099   4 711    20.1 396.90 13.35 20.1
numberofTaxedOver700 <- dim(taxOver700)[1]  #Finds the dimension of the matrix and takes the number of rows
numberofTaxedOver700
#Output: 5 (towns have tax over 700)

#5c. How many of the suburbs in this dataset bound the Charles River? 
##Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
boundCharles <- Boston[Boston$chas == 1,]
boundCharles
numberBoundingCharles <- dim(boundCharles)[1]
numberBoundingCharles
#Output: 35 (towns bound the Charles River)

#5d.What is the median pupil-teacher ratio among the towns in this dataset? 
## What is the median pupil-teacher ratio among the suburbs that bound the Charles River?
medianPupilTeacherRatio <- median(Boston$ptratio) 
#Ouput: 19.05 (median pupil-teacher ratio among the towns in this dataset)
medianPupilTeacherRatioBoundingCharles <- median(boundCharles$ptratio)
#Ouput: 17.6 (median pupil-teacher ratio among the suburbs that bound the Charles River)

#5e. Which suburb (report row number) has the lowest median value of owner-occupied homes? 
lowestValueOwnerHome <- Boston[Boston$medv == min(Boston$medv),]
#Output: 399 and 406 (row number of corresponding subsurbs)
# crim zn indus chas   nox    rm age    dis rad tax ptratio  black lstat medv
# 399 38.3518  0  18.1    0 0.693 5.453 100 1.4896  24 666    20.2 396.90 30.59    5
# 406 67.9208  0  18.1    0 0.693 5.683 100 1.4254  24 666    20.2 384.97 22.98    5

#5f. In this dataset, how many of the towns average more than seven rooms per dwelling? More than eight rooms per dwelling?
moreThan7Rooms <- dim(Boston[Boston$rm >7,])[1]
moreThan7Rooms
#Output: 64 (towns average more than 7 rooms per dwelling)
moreThan8Rooms <- dim(Boston[Boston$rm >8,])[1]
moreThan8Rooms
#Output: 13 (towns average more than 8 rooms per dwelling)
#############################################################################################################################
