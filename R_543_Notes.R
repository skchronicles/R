#########################################################################################
#DATASETS available here :
#https://drive.google.com/a/mymail.vcu.edu/folderview?id=0B2WgGjvd16kbNzA0bVhBSGJsT3M&usp=sharing#
#########################################################################################
#Skyler Kuhn
#Day 1: Introduction to R

#To set the working directory, to organize where your files will be saved
setwd("~/Desktop/R") #created this directory to save all my R notes/scripts
getwd() #returns your current working directory like 'pwd' 
    #it returned: "/Users/nbskuhn/Desktop/R"

#Reading in a CSV file, a window will pop up you must choose where the file is saved.
#If reading in a txt file, you must chnange csv to txt
data1 <- read.csv(file.choose(), header = TRUE)
data1

#Working on a dataset, use '$" to grab variables
# Age  PCB
# 1    1  0.6
# 2    1  1.6
# 3    1  0.5
# 4    1  1.2
# 5    2  2.0
# 6    2  1.3
# 7    2  2.5
data1$Age #this will grab all the info in the Age column
PCB1 <- data1 

#Grabbing an element in a matrix data[row, column]:
##xample: 
PCB1[4,2] #row 4, column 2
PCB1[1:4,1:2] #row 1 through 4, cloumn 1 through 2

#Logic in R:
## < less than 
## > greater than 
## == equal to 
PCB1[PCB1$Age > 3, ] #grab data where the Age is greater than 3
 
#Multiple Logic statments:
#'&' is equal to and
#'|' is equal to or
PCB1[PCB1$Age > 3 & PCB1$PCB <9, ]



#We are now using a different dataset called pain.csv

pain1 <- read.csv(file.choose(), header = TRUE)

# HairColour Pain
# 1     LightBlond   62
# 2     LightBlond   60
# 3     LightBlond   71
# 4     LightBlond   55
# 5     LightBlond   48
# 6      DarkBlond   63
# 7      DarkBlond   57
# 8      DarkBlond   52
# 9      DarkBlond   41
# 10     DarkBlond   43
# 11 LightBrunette   42
# 12 LightBrunette   50

#Dealing with text in a dataset:
pain1[pain1$HairColour == "LightBlond", ] #we are grabbing all the LightBlond data in the HairColour column of the dataset

#If, else statements
#if the condition is met do this, else do something else

#To find the number of rows in your dataset: nrow()
#To generate uniform random numbers: runif(10, 0, 1)     #In this expample, 10 numbers will be created distributed between 0 and 1
U1 <- runif(nrow(Pain1), 0, 1)
ifelse(U1 > 0.5, 1, 0)

#Entering in Data manually:
Data4 <- c(1,2,3,5,7,8,9) #stored data
Data5 <- c(2,3,4,5,6,7,8) #stored data 
DataColumnBound <- cbind(Data4,Data5) #cbind() will bind the two columns of data together
##Result:
# Data4 Data5
# [1,]     1     2
# [2,]     2     3
# [3,]     3     4
# [4,]     5     5
# [5,]     7     6
# [6,]     8     7
# [7,]     9     8

DataRowBound <- rbind(Data4,Data5)
# [,1] [,2] [,3] [,4] [,5] [,6] [,7]
# Data4    1    2    3    5    7    8    9
# Data5    2    3    4    5    6    7    8


#Entering in a Header for the data:
colnames(DataColumnBound) <- (c('Column1', 'Column2')) #not working

#Creating a Dataframe:
employee <- c('John Doe','Peter Gynn','Jolie Hope')
salary <- c(21000, 23400, 26800)
employ.data <- data.frame(employee,   salary)  
#Output:
# employee salary
# 1   John Doe  21000
# 2 Peter Gynn  23400
# 3 Jolie Hope  26800
#########################################################################################
#Skyler Kuhn
#Day 2: Intro R (help function, general functions (summary function), packages, logic statements, graphs, etc.)

#To find what a function does, google it or "help()"
help(mean)
#will output (with additional info):
#Generic function for the (trimmed) arithmetic mean.

#reading in the files again-- to use later:
pain1 <- read.csv(file.choose(), header = TRUE)
data1 <- read.csv(file.choose(), header = TRUE)
data1

PCB1 <- data1 

#Packages in R (easy to to in R studio):
##https://cran.r-project.org/web/packages/available_packages_by_name.html
##They are very helpful, go to the R website and click on packages. 
##You can use them to perform specialized functions (pre-written code).
##There are a ton on the website. You can find whatever you need. 
##The window to your left in R Studio (----->) shows all the packages you have downloaded. You can also download packages here (click on install).

#Importing a Package "library(nameOfPackage)":
library(brew)   #importing a package called brew
#Write the name of the package inside of the library function. Packages must be imported everytime you want to use them.

#Another way to install a package: 
#install.packages("nameOfThePackage")    #double quotes are needed

#"Foreign" Package:
##can be used to read in different types of data set: minitab, S, SAS, SPSS, etc. To learn more google search.

#Basic statistics in R:
## Sum:
sum()
##Mean:
mean()
##SD:
sd()
##Max:
max()
##Example:
mean(PCB1$Age)
max(PCB1$Age)
min(PCB1$Age)
median(PCB1$Age)
sd(PCB1$Age)

#Finding Quantiles (two arguments):
quantile(PCB1$Age, 0.25) #first quantile

#Apply Fucntion "apply()":
##often you want to apply the same fucntion to multiple rows or columns:
#apply(nameofDataset, 1, function) #1, for rows
#apply(nameofDataset, 2, function) #2, for columns
apply(PCB1,1,mean) #applying the fucntion to all the rows in the dataset
apply(PCB1,2,mean) #applying the function to all the columns in the dataset

#"Tapply" function:
##use when you want to apply a fucntion to a group in a dataset (ex. Haircolour)
tapply(pain1$Pain, pain1$HairColour, mean)

#Correlation determinate:
correlation1 <- cor(PCB1)
correlation1
##output:
# Age       PCB
# Age 1.0000000 0.7363613
# PCB 0.7363613 1.0000000
##grab the value in the first row and second column and square it:
r2 <- cor(PCB1)[1,2]^2
r2

#Linear Regression (to fit a linear regression line):
##use "lm(y ~ x)
lm(PCB1$PCB ~ PCB1$Age) #example

#Summary Fucntion (WILL USE ALOT, IMPORTANT!):
##will give you a ton of useful info
##summary()
SummaryofPCBdataset <- summary(PCB1) #example
SummaryofPCBdataset
SummaryofPain1dataset <- summary(pain1) #example
SummaryofPain1dataset
##Ouput of Pain1 summary:
# Age              PCB        
# Min.   : 1.000   Min.   : 0.500  
# 1st Qu.: 2.750   1st Qu.: 2.150  
# Median : 5.500   Median : 4.300  
# Mean   : 5.536   Mean   : 7.171  
# 3rd Qu.: 8.000   3rd Qu.: 9.900  
# Max.   :12.000   Max.   :30.400 

#Tables:
table(pain1$HairColour)

#Random Number Generation:
##runif(numberofNumbersYouWant, lowerlimit, upperlimit)

#Pulling a random Sample from Dataset:
#sample(dataset, sizeofSample)
sample(PCB1, 15)
##genertaing a sample from a normal distribution:
#rnorm(numberofNumbersYouWant, mean, sd)

#Looping and Logic statments(run the same fucntion throughout a dataset):
##For Looping:
#for(i in 1:I){
#   stuff
#}

##While looping:
# while(logicstatement){
#   stuff
# }

#If statements:
#If (condition){
#   do something
#}

#If, else statement example:
#If (condition){
#   do something
#}else{
#   do something else
#}

#Graphics in R:
##publication quality plots, graphs, etcs.
#you must program the graphics (must type in commands, very easy though)

#DATASETS available here :
#https://drive.google.com/a/mymail.vcu.edu/folderview?id=0B2WgGjvd16kbNzA0bVhBSGJsT3M&usp=sharing#
#Reading in a new Dataset (must downlaod first before reading it in)
CherryTree <- read.csv(file.choose(), header = TRUE)

#Creating Graphs
##Histogram:
#http://www.ats.ucla.edu/stat/r/gbe/histogram.htm (for more information on histograms, such as adding a normal curve)
hist(CherryTree$Diam) #does not look too nice, the axis labels are poor

hist(CherryTree$Diam,    #comma is neeeded
     main = "Histogram of Tree Diameter", # changes the Legend Header/title #comma is neeeded
     xlab = "Changes the x-axis label",   #changes the X-axis label   #comma is neeeded
     col = "green")  #changes the bar green, there is a huge selection of colors (thousands) google search it!

#########################################################################################
#Skyler Kuhn
#Day 3:  Histograms, Blox Plots, Scatterplots, Advanced Scatterplots, Two Plots per Window 

#Datasets we will be using today
Heartdata <- read.csv(file.choose(), header = TRUE)
Heartdata
# BeforeA AfterA BeforeB AfterB
# 1       55     87      48     79
# 2       65     91      63     64
# 3       58     90      57     65
# 4       54     94      61     61
# 5       62     90      52     73
# 6       63     95      59     64
# 7       63     97      61     66

#Creating a histogram of the Heartdata set
hist(Heartdata$BeforeA,main="Hist",xlab="BeforeA",col="red")

#Reading in another dataset
Heartgroup <- read.csv(file.choose(), header = TRUE)
Heartgroup
# Before After
# 1       69    90
# 2       59    81
# 3       58    77
# 4       68    95
# 5       52    87

##Boxplots
#Creating a boxplot of the Heartgroup data
#Boxplots are useful for comparing the distributions of various groups.
# Create a histogram of the Before heart rates.
# Create a boxplot of the Before heart rates.
# Be sure to title and label the plots accordingly
boxplot(Heartgroup$Before,main="boxplot",ylab="Before",col="blue")
boxplot(Heartgroup$Before,Heartgroup$After, 
        main="boxplot",ylab="Before & After",
        names= c("Before","After"),col=c("blue","red"))  #c() fucntion (order matters), blue matches to before, red to after 

#Exercise 1: Do later for practice!
# Use the heartgroups.csv dataset
# Create two side-by-side boxplots labeled appropriately.
# Group Before together and After together
# Group group A together and group B together
# You may wish to explore the relationship between two or more variables. 
# Scatterplots are great for this.
# They allow one to look for correlations in the data.

#We will use PCB dataset-- reading it in:
PCB1 <- read.csv(file.choose(), header = TRUE)
PCB1
# Age  PCB
# 1    1  0.6
# 2    1  1.6
# 3    1  0.5
# 4    1  1.2

##Scatter Plots 
# You may wish to explore the relationship between two or more variables. 
# Scatterplots are great for this.
# They allow one to look for correlations in the data.
plot(PCB1$Age, PCB1$PCB) #the labeled axis are horrible, we will change them below
#To Change the color of points and line use pch and lty
plot(PCB1$Age,PCB1$PCB,
     main="PCB vs. Age",
     xlab="Age",
     ylab="PCB",
     col="blue", pch=13) #pch changes the shape of the points/plotting symbols (just change the number)
#Here is more info on plot characteristics: http://vis.supstat.com/2013/04/plotting-symbols-and-color-palettes/
#To add a line to the plot use:
abline( lm( PCB1$PCB ~ PCB1$Age ),col="red",lty=2 ) #adding a line
#to change the line types: http://www.statmethods.net/advgraphs/parameters.html

##Exercise 2: do later for practice!
# Use the Skier.csv dataset and create two scatterplots.
# One for CPK vs. Age
# One for CPK vs. Weight
# Play around the the colors, line types and point types
#To learn more points and line types
#Visit the following link http://www.statmethods.net/advgraphs/parameters.html
 
##Advanced Scatter Plots:
# You may have different groups that you need to plot simultaneoulsy on the same scatterplot. 
# Hence you need to add the points of the groups to the plot.
# The points() function will add these points to the plot.
# Modify colors
# Modify point type
# Using the Babies.csv dataset.
# Three groups based on age.
# X1 and X2 are proprietary variables from a Hong Kong Hospital.
# We want to see if the relationship is the same for each of the groups.
#Babies dataset:
babies1 <- read.csv(file.choose(),header=TRUE)
babies1
# x1    x2 Age
# 1  0.729 280.1   3
# 2  0.785 402.2   3
# 3  0.625 351.4   3
# 4  0.604 315.5   3
# 5  0.701 306.0   3
# 6  0.957 315.0   3
babies3 <- babies1[ babies1$Age==3, ]   #pulling out all of the babies data which are 3 months old 
babies12 <- babies1[ babies1$Age==12, ] #pulling out all of the babies data which are 12 months old 
babies24 <- babies1[ babies1$Age==24, ] #pulling out all of the babies data which are 24 months old 

plot(babies3$x1, babies3$x2,    #creating a plot 
     xlab="x1",                 #changes the x-axis 
     ylab="x2",                 #changes the y-axis
     main="x1 vs x2",           #changes the header
     col="red",                 #changes the color of points
     pch=1)                     #chnages the plotting symbol shape
points(babies12$x1, babies12$x2,  #this code adds points to the plot above from 12 month old babies data
       xlab="x1",
       ylab="x2",
       main="x1 vs x2",
       col="blue",
       pch=2)
points(babies24$x1, babies24$x2,   #this code adds points to the plot above from 24 month old babies data
       xlab="x1",
       ylab="x2",
       main="x1 vs x2",
       col="green",
       pch=13)

##Fixing the scale of the Graph above:
Minx1 <- min(babies1$x1)  #finding the min of the x1 column
Maxx1 <- max(babies1$x1)  #finding the max of the x2 column, etc...
Minx2 <- min(babies1$x2)
Maxx2 <- max(babies1$x2)
plot(babies3$x1, babies3$x2, #changing the scale of the graph based on these changes
     xlab="x1", ylab="x2", main="x1 vs x2",
     col="red", pch=1,
     xlim=c(Minx1,Maxx1), ylim=c(Minx2,Maxx2)
)
points(babies12$x1, babies12$x2, pch=2, col="blue" )
points(babies24$x1, babies24$x2,
       pch=3,
       col="green")
#Adding a legend
legend(0.25, 400,   #this determines the location of the legend (where it is located in the graph (0.89, 250 -- will move it to the lower right))
       c("3m", "12m", "24m"),   #defining the groups 
       col=c("red", "blue", "green"), #this needs to match with what you did in the previous lines
       pch=c( 1, 2, 3))               #this needs to match with what you did in the previous lines

##To partition the window where you have two rows of plots use. 
#This will give you two plots per page (to save more space in a publication)
#Using the Cherrytree data from Day2:
par(mfrow=c(1,2))   #if graphs look compressed expand window 
plot(CherryTree$Volume,CherryTree$Height, main="", 
     xlab="Height", 
     col="sea green")
plot(CherryTree$Volume,CherryTree$Diameter, main="",
     xlab="Diameter",
     col = "sea green")
par(bty='n')
#To get a description of the version of R and its attached packages used in the current session, we can use the sessionInfo function”
sessionInfo()

#########################################################################################
#Skyler Kuhn
#Day 4:

#Finding the p-Value
help(prop.test) #will output what is directly below
# prop.test(x, n, p = NULL,
#           alternative = c("two.sided", "less", "greater"),
#           conf.level = 0.95, correct = TRUE)
prop.test(x=270, n=500, p = 0.05,
          alternative = c("greater"),
          conf.level = 0.95, correct = FALSE)
##Ouput:
# 1-sample proportions test without continuity correction
# 
# data:  270 out of 500, null probability 0.05
# X-squared = 2527.4, df = 1, p-value < 2.2e-16
# alternative hypothesis: true p is greater than 0.05
# 95 percent confidence interval:
#   0.5032207 1.0000000
# sample estimates:
#   p 
# 0.54 

#Z-TEST and Critical Value:
#example:
#pbar = 270/500
#p0 = .5
#n = 500
#z = (pbar-p0)/sqrt(p0*(1-p0)/n) #test statistic

##############################################################################################
#Skyler Kuhn
#2/22/16
#Homework 2

#1B. Using R to find the Test Statistic (z):
#USE: prop.test(x= ... , n= ..., p= pO, correct=FALSE)
##i) H0: p=0.50 vs. Ha: p > 0.50 , n = 360, p-hat =0.56
prop.test(x=201, n=360, p= 0.50, correct= FALSE) # Output: z = 2.214
##ii)H0: p=0.50 vs. Ha: p ≠ 0.50 , n = 360, p-hat = 0.56
prop.test(x=202, n=360, p= 0.50, correct= FALSE) #Output: z= 2.319
##iii)H0: p=0.37 vs. Ha: p < 0.37 , n = 1200, p-hat = 0.35
prop.test(x=420, n=1200, p=0.37, correct= FALSE) #Output: z= 1.435 (should be negative though)

#1C. Using R to find the p-Value:
#USE: prop.test(x= ..., n= ..., p= pO, alternative= "two.sided,less,greater", correct=FALSE) #alternative depends on Ha
##i) H0: p=0.50 vs. Ha: p > 0.50 , n = 360, p-hat =0.56, α=0.05 
prop.test(x=201, n=360, p= 0.50, alternative= "greater", correct= FALSE) # Output: pvalue= 0.01343
##ii)H0: p=0.50 vs. Ha: p ≠ 0.50 , n = 360, p-hat = 0.56, α=0.05 
prop.test(x=202, n=360, p= 0.50, alternative= "two.sided", correct= FALSE) # Output: pvalue= 0.02039
##iii)H0: p=0.37 vs. Ha: p < 0.37 , n = 1200, p-hat = 0.35, α=0.05 
prop.test(x=420, n=1200, p= 0.37, alternative= "less", correct= FALSE) # Output: pvalue= 0.07564

#1D. Using R to find the Critical Value:
##i) H0: p=0.50 vs. Ha: p > 0.50 , n = 360, p-hat =0.56
phat <- 0.56  #sample proportion
pO <- 0.5     #hypothesized value
n <- 360      #sample size
z <- (phat - pO) / sqrt(pO*(1 - pO)/n)  #test statistic
##now we compute critical values at alpha (in this case 0.05, see directly below)
alpha = 0.05
##Right Tailed Test
z.alpha_right <- qnorm(1 - alpha)
z.alpha_right #Output: Critical Value = 1.644854
#Left Tailed Test
z.alpha_left <- qnorm(alpha)
z.alpha_left #Output: Critical Value = 1.644854
#Two Tailed Test
z.alpha_two <- qnorm(1 - alpha / 2)
z.alpha_two #Output: Critical Value = 1.959964

#1E. Using R code to calculate the Confidence Interval:
##Case i) 
#Below I am creating a function to calculate the confidence interval
#Confidence Interval Function:
confidence.int <- function(phat,po,cv,n){
  pe <- phat
  se <- sqrt(pe*(1 - pe)/n)
  ci_1 <- pe + (cv * se)
  ci_2 <- pe - (cv * se)
  if (ci_1 < ci_2){
    return (c(ci_1,ci_2))
  }else{
    return (c(ci_2, ci_1))
  } #the if, else statement is just to make sure the value pair it is returning starts with the lowest of the two numnbers
}
#H0: p=0.50 vs. Ha: p > 0.50 , n = 360, p-hat =0.56, alpha = 0.05 (95%), z = 2.277
confidence.int(phat=0.56, po=0.50, cv= 1.96, n= 360) #calling the confidence interval
#Output: CI = (0.5166504, 0.6033496)
##Case ii)
#H0: p=0.50 vs. Ha: p ≠ 0.50 , n = 360, p-hat = 0.56, α=0.05, z = 2.277 :
confidence.int(phat=0.56, po=0.50, cv= 1.96, n= 360)
#Output: CI = (0.5083495, 0.6116505)
##Case iii)
#H0: p=0.37 vs. Ha: p < 0.37 , n = 1200, p-hat = 0.35, α=0.05, z= -1.435:
confidence.int(phat=0.35, po=0.37, cv= -1.645, n= 1200)
#Output: CI = (0.327073, 0.372927)


#2A. Using R code to calculate the Test Statistic (z):
#Example: prop.test(x= ... , n= ..., p= pO, correct=FALSE)
#H0 :p=0.90 vs. Ha: p < 0.90 , n = 850, p-hat =0.8106
prop.test(x=689, n=850, p= 0.90, correct= FALSE) # Output: z = 8.689 (should be negative though)

#2B. Using R code to calculate the p-value:
#H0: p=0.90 vs. Ha: p < 0.90 , n = 850, p-hat = 0.8106, α=0.05, x= 689
prop.test(x=689, n=850, p= 0.90, alternative= "less", correct= FALSE) # Output: pvalue= 2.2e-16 (very small)

#2D. Using R code, find the critical value:
##Compute critical values at alpha (in this case 0.01 see directly below)
alpha <- 0.01
#Left Tailed Test
z.alpha_left = qnorm(alpha)
z.alpha_left #Output: Critical Value = -2.326348

#2E. Using R code to calculate the Confidence Interval:
# confidence.int <- function(phat,po,cv,n){
#   pe <- phat
#   se <- sqrt(po*(1 - po)/n) #this calculates the standard error
#   ci_1 <- pe + (cv * se)    #calculating one of the CI
#   ci_2 <- pe - (cv * se)    #calculating the second CI
#   return (c(ci_2, ci_1))
# }
#Below I am creating a function to calculate the confidence interval
#Confidence Interval Function:
confidence.int <- function(phat,po,cv,n){
  pe <- phat
  se <- sqrt(po*(1 - po)/n)
  ci_1 <- pe + (cv * se)
  ci_2 <- pe - (cv * se)
  if (ci_1 < ci_2){
    return (c(ci_1,ci_2))
  }else{
    return (c(ci_2, ci_1))
  } #the if, else statement is just to make sure the value pair it is returning starts with the lowest of the two numnbers
}
#H0: p=0.90 vs. Ha: p < 0.90 , n = 850, p-hat = 0.8106, α=0.05, x= 689, CV= -2.326348
confidence.int(phat=0.8106, po=0.90, cv= -2.3263, n= 850) #calling the confidence interval
#Output: CI = (0.7866626, 0.8345374)

#3A. Using R code to calculate the Test Statistic (z):
#Example: prop.test(x= ... , n= ..., p= pO, correct=FALSE)
#H0 :p=0.72 vs. Ha: p > 0.72 , n = 900, x= 674, p-hat =0.74889, po=0.72
prop.test(x=674, n=900, p= 0.72, correct= FALSE) # Output: z = 1.930 

#3B. Using R code to calculate the p-value:
#H0 :p=0.72 vs. Ha: p > 0.72 , n = 900, x= 674, p-hat =0.74889, po=0.72
prop.test(x=674, n=900, p= 0.72, alternative= "greater", correct= FALSE) # Output: pvalue= 0.02679
#reject the Ho

#3D. Using R code, find the critical value:
##Compute critical values at alpha (in this case 0.05, see directly below)
alpha <- 0.05
##Right Tailed Test
z.alpha_right <- qnorm(1 - alpha)
z.alpha_right #Output: Critical Value = 1.644854

#3E. Using R code to calculate the Confidence Interval:
#Below I am creating a function to calculate the confidence interval
#Confidence Interval Function:
confidence.int <- function(phat,po,cv,n){
  pe <- phat
  se <- sqrt(po*(1 - po)/n)
  ci_1 <- pe + (cv * se)
  ci_2 <- pe - (cv * se)
  if (ci_1 < ci_2){
    return (c(ci_1,ci_2))
  }else{
    return (c(ci_2, ci_1))
  } #the if, else statement is just to make sure the value pair it is returning starts with the lowest of the two numnbers
}
#H0: p=0.72 vs. Ha: p > 0.72 , n = 900, p-hat = 0.7489, α=0.05, x= 674, CV= 1.645
confidence.int(phat=0.7489, po=0.72, cv= 1.645, n= 900) #calling the confidence interval
#Output: CI = (0.7242799,0.7735201)


confidence.int(phat=0.1065, po=0.12, cv= -1.645, n= 1550)

#4A. Using R code to determine the Test Statistic (z):
#Example: prop.test(x= ... , n= ..., p= pO, correct=FALSE)
#H0 :p=0.12 vs. Ha: p < 12 , n = 1550, x= 165, p-hat =0.1065, po=0.12
prop.test(x=165, n=1550, p= 0.12, correct= FALSE) # Output: z = -1.641

#4B. Using R code to calculate the p-value:
#H0 :p=0.12 vs. Ha: p < 0.12 , n = 1550, x= 165, p-hat =0.1065, po=0.12, α = 0.05
prop.test(x=165, n=1550, p= 0.12, alternative= "less", correct= FALSE) # Output: pvalue= 0.05035
#fail to reject the Ho

#4C. Using R to compute Critical Values:
alpha = 0.05
#Left Tailed Test
z.alpha_left <- qnorm(alpha)
z.alpha_left #Output: Critical Value = -1.644854

#4D: Using R to Calculate Confidence Intverals 
#Calling the fucntion I created earlier (look above to see code)
confidence.int(phat=0.1065, po=0.12, cv= -1.645, n= 1550) 
#Output: CI = (0.09292212, 0.12007788)

###########################################################################################
#Skyler Kuhn
#STAT:543 
#Homework 1: Dataset available on blackboard-> Dataset ->R Class Datasets


#2a)Read the dataset Density.csv into R and store it in Dens1data.
Dens1data <- read.csv(file.choose(), header = TRUE)
Dens1data

#Output:
#   Dens year site
# 1    307    0    1
# 2    327    1    1
# 3    334    2    1
# 4    374    3    1
# 5    382    4    1

#######################################

#2b) Create a subset dataset that includes observations for Sites 1 through 10 only and name it Density10.
Density10 <- Dens1data[Dens1data$site >= 1 & Dens1data$site <= 10, ] #[row, column]:selecting values between 1 and 10
Density10

# Output:
#   Dens year site
# 1    307    0    1
# 2    327    1    1 
# cont...
# 98   458    9    9
# 99   470   10    9
# 100  288    0   10
# 101  306    1   10
# 102  318    2   10

#######################################

#2c) Create a subset dataset that includes observations for Sites 11 through 20 only and name it Density20.
Density20 <- Dens1data[Dens1data$site > 10 & Dens1data$site < 21, ] #[row, column]:selecting values between 11 and 20
Density20

# Output: 
#   Dens year site
# 111  302    0   11
# 112  320    1   11
# 113  336    2   11
# cont ...
# 208  464    9   19
# 209  491   10   19
# 210  292    0   20
# 211  301    1   20

#######################################
#2d) Create a subset dataset that includes observations for Sites 21 through 30 only and name it Density30.
Density30 <- Dens1data[Dens1data$site > 20 & Dens1data$site < 31, ] #[row, column]:selecting values between 21 and 30
Density30

# Output:
#   Dens year site
# 221  299    0   21
# 222  329    1   21
# 223  339    2   21
# cont ...
# 319  492   10   29
# 320  327    0   30
# 321  343    1   30
# 322  352    2   30

#######################################
#2e) For the Dens1data, report the summary statistics for each variable taking into account the type of each variable.

##Summary Statistics for the variable "Den"
densitySummary <- Dens1data$Dens
mean(densitySummary)     #Output: 392.4091
max(densitySummary)      #Output: 519
min(densitySummary)      #Output: 275
median(densitySummary)   #Output: 392
sd(densitySummary)       #Output: 59.03381
summary(densitySummary) 
# Output:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 275.0   344.0   392.0   392.4   441.8   519.0 

##Summary Statistics for the variable "year"
yearSummary <- Dens1data$year
mean(yearSummary)     #Output: 5
max(yearSummary)      #Output: 10
min(yearSummary)      #Output: 0
median(yearSummary)   #Output: 5
sd(yearSummary)       #Output: 3.16708
summary(yearSummary)
# Output:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0       2       5       5       8      10 

##Summary Statistics for the variable "site"
siteSummary <- Dens1data$site
mean(siteSummary)     #Output: 15.5
max(siteSummary)      #Output: 30
min(siteSummary)      #Output: 1
median(siteSummary)   #Output: 15.5
sd(siteSummary)       #Output: 8.668586
summary(siteSummary)
# Output:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0     8.0    15.5    15.5    23.0    30.0 

#######################################
#2f) For each of the datasets Dens1data, Density10, Density20, and Density30, consider the variable Dens and create a histogram. 
#2f cont.) Make sure to add an informative title and labels.

##Dens1data Histogram 
hist(Dens1data$Dens,    
     main = "Histogram of Density from Dens1data dataset", # changes the Legend Header/title #comma is neeeded
     xlab = "Density",   #changes the X-axis label   #comma is neeeded
     col = "green")  #changes the bar colour 

##Density10 Histogram
hist(Density10$Dens,    #comma is neeeded
     main = "Histogram of Density from Density10 dataset", # changes the Legend Header/title #comma is neeeded
     xlab = "Density",   #changes the X-axis label   #comma is neeeded
     col = "red")  #changes the bar color

##Density20 Histogram
hist(Density20$Dens,    #comma is neeeded
     main = "Histogram of Density from Density20 dataset", # changes the Legend Header/title #comma is neeeded
     xlab = "Density",   #changes the X-axis label   #comma is neeeded
     col = "blue")  #changes the bar color

##Density30 Histogram
Minden30 <- min(Density30$Dens)  #finding the min of the x1 column (used to fix the scale of the axis)
Maxden30 <- max(Density30$Dens)  #finding the max of the x2 column, etc... (used to fix the scale of the axis)
#Minx2 <- min(babies1$x2)  #(used to fix the scale of the axis)
#Maxx2 <- max(babies1$x2)  #(used to fix the scale of the axis)
hist(Density30$Dens,    #comma is neeeded
     main = "Histogram of Density from Density30 dataset", # changes the Legend Header/title #comma is neeeded
     xlab = "Density",   #changes the X-axis label   #comma is neeeded
     col = "yellow",  #changes the bar color
     xlim=c(Minden30,Maxden30)) #used to change the scale of the axis (it was messed up)

#######################################
#2g) g)	Create a boxplot for the variable Dens, considering Density10, Density20, and Density30. 
#2g cont.) Make sure to add an informative title and labels.

##Dens1data Boxplot
boxplot(Dens1data$Dens,
        main="Boxplot of Density from Dens1data 
        dataset",
        xlab= "Density Variable",
        ylab="Count",
        col="green")

##Density10 Boxplot
boxplot(Density10$Dens,
        main="Boxplot of Density from Density10 
        subdataset",
        xlab= "Density Variable",
        ylab="Count",
        col="red")

##Density20 Boxplot
boxplot(Density20$Dens,
        main="Boxplot of Density from Density20 
        subdataset",
        xlab= "Density Variable",
        ylab="Count",
        col="blue")

##Density30 Boxplot
boxplot(Density30$Dens,
        main="Boxplot of Density from Density30 
        subdataset",
        xlab= "Density Variable",
        ylab="Count",
        col="yellow")

##Bloxplot of Densities from Dens1data, Density10, Density20, and Density30
boxplot(Density30$Dens, Density10$Dens, Density20$Dens, Density30$Dens,
        main="Boxplot of Densities from Dens1Data, 
        Density10, Density20, and Density30",
        ylab="Counts",
        names= c("Dens1data","Density10","Density20","Density30"),
        col= c("green", "red", "blue", "yellow"))


###########################################################################################
#Skyler Kuhn 
#One-Sampled Proportion Hypothesis Tester:  Back-up Code (need to evaluate CV), see finished code below
# ##TO RUN: one.sampled.hypothesis.analysis(po= ?, n= ?, x= ?, alpha= 0.01 or 0.05, alternative= "less" or "greater" or "two.sided")
# one.sampled.hypothesis.analysis <- function(po, n, x, alpha, alternative){
#   samplesizecheck1 <- n * po
#   samplesizecheck2 <- n * (1 - po)
#   phat <- x / n
#   pe <- phat
#   se <- sqrt(po*(1 - po)/n)
#   #ci_1 <- pe + (cv * se)
#   #ci_2 <- pe - (cv * se)
#   #error_string <- 'The sample is of an insufficent size. Please gather more data and or check your values for po and n'
#   if (samplesizecheck1 > 5 & samplesizecheck2 > 5){
#     testStatistic <- phat - po / sqrt(po*(1 - po)/n)
#     if (alternative == "greater" & alpha == 0.05){
#       CV = 1.645
#       ci_1 <- pe + (CV * se)
#       ci_2 <- pe - (CV * se)     #Must insert another logic control if testStatistic is greater thn CV
#       
#       if (ci_1 < ci_2){
#         return (c(ci_1,ci_2))
#       }else{
#         return (c(ci_2, ci_1))       
#       } #the if, else statement is just to make sure the value pair it is returning starts with the lowest of the two numnbers
#     }
#     else if (alternative == "less" & alpha == 0.05){   #testing here
#       CV = -1.645
#       ci_1 <- pe + (CV * se)
#       ci_2 <- pe - (CV * se)
#       if (ci_1 < ci_2){
#         return (c(ci_1,ci_2))
#       }else{
#         return (c(ci_2, ci_1))
#       }
#     }
#     else if (alternative == "two.sided" & alpha == 0.05){
#       CV = 1.96
#       ci_1 <- pe + (CV * se)
#       ci_2 <- pe - (CV * se)
#       if (ci_1 < ci_2){
#         return (c(ci_1,ci_2))
#       }else{
#         return (c(ci_2, ci_1))
#       }
#     }
#     else if (alternative == "greater" & alpha == 0.01){
#       CV = 2.33
#       ci_1 <- pe + (CV * se)
#       ci_2 <- pe - (Cv * se)
#       if (ci_1 < ci_2){
#         return (c(ci_1,ci_2))
#       }else{
#         return (c(ci_2, ci_1))
#       }
#     }
#     else if (alternative == "less" & alpha == 0.01){
#       CV = -2.33
#       ci_1 <- pe + (CV * se)
#       ci_2 <- pe - (CV * se)
#       if (ci_1 < ci_2){
#         return (c(ci_1,ci_2))
#       }else{
#         return (c(ci_2, ci_1))
#       }
#     }
#     else if (alternative == "two.sided" & alpha == 0.01){
#       CV = 2.575
#       ci_1 <- pe + (CV * se)
#       ci_2 <- pe - (CV * se)
#       if (ci_1 < ci_2){
#         return (c(ci_1,ci_2))
#       }else{
#         return (c(ci_2, ci_1))
#       }
#     }
#     else{
#       return(c("Please either enter a valid alpha level, ex. 0.01 or 0.05 or please enter a valid hypothesis test: ex. greater, less, or two.sided"))
#     }
#   }  
#   else{
#     return (c("The sample is of an insufficent size."))
#   }
# }
# 
# #one.sampled.hypothesis.analysis(po, n, x, alpha, alternative)
# one.sampled.hypothesis.analysis(po= 0.72, n= 900, x= 674, alpha= 0.05, alternative= "less")
# 
# #confidence.int(phat=0.7489, po=0.72, cv= 1.645, n= 900) #calling the confidence interval
# #Output: CI = (0.7242799,0.7735201)

##############################################################################################################
#One Sampled Proportion Hypothesis Function:
#TO RUN: one.sampled.hypothesis.analysis(po= ?, n= ?, x= ?, alpha= 0.01 or 0.05, alternative= "less" or "greater" or "two.sided")
#use to round numbers: round(x, digits = 0)
one.sampled.hypothesis.analysis <- function(po, n, x, alpha, alternative){
  samplesizecheck1 <- n * po
  samplesizecheck2 <- n * (1 - po)
  phat <- x / n
  pe <- phat
  se <- sqrt(po*(1 - po)/n)
  if (samplesizecheck1 > 5 & samplesizecheck2 > 5){
    testStatistic <- (phat - po) / sqrt(po*(1 - po)/n)
    if (alternative == "greater" & alpha == 0.05){
      CV = 1.645
      ci_1 <- pe + (CV * se)
      ci_2 <- pe - (CV * se)     #inserting another logic control, if testStatistic is greater thn CV
      if(testStatistic > CV){
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4),
                      "Reject the Ho in favor of the Ha")     #round(x, digits = 4), will round to 4 numbers
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values) #Creating a dataframe to hold our vaules
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Reject Ho in favor of the Ha"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4),
                      "Reject the Ho in favor of the Ha")     #round(x, digits = 4), will round to 4 numbers
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values) #Creating a dataframe to hold our vaules
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_2, ci_1, "Reject the Ho in favor of the Ha"))       
        } #the if, else statement is just to make sure the value pair it is returning starts with the lowest of the two numnbers
      }else{
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4),
                      "Fail to Reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Fail to reject Ho"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4),
                      "Fail to Reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_2, ci_1, "Fail to reject the Ho"))    ##########testing here   
        } 
      }
    }
    else if (alternative == "less" & alpha == 0.05){   
      CV = -1.645
      ci_1 <- pe + (CV * se)
      ci_2 <- pe - (CV * se)
      if(testStatistic < CV){
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4),
                      "Reject the Ho in favor of the Ha")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Reject Ho in favor of the Ha"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4),
                      "Reject the Ho in favor of the Ha")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_2, ci_1, "Reject the Ho in favor of the Ha"))       
        } #the if, else statement is just to make sure the value pair it is returning starts with the lowest of the two numnbers
      }else{
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Fail to reject Ho"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_2, ci_1, "Fail to reject the Ho"))       
        } 
      }
    }
    else if (alternative == "two.sided" & alpha == 0.05){
      CV = 1.96
      ci_1 <- pe + (CV * se)
      ci_2 <- pe - (CV * se)
      if(testStatistic > CV){
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4),
                      "Reject the Ho in favor of the Ha")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Reject Ho in favor of the Ha"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4),
                      "Reject the Ho in favor of the Ha")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_2, ci_1, "Reject the Ho in favor of the Ha"))       
        } #the if, else statement is just to make sure the value pair it is returning starts with the lowest of the two numnbers
      }else{
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Fail to reject Ho"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_2, ci_1, "Fail to reject the Ho"))       
        } 
      }
    }
    else if (alternative == "greater" & alpha == 0.01){
      CV = 2.33
      ci_1 <- pe + (CV * se)
      ci_2 <- pe - (Cv * se)
      if(testStatistic > CV){
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4),
                      "Reject the Ho in favor of the Ha")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Reject Ho in favor of the Ha"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4),
                      "Reject the Ho in favor of the Ha")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_2, ci_1, "Reject the Ho in favor of the Ha"))       
        } #the if, else statement is just to make sure the value pair it is returning starts with the lowest of the two numnbers
      }else{
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Fail to reject Ho"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_2, ci_1, "Fail to reject the Ho"))       
        } 
      }
    }
    else if (alternative == "less" & alpha == 0.01){
      CV = -2.33
      ci_1 <- pe + (CV * se)
      ci_2 <- pe - (CV * se)
      if(testStatistic < CV){
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4),
                      "Reject the Ho in favor of the Ha")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Reject Ho in favor of the Ha"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4),
                      "Reject the Ho in favor of the Ha")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_2, ci_1, "Reject the Ho in favor of the Ha"))       
        } #the if, else statement is just to make sure the value pair it is returning starts with the lowest of the two numnbers
      }else{
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Fail to reject Ho"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_2, ci_1, "Fail to reject the Ho"))       
        } 
      }
    }
    else if (alternative == "two.sided" & alpha == 0.01){
      CV = 2.575
      ci_1 <- pe + (CV * se)
      ci_2 <- pe - (CV * se)
      if(testStatistic > CV){
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4),
                      "Reject the Ho in favor of the Ha")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Reject Ho in favor of the Ha"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4),
                      "Reject the Ho in favor of the Ha")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_2, ci_1, "Reject the Ho in favor of the Ha"))       
        } #the if, else statement is just to make sure the value pair it is returning starts with the lowest of the two numnbers
      }else{
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Fail to reject Ho"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_2, ci_1, "Fail to reject the Ho"))       
        } 
      }
    }
    else{
      return(c("Please either enter a valid alpha level-- ex. 0.01 or 0.05, or please enter a valid hypothesis test-- ex. greater, less, or two.sided"))
    }
  }  
  else{
    return (c("The sample is of an insufficent size. Please re-check vaules or re-sample data!"))
  }
}

#one.sampled.hypothesis.analysis(po, n, x, alpha, alternative)
one.sampled.hypothesis.analysis(po= 0.72, n= 900, x= 674, alpha= 0.05, alternative= "greater")

#Output:
# > one.sampled.hypothesis.analysis(po= 0.72, n= 900, x= 674, alpha= 0.05, alternative= "greater")
# One.Sampled.Hypothesis.Test                           Values
# 1         Null Hypothesis (Ho):                              0.72
# 2  Alternative Hypothesis (Ha):                           greater
# 3                    Alpha (α):                              0.05
# 4              Sample Size (n):                               900
# 5    p-hat (sample proportion):                            0.7489
# 6     Confidence Interval (CV):                             1.645
# 7           Test Statistic (z):                            1.9302
# 8    Lower Confidence Interval:                            0.7243
# 9    Upper Confidence Interval:                            0.7735
# 10       Statistical Inference:  Reject the Ho in favor of the Ha

# #Creating Another Dataframe (for hypothesis analysis):
# po <- 0.72
# alternative <- "greater"
# alpha <- 0.05
# n <- 300
# phat <- 0.75
# CV <- 1.645
# testStatistic <- 1.782
# ci_1 <- 0.7012
# ci_2 <- 0.7892
# 
# One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
#                                  "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
#                                  "Test Statistic (z): ", "Confidence Interval 1: ", "Confidence Interval 2: ",
#                                  "Statistical Inference: ")
# Values <- c(po, alternative, alpha,
#             n, phat, CV,
#             testStatistic, ci_1, ci_2,
#             "enter text here for inference")
# One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)

##########################################################################################

