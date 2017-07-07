###########################################################################################
#Skyler Kuhn
#STAT:543 
#Homework 1: Dataset available on blackboard-> Dataset ->R Class Datasets
########################################################################

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

########################################################################

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

########################################################################

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

########################################################################
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

########################################################################
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

########################################################################
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
     
########################################################################
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
#STAT:543 
#Homework 1: Dataset available on blackboard-> Dataset ->R Class Datasets
########################################################################


