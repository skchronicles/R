#Skyler Kuhn: R Code
# The hashtag is the comment symbol
# The consule window is where outputs will be displayed

# Manually enter data
# Enter a single value in a variable
x1 <- 5  #you can also use: x1 = 5

# Enter data in a vector
x2 <- c(1, 2, 3, 4, 5, 6, 7, 8)

# Enter Data into a Matrix
x3 <- matrix( c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3)
x3 #this will print out x3 matrix, R interprets data as a vertical columns lists

#     [,1] [,2] [,3]
#[1,]    1    4    7
#[2,]    2    5    8
#[3,]    3    6    9


# To retrieve a single value from a vector, the first element in a array starts a '1'
x2[5]

# To retrieve a single value from a matrix
x3 [2,3]  #variablle [row,column] (pulls out the value 8)

# To retrieve a portion of a vector....
3:5
x2[3:5] # will pull out all the numbers between 3-5

# To create a text vector
t1 <- c("Monday, Tuesday, Wednesday, Thursday. Friday")
t1    # print
t1[3] # Will pull out Wednesday

# Some logic Operators to keep in mind
# > greater than 
# < less than 
# == equal (don't forget that it is two equal signs)
# != not equal to

#Arithmetic 
# + addition
# - subtraction
# * Multiplication
# / division
# ^ exponent
# %*% matrix multiplication

x1*x2
#example of multiplying things together

# Try something alittle more complicated
x4 <- c (.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)
# If the vectors are the same length * produces element wise multiplcation
x2*x4 #vectors must be the same length

# Some handy function
sum(x2)
mean(x2) #same as average
sd(x2)
 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

#Second day of R (Day 12): 10/12/2015 
#Learning how to read data in to R

#Read in a CSV file.
Defect1 <- read.csv(file.choose(), header = TRUE)      #more of the point and click way of doing it
#header = TRUE    # means the first row is variables , like python, otherwise set it as header =FALSE
names (Defect1)
#gives you the names for each column, it is handy

Defect2 <- read.csv("C:/Users/hasuser/Desktop/Defect1.csv") #mac
#Defect2 <- read.csv("C:\\Users\\hasuser\\Desktop\\Defect1.csv")  #using a pc
#best way to read in a file, because you will know what dataset you are using 
head(Defect2)
#gives you the name and example of data

#Grab a Specific Column use $. (go to this dataframe and grab this column)
Defect2$Rate
mean(Defect2$Defects) #Defects are a column variable
sd(Defect2$Defects)
summary(Defect2) #gives you the min, median, mean, max, etc. #NOT THE FIVE NUMBER SUMMARY
min(Defect2$Defects) #you must specicfy the column (in this example it was Defects) or it will give you the lowest value in the dataset
range(Defect2$Defects) #this is not the range, it is the max and min (two values), statistifaclly speaking
max(Defect2$Defects) - min(Defect2$Defects) # This is the range, Statisically Range (one value)

#Store some quantities into variables.
MeanDefects1 <- mean(Defect2$Defects)
SdDefects1 <- sd(Defect2$Defects)
#you can use this to make Zscores <- (Defect2$Defects - MeanDefects1) / SdDefects1
ZDefects1 <- (Defect2$Defects - MeanDefects1) / SdDefects1  #This will give you the Zscores
#highlight just the variable to get it to print in the console

hist(ZDefects1)
#if you get an error message (figure margins too large), you have a to make the plotting window larger, it is silly 
#we are going to change the format of the histogram
hist(ZDefects1,
     xlab = "Standradized Defects", #changes the xaxis label, ylab will change the ylabel
     main = "Histogram of Standardized Defects" #changes the header label
     )

boxplot(Defect2$Defects)  #see to outliers and if the data is skewed, can be easier than looking at a histogram
boxplot(Defects ~ Rate,   #tilda plots defects against rate, tilda means plotting one against another
        data = Defect2)

boxplot(Defects ~ Line,         #tilda plots defects against line (variable in Defect2), tilda means plotting one against another
        data = Defect2,
        col = "red",              #color palatte: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
        xlab = "Line",            # label the x-axis
        ylab = "# of Defects",    # label the y-axis
        main = "Defects by Line") # label the main heading    

#Changes the color palatee to rainbow
#boxplot(Defects ~ Line,         #tilda plots defects against line (variable in Defect2), tilda means plotting one against another
#        data = Defect2,
#        col = rainbow(7),              #color palatte: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
#        xlab = "Line",            # label the x-axis
#        ylab = "# of Defects",    # label the y-axis
#        main = "Defects by Line") 

#Changes the color palatee to heatcolors
#boxplot(Defects ~ Line,         #tilda plots defects against line (variable in Defect2), tilda means plotting one against another
#        data = Defect2,
#        col = heat.colors(7),              #color palatte: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
#        xlab = "Line",            # label the x-axis
#        ylab = "# of Defects",    # label the y-axis
#        main = "Defects by Line") 

#Creating a Histogram with heatcolors
hist(Defect2$Defects,
     col = heat.colors(10)) #the higher the number the less subtle the changes in color

#Making a scaterplot
plot(Defect2$Rate, Defect2$Defects,
     xlab = "Rate",            # label the x-axis
     ylab = "# of Defects",    # label the y-axis
     main = "Defects by Rate") # label the main heading    

#Making a scaterplot, changing the value markers (the dots that represent the a x,y point, called pch marker)
#http://www.statmethods.net/advgraphs/parameters.html
plot(Defect2$Rate, Defect2$Defects,  #(x first, then y)
     xlab = "Rate",            # label the x-axis
     ylab = "# of Defects",    # label the y-axis
     main = "Defects by Rate",
     pch = 23,         #type of pch marker, looks like a diamond
     bg = "lightgreen",  #color of the pch filler
     col = "blue")     #color of the pch border 

#Add a regression line
abline(lm(Defect2$Defects ~ Defect2$Rate), #(y first then, x )
       lty = 2,   #make the line a dotted
       lwd = 3,     #makes the line thicker (more bold)
       col = "firebrick")   #chaneges the color of the line
abline(h = mean(Defect2$Defects),  #creating a new line horizontial line at the mean
       lty = 3,   #make the line a dotted
       lwd = 0.5, #decreases the lines weight boldness
       col = "gray59")  
abline(v = mean(Defect2$Rate),  #creating a new line vertical line at the mean
       lty = 3,   #make the line a dotted
       lwd = 0.5, #decreases the lines weight boldness
       col = "gray59")  
points(30, 100)    # adds a point at (x=30,y=100)
#to export the image you have created, use the export button (in the graph viewing window)
##################################

#Exporting a picture as a PDF
#Need to copy and past the block of code you want to export
pdf("C:/Users/hasuser/Desktop/DefectvsRate.pdf")
#Making a scaterplot, changing the value markers (the dots that represent the a x,y point, called pch marker)
#http://www.statmethods.net/advgraphs/parameters.html
plot(Defect2$Rate, Defect2$Defects,  #(x first, then y)
     xlab = "Rate",            # label the x-axis
     ylab = "# of Defects",    # label the y-axis
     main = "Defects by Rate",
     pch = 23,         #type of pch marker, looks like a diamond
     bg = "lightgreen",  #color of the pch filler
     col = "blue")     #color of the pch border 

#Add a regression line
abline(lm(Defect2$Defects ~ Defect2$Rate), #(y first then, x )
       lty = 2,   #make the line a dotted
       lwd = 3,     #makes the line thicker (more bold)
       col = "firebrick")   #chaneges the color of the line
abline(h = mean(Defect2$Defects),  #creating a new line horizontial line at the mean
       lty = 3,   #make the line a dotted
       lwd = 0.5, #decreases the lines weight boldness
       col = "gray59")  
abline(v = mean(Defect2$Rate),  #creating a new line vertical line at the mean
       lty = 3,   #make the line a dotted
       lwd = 0.5, #decreases the lines weight boldness
       col = "gray59")  
points(30, 100)    # adds a point at (x=30,y=100)
#to export the image you have created, use the export button (in the graph viewing window)
dev.off() #this statement is need to tell the program where to stop, needed to end

##############################

#Exporting a picture as a jpeg #picture wil have lower quality bc it is compressed, the quality = 100 helps though
#Need to copy and past the block of code you want to export
jpeg("C:/Users/hasuser/Desktop/DefectvRate.jpeg", quality = 100)
#Making a scaterplot, changing the value markers (the dots that represent the a x,y point, called pch marker)
#http://www.statmethods.net/advgraphs/parameters.html
plot(Defect2$Rate, Defect2$Defects,  #(x first, then y)
     xlab = "Rate",            # label the x-axis
     ylab = "# of Defects",    # label the y-axis
     main = "Defects by Rate",
     pch = 23,         #type of pch marker, looks like a diamond
     bg = "lightgreen",  #color of the pch filler
     col = "blue")     #color of the pch border 

#Add a regression line
abline(lm(Defect2$Defects ~ Defect2$Rate), #(y first then, x )
       lty = 2,   #make the line a dotted
       lwd = 3,     #makes the line thicker (more bold)
       col = "firebrick")   #chaneges the color of the line
abline(h = mean(Defect2$Defects),  #creating a new line horizontial line at the mean
       lty = 3,   #make the line a dotted
       lwd = 0.5, #decreases the lines weight boldness
       col = "gray59")  
abline(v = mean(Defect2$Rate),  #creating a new line vertical line at the mean
       lty = 3,   #make the line a dotted
       lwd = 0.5, #decreases the lines weight boldness
       col = "gray59")  
points(30, 100)    # adds a point at (x=30,y=100)
#to export the image you have created, use the export button (in the graph viewing window)
dev.off() #this statement is need to tell the program where to stop, needed to end



###########################################################################################################
#Missing class notes
#R:Day 4 Notes
# 10/19/2015
# More Picture Stuff (We downloaded babies.csv)

babies1 <- read.csv( "C:\\Users\\hasuser\\Desktop\\Babies.csv", header = TRUE)
head(babies1)

# plot x1 vs x2
plot( babies1$x1, babies1$x2 )
# Can't tell age of baby from plot

# Subset data so that we can plot each of the age groups
babies3 <- babies1[ babies1$Age == 3 , ]
babies12 <- babies1[ babies1$Age == 12 , ]
babies24 <- babies1[ babies1$Age == 24 , ]
# babies1[ row, column]
# Inside the bracket notation you can put a logic argument
# "R is not fun to deal with when subsetting data...it's awkward"

# plot x1 vs x2 for 3 month olds
plot( babies3$x1, babies3$x2,
      col = "seagreen",
      xlim = c( min(babies1$x1), max(babies1$x1) ),
      ylim = c( min(babies1$x2), max(babies1$x2) ),
      xlab = expression(x[1]),
      ylab = expression(x[2])
)
points( babies12$x1, babies12$x2,
        col = "midnightblue",
        pch = 14
)
points( babies24$x1, babies24$x2,
        col = "firebrick",
        pch = 10
)

# Putting the points() code after a plot() code overlays new datapoints onto an existing plot
# This is done so you don't have to create multiple 'plots', and can overlap them, basically 
# Xlim is the limits of your axis --> limits of what the plot will show you of your data
# You put the min and max of the original dataset --> it has ALL of the datapoints
# Will give you an appropriate plot window
# xlab = expression(x[1]) makes the 1 a subscript
# The expression command allows you to do a lot of cool stuff with characters, but is tedious
# There are a lot of options --> look online 
# for a character list: http://statmethods.net/advgraphs/parameters.html

# Creating a Legend
legend( .3, 400,
        c("3m", "12m", "24m"),
        pch = c(1, 14, 10),
        col = c("seagreen", "midnightblue", "firebrick")
)
# legend( .3, 400,                       <-- x and y coordinates where you want to place legend in plot
#        c("3m", "12m", "24m"),          <-- creating the lables in the legend (ORDER MATTERS NOW)
#        pch = c(1, 14, 10),             <-- characters in legend (HAS TO BE IN THE SAME ORDER)
#        col = c("seagreen", "midnightblue", "firebrick")) <-- Color (AGAIN IN THE SAME ORDER)
#

# We're going to make a pdf of the plot
pdf("C:\\Users\\hasuser\\Desktop\\Babies.pdf")
plot( babies3$x1, babies3$x2,
      col = "seagreen",
      xlim = c( min(babies1$x1), max(babies1$x1) ),
      ylim = c( min(babies1$x2), max(babies1$x2) ),
      xlab = expression(x[1]),
      ylab = expression(x[2])
)
points( babies12$x1, babies12$x2,
        col = "midnightblue",
        pch = 14
)
points( babies24$x1, babies24$x2,
        col = "firebrick",
        pch = 10
)

legend( .3, 400,
        c("3m", "12m", "24m"),
        pch = c(1, 14, 10),
        col = c("seagreen", "midnightblue", "firebrick")
)
dev.off()

# Apparently there has been a very easy way to do all of this w/o a lot of code... 
plot( babies1$x1,babies1$x2,
      col = babies1$Age,
      pch = babies1$Age,
      xlab = expression(x[1]),
      ylab = expression(x[2])
)
# This tells it to change color based on age, and the characters based on age
# The simple pch = babies1$Age and col = same, won't work in here so you have to go back
#   and identify the characters
# You need to put in 'unique' --> it was just a trick he taught, not logical
legend( .3, 400,
        c("3m", "12m", "24m"),
        pch = unique( babies1$Age ),
        col = unique( babies1$Age )
)
# Unique will have your butt
# Tells you to pull unique values 


#This is a whole lot shorter than our first attempt (lines 306-337)
plot( babies1$x1,babies1$x2,
      col = babies1$Age,
      pch = babies1$Age,
      xlab = expression(x[1]),
      ylab = expression(x[2])
)
legend( .3, 400,
        c("3m", "12m", "24m"),
        pch = unique( babies1$Age ),
        col = unique( babies1$Age )
)



# Teaching us how to automate it --> if you're getting datasets each month, you can just create one file
# You'd have to change the month label, maybe one other thing, but it's much easier than creating new plots each month

title1 <- "Hong Kong Babies Data"

# We don't need title 2 because we are combining title1 and month1 so we only need to enter new month names
title2 <- "Hong Knog Babies Data \n January"
# Unnecessary title2, but shows you what it will look like when combined (Title1 and Month1)

Month1 <- "October"
title3 <- paste( title1, "\n", Month1 )
# The paste() thing combines things so:
# paste (title1 (Hong Kong Babies Data), \n (New line) , Month1 (Whatever month it is))
# All you have to do is change the Month1 label each month 

plot( babies1$x1,babies1$x2,
      col = babies1$Age,
      pch = babies1$Age,
      xlab = expression(x[1]),
      ylab = expression(x[2]),
      main = title3
)
legend( .3, 400,
        c("3m", "12m", "24m"),
        pch = unique( babies1$Age ),
        col = unique( babies1$Age )
)
# the \n command puts in a new line 



#############################################################################################################
#Skyler Kuhn
#R: Day 5 Notes: 10/21/2015
#USing CyclerCPK data set: Wide to Long

Cycler1 <- read.csv( "C:\\Users\\hasuser\\Desktop\\CyclerCPK.csv", 
                     header = TRUE) #reading in the dataset into R
head(Cycler1) #first line is a header (ignore)

#####This dataset is in a wide file, we what to make it a long file!

#Grab the CPK1 data, (you have to count the columns things are in) #we want all the rows so we  put nothing in the first bracket ex.[ ,1:5]
CyclerCPK1 <- Cycler1[ ,1:5] #grabs columns 1 through 5
CyclerCPK2 <- Cycler1[ , c(1:4,6) ] #grabs columns 1 through 4 and 6 #THE C IN "C()" must be lowercase
CyclerCPK3 <- Cycler1[ , c(1,2,3,4,7) ] #grabs columns 1, 2, 3, 4, 7
CyclerCPK4 <- Cycler1[ , c(1:4, 8)] #grabs columns listed 

#Create a time Variable for each time.
time1 <- rep(1,nrow(Cycler1))   #nrow means for each row 
time2 <- rep(2,nrow(Cycler1))
time3 <- rep(3,nrow(Cycler1))
time4 <- rep(4,nrow(Cycler1))  #attach these to our data, see below
#Add column corresponding to time to each dataset
CyclerCPK1t <- cbind(CyclerCPK1, time1)
CyclerCPK2t <- cbind(CyclerCPK2, time2)
CyclerCPK3t <- cbind(CyclerCPK3, time3)
CyclerCPK4t <- cbind(CyclerCPK4, time4)

#Rename the columns for the datasets, Time was Time1, TIme2, Time3, Time4 #R was getting confused
#So you have to rename all the variables because R is stupid
#they must all be the same for R to merge the columns and rows 
names(CyclerCPK1t) <- c("Subject",
                        "Age",
                        "Gender",
                        "TRT",
                        "CPK",
                        "Time")
names(CyclerCPK2t) <- c("Subject",
                        "Age",
                        "Gender",
                        "TRT",
                        "CPK",
                        "Time")
names(CyclerCPK3t) <- c("Subject",
                        "Age",
                        "Gender",
                        "TRT",
                        "CPK",
                        "Time")
names(CyclerCPK4t) <- c("Subject",
                        "Age",
                        "Gender",
                        "TRT",
                        "CPK",
                        "Time")

Cycler1Long <- rbind(CyclerCPK1t,
                    CyclerCPK2t,
                    CyclerCPK3t,
                    CyclerCPK4t)

boxplot( CPK ~ Time, data = Cycler1Long,
        col = c("red", "blue", "seagreen", "plum"),
        names = c("T1", "T2", "T3","T4"),
        xlab = "CPK",
        horizontal = TRUE)


#We are making the same boxplot as above (this way is just simpler), no binding required
boxplot( Cycler1$CPK1,
         Cycler1$CPK2,
         Cycler1$CPK3,
         Cycler1$CPK4,
         col = c("red", "blue", "seagreen", "plum"),
         names = c("T1", "T2", "T3","T4"),
         xlab = "CPK",
         horizontal = TRUE)

#Turn a long a file into a wide file
Cycler1Wide <- cbind(Cycler1Long[Cycler1Long$Time == 1, 1:5], #specifiying the time (where is equal 1, 2,3,4) we want to pull and the columns
                     Cycler1Long[Cycler1Long$Time == 2, 5],
                     Cycler1Long[Cycler1Long$Time == 3, 5],
                     Cycler1Long[Cycler1Long$Time == 4, 5])

names(Cycler1Wide) <- c("Suject",   #here we are changing the titles to accommdate for the new columns we are adding (to create wide dataset)
                        "Age",
                        "Gender",
                        "TRT",
                        "CPK1",
                        "CPK2",
                        "CPK3",
                        "CPK4")

#############################################################################################################
#Skyler Kuhn
#R: Day 6 Notes: 10/26/2015
#Using cherry.csv dataset

Tree1 <- read.csv( "C:\\Users\\hasuser\\Desktop\\cherry.csv", header = TRUE)
head(Tree1)
#  Diam Height Volume
#1  8.3     70   10.3
#2  8.6     65   10.3
#3  8.8     63   10.2
#4 10.5     72   16.4
#5 10.7     81   18.8
#6 10.8     83   19.7

#Scatter plot between Diameter and Height
plot(Tree1$Diam, Tree1$Height,
     xlab = "Diameter(inches)",
     ylab = "Height (feet)")

#Scatter plot between Diameter and Height
plot(Tree1$Diam, Tree1$Volume,
     xlab = "Diameter(inches)",
     ylab = expression(Volume (ft^3)))

#Scatter plot between Height and Volume
plot(Tree1$Height, Tree1$Volume,
     xlab = "Height(ft)",
     ylab = expression(Volume (ft^3)))   #expression is used to create a to the thrid power 

#####Put all three graphs in one picture
par(mfrow = c(2,2))    #paritioning the window so you can put multiple pictures in one picture
#once you parition the window you must unpartition the window, see bellow

#Scatter plot between Diameter and Height
plot(Tree1$Diam, Tree1$Height,
     xlab = "Diameter(inches)",
     ylab = "Height (feet)")

#Scatter plot between Diameter and Height
plot(Tree1$Diam, Tree1$Volume,
     xlab = "Diameter(inches)",
     ylab = expression(Volume (ft^3)))

#Scatter plot between Height and Volume
plot(Tree1$Height, Tree1$Volume,
     xlab = "Height(ft)",
     ylab = expression(Volume (ft^3)))   #expression is used to create a to the thrid power 
#http://sphaerula.com/legacy/R/multiplePlotFigure.html   #if you want to center the odd left over graph


#Creating a 'style' Matrix Graph
par(mfrow = c(1,1)) #unpartitioning the window, resetting back the window (to show one graph per window)
pairs(Tree1) #this creates a weird matrix style scatterplot that graphs each variable against each other

#To use R help function "?_thenwhateveryouaresearching" into the consule, ex ?pairs
#we got this using "?pairs" help fucntion and copying and pasting code
#put histograms on the diagonal, section of the help code
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "plum", ...)
}
pairs(Tree1, panel = panel.smooth,   #put data where "Tree1" is, htat is the only thing you need to change 
      cex = 1, pch = 24, bg = "light blue",  #cex = is the size of something (datapoint markers, headers, variable names)
      diag.panel = panel.hist, cex.labels = 1.5, font.labels = 2)

#Run a regression analysis
Tree1.lm <- lm(Volume ~ Diam,   #lm creates an object
   data = Tree1)

names(Tree1.lm)
#now that the object is created we can grab all the attributes of names(Tree1.lm), gives us output
Tree1.lm$coefficients 
Tree1.lm$residuals
summary(Tree1.lm)   #gives you the a summary (Std., Error, pvalue)
Tree1.lm.sum <- summary(Tree1.lm) #we are putting the summary into a variable
names(Tree1.lm.sum)
#This gives you the table with all the pvalues
Tree1.lm.sum$coefficients
Tree1.lm.sum$coefficients[2,4] #gives you the pvalue for the diameter in the table just above (row 2, column 4)
#############################################################################################################
#Skyler Kuhn
#R: Day 7 Notes: 10/28/2015
#Get these Notes!!!!!!!
# 10/28/2015
# Functions

# myfunction <- function( inputs ) {
# Do Stuff
# return( output )
# }



# Must run the summary in the console before you can use it
dumb1 <- function( x1 ) {
  out1 <- x1*5
  return( out1 )
}

dumb1( 5 )

summary1 <- function( x1 ){
  print( paste( "Mean: ", mean(x1) ))
  print( paste( "Std Dev: ", sd(x1) ))
  hist(x1)
}
# Print prints our values out
# Paste puts whatever is in ""s before the output 



# (this is our fake/example dataset)
input1 <- c( 5, 18, 20, 94, 1, 52, 17, 97, 19, 56.8, 10,
             26, 32, 0, 7, 22, 74, 16, 2, 77, 4  )

summary1( input1 )


# We're rethinking our function so it gives us a better output --> easier to read
summary2 <- function( x1 ){
  out1 <- matrix( 0, nrow = 2, ncol = 1)    # Container to hold results 
  out1[ 1, 1] <- mean(x1) # put this in the first row, first column
  out1[ 2, 1] <-sd(x1) # put this in the second row, first column
  colnames(out1) <- "Value"
  rownames(out1) <- c( "Mean: ", "Std Dev: ")
  print( out1)  # Hands the values back so you can use them later
  hist(x1)
}

summary2( input1 )
# Output table looks like this  (
#              Value
# Mean:     28.56190
# Std Dev:  31.43073
#)

summary2 <- function( x1 ){
  out1 <- matrix( 0, nrow = 2, ncol = 1)    # Container to hold results 
  out1[ 1, 1] <- mean(x1) # put this in the first row, first column
  out1[ 2, 1] <-sd(x1) # put this in the second row, first column
  colnames(out1) <- "Value"
  rownames(out1) <- c( "Mean: ", "Std Dev: ")
  return( out1)  # Hands the values back so you can use them later
  hist(x1)
}

output1 <- summary2( input1 )
# This hands us our table as a variable now

# Create some z-scores for the values of input1
# z = (x-xbar)/stdev
(input1 - output1[ 1, 1 ])/output1[2,1]

# Return better output...again
# This whole class is 
summary3 <- function( x1 ){
  out1 <- matrix( 0, nrow = 2, ncol = 1)   
  m1 <- mean(x1) 
  s1 <-sd(x1) 
  out1 <- list( mean = m1, stdev = s1) # The list is the object we're creating
  return(out1)
}

summary3( input1 )
output3 <- summary3( input1 )
output3$mean
output3$stdev

# Return better ouput
summary4 <- function( x1 ){
  m1 <- mean( x1 ) 
  s1 <- sd( x1 ) 
  m2 <- median( x1 )
  q1 <- quantile( x1, c(0, 0.25, 0.5, 0.75, 1) )
  out1 <- list( mean = m1, 
                stdev = s1,
                median = m2, 
                quantiles = q1)
  return(out1)
}

# x1 <- input1
summary4( input1 )

#############################################################################################################
#Skyler Kuhn
#R: Day 8 Notes: 11/02/2015
# 11/2/2015
# Random number generation


# Uniform Distribution
# Each value between 0 and 1 (or a and b) are equally likely
# runif(how_many_values_it_spits_out, minimum_possible_value, Maximum_possible_value)
runif( 1, 0, 1)


# If you don't like the randomness, set a seed
# A seed is a starting point (starting point by default is exact time you hit 'run')
# For repeatability --> set.seed
set.seed(123)
runif( 1, 0, 1 )


# Create a histogram of a uniform random variable 
x1 <- runif( 10000, 0, 1)
hist( x1 ) 

# Poisson random variable --> generates ONLY whole numbers 
# rpois( how_many_you_want, mean(this_also_=_variance))
# Poisson's are always skewed right and are never negative 
rpois(10, 5)
x2 <- rpois( 10000, 5)
hist( x2 ) 


# Beta random variable
# rbeta( how_many_we_want, we_will_learn_these_later, /we_will_learn_by_playing_with_numbers)
# Not normally distributed --> we told it to come from beta dist. not a normal dist.
rbeta( 10, 2, 5)
x3 <- rbeta( 10000, 5, 2)
hist( x3, xlim=c( 0, 1 ) ) 


# Gamma random variable
# As n gets bigger, a gamma distribution gets closer and closer (approaches) to normal dist. --> will never
#    be normally distributed 
rgamma( 10, 2, 5)
x4 <- rgamma( 10000, 5000, 2)
hist( x4 ) 


# Normal Random Variable
# rnorm( how_many_we_want, mean, standard_deviation)
rnorm( 10, 10, 2)
x5 <- rnorm( 10000, 10, 2 )
hist( x5 )


# Sample from a vector
# sample(what_vector, how_many_you_want_to_pull, replace?, Prob=Null?)
# replacement --> if it is used, can't be used again --> default is no
# for bootstrapping replace=true
# prob=NULL means all values are equally likely 
# prob--> you can increase a certain demographic's probability of being pulled (weighting)
sample()

#############################################################################################################
#Skyler Kuhn
#R: Day 9 Notes: 11/04/2015
#Topic: Looping

# for(i in l:n){
#   Do stuff repeastedly
# }
# Repeat n times, i is the index  (it will iterate through)

#Simple Looping
data1 <- c(14, 22, 183, 94, 63, 13, 25, 98, 5, 10.2, 56, 19, 77, 99, 17.3, 3, 5, 86, 1, 2)
#manaually entering data that will be used later

data1.sum <- 0  #container for the result initialized at 0
n1 <- length(data1)  

for (i in 1:n1){
  data1.sum <- data1.sum + data1[i]
  print (data1.sum)
} #starts at 1 ends at n1, adding each value to find the sum

data1.mean <- 0  #container for the result initialized at 0
n1 <- length(data1)  

for (i in 1:n1){
  data1.mean <- data1.mean + data1[i]/n1  #n1 is the length or # of values, needed to find mean
  print (data1.mean)
} #starts at 1 ends at n1, adding each value to find the sum
#you could have taken data1.sum and divided by n1 #this is faster when it is compiled
#this is a bad example, essentially

#Integrate the normal distribution
normal1 <- function(x, mu, sigma){  #creating a function for the normal dist
  out1 <- 1/sqrt(2*pi*sigma^2)
  exponent1 <- -1/(2*sigma^2)*(x-mu)^2
  out1 <- out1*exp(exponent1)
}

mu1 <-15     #variables used for the normal dist fucntion
sigma1 <- 2
x1 <- seq(-15,35, by = 0.001) #this is a sequence, goes from -15 to 35 by a very small increment(0.001) 
#the smaller the increment "0.001" the more accurate the answer

normaldist1 <- normal1(x1, mu1, sigma1) #passing variables into function
plot(x1, normaldist1, type = "l")  #this make the plot of the normal dist

#####
#Use this to run the function (just change the variables):
mu1 <-0    #variables used for the normal dist fucntion
sigma1 <- 1
x1 <- seq(-15,-1, by = 0.001)

Integrated.normal <- 0  #container for the result initialized at 0
n1 <- length(x1)  

for (i in 1:n1){   #the for loop is used to integrate the function, this will do it for you
  Integrated.normal <- Integrated.normal + normal1(x1[i], mu1, sigma1)*0.001  #n1 is the length or # of values, needed to find mean
  #print (data1.mean)
} 
Integrated.normal #should be equal to one

#############################################################################################################
#Skyler Kuhn
#R: Day 10 Notes: 11/09/2015
#Topic: Looping with functions
# Put loops and functions together 

Cyclerdata1 <- read.csv( file.choose(), header = TRUE )
head( Cyclerdata1 )

# Want to create our own summary function
my.summary1 <- function( X ){  #x is our input
  x1 <- as.matrix( X ) #x1 we are forceing it to be a matrix
  col1 <- ncol( x1 ) # col1 is telling us how many columns (variables) we are using 
  colnames1 <- colnames(x1) #variable used to create matrix header (CPK1,CPK2,CPK3,CPK4)
  #container for the results
  result1 <-matrix(0, ncol = col1, nrow = 8) #8 for m1,s1,m2,q1,0,1,0.25,0.5,0.75, 1
  for(i in 1:col1){
    m1 <- mean( x1[ ,i] ) #will loop through each iteration in columns [row,column]
    s1 <- sd( x1[ ,i]) 
    m2 <- median( x1[ ,i])
    q1 <- quantile( x1[ ,i], c(0, 0.25, 0.5, 0.75, 1) ) 
    result1[1,i] <- m1
    result1[2, i] <- s1
    result1[3, i] <- m2
    result1[4:8, i] <- q1
  }
  colnames(result1) <- colnames1 #adding labels to each column
  rownames(result1) <-c("Mean", "St. Dev", "Median", "Min", "Q1", "Q2", "Q3", "Maximum") #adding labels to each row
  return(result1)
}


my.summary1(Cyclerdata1[ ,5:8]) 

#### While Looping: Another type of looping
#Does something unitl the condition is met (while something is true keep going)
#DON'T FORGET: you must initialize the condition above the while statement!
#
# while(condition){
#   
#   do stuff repeatedly
#   
#   }

#we want to creat eour own sumamry function USING A WHILE LOOP!!!
# Want to create our own summary function
my.summary2 <- function( X ){  #x is our input
  x1 <- as.matrix( X ) #x1 we are forceing it to be a matrix
  col1 <- ncol( x1 ) # col1 is telling us how many columns (variables) we are using 
  colnames1 <- colnames(x1) #variable used to create matrix header (CPK1,CPK2,CPK3,CPK4)
  #container for the results
  result1 <-matrix(0, ncol = col1, nrow = 8) #8 for m1,s1,m2,q1,0,1,0.25,0.5,0.75, 1
  #INTIALIZE WHILE LOOP!
  i <-1
  while(i <= col1){  #col1 is equal to four
    m1 <- mean( x1[ ,i] ) #will loop through each iteration in columns [row,column]
    s1 <- sd( x1[ ,i]) 
    m2 <- median( x1[ ,i])
    q1 <- quantile( x1[ ,i], c(0, 0.25, 0.5, 0.75, 1) ) 
    result1[1,i] <- m1
    result1[2, i] <- s1
    result1[3, i] <- m2
    result1[4:8, i] <- q1
    #UPDATE THE CONDITION
    i <- i + 1
  }
  colnames(result1) <- colnames1 #adding labels to each column
  rownames(result1) <-c("Mean", "St. Dev", "Median", "Min", "Q1", "Q2", "Q3", "Maximum") #adding labels to each row
  return(result1)
}

my.summary2(Cyclerdata1[ ,5:8])

#############################################################################################################
#Skyler Kuhn
#R: Day 11 Notes: 11/11/2015
#Topic: 
#using portfolio.csv file


port1 <- read.csv(file.choose(), header = TRUE) #use the window that soon opens to choose your file (that we saved to desktop)
head(port1)

#If statement example:
#Logic Statements
#If (condition){
#   do something
#}

#If, else statement example:
#If (condition){
#   do something
#}else{
#   do something else
#}

# Conversion Rates:
# USD to GBP: 0.66  (1 USD is 0.66 GBP)
# USD to CAD: 1.33 (1 USD is 1.33 GBP)

#COnvert all of the Portfolio values to USD
dim(port1)   # tells you how many rows you have, it gives you the dimensions of the dataset (29570 X 4)



ValueinUSD <- rep(0, n1) #Create a container to hold a new value
n1 <- dim(port1)[1] #grabs the number of rows 
for(i in 1:n1){
  currentrow <- port1[i, ]
  if (currentrow$Country == "UK"){
    value <- currentrow$Portfolio/0.66
  }
  if (currentrow$Country == "CAN"){        #the way it is written in The dataset
    value <- currentrow$Portfolio/1.33
  }
  if (currentrow$Country == "USA"){        #the way it is written in the dataset
    value <- currentrow$Portfolio
  }
  ValueinUSD[i] <- value
}

port2 <- cbind(port1, ValueinUSD)
head(port2)

#Complex rules to charge fees (making this up).
#based on the value of after the decimal point (the penny amount)
#BASED OFF OF A FLAT FEE OF:
  #mANGAED: $100
  #UNMANGED: $250
  #A PERCENTAGE BASED OFF OF CENTS

Yacht1 <- rep(0,n1)
for(i in 1:n1){
  currentrow <- port2[i, ]
  Fee <- 0
  if(currentrow$Country == "UK"){
      if (currentrow$Managed == "Y"){
        Fee <- Fee + 100
      
      }else{
        Fee <- Fee + 250
      }
      cents1 <- currentrow$ValueinUSD - floor(currentrow$ValueinUSD) 
      if(cents1 <= 0.25){
        Fee <- Fee + 0.085*currentrow$ValueinUSD
      }
      if (cents1 > 0.25 & cents1 <= 0.5){
        Fee <- Fee + 0.1*currentrow$ValueinUSD 
      }
      if (cents1 > 0.5 & cents1 <= 0.99){
        Fee <- Fee + 0.113*currentrow$ValueinUSD 
      }
      
  }
 Yacht1[i] <- Fee 
}  
port3 <- cbind(port2, Yacht1)
head(port3) 
#port3[port3$Yahct1 == 100 | port3$Yahct1 == 250, ]
sum(port3$Yacht1) #our total earnings for the UK
  
#############################################################################################################
#Skyler Kuhn
#R: Day 12 Notes: 11/12/2015
#Topic: while loop-ing with a function (finding the area under the curve)

#while(condition){
# do somthing here
#}

## This code estimates the area under the t curve with df=5 between
## two values a=2 and b=2.  (You can change a, b, and the d.f. at the
## top of the code to adjust the program.)



## These are the quantiles we're trying to find.
a <- -2
b <- 2
## This is degrees of freedom we're using.
df <- 5



## Start with epsilon set to NULL and currApprox set to 0.
epsilon <- NULL
currApprox <- 0.0
## Start with n boxes.
n <- 10
while ( is.null(epsilon) || (epsilon > 0.000001) ){
  
  ## Calculate the box width and the midpoints of the boxes, based on how many there.
  widthBox <- (b-a)/n
  midPts <- seq(from=a+(0.5*widthBox), by=widthBox, length=n)
  
  ## Evaluate the height at the midpoints.
  hts <- dt(midPts, df)
  ## Evaluate the areas of the boxes.
  areasBoxes <- hts * widthBox
  ## Adding up these areas gives the approximation to the area under the curve.
  newApprox <- sum(areasBoxes)
  
  
  ## What is the absolute difference between this approximation vs the last one?
  epsilon <- abs(currApprox - newApprox)
  ## Print out new approximation and epsilon.
  cat(paste0("With n=", n, " we get answer=", newApprox, ", with abs. difference=", epsilon, "\n"))
  
  
  ## Replace current approximation with new one.
  currApprox <- newApprox
  ## Increase number of boxes for next step.
  n <- 2*n
}



## For use in checking our work:
realAnswer <- pt(b, df) - pt(a, df)
diffRealMinusApprox <- realAnswer - currApprox
print(diffRealMinusApprox)


#####################Second Half of Stuff:
## This code provides a function to estimate the c.d.f. of the t
## distribution.  Note that it begins evaluation at a lower limit of
## -100, since we cannot tell the computer to start at negative
## infinity.  As a result, it doesn't do a great job at evaluation the
## c.d.f. for very low t values.  So, I've limited the function so
## that it only accepts values greater than -4 for evaluation.  (It
## returns an error otherwise.)


approxTcdf <- function(tValue, df, nBoxes=10){
  
  ## Start with epsilon set to NULL and currApprox set to 0.
  epsilon <- NULL
  currApprox <- 0.0
  ## Set n to nBoxes.
  n <- nBoxes
  ## Set the lower limit to -100.0.
  lowLimit <- -100.0
  
  ## This function doesn't work well with quantiles less - 4.
  if (tValue <= lowLimit)
    stop("\nThis function only accepts quantiles greater than -4.\n")
  
  while ( is.null(epsilon) || (epsilon > 0.00000001) ){
    
    ## Calculate the box width and the midpoints of the boxes,
    ## based on how many there.
    widthBox <- (tValue-lowLimit)/n
    midPts <- seq(from=lowLimit+(0.5*widthBox), by=widthBox, length=n)
    
    ## Evaluate the height at the midpoints.
    hts <- dt(midPts, df)
    ## Evaluate the areas of the boxes.
    areasBoxes <- hts * widthBox
    ## Adding up these areas gives the approximation to the area
    ## under the curve.
    newApprox <- sum(areasBoxes)
    
    
    ## What is the absolute difference between this approximation
    ## vs the last one?
    epsilon <- abs(currApprox - newApprox)
    
    
    ## Replace current approximation with new one.
    currApprox <- newApprox
    ## Increase number of boxes for next step.
    n <- 2*n
  }
  return(currApprox)
}



## Call function for a few test values.
df <- 10
tValue <- 2.0
approx <- approxTcdf(tValue, df)
pt(tValue, df)

df <- 5
tValue <- -1.5
approx <- approxTcdf(tValue, df)
pt(tValue, df)


df <- 3
tValue <- -4.0
approx <- approxTcdf(tValue, df)
pt(tValue, df)


df <- 30
tValue <- 6.0
approx <- approxTcdf(tValue, df)
pt(tValue, df)

#############################################################################################################
#Skyler Kuhn
#R: Day 13/14 Notes: 11/(18/23)/2015
#Topic: Using a package: twitteR, username: bobsagett13 Abc12345
#https://twitter.com/

#Using the package we have downloaded
library(httr) 
library(twitteR)

#now we are going to get 
#http://www.r-bloggers.com/getting-started-with-twitter-in-r/

#getTwitterOAuth(consumer_key,consumer_secret)

######Application Settings
#Consumer Key (API Key) GsYm2mzsuIZbsWxpwZL7k1Yc4
#Consumer Secret (API Secret) ccenJqNP3U8KSWTGndD5TdB6eoYTfza8A5mwHn0aunrYxDqk3O
#Access Level Read and write (modify app permissions)
#Owner bobsagett13
#Owner ID 4219122813 

######Access Token
#Access Token 4219122813-7WryAjedOdx1QfKQZgSHhNzDQchhGbGuueweDG0
#Access Token Secret QJHhv7C1nPewgDBh4uGXjqCMpaP3l1bUrkhg1TRZ7QadD
#Access Level Read and write
#Owner bobsagett13
#Owner ID 4219122813
######Call Back:
#http://127.0.0.1:1410

#Twitter Code 
# authorisation
if (!require("pacman")) install.packages("pacman")
pacman::p_load(twitteR, ROAuth, RCurl)

api_key = "GsYm2mzsuIZbsWxpwZL7k1Yc4"
api_secret = "ccenJqNP3U8KSWTGndD5TdB6eoYTfza8A5mwHn0aunrYxDqk3O"
access_token = "4219122813-7WryAjedOdx1QfKQZgSHhNzDQchhGbGuueweDG0"
access_token_secret = "QJHhv7C1nPewgDBh4uGXjqCMpaP3l1bUrkhg1TRZ7QadD"

# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

# set up the URLs
reqURL = "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"

twitCred = OAuthFactory$new(consumerKey = api_key, consumerSecret = api_secret, requestURL = reqURL, accessURL = accessURL, authURL = authURL)

twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

7552147

if (!require("pacman")) install.packages("pacman")
pacman::p_load(devtools, installr)
install.Rtools()
install_url("http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")

########## This is where the code starts to work and the setup is done.  Or should be done ###########
##1.) Hightlight from here:
if (!require("pacman")) install.packages("pacman")
pacman::p_load(twitteR, sentiment, plyr, ggplot2, wordcloud, RColorBrewer, httpuv, RCurl, base64enc)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

api_key = "GsYm2mzsuIZbsWxpwZL7k1Yc4"
api_secret = "ccenJqNP3U8KSWTGndD5TdB6eoYTfza8A5mwHn0aunrYxDqk3O"
access_token = "4219122813-7WryAjedOdx1QfKQZgSHhNzDQchhGbGuueweDG0"
access_token_secret = "QJHhv7C1nPewgDBh4uGXjqCMpaP3l1bUrkhg1TRZ7QadD"


setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# setup_twitter_oauth(api_key,api_secret)

# harvest some tweets
some_tweets = searchTwitter("Starbucks", n=100, lang="en")  ##2.) End highlighting here:
head(some_tweets)                                       ##3.) them run this!!! To get tweets :)

some_tweets2 <- twListToDF(some_tweets) #puts information in a dataframe, can be used to grab stuff (like usernames)
head(some_tweets2)
 
#Get the usernames of those who tweeted
some_tweets2_user <- some_tweets2$screenName
some_tweets2_user #grabs usernames 


#Search the text terms

tweet_text <- some_tweets2$text #grabbing the text only
tweet_text [1]
regexpr("away", tweet_text[1])   #Searches through a text string for a given word. Gives you the position of the word being searched, if it returns "-1" the word is not there.

Bad1 <- 0  #Container to count the bad responses
Good1 <- 0   #Container to count the good responses

#Create  a dictionary of "Good " words
GW1 <- c("love", "like", "happy", "yummy")
BW1 <- c("hate", "horrible", "stupid", "sucks", "burnt")

Badpeople <- c()
Goodpeople <- c()

for(i in 1:1000)
{
  if(!is.na(tweet_text[i]))    # avoids empty tweets (tweets with no text)
  {
  for(goodword in GW1)  #Iterating through our keywords and grabbing there username if their tweet contains one of our words
  {
      goodflag <- regexpr(goodword, tweet_text[i])[1] 
      if(goodflag  >  0)
      {
        Good1 <- Good1 + 1 
        Goodpeople <- rbind(Goodpeople, some_tweets2_user[i])
      }
  }
  for(badword in BW1)
  {
    badflag <- regexpr(badword, tweet_text[i])[1] 
    if(badflag  >  0)
    {
      Bad1 <- Bad1 +1
      Badpeople <- rbind(Badpeople, some_tweets2_user[i])
    }
  }}
}

print(Badpeople) #prints the usernames 
print(Goodpeople)


#Word Cloud (Was not working earlier)
tweet1 <- Corpus(VectorSource(some_tweets2$text))
tweet2 <- tm_map(tweet1, removePunctuation) #goes through list and removes punctuation
tweet3 <- tm_map(tweet2, removeWords, c('starbucks', stopwords("en")))  #giving it a list of words to remove 
wordcloud(tweet3, max.words = 50)











#######################################################################################################
###################################################################Code we did not use in Twitter Hack
#######################################################################################################
# get the text
some_txt = sapply(some_tweets, function(x) x$getText())

# Perform Sentiment Analysis
# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]
# Create data frame with the results and obtain some general statistics
# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))



# Separate the text by emotions and visualize the words with a comparison cloud
# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = some_txt[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)

#######################################################################################################
#######################################################################################################

