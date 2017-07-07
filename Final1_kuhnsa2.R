#Skyler Kuhn: STAT 321 Final Exam
#Version: 1
#12/7/2015

IC1 <- read.csv( file.choose(), header = TRUE )
head( IC1 )

# Determine the total income for the home
TotalIncome1 <- IC1$Income1 + 
           ifelse(is.na(IC1$Income2), 0, IC1$Income2 ) +
           ifelse(is.na(IC1$Income3), 0,  IC1$Income3 ) +
           ifelse(is.na(IC1$Income4), 0, IC1$Income4 )

# Determine the total distance to work for all in household
Dist1 <- ifelse( is.na( IC1$Distance1 ), 0, IC1$Distance1 ) +
         ifelse( is.na( IC1$Distance2 ), 0, IC1$Distance2 ) +
         ifelse( is.na( IC1$Distance3 ), 0, IC1$Distance3 ) +
         ifelse( is.na( IC1$Distance4 ), 0, IC1$Distance4 ) 

# Clean up missing values from the car ages.
CarAge1 <-  ifelse( is.na(IC1$CarAge1), 0, IC1$CarAge1 )
CarAge2 <-  ifelse( is.na(IC1$CarAge2), 0, IC1$CarAge2 )
CarAge3 <-  ifelse( is.na(IC1$CarAge3), 0, IC1$CarAge3 )
#CarAge3 < - ifelse( is.na(IC1$CarAge3), 0, IC1$CarAge3 ) #Error: object 'CarAge3' not found
CarAge4 <-  ifelse( is.na(IC1$CarAge4), 0, IC1$CarAge4 )

# Clean up missing values from the Incomes
Income1 <- ifelse(is.na(IC1$Income1), 0, IC1$Income1 )
Income2 <- ifelse(is.na(IC1$Income2), 0, IC1$Income2 )
Income3 <- ifelse(is.na(IC1$Income3), 0,  IC1$Income3 )
Income4 <- ifelse(is.na(IC1$Income4), 0, IC1$Income4 )

# Determine the average car age
n1 <- dim( IC1 )[1]
AvgCarAge1 <- rep( 0, n1 )
for( i in 1:n1 ){
  n_car1 <- 0
  SumCarAge1 <- CarAge1[i] + CarAge2[i] + CarAge3[i] + CarAge4[i]
  if( is.na( IC1$CarAge1[i]) ){
    n_car1 <- NA
  }else{
    n_car1 < - n_car1 + 1
  }
  if( !is.na(IC1$CarAge2[i]) ){
    n_car1 <- n_car1 + 1
  }
  if( !is.na(IC1$CarAge3[i]) ) {
    n_car1 <- n_car1 + 1
  }
  if( !is.na(IC1$CarAge4[i]) ){
    n_car1 <- n_car1 + 1
  }
  AvgCarAge1[i] = SumCarAge1/n_car1
}

MaxCarAge <- rep( 0, n1 )
# Determine the Maximum car age.
for( i in 1:n1 ){
  max1 <- 0
  if( is.na(IC1$CarAge1[i]) ){
    max1 <- NA
  }else{
    max1 <- CarAge1[i]
  }
  if( !is.na(IC1$CarAge2[i]) ){
    if( CarAge2[i] > max1 ){
      max1 <- CarAge2[i]
    }
  }
  if( !is.na(IC1$CarAge3[i]) ){
    if( CarAge3[i] > max1 ){
      max1 < - CarAge3[i]
    }
  }
  if( !is.na(IC1$CarAge4[i]) ){
    if( CarAge4[i] > max1 ){
      max1 <- CarAge4[i]
    }
  }
  MaxCarAge[i] <- max1
}

# Remove odd incomes from the dataset.
hist(TotalIncome1)

# Find the average and standard deviation of the middle 50% of the data
Upper.25.Income1 <- quantile( TotalIncome1, 0.75 )
Lower.25.Income1 <- quantile( TotalIncome1, 0.25 )
Middle.Income1 <- TotalIncome1[TotalIncome1 < Upper.25.Income1 & TotalIncome1 > Lower.25.Income1 ]
Mean.Middle.Income1 <- mean( Middle.Income1 )
Sd.Middle.Income1 <- sd( Middle.Income1)

Too.Low.Income1 <- Mean.Middle.Income1 - 2*Sd.Middle.Income1

Keep.Income1.index <- 1:n1[ TotalIncome1 > Too.Low.Income1 ]

# Find the priority score for each person.
n_keep1 <- length( Keep.Income1.index )
Priority1 <- rep( 0, n1 ) 

for( i in Keep.Income1.index ){
  priority.score1 <- 0
  if( !is.na( MaxCarAge[i]) ){
    if( MaxCarAge[i] > 7 ){
      priority.score1 <- priority.score1 + 10
    }
    if( MaxCarAge[i] > 10 ){
      priority.score1 <- priority.score1 + 10
    }
  }
  if( TotalIncome1[i] > Mean.Middle.Income1 ){
    priority.score1 <- priority.score1 + 10
  }
  if( TotalIncome1[i] > Upper.25.Income1 ){
    priority.score1 <- priority.score1*TotalIncome1[i]/Upper.25.Income1
  }
  Priority1[i] <- priority.score1
}

IC2 <- cbind(IC1, Priority1) #binding the new containers to a dataset
head(IC2)  #shows the first six lines of new dataset

########################### Determine the households with a priority.score in the top 10%.
print(Priority1)
Priority10 <- sort(Priority1, decreasing=TRUE)[1:(length(Priority1)/10)]
#The number of households in the top ten percent:
length(Priority10)

#manaually entering data that will be used later

counterd <- 0
data1.sum <- 0  #container for the result initialized at 0
n1 <- length(IC2)  
currentrow <- IC2[i, ]
n1 <- dim(IC2)[1] #grabs the number of rows 

for(i in 1:n1)
{
  currentrow <- IC2[i, ]
   if(is.element(currentrow$Priority, Priority10)){
    data1.sum <- data1.sum + currentrow$Distance1
    counterd <- counterd + 1
  }
}

#Total Sum of Distance1 (with a priority score of in the top 10%)
print(data1.sum)
print(counterd)

#Average Distance Traveled in the Distance1 Variable for the households with a priority score in the top 10%
distance1average <- data1.sum / counterd
print(distance1average)


# for (i in 1:n1)
# {
#   for(j in IC2)
#   {
#     if(data1[i] == currentrow$Priority1)
#     {
#     data1.sum <- data1.sum + currentrow$Distance1
#     print (data1.sum)
#     }
#   }
# } 
# print(data1.sum)
# 
# 
# #starts at 1 
# n2 <- dim(IC2)[1] #grabs the number of rows, used for looping
# 
# distance1list <- rep(0, n1) #Create a container to hold a new value
# 
# for(i in 1:n2)
# {
#   currentrow <- IC2[i, ] #[row,column], variable to hold the current row in the for loop iteration
#   if (currentrow$Priority1 ==  Priority10)
#   {
#     distance1a <- distance1a + currentrow$Distance1
#   }
#   distance1list[i] <- distance1a
# }


