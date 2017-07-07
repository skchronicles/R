####################################################################################################
#Skyler Kuhn
#CSCI 241: Data Structures and Algorithms 
#Creating a graph of the average of five performance test runs of two different sorting algorithms
##Algorithms used were insertion sort and selection sort 
###See python program, Sort.py, for code
####################################################################################################

library(ggplot2)

#######################
##Increasing Array List
#Average time for sorting arrays sized 1000-10000 using the Insertion method:
inc1000InsertionSort.avg <- mean(c(0.00028000000000000247,0.00030499999999999972,0.00027800000000000047,0.00027100000000002122))
inc2500InsertionSort.avg <- mean(c(0.00073199999999998266,0.00067099999999997717,0.0006610000000000226,0.00067200000000000593))
inc5000InsertionSort.avg <- mean(c(0.00129600000000007487,0.00130899999999989358,0.00130700000000005812,0.00129999999999996785))
inc7500InsertionSort.avg <- mean(c(0.00367899999999998784,0.00197000000000002728,0.00217999999999962668,0.00200899999999970547))
inc10000InsertionSort.avg <- mean(c(0.00434200000000029007,0.00291300000000038750,0.00325299999999995038,0.00261499999999958987))
#Average time for sorting arrays sized 1000-10000 using the Selection method:
inc1000SelectionSort.avg <- mean(c(0.060978,0.055868,0.055665,0.055377))
inc2500SelectionSort.avg <- mean(c(0.36791700000000004955,0.35557599999999994766,0.36621100000000006425,0.35592699999999999338))
inc5000SelectionSort.avg <- mean(c(1.440655,1.435397,1.550557,1.463427))
inc7500SelectionSort.avg <- mean(c(3.225616,3.559395,3.832289,3.267347))
inc10000SelectionSort.avg <- mean(c(5.761511,6.968419,7.148877,5.813407))

ArraySize <- c(1000,2500,5000,7500,10000)
incArrayData.Insertion <- c(inc1000InsertionSort.avg,inc2500InsertionSort.avg,inc5000InsertionSort.avg,
                            inc7500InsertionSort.avg,inc10000InsertionSort.avg)
incArrayData.Selection <- c(inc1000SelectionSort.avg,inc2500SelectionSort.avg,inc5000SelectionSort.avg,
                          inc7500SelectionSort.avg,inc10000SelectionSort.avg)
df_IncArr <- data.frame(ArraySize, incArrayData.Insertion, incArrayData.Selection)

#Creating the Graph wih ggplot2:
graphIncr <- ggplot(df_IncArr, aes(x= ArraySize, y=incArrayData.Insertion)) +
  geom_line(aes(color="Insertion")) + geom_line(aes(y=incArrayData.Selection, color="Selection")) +
  xlab("Size of Array (Elements)") + ylab("Time Requiredto Sort Array (Seconds)") + ggtitle("Array of Increasing Values") + 
  scale_color_discrete(name="Sorting Algorithm") + theme(plot.title = element_text(size=17, face="bold", 
                                                                                   margin = margin(10, 0, 10, 0))) #adds space betwwen title and graph
graphIncr

#######################
##Decreasing Array List
#Average time for sorting arrays sized 1000-10000 using the Insertion method:
dec1000InsertionSort.avg <- mean(c(0.13185999999999964416,0.16380299999999969884,0.16318099999999979843,0.14073299999999910881))
dec2500InsertionSort.avg <- mean(c(0.83652800000000127056,1.03068899999999885608,1.02892799999999873251,0.84257099999999951478))
dec5000InsertionSort.avg <- mean(c(3.35274600000000155831,4.11355300000000134730,4.11568100000000214322,3.38294699999999970430))
dec7500InsertionSort.avg <- mean(c(7.47098500000000242949,9.29339999999999832880,8.26715900000000303294,7.72270500000000126306))
dec10000InsertionSort.avg <- mean(c(13.33190699999999750958,16.47400299999999617739,13.41478699999999690817,13.66432700000000011187))
#Average time for sorting arrays sized 1000-10000 using the Selection method:
dec1000SelectionSort.avg <- mean(c(0.05970099999999867180,0.06931400000000031980,0.07021199999999971908,0.05979100000000059367))
dec2500SelectionSort.avg <- mean(c(0.83652800000000127056,0.44437499999999907629,0.44787699999999830425,0.37134500000000159048))
dec5000SelectionSort.avg <- mean(c(1.48510500000000078558,1.82723999999999975330,1.83534899999999723263,1.56106700000000131467))
dec7500SelectionSort.avg <- mean(c(3.34111000000000046839,4.11433599999999799479,3.38827399999999911984,3.43813000000000101863))
dec10000SelectionSort.avg <- mean(c(6.09477900000000261116,7.26632000000000033424,5.91874500000000125510,5.96806000000000125283))

ArraySize <- c(1000,2500,5000,7500,10000)
decArrayData.Insertion <- c(dec1000InsertionSort.avg,dec2500InsertionSort.avg,dec5000InsertionSort.avg,
                            dec7500InsertionSort.avg,dec10000InsertionSort.avg)
decArrayData.Selection <- c(dec1000SelectionSort.avg,dec2500SelectionSort.avg,dec5000SelectionSort.avg,
                            dec7500SelectionSort.avg,dec10000SelectionSort.avg)

#Data needs to be stored in a data frame for ggplots 
df_decArr <- data.frame(ArraySize, decArrayData.Insertion, decArrayData.Selection)

#Creating the Graph wih ggplot2:
graphDecr <- ggplot(df_decArr, aes(x= ArraySize, y=decArrayData.Insertion)) +
  geom_line(aes(color="Insertion")) + geom_line(aes(y=decArrayData.Selection, color="Selection")) +
  xlab("Size of Array (Elements)") + ylab("Time Required to Sort Array (Seconds)") + ggtitle("Array of Decreasing Values") + 
  scale_color_discrete(name="Sorting Algorithm") + theme(plot.title = element_text(size=17, face="bold", 
                                                                                   margin = margin(10, 0, 10, 0))) #adds space betwwen title and graph
graphDecr

###################
##Random Array List
#Average time for sorting arrays sized 1000-10000 using the Insertion method:
ran1000InsertionSort.avg <- mean(c(0.07169600000000286855,0.08386200000000343380,0.06857400000000524187,0.06971500000000219188))
ran2500InsertionSort.avg <- mean(c(0.44792999999999949523,0.53861100000000305954,0.41583699999999623742,0.42413099999999559486))
ran5000InsertionSort.avg <- mean(c(1.68547600000000130649,2.15412999999999499323,1.67097899999999555121,1.72038500000000027512))
ran7500InsertionSort.avg <- mean(c(3.77163900000000040791,5.14552200000000681257,3.77351600000000075852,3.81280100000000032878))
ran10000InsertionSort.avg <- mean(c(7.09342099999999931015,9.40723699999999496413,6.71576899999999454849,6.92012699999999369993))
#Average time for sorting arrays sized 1000-10000 using the Selection method:
ran1000SelectionSort.avg <- mean(c(0.05785099999999943066,0.07188299999999969714,0.06364800000000059299,0.06220799999999826468))
ran2500SelectionSort.avg <- mean(c(0.37046700000000498676,0.46810099999999721376,0.36879400000000117643,0.38567199999999957072))
ran5000SelectionSort.avg <- mean(c(1.53563499999999919510,1.86729700000000065074,1.54721299999999928332,1.51780300000000067939))
ran7500SelectionSort.avg <- mean(c(3.43027299999999968350,4.80222099999998874864,3.39512099999999605870,3.46561899999999667443))
ran10000SelectionSort.avg <- mean(c(6.12301499999999521151,8.47622800000000609089,6.26509899999999220199,6.26319499999999607098))

ArraySize <- c(1000,2500,5000,7500,10000)
ranArrayData.Insertion <- c(ran1000InsertionSort.avg,ran2500InsertionSort.avg,ran5000InsertionSort.avg,
                            ran7500InsertionSort.avg,ran10000InsertionSort.avg)
ranArrayData.Selection <- c(ran1000SelectionSort.avg,ran2500SelectionSort.avg,ran5000SelectionSort.avg,
                            ran7500SelectionSort.avg,ran10000SelectionSort.avg)

#Data needs to be stored in a data frame for ggplots 
df_ranArr <- data.frame(ArraySize, ranArrayData.Insertion, ranArrayData.Selection)

#Creating the Graph wih ggplot2:
graphranr <- ggplot(df_ranArr, aes(x= ArraySize, y=ranArrayData.Insertion)) +
  geom_line(aes(color="Insertion")) + geom_line(aes(y=ranArrayData.Selection, color="Selection")) +
  xlab("Size of Array (Elements)") + ylab("Time Required to Sort Array (Seconds)") + ggtitle("Array of Random Values") + 
  scale_color_discrete(name="Sorting Algorithm") + theme(plot.title = element_text(size=17, face="bold", 
                                                                                    margin = margin(10, 0, 10, 0))) #adds space betwwen title and graph
graphranr

####################################################################################################
