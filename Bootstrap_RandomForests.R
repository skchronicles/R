#######################################################################################################################
# Skyler Kuhn
# BIOS 667: Data Mining and Machine Learning 
# Bootstrap and Random Forest Application
#######################################################################################################################

# Random Forest Application ###################

# Using the hmwk5.csv dataset, derive a random forest consisting of 2500 classification trees in the forest, where
# again the classifier predicts type using all other variables in the hmwk5.csv dataset. Use set.seed(123) prior to
# running your RF.

library(randomForest)

hm5<-read.csv("hmwk5.csv",header=TRUE)

set.seed(123)

hm5.rf<-randomForest(y = hm5$type, x = hm5[-1] ,data=hm5, importance=TRUE, proximity=TRUE, ntree=2500)

hm5.rf

# Call:
#   randomForest(x = hm5[-1], y = hm5$type, ntree = 2500, importance = TRUE,      proximity = TRUE, data = hm5) 
# Type of random forest: classification
# Number of trees: 2500
# No. of variables tried at each split: 14
# 
# OOB estimate of  error rate: 18.99%
# Confusion matrix:
#   advanced early HCC Normal class.error
# advanced       31     1   1      0  0.06060606
# early          11     0   1      0  1.00000000
# HCC             1     0  27      0  0.03571429
# Normal          0     0   0      6  0.00000000


# a) What is the out-of-bag estimate of error for the random forest?
## OOB error rate is 18.99%


# b) Looking at the confusion matrix, which class is most frequently misclassified?
## the "early" class is most often missclassified with a classification error -- rate of 100%


# c) Plot the mean decrease in accuracy and the mean decrease in Gini index variable importance measures.
par(mfrow = c(1,2))
varImpPlot(hm5.rf, type = 1, pch = 19, col = 1, cex = 0.5, main = "")
varImpPlot(hm5.rf, type = 2, pch = 19, col = 1, cex = 0.5, main = "")


# d) Suppose you set a rule such that the jth variable is considered important, if it's mean Decrease in Accuracy
# (Dj) is greater than the overall average Mean Decrease in Accuracy ( ^mD ) + 1.96 times the SD ( ^sD ). How
# many variables are important, and what are they?

#overall average mean decrease in accuracy
over.mean<-mean(hm5.rf$importance[,"MeanDecreaseAccuracy"])

## Extract the values
Acc = importance(hm5.rf, scale = F)[,"MeanDecreaseAccuracy"]
##Gini = importance(hm5.rf)[,"MeanDecreaseGini"]

# Set the indicator! 
I = ifelse(Acc > mean(Acc) + 1.96*sd(Acc), 1, 0)
sum(I)
names(I[I==1])
which(I==1)


### You can also extract importance directly from the randomForest object.
imp = hm5.rf$importance
mean = mean(imp[,5])
sd = sd(imp[,5])
which(imp[,5] > mean + 1.96*sd)


## As an aside, let's see the performance using this list of 11 variables

x.imp = hm5[,names(I[I==1])]

hm5.rfimp <-randomForest(y = hm5$type, x = x.imp ,data=hm5, importance=TRUE, proximity=TRUE, ntree=2500)
print(hm5.rfimp)
print(hm5.rf)

### Note, OOB error is the same; thus the 7 variables creates a more parsimonious set! 

# e) Perform multi-dimensional scaling in 2 dimensions using the proximities and produce a scatterplot with a
# label indicating the sample class (type). Which classes are well separated and which are not? Do you
# suspect any outliers? [MDS can be implemented using the *cmdscale* function]. 

MDSplot(hm5.rf, hm5$type, k = 2, pch = 19)
legend("topleft", pch = c(19,19,19,19), col = c("chartreuse4", "blue", "red", "purple"), legend = c("HCC", "early", "Advanced", "Normal"), title = "Cancer Type") 

### HCC and Normal well separated. Advanced and Early are not so well separated. 
### HCC point close to (0, -0.1) can be an outlier;
### Early point located at (-0.2, -0.1) can be an outlier!


#######################################################################################################################
# Bootstrap Application

# Eighty-eight students took 5 separate tests in the following quantitative subjects: mechanics, vectors, algebra,
# analysis, and statistics. The first two tests were closed book, while the last three tests were open book. Download
# the test results (see the R workspace scor.R in blackboard) to an appropriate directory. Use the command >
# source("scor.R") to get the data matrix into R.
source("scor.R")

# Reported the eigenvalues and eigenvectors.

G<-cov(scor)

eig<-eigen(G)

#eigenvalues
eig$values
## [1] 686.98981 202.11107 103.74731  84.63044  32.15329

#eigenvectors
eig$vectors
# Output:
#           [,1]        [,2]       [,3]         [,4]        [,5]
# [1,] -0.5054457  0.74874751 -0.2997888  0.296184264 -0.07939388
# [2,] -0.3683486  0.20740314  0.4155900 -0.782888173 -0.18887639
# [3,] -0.3456612 -0.07590813  0.1453182 -0.003236339  0.92392015
# [4,] -0.4511226 -0.30088849  0.5966265  0.518139724 -0.28552169
# [5,] -0.5346501 -0.54778205 -0.6002758 -0.175732020 -0.15123239


# b.) Calculate theta for score data
theta.hat<-eig$values[1]/sum(eig$values)
print(theta.hat)
## [1] 0.619115
#  or, 
theta.hat<- max(eig$values)/sum(eig$values)
print(theta.hat)


# c.) Use the bootstrap method to estimate the standard error of Theta.
thetab<-numeric()
set.seed(123)
for (b in 1:100000){
  new.sample<-sample(1:dim(scor)[1], replace=TRUE)
  eigs<-eigen(cov(scor[new.sample,]))$values
  thetab[b]<-max(eigs)/sum(eigs)
}


#hist(thetab)
#standard error of theta hat
sqrt(var(thetab))

### [1] 0.04755293


#### Instead of programming, you can taking advantage of the boot() function available in the R library *boot*
myfun = function(data, i, expr) {
  d = data[i,]
  g = cov(d)
  eig = eigen(g)
  return( eig$values[1]/sum(eig$values) )
}

set.seed(123)
boot.theta = boot(data = scor, statistic = myfun, R = 100000)
std.error = sd(boot.theta$t)
print(std.error)

### [1] 0.0475263



