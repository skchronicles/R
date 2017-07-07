#######################################################################################################################
# Skyler Kuhn
# BIOS 667: Data Mining and Machine Learning 
# Incremental Forward Stagewise Regression Function
#######################################################################################################################

# Write a function implementing Algorithm 3.4 “Incremental Forward Stagewise Regression”, available on 
# page 86 of your text in R, letting epsilon=0.001. Interpret the statement in  step  4 that reads,  “repeat...  until
# the residuals are uncorrelated with all the predictors,”  as “repeat  until  all  correlations  between  predictor  variables  
# and  the  current  residual  <  0.20.” Your  function may be of the form  f(y, xmatrix, epsilon=0.001, cor.threshold=0.20).
#----------------------------------------------------#
#----------------------------------------------------#


# The Incremental Forward Stepwise Procedure, given on p. 86, is a 
# LARS-like algothrim that selects the variable most correlated with the 
# RESIDUAL (not outcome), and adds a small-incremental step (epsilon) to the
# beta vector. The residual is recalculated and repeated. 


inc_fsr<-function(y, X, epi, cor.threshold) {
  
  ##Parameters
  #y = a vector (you can specify from data frame), which is the outcome of interest
  #X = a matrix of inputs (could just be extracted from data frame); doesn't have to be standardized, but we force standardization for Prob. # 4. 
  #epi = the incremental step added to the beta values at each level, = 0.001
  #cor.threshold = the maximum allowable correlation between the residual and the matrix of inputs, = 0.20
  
  
  #Standardize the Data 
  # z<-scale(as.matrix(X)) ### so, you may not use this line if you want to have a more general program
  
  z = as.matrix(X)
  
  beta<-rep(0,ncol(z)) ### initialize beta coefficients = 0
  delta<-rep(0,ncol(z)) 
  
  r = y ## Setting residual r = y
  
  iter<-1   #Iteration initalize--this is to count and avoid infinite loops.
  
  while( max(abs(cor(z,r)))>= cor.threshold && iter <= 10000000 ) {
    
    mc<-which.max(abs( cor(z,r) ) )
    delta[mc]<-epi*sign(cor(z[,mc],r))	#or sign(z[,mc]%*%r)
    
    beta[mc]<-beta[mc]+delta[mc]  ## Update beta
    r<- r-delta[mc]*z[,mc]	      ## Update residual
    iter<-iter+1
  }
  
  names(beta)<-colnames(X) #add names to beta_vector
  
  ###return the values you need
  return(   list("beta"=beta, "CorMax"=cor.threshold, "step_size"=epi, "iteration"=iter) )
  
}

#######################################################################################################################
# Testing:
# Applying the  function  written above to  a  prostate  cancer  dataset to  predict  lpsa  
# (use training observations only), after first standardizing the covariates. Compare the results from 
# this algorithm to the Lasso column of Table 3.3 on page 63 of your text with respect to the variable 
# selection results.
#----------------------------------------------------#
#----------------------------------------------------#
# Reading in the Dataset

prostate<-read.table("prostate.txt", row.names=1, header=TRUE)
prostate<-prostate[which(prostate$train==TRUE), ] ### Note, the prostate dataset has 67 *training* samples, the rest is *testing*
head(prostate)
length(prostate[,1]) ## Check whether we have 67 training samples extracted? 


#Outcome(Y) is log psa and X is matrix of covariates (cols 1-8) 
X<- scale(as.matrix(prostate[1:8]))
head(X)



##X1 = data.frame(int  = matrix(rep(1,67), nrow = 67), X) 

##Run the Incremental Forward Stagewise Procedure (IFSP)
prostate_fsr<-inc_fsr(y=prostate$lpsa, X, 0.001, 0.20)

prostate_fsr
prostate_fsr$beta


#Comments: LASSO and IFSP agree on eliminiting of age, lcp, and gleason. 
#ALthough ppg45 is in the IFSP model, it is very small and I can see
#why LASSO elimianted ppg45. If we lower the correlation thershold, ppg45 would
#probably be the next to go.

#######################################################################################################################
