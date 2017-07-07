#######################################################################################################################
# Skyler Kuhn
# BIOS 667: Data Mining and Machine Learning 
# Successive Orthogonalization Function
#######################################################################################################################

# Write a function that performs regression by successive orthogonalization (pp. 53-54) for two predictor variables. 
# To simplify, your function should be of the form f(y, var1, var2), rather than using model syntax. The function should return, bhat0, bhat1 and
# bhat2 as output. Apply your function to the simulated.csv data. Compare your results to those obtained  using  the lm function where y~x1+x2.
#----------------------------------------------------#
#----------------------------------------------------#


sim<-read.csv("simulated.csv",header=TRUE)


gs.ortho<-function(y,var1,var2) {
  beta<-c(0,0)
  #first solve for beta2 then solve for beta1
  for(i in 2:1){
    z0<-x0<-rep(1,length(y))
    
    #x?_1=?_01 z_0
    
    gam01<-crossprod(z0,var1)/crossprod(z0,z0)
    z1<-var1-gam01*z0
    
    
    ## x_2=?_02 z_0+?_12 z_1
    
    gam02<-crossprod(z0,var2)/crossprod(z0,z0)
    gam12<-crossprod(z1,var2)/crossprod(z1,z1)
    
    z2<-var2-(gam02*z0+gam12*z1)
    
    #Step 3
    beta[i]<-crossprod(z2,y)/crossprod(z2,z2)
    
    
    oldVar1<-var1
    var1<-var2
    var2<-oldVar1
  }
  
  beta0<-mean(y)-crossprod(beta[1],mean(var1))-crossprod(beta[2],mean(var2))
  
  return(c(beta0,beta))
}


print(estimates<-gs.ortho(y,x1,x2))



#### Now, check with the lm() fit. What do you infer?? 

q1.fit<-lm(y~(x1+x2),data=sim)
coef(q1.fit)