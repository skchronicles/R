##############################################################################################################
#One Sampled Proportion Hypothesis Function:
#Skyler Kuhn 
#TO RUN: one.sampled.hypothesis.analysis(po= ?, n= ?, x= ?, alpha= 0.01 or 0.05, alternative= "less" or "greater" or "two.sided")
#use to round numbers: round(x, digits = 0)
one.sampled.hypothesis.analysis <- function(po, n, x, alpha, alternative){
  samplesizecheck1 <- n * po
  samplesizecheck2 <- n * (1 - po)
  phat <- x / n
  pe <- phat
  #se <- sqrt(po*(1 - po)/n)
  se <- sqrt(phat*(1 - phat)/n)
  if (samplesizecheck1 > 5 & samplesizecheck2 > 5){
    testStatistic <- ((phat - po) / (sqrt(po*(1 - po)/n)))
    if (alternative == "greater" & alpha == 0.05){
      CV = 1.645
      CVforCI = 1.96
      ci_1 <- pe + (CVforCI * se)
      ci_2 <- pe - (CVforCI * se)
      pvalue <- (1 - pnorm(testStatistic))
      #ci_1 <- pe + (CV * se)
      #ci_2 <- pe - (CV * se)     #inserting another logic control, if testStatistic is greater thn CV
      if(testStatistic > CV){
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4), round(pvalue, digits = 5),
                      "Reject the Ho in favor of the Ha")     #round(x, digits = 4), will round to 4 numbers
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values) #Creating a dataframe to hold our vaules
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Reject Ho in favor of the Ha"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ", 
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4), round(pvalue, digits = 5),
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
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4), round(pvalue, digits = 5),
                      "Fail to Reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Fail to reject Ho"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ", 
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4), round(pvalue, digits = 5),
                      "Fail to Reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_2, ci_1, "Fail to reject the Ho"))    ##########testing here   
        } 
      }
    }
    else if (alternative == "less" & alpha == 0.05){   
      CV = -1.645
      CVforCI = 1.96
      ci_1 <- pe + (CVforCI * se)
      ci_2 <- pe - (CVforCI * se)
      pvalue <- pnorm(testStatistic)
      if(testStatistic < CV){
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4), round(pvalue, digits = 5),
                      "Reject the Ho in favor of the Ha")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Reject Ho in favor of the Ha"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4), round(pvalue, digits = 5),
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
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4), round(pvalue, digits = 5),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Fail to reject Ho"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4), round(pvalue, digits = 5),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_2, ci_1, "Fail to reject the Ho"))       
        } 
      }
    }
    else if (alternative == "two.sided" & alpha == 0.05){
      CV = 1.96
      CVforCI = 1.96
      ci_1 <- pe + (CVforCI * se)
      ci_2 <- pe - (CVforCI * se)
      pvaule <- 2 * (1 - abs(pnorm(testStatistic)))
      if(testStatistic > CV){
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4), round(pvalue, digits = 5),
                      "Reject the Ho in favor of the Ha")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Reject Ho in favor of the Ha"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4), round(pvalue, digits = 5),
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
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4), round(pvalue, digits = 5),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Fail to reject Ho"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4), round(pvalue, digits = 5),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_2, ci_1, "Fail to reject the Ho"))       
        } 
      }
    }
    else if (alternative == "greater" & alpha == 0.01){
      CV = 2.33
      CVforCI = 2.575
      ci_1 <- pe + (CVforCI * se)
      ci_2 <- pe - (CVforCI * se)
      pvalue <- (1 - pnorm(testStatistic))
      if(testStatistic > CV){
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4), round(pvalue, digits = 5),
                      "Reject the Ho in favor of the Ha")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Reject Ho in favor of the Ha"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4), round(pvalue, digits = 5),
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
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4), round(pvalue, digits = 5),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Fail to reject Ho"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4), round(pvalue, digits = 5),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_2, ci_1, "Fail to reject the Ho"))       
        } 
      }
    }
    else if (alternative == "less" & alpha == 0.01){
      CV = -2.33
      CVforCI = 2.575
      ci_1 <- pe + (CVforCI * se)
      ci_2 <- pe - (CVforCI * se)
      pvalue <- pnorm(testStatistic)
      if(testStatistic < CV){
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4), round(pvalue, digits = 5),
                      "Reject the Ho in favor of the Ha")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Reject Ho in favor of the Ha"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4), round(pvalue, digits = 5),
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
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4), round(pvalue, digits = 5),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Fail to reject Ho"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4), round(pvalue, digits = 5),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_2, ci_1, "Fail to reject the Ho"))       
        } 
      }
    }
    else if (alternative == "two.sided" & alpha == 0.01){
      CV = 2.575
      CVforCI = 2.575
      ci_1 <- pe + (CVforCI * se)
      ci_2 <- pe - (CVforCI * se)
      pvaule <- 2 * (1 - abs(pnorm(testStatistic)))
      if(testStatistic > CV){
        if (ci_1 < ci_2){
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4), round(pvalue, digits = 5),
                      "Reject the Ho in favor of the Ha")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Reject Ho in favor of the Ha"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4), round(pvalue, digits = 5),
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
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_1, digits = 4), round(ci_2, digits = 4), round(pvalue, digits = 5),
                      "Fail to reject the Ho")
          One.Sampled.Hypothesis.Test.Data <- data.frame(One.Sampled.Hypothesis.Test, Values)
          return(One.Sampled.Hypothesis.Test.Data)
          #return (c(ci_1,ci_2,"Fail to reject Ho"))
        }else{
          One.Sampled.Hypothesis.Test <- c("Null Hypothesis (Ho): ", "Alternative Hypothesis (Ha): ", "Alpha (α): ",
                                           "Sample Size (n): ", "p-hat (sample proportion): ", "Confidence Interval (CV): ",
                                           "Test Statistic (z): ", "Lower Confidence Interval: ", "Upper Confidence Interval: ",
                                           "p-value: ", "Statistical Inference: ")
          Values <- c(po, alternative, alpha,
                      n, round(phat, digits = 4), CV,
                      round(testStatistic, digits = 4), round(ci_2, digits = 4), round(ci_1, digits = 4), round(pvalue, digits = 5),
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
one.sampled.hypothesis.analysis(po= 0.37, n= 1200, x= 420, alpha= 0.05, alternative= "two.sided")

#####################################################################################################
#Testing Random Parts of Code:
#---------------------------------------------------------------------------------------------------#
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

##Creating Dataframe results(for hypothesis analysis):
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

##Testing (Evreything works fine):
#Testing p vaule:
# z <- -1.435
# ##Ha: less
# pvalue <- pnorm(z) 
# ##Ha: greater
# pvalue <- 1 - pnorm(z)
# ##Ha: two.sided
# pvaule <- 2 * (1 - abs(pnorm(z)))

#----------------------------------------------------------------------------------------------------#











