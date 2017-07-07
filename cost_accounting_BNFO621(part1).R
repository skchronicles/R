################################################################################################
#Skyler Kuhn
#2/17/2016
##Cost Accouting: BNFO621
### In this script we are going to determine some basic calculations of cost acccounting of 
### a hypothetical espresso business. We will:
### 1.) determine price for each component,
### 2) adjust for shrinkage, 
### 3.) separate costs into fixed and variable, 
### 4.) determine VC per unit,
### 5.) guess how many units you will sell (use 5,000 cups),
### 6.) determine FC per unit at 5,000 cups
### 7.) Calculate the selling price
### 8.) Calculate "Break-even"
### 9.) Calculate the Net Income 
### 10.) Summary of Important Statistics
### 11.) Table of Important Statistics (summary run here)
################################################################################################

#1.) Determine the price for each component (taken from PowerPoint Slides): 
license <- 500
rent_of_space <- 3500
rent_of_equipment <- 1240
cost_of_coffee	<- 9 #per lb (16ounces per lb, 1 cup per ounce), assume one cup per ounce and assume 5% shrinkage
cost_of_cups <- 9 #for 160 cups, assume 5% shrinkage
demand <- 5000 #cups per year, not price sensitive

#2) Component Costs Adjusted after Shrinkage (5% in this case):
cost_of_coffeePerCup_afterShrinkage <- (cost_of_coffee / 16) * 1.05  #this is the price per cup
cost_of_coffeePerCup_afterShrinkage
cost_of_cupsPerCup_afterShrinkage <- (cost_of_cups / 160) * 1.05     #this is the price per cup
cost_of_cupsPerCup_afterShrinkage

#3.) Separate costs into Fixed and Variable:

##Fixed Costs (total costs do not change as the level of output changes):
### license <- 500
### rent_of_space <- 3500
### rent_of_equipment <- 1240

##Variable Costs (Total costs change in proportion to the change in output, demand = 50000):
### cost_of_coffeePerCup_afterShrinkage <- (cost_of_coffee / 16) * 1.05  #this is the price per cup
### cost_of_cupsPerCup_afterShrinkage <- (cost_of_cups / 160) * 1.05     #this is the price per cup

#4.) Determine VC per unit (or per cup):
variable_Cost_perUnit <- cost_of_cupsPerCup_afterShrinkage + cost_of_coffeePerCup_afterShrinkage 
variable_Cost_perUnit # above is the total variable cost per unit (or per cup sold)
variable_Cost_Total <- variable_Cost_perUnit * demand

#5.) Guess how many units you will sell (use 5,000 cups):
## this is can be viewed above in the varibable called "demand", we are saying 5000 in this example (*see above in 1.)
## demand = 5000

#6.) Determine FC per unit at 5,000 cups:
fixed_CostTotal <- license + rent_of_equipment + rent_of_space
fixed_Cost_perUnit <- fixed_CostTotal / demand
fixed_Cost_perUnit
## above is the total fixed cost per unit
## we must divide by demand (or in this case ~5000) to find the price per unit

#7.) Calculate the Selling Price:
## Selling price = VC per unit + FC per unit + profit (whateever amount of profit we want)
## In this example we’ll just double the total cost per unit to get selling price
total_Cost_perUnit <- variable_Cost_perUnit + fixed_Cost_perUnit
selling_Price <- total_Cost_perUnit * 1.3 # 1.3 is for 30% profit, doubling would be 2
selling_Price

#8.) Calculate "Break-even":
## Break-even = Fixed costs / CM_perUnit
## CM_perUnit = Sales_Revenue_perUnit (aka the selling_Price per unit) - VC_perUnit
cm_perUnit <- selling_Price - variable_Cost_perUnit
cm_perUnit
break_Even <- fixed_CostTotal / cm_perUnit
break_Even #if you want to find break per day divide this number by 365

#9.) Calculate the Net Income:
## cm_Totol is equal to the cmPerUnit * the total number of units (demend or 5000)
## net_Income = cmTotal - fixed_CostTotal
cm_Total <- cm_perUnit * demand  #in this case it is 5000 units
cm_Total
net_Income_Total <- cm_Total - fixed_CostTotal
net_Income_Total
net_Income_PerUnit <- net_Income_Total / demand 
net_Income_PerUnit  #aka profit per cup sold

#10.) Summary of Important Statistics:

##Sales Total:
sales_Total <- selling_Price * demand
##Sales per Unit:
selling_Price

##Variable Cost Total:
variable_Cost_Total
##Variable Cost per Unit:
variable_Cost_perUnit

##Contribution Margin (cm) Total:
cm_Total
##Contribution Margin (cm) per Unit:
cm_perUnit

##Fixed Costs Total:
fixed_CostTotal
##Fixed Cost per Unit:
fixed_Cost_perUnit

##Net Income Total:
net_Income_Total
##Net Income per Unit Sold:
net_Income_PerUnit

#11.) Table of Important Statistics
## creation of the table below (run from here)
total <- c(sales_Total,variable_Cost_Total,cm_Total,fixed_CostTotal,net_Income_Total) 
perUnit <- c(selling_Price,variable_Cost_perUnit,cm_perUnit,fixed_Cost_perUnit,net_Income_PerUnit) 
data_ColumnBound <- cbind(total,perUnit) #cbind() will bind the two columns of data together
colnames(data_ColumnBound) <- c("Total ($)","Per Unit ($)")
rownames(data_ColumnBound) <- c("Sales","Variable Cost","Contribution Margin   ","Fixed Cost ","Net Income ")
data_ColumnBound#-------------------------
#-------------------------------------------
demand 
break_Even
#(end run here)
################################################################################################
#Output: demand ~4000
# > data_ColumnBound#-------------------------
# Total ($) Per Unit ($)
# Sales                   15677.50    3.9193750
# Variable Cost            2598.75    0.6496875
# Contribution Margin     13078.75    3.2696875
# Fixed Cost               5240.00    1.3100000
# Net Income               7838.75    1.9596875
# > #-------------------------------------------
 
