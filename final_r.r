# clear working memory
rm ( list = ls ())

# Title: Final
# Author: Samarth Bhaskar
# Date Created:March 9, 2011

#Loading Foreign Library

library ( foreign )
#Setting Working Directory

setwd ( "/Users/samarthbhaskar/Desktop/Classes/Winter 11/PLSC 41800 Causal Inference/Final Paper/R files/" )
sink ( "Final.txt" )

#Read the data
## Downloaded GSS data from http://www3.norc.org/GSS+Website/Download/STATA+v8.0+Format/
data < - r ead.dta ( "2004.dta" )

#attach the data
attach ( data )

# R's self-assessed ability to use World Wide Web (explanatory)
summary (as.numeric(webable)
mean (as.numeric(webable)
sd (as.numeric(webable))
length (as.numeric(webable))
      
# Joined an Internet political forum or discussion (outcome)
summary (as.numeric(interpol))
mean (as.numeric (interpol))
sd (as.numeric (interpol))
length (as.numeric (interpol))
      
# HOW INTERESTED R IS IN POLITICS(control)
summary (data$govdook)
data$govdook <- as.numeric (data$govdook)
summary (data$govdook)
mean (data$govdook, na.rm = T)
sd (data$govdook, na.rm = T)
length (data$govdook)

# checking class
class (interpol)
class (webable)
class (govdook)

class (rincome)
class (age)

# recoding outcome variable (interpol as interpol1)
data$interpolnum <- as.numeric ( data $ interpol )
data$interpol1<- ifelse (data$interpolnum<=3,1,0)
#1=have joined
#0=not joined
#new outcome variable is interpol1
table(data$interpol1)
#recoding explanatory variable (webable as webable1)
data$webablenum <- as.numeric(data$webable)
data$webable1 <- ifelse(data$webablenum<=4,1,0)
#1 = are able
#0 = are not able
#new explanatory variable is webable1
table(data$webable1)
      
#recoding control variable (govdook as govdooknum)
data$govdooknum <- as.numeric (govdook)
data$govdooknum <- ifelse (data$govdooknum == 4,1,0)
#1 = trust government
#2 = do not trust government
table ( data $ govdooknum )
      
#recoding income(rincome as rincomenum)
data$rincomenum <- as.numeric (data$rincome ) - 1
table (data$rincomenum)
#plotting all variables of interest
hist (data$webable1, main = "Histogram of self assessed ability to use WWW" ,ylab = "Frequency" , xlab = "Ability to use WWW" , xlim = c(0,1))
hist(data$interpol1,main = "Histogram of joining political forum ",ylab = "Frequency" , xlab = "Joining political forum" , xlim = c (0,1))
hist (data$govdooknum , main = "Histogram of Interest in Government" ,ylab = "Frequency" , xlab = "Interest in government" , xlim = c (0,1))
hist(data$rincomenum , main = "Histogram of Income" , ylab = "Frequency" ,xlab = "Income bracket" , xlim = c (0,12))
legend (x = "center" , y ="top" , c ("Under $1000", "$1000-$2999", "$3000-$3999", "$4000-$4999", "$5000-$5999", "$6000-$6999", "$7000-$7999", "$8000-$9999", "$10000-$14999", "$15000-$19999", "$20000-$24999", "$25000 or over" ), lty = 2:1)
hist (age, main = "Histogram of Age", ylab = "Frequency" , xlab = "Age" )

# Initial correlation
print ("Interpol1 and Webable1 Correlation")
cor (data$interpol1, data $ webable1 , use = " na.or.complete" )
print ("Interpol1 and Age Correlation")
cor (data$interpol1, data$age, use = "na.or.complete" )
print ("Interpol1 and Rincomenum Correlation")
cor (data$interpol1, data$rincomenum, use = "na.or.complete")
print ("Interpol1 and Govdooknum Correlation")
cor (data$interpol1 , data$govdooknum, use = "na.or.complete")
      
#running linear regression between explanatory and outcome variable
regression1 <- lm ((interpol1)~(webable1), data = data , na.action = na.omit)
summary (regression1)
      
#running a linear regression between control and outcome variable
regression2 <- lm ((interpol1)~ as.numeric (govdook), data = data, na.action = na.omit)
summary (regression2)
      
#running linear regression with control
regression_control <- lm ((interpol1)~(webable1)+ as.numeric (govdook),
data = data , na.action = na.omit)
summary (regression_control)
      
#running logit in R
library (Design)
logitregression <- glm (interpol1 ~ webable1 + govdooknum + rincomenum + age , data = data , na.action = na.omit , family = binomial ( link = "logit" ))
summary (logitregression)
      
#predicting probabilities from logitregression
data$predicted <- predict (logitregression , newdata = data , type = "response" , na.action = na.pass )
summary (data$predicted)
      
#graphing predicted
hist (data$predicted, main = "Histogram of predicted values" , ylab = "Frequency of Probability" , xlab = "Predicted Value from Logit Regression")
      
#plotting age versus predicted values
pdf (file = 'AgevsPredicted.pdf', height = 7 , width = 11)
plot (data$age , data$predicted , range (-1 , 1), c(0, 1.5), type = 'p' , pch = "R" ,
col = "black" , main = "Age vs. Predicted Probability Outcomes", xlab = "Respondent's Age" , ylab = "Predicted Probability of Joining Political Forum or Discussion Online")

points (data$age , data$predicted , type = 'p' , pch = "D" , col = "grey30")
fit < - lsfit ((data$age ), as.numeric (data$predicted))
abline (fit, col = "grey15")
dev.off ()

sink ()
