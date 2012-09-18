# clear working memory
rm ( list = ls ())

# Title: Final
# Author: Samarth Bhaskar
# Date Created:March 9, 2011

#Loading Foreign Library

library ( foreign )
#Setting Working Directory

setwd ( "/Users/samarthbhaskar/Desktop/Classes/Winter 11/PLSC 41800 Causal
I nference/Final Paper/R files/" )
sink ( "Final.txt" )
#Read the data
data < - r ead.dta ( "2004.dta" )
#attach the data
attach ( data )
# R's self-assessed ability to use World Wide Web (explanatory)
summary ( as.numeric ( webable )
mean ( as.numeric ( webable )
sd ( as.numeric ( webable ))
length ( as.numeric ( webable ))
# Joined an Internet political forum or discussion (outcome)
summary ( as.numeric ( interpol ))
mean ( as.numeric ( interpol ))
sd ( as.numeric ( interpol ))
length ( as.numeric ( interpol ))
# HOW INTERESTED R IS IN POLITICS(control)
summary ( data $ govdook )
data $ govdook <- a s.numeric ( data $ govdook )
summary ( data $ govdook )
mean ( data $ govdook , n a.rm = T )
sd ( data $ govdook , n a.rm = T )
length ( data $ govdook )

# checking class
class ( interpol )
class ( webable )
class ( govdook )

c lass ( rincome )
class ( age )

# recoding outcome variable (interpol as interpol1)
data $ interpolnum <- as.numeric ( data $ interpol )
data $ interpol1 < - i felse ( data $ interpolnum < = 3 , 1 , 0 )
# 1 = have joined
# 0 = not joined
# new outcome variable is interpol1
table ( data $ interpol1 )
# recoding explanatory variable (webable as webable1)
data $ webablenum <- as.numeric ( data $ webable )
data $ webable1 <- ifelse ( data $ webablenum < = 4 , 1 , 0 )
# 1 = are able
# 0 = are not able
# new explanatory variable is webable1
table ( data $ webable1 )
#recoding control variable (govdook as govdooknum)
data $ govdooknum <- as.numeric ( govdook )
data $ govdooknum <- ifelse ( data $ govdooknum = = 4 , 1 , 0 )
#1 = trust government
#2 = do not trust government
table ( data $ govdooknum )
#recoding income(rincome as rincomenum)
data $ rincomenum <- as.numeric ( data $ rincome ) - 1
table ( data $ rincomenum )
#plotting all variables of interest
hist ( data $ webable1 , m ain = "Histogram of self assessed ability to use WWW" ,
y lab = "Frequency" , x lab = "Ability to use WWW" , x lim = c ( 0 , 1 ))
hist ( data $ interpol1 , m ain = "Histogram of joining political forum " ,
y lab = "Frequency" , x lab = "Joining political forum" , x lim = c ( 0 , 1 ) )
hist ( data $ govdooknum , m ain = "Histogram of Interest in Government " ,
y lab = "Frequency" , x lab = "Interest in government" , x lim = c ( 0 , 1 ) )
hist ( data $ rincomenum , m ain = "Histogram of Income " , y lab = "Frequency" ,
x lab = "Income bracket" , x lim = c ( 0 , 12 ))
legend ( x = "center" , y = "top" , c ( "Under $1000" , " $1000-$2999" , " $3000-$3999" ,
" $4000-$4999" , " $5000-$5999" , " $6000-$6999" , " $7000-$7999" , " $8000-$9999" ,
" $10000-$14999" , " $15000-$19999" , " $20000-$24999" , " $25000 or over" ),
l ty = 2 : 1 )
hist ( age , m ain = "Histogram of Age" , y lab = "Frequency " , x lab = "Age" )

# Initial correlation
print ( "Interpol1 and Webable1 Correlation " )
cor ( data $ interpol1 , d ata $ webable1 , u se = " na.or.complete" )
print ( "Interpol1 and Age Correlation " )
cor ( data $ interpol1 , d ata $ age , u se = "na.or.complete " )
print ( "Interpol1 and Rincomenum Correlation" )
cor ( data $ interpol1 , d ata $ rincomenum , u se = "na.or.complete" )
print ( "Interpol1 and Govdooknum Correlation" )
cor ( data $ interpol1 , d ata $ govdooknum , u se = "na.or.complete" )
#running linear regression between explanatory and outcome variable
regression1 <- lm (( interpol1 )~( webable1 ), d ata = data , n a.action = na.omit )
summary ( regression1 )
#running a linear regression between control and outcome variable
regression2 <- lm (( interpol1 )~ as.numeric ( govdook ), d ata = data ,
n a.action = na.omit )
summary ( regression2 )
#running linear regression with control
regression_control <- lm (( interpol1 )~( webable1 )+ as.numeric ( govdook ),
d ata = data , n a.action = na.omit )
summary ( regression_control )
#running logit in R
library ( Design )
logitregression <- g lm ( interpol1 ~ w ebable1 + g ovdooknum + r incomenum + a ge ,
d ata = data , n a.action = na.omit , f amily = b inomial ( link = "logit" ))
summary ( logitregression )
#predicting probabilities from logitregression
data $ predicted <- predict ( logitregression , n ewdata = data , t ype = "response" ,
n a.action = na.pass )
summary ( data $ predicted )
#graphing predicted
hist ( data $ predicted , m ain = "Histogram of predicted values " , y lab = "Frequency
o f Probability" , x lab = "Predicted Value from Logit Regression" )
#plotting age versus predicted values
pdf ( file = 'AgevsPredicted.pdf' , height = 7 , w idth = 11 )
plot ( data $ age , d ata $ predicted , r ange (- 1 , 1 ), c ( 0 , 1.5 ), t ype = 'p' , pch = "R" ,
c ol = "black" , main = "Age vs. Predicted Probability Outcomes" ,
x lab = "Respondent's Age" , y lab = "Predicted Probability of Joining Political
Forum or Discussion Online" )

points ( data $ age , d ata $ predicted , t ype = 'p' , p ch = "D" , c ol = "grey30" )
fit < - l sfit (( data $ age ), a s.numeric ( data $ predicted ))
abline ( fit , c ol = "grey15" )
dev.off ()

sink ()
