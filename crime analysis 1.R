attach(crime_data1)
str(crime_data1)
crime_data2<-lm(crime_data1$Population1~crime_data1$`Violent
crime2`+crime_data1$`Violent 
crime 
rate`+crime_data1$`Murder and
nonnegligent 
manslaughter`+crime_data1$`Rape
(legacy 
definition4)`+crime_data1$Robbery+crime_data1$`Aggravated 
assault`+crime_data1$`Property 
crime`+crime_data1$`Property 
crime 
rate`+crime_data1$Burglary+crime_data1$`Larceny-
theft`+crime_data1$`Larceny-
theft rate`+crime_data1$`Motor 
vehicle 
theft`)
summary(crime_data2)
crime_data3<-lm(crime_data1$Population1~crime_data1$`Murder and 
nonnegligent 
manslaughter 
rate`+crime_data1$`Rape
(legacy 
definition) 
rate4`+crime_data1$`Robbery 
rate`+crime_data1$`Aggravated 
assault rate`+crime_data1$`Burglary 
rate`+crime_data1$`Motor 
vehicle 
theft 
rate`)
summary(crime_data3)
plot(crime_data2)
scatter.smooth(x = crime_data1$Population1, y = crime_data1$`Violent
crime2`, main = 'Population vs Violent Crime')
scatter.smooth(x = crime_data1$Population1,crime_data1$Robbery,main = 'Population vs Robberies')
scatter.smooth(x = crime_data1$Population1,y = crime_data1$`Murder and
nonnegligent 
manslaughter`,main = 'Murder and
nonnegligent 
manslaughter`vs Population')
scatter.smooth(x= crime_data1$Population1,y = crime_data1$`Rape
(legacy 
definition4)`,main ='Rape Legacy vs Population' )
scatter.smooth(x = crime_data1$Population1,y=crime_data1$`Aggravated 
assault`,main = 'Aggrevated Assault vs Population')
scatter.smooth(x = crime_data1$Population1,y = crime_data1$`Property 
crime`,main = 'Prorpertycrime vs Population')
crime_data2$coefficients
crime_data4<-lm(crime_data1$`Violent
crime2`~crime_data1$Population1)
summary(crime_data4)
plot(crime_data4)
crime_data5<-lm(crime_data1$`Rape
(legacy 
definition4)`~crime_data1$Population1)
summary(crime_data5)
crime_data6<-lm(crime_data1$`Violent
crime2`~crime_data1$Burglary+crime_data1$`Larceny-
theft`+crime_data1$`Motor 
vehicle 
theft`)
summary(crime_data6)
crime_data7<-lm(crime_data1$`Aggravated 
assault`~crime_data1$Population1)
summary(crime_data7)
library("e1071", lib.loc="/usr/lib/R/site-library")
par(mfrow=c(1, 2)) 
plot(density(crime_data1$`Violent
crime2`), main="Density Plot: ViolentCrime", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(crime_data1$`Violent
crime2`), 2)))
