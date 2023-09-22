#All Employees: Total Non-farm (80 percent of the workers)
#who contribute to Gross Domestic Product (GDP).
payems_data = read.csv("~/Desktop/payems.csv")

payems <- ts(payems_data[,2],start=c(1939,1),frequency = 12)
ts.plot(payems,main = "Time plot:PAYEMS per month", ylab = "Thousands of Persons")
#Data is not stationary: has an upward linear trend 
#Variance seems constant

nt = length(payems)
fit <-lm(payems ~ as.numeric(1:nt)); abline(fit, col="red")

#Difference at lag 1
#payemsdiff1 <-diff(payems)
#plot(payemsdiff1)

#Training dataset 
payems_training = payems[c(1:815)]
#Test DataSet 
payems_test = payems[c(816:960)]

ts.plot(payems_training)

fit <-lm(payems_training ~ as.numeric(1:length(payems_training))); abline(fit, col="red")
abline(h=mean(payems), col ="blue")

hist(payems_training, col = "light blue", xlab = "", main = "histogram; payems data")
#Histogram does not follow Normal Distr. 

acf(payems_training, lag.max = 40, main = "ACF of Payems Data")
#Not within CI


#Difference at lag 1
payemsdiff1 <-diff(payems_training)
ts.plot(payemsdiff1,main = "Time plot:PAYEMS_Training data diff1", ylab = "Thousands of Persons")

acf(payemsdiff1)
#ACF still has a linear downward trend
pacf(payemsdiff1)
#PACF also has a linear downward trend



#Difference again at lag 1
payemsdiff2 <-diff(payemsdiff1)
ts.plot(payemsdiff2,main = "Time plot:PAYEMS_Training data diff2", ylab = "Thousands of Persons")

acf(payemsdiff2)
#linear trend was removed
pacf(payemsdiff2)
#No longer has a downward linear trend

hist(payemsdiff2, col = "light blue", xlab = "", main = "histogram; payemsdiff2 data")
#data appears symmetric 











