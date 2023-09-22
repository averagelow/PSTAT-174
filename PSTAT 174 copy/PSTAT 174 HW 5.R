library(tsdl)
library(forecast)

meta_tsdl$description[[9]]
monthly_sales <- tsdl[[9]]

ts.plot(monthly_sales)

acf(monthly_sales)
pacf(monthly_sales)

# To make it more stationary diff at lag(1), lag(12)
mmonthly_sales <- diff(monthly_sales, 12)
mmmonthly_sales <- diff(mmonthly_sales, 1)

acf(mmmonthly_sales, lag.max=12)
pacf(mmmonthly_sales, lag.max=12)

library(astsa)
# Model 1:
fit.1 <- sarima(xdata=monthly_sales, details=F,
                p=2, d=0, q=2, P=1, D=1, Q=1, S=12)
cat('Coefficients'); fit.1$fit$coef

cat('Variance of white noise'); fit.1$fit$sigma2








