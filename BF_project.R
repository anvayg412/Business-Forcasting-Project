library(fpp)
library(fpp2)
library(fpp3)
Projdata <- read.csv("C:\\Users\\anvay\\Desktop\\BF\\project_final.csv")
View(Projdata)
dim(Projdata)
class(Projdata)
summary(Projdata)
Projdata_TS <- ts(Projdata, start=c(2016,1),end=c(2020,12),frequency=12)
plot(Projdata_TS[,-1])
Acf(Projdata_TS[,-1])
boxplot(Projdata_TS[,-1])
summary(Projdata_TS[,-1])
Projdata_TS_decomp <- decompose(Projdata_TS[,-1]/10000)
plot(Projdata_TS_decomp)
#Mean Forecast
mean_fcst <- meanf(Projdata_TS[,-1],12)
mean_fcst
plot(mean_fcst,mean_fcst$residuals)
plot(mean_fcst)
accuracy(mean_fcst)
mean_residual <- mean_fcst$residuals
mean_residual
plot(mean_residual)
hist(mean_residual)
#Naive Forecast
naive_fcst <- naive(Projdata_TS[,-1],12)
naive_fcst
plot(naive_fcst)
accuracy(naive_fcst)
naive_residual <- naive_fcst$residuals
naive_residual
hist(naive_residual)
#Seasonal Naive Forecast
snaive_fcst <- snaive(Projdata_TS[,-1],12)
snaive_fcst
plot(snaive_fcst)
accuracy(snaive_fcst)
snaive_residual <- snaive_fcst$residuals
snaive_residual
hist(snaive_residual)
#Random Walk Forecast
rwf_fcst <- rwf(Projdata_TS[,-1],drift=TRUE,12)
rwf_fcst
plot(rwf_fcst)
accuracy(rwf_fcst)
rwf_residual <- rwf_fcst$residuals
rwf_residual
hist(rwf_residual)
#Moving Average Forecast
MA3_fcst <- ma(Projdata_TS[,-1],order = 3,12)
MA3_fcst
plot(MA3_fcst)
MA3 <- forecast(MA3_fcst)
MA6_fcst <- ma(Projdata_TS[,-1],order = 6,12)
MA6_fcst
plot(MA6_fcst)
MA12_fcst <- ma(Projdata_TS[,-1],order = 12,12)
MA12_fcst
plot(MA12_fcst)
plot(Projdata_TS[,-1])
lines(MA3_fcst,col = "Blue")
lines(MA6_fcst,col = "Red")
lines(MA12_fcst,col = "Green")
#ETS Forecast
ets_fcsts <- ets(Projdata_TS[,-1])
ets_fcsts
plot(ets_fcsts)
accuracy(ets_fcsts)
ets_forecst <- forecast(ets_fcsts,h=12)
ets_forecst
plot(ets_forecst)
accuracy(ets_forecst)
ets_forecst
ets_fcsts
ets_residual <- ets_forecst$residuals
ets_residual
hist(ets_residual)
#Holts Winter Forecast
hw_fcst_levl <- HoltWinters(Projdata_TS[,-1],beta=FALSE,gamma=FALSE)
hw_fcst_levl
plot(hw_fcst_levl)
hw_fcst_tnd <- HoltWinters(Projdata_TS[,-1],gamma=FALSE)
hw_fcst_tnd
plot(hw_fcst_tnd)
hw_fcst_seas <- HoltWinters(Projdata_TS[,-1])
hw_fcst_seas
plot(hw_fcst_seas)
hw_forecast_all <- forecast(hw_fcst_seas,12)
hw_forecast_all
plot(hw_forecast_all)
accuracy(hw_forecast_all)
hw_residual <- hw_forecast_all$residuals
hw_residual
hist(hw_residual)
#ARIMA
auto.arima(Projdata_TS[,-1])
forecast(auto.arima(Projdata_TS[,-1]))
plot(forecast(auto.arima(Projdata_TS[,-1])))