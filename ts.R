setwd("~/Desktop/CSC425/FinalProj")

##Library
library(rugarch)
library(tseries)
library(fBasics)
library(forecast)
library(zoo)

# import data in R and compute log returns
# import libraries for TS analysis
myd= read.table('app_new.csv', header=T, sep=',')

aplets<-zoo(myd$Adj.Close , as.Date(as.character(myd$Date), format=c("%Y-%m-%d")))

myd$Date<-as.Date(as.character(myd$Date))
aplets = zoo(myd$Adj.Close, myd$Date)
plot(aplets)

#log return time series
rets = log(aplets/lag(aplets, -1))
basicStats(rets)
plot(rets)
###JB-TEST
jarque.bera.test(rets)

# strip off the dates and just create a simple numeric object
ret = coredata(rets);

head(ret)
pacf(ret,lag=10)
# Plots ACF function of vector data
acf(ret)
# Plot ACF of squared returns to check for ARCH effect 
acf(ret^2)
# Plot ACF of absolute returns to check for ARCH effect 
acf(abs(ret))

##TRY arima
m= auto.arima(ret, trace=T, seasonal=T,ic=c("bic"))
m

#specify model using functions in rugarch package
########normally distributed Error
##########Fit ARMA(0,0)-GARCH(1,1) model 
garch11.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)))
#estimate model 
garch11.fit=ugarchfit(spec=garch11.spec, data=rets)
garch11.fit

###T-GARCH
tgarch11.spec=ugarchspec(variance.model=list(model = "gjrGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)))
#estimate model 
tgarch11.fit=ugarchfit(spec=tgarch11.spec, data=rets)
tgarch11.fit

###E-GARCH
egarch11.spec=ugarchspec(variance.model=list(model = "eGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)))
#estimate model 
egarch11.fit=ugarchfit(spec=egarch11.spec, data=rets)
egarch11.fit

#########E-GARCH with t-distributed error
egarch11.t.spec=ugarchspec(variance.model=list(model = "eGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
#estimate model 
egarch11.t.fit=ugarchfit(spec=egarch11.t.spec, data=rets)
egarch11.t.fit

plot(egarch11.t.fit)

#########E-GARCH with skewed t-distributed error

egarch11.st.spec=ugarchspec(variance.model=list(model = "eGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "sstd")
#estimate model 
egarch11.st.fit=ugarchfit(spec=egarch11.st.spec, data=ret)
egarch11.st.fit

####5-step ahead forecast
egarch11.t.fcst=ugarchforecast(egarch11.t.fit, n.ahead=5)
egarch11.t.fcst
plot(egarch11.t.fcst)

###1000-out-of-sample one-step ahead rolling forecast
egarch11.t.fit.roll = ugarchfit(egarch11.t.spec, data=rets, out.sample=1000)
egarch11.t.fcst.roll = ugarchforecast(egarch11.t.fit.roll, n.roll=1000, n.ahead=1)
egarch11.t.fcst.roll

plot(egarch11.t.fcst.roll)






###iGARCH

igarch11.t.spec=ugarchspec(variance.model=list(model = "iGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)),distribution.model = "std")
igarch11.t.fit=ugarchfit(spec=igarch11.t.spec, data=ret)
igarch11.t.fit

igarch12.t.spec=ugarchspec(variance.model=list(model = "iGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)))
igarch12.t.fit=ugarchfit(spec=igarch12.t.spec, data=ret)
igarch12.t.fit

igarch13.t.spec=ugarchspec(variance.model=list(model = "iGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)),distribution.model = "sstd")
igarch13.t.fit=ugarchfit(spec=igarch13.t.spec, data=ret)
igarch13.t.fit

###E-GARCH#### Norm sig na ja

egarch11.t.spec=ugarchspec(variance.model=list(model = "eGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
#estimate model 
egarch11.t.fit=ugarchfit(spec=egarch11.t.spec, data=rets)
egarch11.t.fit

plot(egarch11.t.fit)

egarch11.st.spec=ugarchspec(variance.model=list(model = "eGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "sstd")
#estimate model 
egarch11.st.fit=ugarchfit(spec=egarch11.st.spec, data=ret)
egarch11.st.fit

egarch11.spec=ugarchspec(variance.model=list(model = "eGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)))
#estimate model 
egarch11.fit=ugarchfit(spec=egarch11.spec, data=rets)
egarch11.fit



plot(egarch11.fit)

egarch11.t.fcst=ugarchforecast(egarch11.t.fit, n.ahead=5)
egarch11.t.fcst
plot(egarch11.t.fcst)


egarch11.t.fit.roll = ugarchfit(egarch11.t.spec, data=rets, out.sample=1000)
egarch11.t.fcst.roll = ugarchforecast(egarch11.t.fit.roll, n.roll=1000, n.ahead=1)
egarch11.t.fcst.roll
plot(egarch11.t.fcst.roll)

###T-GARCH
tgarch11.spec=ugarchspec(variance.model=list(model = "gjrGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)))
#estimate model 
tgarch11.fit=ugarchfit(spec=tgarch11.spec, data=ret)
tgarch11.fit


tgarch11.t.spec=ugarchspec(variance.model=list(model = "gjrGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
#estimate model 
tgarch11.t.fit=ugarchfit(spec=tgarch11.t.spec, data=ret)
tgarch11.t.fit
