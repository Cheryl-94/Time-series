setwd("~/Desktop")
data = read.csv(file="imports.csv",header=T)
time=data[,1]
imports=data[,2]
xt<-ts(imports,start=c(1965,7),end=c(1993,10),frequency=12)
ts.plot(xt,xlab="Year",ylab="Monthly Imports(Thousands of dollars)",main="Monthly Australian imports from Japan")

#Try First difference to raw data
xd=diff(xt)
ts.plot(xd,xlab="Year",ylab="diff(Imports(Thousands of dollars))",main="Differenced Monthly Australian imports from Japan")

#variance not constant
n=length(xt)
xt1=xt[1:(n/2)]
xt2=xt[(n/2+1):n]
(Firsthalf=var(xt1))
(Secondhalf=var(xt2))
sqrt(Secondhalf/Firsthalf)

#Try Log
yt=log(xt)
ts.plot(yt,xlab="Year",main="Log(Monthly Imports)")

n=length(yt)
yt_1=yt[1:(n/2)]
yt_2=yt[(n/2+1):n]
(Firsthalf=var(yt_1))
(Secondhalf=var(yt_2))
sqrt(Secondhalf/Firsthalf)
yd=diff(yt) #first difference
ts.plot(yd,xlab="Year",ylab="",main="diff(Log(Monthly Imports))")
acf(yd,300, main="ACF of diff(Log(Monthly Imports))")

ydsd=diff(yd,12) #first seasonal difference and first difference
ts.plot(ydsd,xlab="Year",main="diff(diff(Log(Monthly Imports)),12)")

par(mfrow=c(2,1))
acf(ydsd,300,main="ACF and PACF of diff(diff(Log(Monthly Imports)),12)")
pacf(ydsd,300)
acf2(ydsd,50)

#variance constant
n=length(ydsd)
xt1=ydsd[1:(n/2)]
xt2=ydsd[(n/2+1):n]
Firsthalf=var(xt1)
Secondhalf=var(xt2)
Firsthalf
Secondhalf

sarima(yt,0,1,1,0,1,1,12)
sarima(yt,0,1,1,3,1,0,12)
sarima(yt,0,1,1,3,1,1,12)
sarima(yt,2,1,0,0,1,1,12)
sarima(yt,2,1,0,3,1,0,12)
sarima(yt,2,1,0,3,1,1,12)
sarima(yt,2,1,1,0,1,1,12)#best fit
sarima(yt,2,1,1,3,1,0,12) 
sarima(yt,2,1,1,3,1,1,12) 

#prediction
yt.fore=sarima.for(yt,10,2,1,1,0,1,1,12)
U1=yt.fore$pred+2*yt.fore$se
L1=yt.fore$pred-2*yt.fore$se

xt.fore=exp(yt.fore$pred)
U=exp(U1)
L=exp(L1)
miny=min(xt,L)
maxy=max(xt,U)
ts.plot(xt,xt.fore,lty=1:2,col=1:2,ylim=c(miny,maxy),xlab="Year", ylab="Monthly Imports(Thousands of dollars)",main="Monthly Australian imports from Japan and its prediction for the next 10 observations")
legend(x="topleft",legend=c("Monthly Imports", "Prediction","95% C.I."),lty=1:3,col=c(1,2,4),cex=0.8)
lines(xt.fore,col=2)
lines(U,col=4,lty=3)
lines(L,col=4,lty=3)