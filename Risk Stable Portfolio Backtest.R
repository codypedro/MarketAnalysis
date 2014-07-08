# Cody Pedro
# Load Packages for Analysis
library(fPortfolio)
library(quantmod)
library(PerformanceAnalytics)
library(fields)
library(xtable)
library(DEoptim)
library(tseries)
library(quadprog)
library(fGarch)

weights = data.frame(seq(.1,.9,.01), 1-seq(.1,.9,.01) )
colnames(weights)  = c("Equity","Bonds")
  
## Function for ES of t-GARCH
ESgarch <- function(y, p){
  gfit <- garchFit(formula = ~garch(1, 1), data = y,
                   cond.dist = "std", trace = FALSE)
  sigma <-  predict(gfit, n.ahead = 1)[3]
  df <- coef(gfit)["shape"]
  ES <- sigma * (dt(qt(p, df), df)/(1 - p)) * ((df + (qt(p, df))^2)/(df - 1))
  return(ES)

}

dist    = 2500
mov.win = 60
risk    = .01

start = 19880101
end   = 20150101

DJIA = c("^GSPC","VUSTX")

## Initialize Data Frames
Data    = getYahooData(DJIA[1],start,end,adjust=TRUE)
Data    = to.daily(Data$Close,OHLC=F)
Data    = Data$Close

## Get Data
i = 2
while(i <= (length(DJIA))){   
  yahoo                 = getYahooData(DJIA[i],start,end,adjust=TRUE)
  w.yahoo               = to.daily(yahoo$Close, OHLC=F)
  Data                  = merge(Data,w.yahoo$Close)
  i = i + 1
} 
colnames(Data) = DJIA
Data = na.exclude(Data)
StockReturn = returnSeries(timeSeries(Data),method="continuous")
StockReturn = cbind(StockReturn,0)

## Conducting back-test
s.end  = seq(from=dist, to=nrow(StockReturn),by=mov.win)
s.from = seq(from=1,to=nrow(StockReturn)-dist,by=mov.win)
to    <- time(StockReturn)[s.end]
from  <- time(StockReturn)[s.from]

i = 1
ES.port = data.frame(matrix(NA,nrow=nrow(weights),ncol=4))
wLow  = wEqual =  data.frame(matrix(NA,nrow=length(to),ncol=ncol(StockReturn)))
wCash =  data.frame(matrix(NA,nrow=101,ncol=2))
test  =  data.frame(matrix(NA,nrow=1,ncol=ncol(StockReturn)))



dates = NA
for(i in 1:length(to)){
series     <- window(StockReturn, start = from[i], end = to[i])

j = 1
while(j <= nrow(weights)){    
port.ret = as.matrix(weights[j,1:2]) %*%  t(as.matrix(series[,1:2]))
port.ret = t(port.ret)
port.ret = as.numeric(port.ret)


ES.port[j,1] = weights[j,1]
ES.port[j,2] = weights[j,2]
ES.port[j,3] = -ESgarch(port.ret,p=.95)
ES.port[j,4] = abs(ES.port[j,3] + risk)


j = j + 1
}

vvv          = ES.port[which.max(ES.port[,3]):nrow(ES.port),]

test[1,1:3]     = vvv[which.min(vvv[,4]),1:3]

wCash[,1]      = seq(0,1,.01)
wCash[,2]      = (test[1,3] * (1 - wCash[,1])) + risk

wLow[i,1:2]    = vvv[which.min(vvv[,4]),1:2] * (1-wCash[which.min(abs(wCash[,2])),1])
wLow[i,3]      = wCash[which.min(abs(wCash[,2])),1]


wEqual[i,1:2]  = rep(1/length(DJIA),length(DJIA))
wEqual[i,3]    = 0

dates[i] = last(rownames(series))
i = i + 1
}

rownames(wLow)    = dates

colnames(wLow)       = DJIA
colnames(wEqual)     = DJIA

g.last.Low    = 1
g.Equal.last  = 1

w = 1
u = dist

Growth.Low       = data.frame(matrix(data=NA,nrow=nrow(Data),ncol=1))
Growth.Equal     = data.frame(matrix(data=NA,nrow=nrow(Data),ncol=1))

count = 0
i = 1
while(i<=(length(s.end)-1)){  
  count[i] = (s.end[i+1] - s.end[i]) 
  i = i + 1
  count[i] = nrow(Data) - s.end[i] 
}

while(w <= nrow(wLow)){

  weight.Spread.6  = g.last.Low*wLow[w, ]
  weight.Equal     = g.Equal.last*wEqual[w, ]
  
  Growth = data.frame(matrix(data=NA, nrow =count[w], ncol=length(DJIA)))
  
  i = 1
  while(i <= length(DJIA)){   
    Growth[,i] = Data[(u+1):(min(mov.win+u,nrow(Data))),i]/as.numeric(Data[(u+1),i])
    i = i + 1
  }
  Growth = cbind(Growth,1)
  newGrowth.Spread.6                                      = t(as.matrix(weight.Spread.6) %*% t(as.matrix(Growth)))
  newGrowth.Equal                                         = t(as.matrix(weight.Equal) %*% t(as.matrix(Growth)))
  Growth.Low[(u+1):min(u+mov.win,nrow(Data)),1]           = newGrowth.Spread.6
  Growth.Equal[(u+1):min(u+mov.win,nrow(Data)),1]         = newGrowth.Equal
  g.last.Low                                              = newGrowth.Spread.6[mov.win]
  g.Equal.last                                            = newGrowth.Equal[mov.win]
  
  w = w + 1
  u = u + mov.win
}

GrowthLow    =timeSeries(Growth.Low,as.Date(index(Data),format="%Y-%m-%d"))
colnames(GrowthLow) = "Low"

GrowthEqual  =timeSeries(Growth.Equal,as.Date(index(Data),format="%Y-%m-%d"))
colnames(GrowthEqual) = "Equal"

#=============================
dataSP = getYahooData("^GSPC",start,end,adjust=T)
dataSP = to.daily(dataSP$Close,OHLC=F)

dataTLT = getYahooData("VUSTX",start,end,adjust=T)
dataTLT = to.daily(dataTLT$Close,OHLC=F)

ddd      = na.exclude(merge(as.timeSeries(dataTLT),GrowthLow))
GrowthTLT = ddd[ ,1]/as.numeric(ddd[1,1])
retTLT    = ROC(GrowthTLT,type="discrete")

ddd      = na.exclude(merge(as.timeSeries(dataSP),GrowthLow))
GrowthSP = ddd[ ,1]/as.numeric(ddd[1,1])
retSP    = ROC(GrowthSP,type="discrete")

GrowthLow    = na.exclude(as.xts(GrowthLow))
GrowthEqual  = na.exclude(as.xts(GrowthEqual))
GrowthSP     = na.exclude(as.xts(GrowthSP))
GrowthTLT    = na.exclude(as.xts(GrowthTLT))

retLow     = as.timeSeries(ROC(GrowthLow,type="discrete"))       
retEqual   = as.timeSeries(ROC(GrowthEqual,type="discrete")) 

colnames(retLow)    = "Risk-Stable"
colnames(retEqual)  = "Equal"
colnames(retSP)     = "S&P 500"
colnames(retTLT)    = "VUSTX"


Returns   = data.frame(retLow, retEqual, retSP,retTLT)


plot(GrowthLow,log="y",ylab="Growth",ylim=c(.65,3.70),main="Portfolio Growth")
lines(GrowthEqual,col="blue")
lines(GrowthSP,col="red")
lines(GrowthTLT,col="green")
legend("bottomright",legend=c("Risk-Stable","Equal","S&P 500","VUSTX"),lty=1,col=c("black","blue","red","green"), bty="n")



Equity.weight   = as.timeSeries(wLow[,1])   ,index(as.xts(wLow[,1])))
plot(as.xts(Equity.weight),type="h",ylim=c(0,1),main="Equity Weight")

#Rolling Analysis=================================================================================
chart.RollingPerformance(cbind(retLow,retEqual,retSP,retTLT), width = 252, FUN = "StdDev.annualized" , main="Rolling 1-Year Standard Deviation", legend.loc="topleft",col=c("black","blue","red","green"))
chart.RollingPerformance(cbind(retLow,retEqual,retSP,retTLT), width = 252, FUN = "Return.annualized" , main="Rolling 1-Year Annulized Return", legend.loc="topleft",col=c("black","blue","red","green"))
chart.RollingPerformance(cbind(retLow,retEqual,retSP,retTLT), width = 252, FUN = "SharpeRatio.annualized" , main="Rolling 1-Year Sharpe Ratio", legend.loc="topleft",col=c("black","blue","red","green"))
chart.RollingPerformance(cbind(retLow,retEqual,retSP,retTLT), width = 252, FUN = "Omega" , main="Rolling 1-Year Omega Ratio", legend.loc="bottomright",col=c("black","blue","red","green"))


last(GrowthLow)
last(GrowthEqual)
last(GrowthSP)
last(GrowthTLT)

Return.annualized(retLow)
Return.annualized(retEqual)
Return.annualized(retSP)
Return.annualized(retTLT)

chart.Drawdown(Returns,col=c("black","blue","red","green"))
legend("bottomright",legend=c("Risk-Stable","Equal","S&P 500","VUSTX"),lty=1,col=c("black","blue","red","green"), bty="n")

chart.Boxplot(Returns,col=c("black","blue","red","green"))
legend("bottomright",legend=c("Risk-Stable","Equal","S&P 500","VUSTX"),lty=1,col=c("black","blue","red","green"), bty="n")


stats = data.frame(basicStats(na.exclude(Returns)))*100
xtable(stats)

stats = data.frame(table.DownsideRisk(na.exclude(Returns)))*100
xtable(stats)

stats = data.frame(table.DownsideRisk(retLow),table.DownsideRisk(retMedium),table.DownsideRisk(retHigh),table.DownsideRisk(retSP))*100
xtable(stats)


xtable(Omega(Returns))
xtable(Return.annualized(Returns)*100)
xtable(sd.annualized(Returns)*100)
xtable(Return.annualized(Returns)/sd.annualized(Returns))


