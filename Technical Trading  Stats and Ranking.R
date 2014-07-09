# Cody Pedro
# List various properties of Equities traded on AMEX,NASDAQ,NYSE

# Load Packages================================================
library(forecast)
library(TTR)
library(quantmod)
library(timeSeries)
library(fArma)
library(PerformanceAnalytics)
library(binom)
library(xtable)
library(caTools)
library(ggplot2)
setwd("C:/Users/Cody/Desktop/Market Neutral")


# Grab the stock symbols and Info into the memory================================================
Index = stockSymbols(exchange=c("AMEX","NASDAQ","NYSE"),sort.by=c("Exchange","Exchange"))
write.csv(Index,"Index.csv")
Index = Index[Index[,4]>1000000000, ]
Index = Index[!is.na(Index[,6]), ]

stocks = as.character(Index$Symbol)

start         = 20130301
end           = 20140301
interval      = "daily"
time.frame    = 126


output  = data.frame("Ticker"=NA,"Sector"=NA,"Industry"=NA,"Market.Cap" = NA,"Dollar.Volume"=NA,  "Volatility"=NA,"Liquidity"=NA,"Bid-Ask Spread"=NA,"Performance"=NA,"Short Ratio"=NA)
Data.Main                = getYahooData("^GSPC",start,end, freq=interval, type="price", adjust=T)
Avg.Price                = (Data.Main[,1] + Data.Main[,2] + Data.Main[,3] + Data.Main[,4]) / 4
SPY.Return               = ROC(Avg.Price,type="discrete")

#===================================================================================================================

# Loop Through the stocks grabbed from Yahoo database
  i = 1
  while(i <= length(stocks)){    
    Data                = try(getYahooData(stocks[i],start,end, freq=interval, type="price", adjust=T), TRUE)
    
    if(class(Data)=="try-error" ){         
    }else{
      
      if(nrow(Data) < time.frame){    
      }else{ 
         
        
        Avg.Price      = (Data[,1] + Data[,2] + Data[,3] + Data[,4]) / 4
        Return         = ROC(Avg.Price,type="discrete")
        Dollar.Volume  = Data[,5]*Avg.Price
        
        Net.Return     = tail(Return,time.frame) - tail(SPY.Return,time.frame)
        
        IR             = median(Net.Return)/median(abs(Net.Return))
       
        med.Spread    = median(tail(abs(Return),time.frame))
        med.Volume    = median(tail(Dollar.Volume,time.frame))
        
        
        Liq = med.Spread/med.Volume
        
        t      = getQuote(stocks[i],what=yahooQF(c("Short Ratio","Bid","Ask")))
        short  = as.numeric(t[,2])
        Bid    = as.numeric(t[,3])
        Ask    = as.numeric(t[,4])
        spread = (Ask-Bid)/mean(c(Bid,Ask))
        
        
        output[i,1]    = stocks[i]
        output[i,2]    = Index[i,6]
        output[i,3]    = Index[i,7]
        output[i,4]    = Index[i,4]
        output[i,5]    = med.Volume
        output[i,6]    = med.Spread
        output[i,7]    = last(Liq)
        output[i,8]    = spread
        output[i,9]    = last(IR)
        output[i,10]   = last(short)

      }
    }
    i = i + 1
  }
  

# Order The Output=============================================================
output = na.exclude(output)
output = output[rev(order(output[,5])),]


final       =  output

# Rank into Percentiles================================================================
final[,4]    =  rank(output[,4])/nrow(final)
final[,5]    =  rank(output[,5])/nrow(final)
final[,6]    =  rank(output[,6])/nrow(final)
final[,7]    =  rank(output[,7])/nrow(final)
final[,8]    =  rank(output[,8])/nrow(final)
final[,9]    =  rank(output[,9])/nrow(final)
final[,10]   =  rank(output[,10])/nrow(final)


# Write to a .csv file============================================================================
write.csv(final,"Top1000.csv")







  


