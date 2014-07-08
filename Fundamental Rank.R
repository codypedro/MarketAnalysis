# Value Ranking
#==============================================================================
{   
  
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
  
}
#===============================================================================

start         = 20120601
end           = 20150618
interval      = "daily"

Index = read.csv("Top1000.csv")
Index = Index[,1]

stocks = as.character(Index)

output  = data.frame("Ticker"=NA,"Earnings.Yield"=NA,"Growth"=NA)


i = 1
while(i <= length(stocks)){ 
  Data.Main                     = try(getYahooData(stocks[i],start,end, freq=interval, type="price", adjust=T), TRUE)
  
  if(class(Data.Main)=="try-error" ){         
  }else{
    
    if(nrow(Data.Main) < (260)){    
    }else{ 
      
      Close = Data.Main[(nrow(Data.Main)-252):nrow(Data.Main),4]
      Return = ROC(Close,type="continuous")
      Return = na.exclude(Return)
      
      IR = median(Return)/median(abs(Return))
      
      t = getQuote(stocks[i],what=yahooQF(c("Book Value","Last Trade (Price Only)","Earnings/Share","EPS Estimate Next Year","Price/Book","Price/Sales","Short Ratio","Volume","1 yr Target Price")))
      
      Price =  as.numeric(t[,3])
      E.Y =  as.numeric(t[,4])
      
      E.Y.1 =  as.numeric(t[,5])
      
      short = as.numeric(t[,8])
      growth = (E.Y.1-E.Y)/Price
      

      
      output[i,1] = stocks[i]
      output[i,2] = E.Y/Price
      output[i,3] = growth
      output[i,4] = IR
      output[i,5] = short
 

    }
  }
  i = i + 1
}


output = na.exclude(output)
output[,6] = rank(output[,2])/length(output[,2])
output[,7] = rank(output[,3])/length(output[,3])
output[,8] = rank(output[,4])/length(output[,4])
output[,9] = rank(output[,5])/length(output[,5])


scatter.smooth(output[,7],output[,6])
loe =  loess(output[,6]~output[,7])
output[,11]  = output[,6] - predict(loe)
output[,10]  = (output[,8] + output[,9]) / 2
output[,10]  = rank(output[,10])/length(output[,10])
output[,11]  = rank(output[,11])/length(output[,11])

output = output[order(output[,11]),]


plot(loe)






write.csv(output,"ValueRank.csv")




