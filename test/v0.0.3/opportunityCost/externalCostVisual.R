
ExternalCostTestVisual <- function(sheetName,cashOrNoncash){
  library("XLConnect")
  
  setwd("E://ACUO/projects/acuo-allocation/test/v0.0.3/")
  filePath <- "opportunityCost/externalPerformance.xlsx"
  
  worksheet <- readWorksheetFromFile(filePath,sheet=sheetName)
  numericCols <- c("costObj","liquidityObj","avgCashExternal","avgNoncashExternal","RunTime","dailyCost","RLRatio","movement","imCashAmount","imNoncashAmount")
  worksheet[numericCols] <- sapply(worksheet[numericCols] , as.numeric)
  
  # pref1 = c(10,0)
  resultPref1 <- worksheet[which(worksheet$costObj==10, worksheet$liquidityObj==0),]
  # pref2 = c(8,2)
  resultPref2 <- worksheet[which(worksheet$costObj==8, worksheet$liquidityObj==2),]
  # pref3 = c(5,5)
  resultPref3 <- worksheet[which(worksheet$costObj==5, worksheet$liquidityObj==5),]
  # pref4 = c(2,8)
  resultPref4 <- worksheet[which(worksheet$costObj==2, worksheet$liquidityObj==8),]
  
  if(cashOrNoncash=="cash"){
    cat("Avg. external cost for noncash daily:",mean(worksheet$avgNoncashExternal),'\n')
    cat("Avg. external cost for noncash annualy:",mean(worksheet$avgNoncashExternal)*360*10000)
    
    layout(matrix(c(1,2,3,4),nrow=2))
    
    # x = avgCashExternal, y = imCashAmount
    yMax <- max(worksheet$imCashAmount+worksheet$imNoncashAmount)
    plot(x=resultPref1$avgCashExternal,y=resultPref1$imCashAmount,xlab="Avg. Cash External Cost",ylab="Allocation for IM",
         type='l',col='blue',ylim=c(-1,yMax),bty='n',main="cost=10,liquidity=0")
    lines(x=resultPref1$avgCashExternal,y=resultPref1$imNoncashAmount,col='red')
    
    plot(x=resultPref2$avgCashExternal,y=resultPref2$imCashAmount,xlab="Avg. Cash External Cost",ylab="Allocation for IM",
         type='l',col='blue',ylim=c(-1,yMax),bty='n',main="cost=8,liquidity=2")
    lines(x=resultPref2$avgCashExternal,y=resultPref2$imNoncashAmount,col='red')
    
    plot(x=resultPref3$avgCashExternal,y=resultPref3$imCashAmount,xlab="Avg. Cash External Cost",ylab="Allocation for IM",
         type='l',col='blue',ylim=c(-1,yMax),bty='n',main="cost=5,liquidity=5")
    lines(x=resultPref3$avgCashExternal,y=resultPref3$imNoncashAmount,col='red')
    
    plot(x=resultPref4$avgCashExternal,y=resultPref4$imCashAmount,xlab="Avg. Cash External Cost",ylab="Allocation for IM",
         type='l',col='blue',ylim=c(-1,yMax),bty='n',main="cost=2,liquidity=8")
    lines(x=resultPref4$avgCashExternal,y=resultPref4$imNoncashAmount,col='red')
    
    legend(min(resultPref4$avgCashExternal),yMax/1.6,c("Cash IM","Noncash IM"),col=c('blue','red'),lty=c(1,1))
    
  } else if(cashOrNoncash=="noncash"){
    
    cat("Avg. external cost for cash daily:",mean(worksheet$avgCashExternal),'\n')
    cat("Avg. external cost for cash annualy:",mean(worksheet$avgCashExternal)*360*10000)
    
    layout(matrix(c(1,2,3,4),nrow=2))
    
    # x = avgCashExternal, y = imCashAmount
    yMax <- max(worksheet$imCashAmount+worksheet$imNoncashAmount)
    plot(x=resultPref1$avgNoncashExternal,y=resultPref1$imCashAmount,xlab="Avg. Noncash External Cost",ylab="Allocation for IM",
         type='l',col='blue',ylim=c(-1,yMax),bty='n',main="cost=10,liquidity=0")
    lines(x=resultPref1$avgNoncashExternal,y=resultPref1$imNoncashAmount,col='red')
    
    plot(x=resultPref2$avgNoncashExternal,y=resultPref2$imCashAmount,xlab="Avg. Noncash External Cost",ylab="Allocation for IM",
         type='l',col='blue',ylim=c(-1,yMax),bty='n',main="cost=8,liquidity=2")
    lines(x=resultPref2$avgNoncashExternal,y=resultPref2$imNoncashAmount,col='red')
    
    plot(x=resultPref3$avgNoncashExternal,y=resultPref3$imCashAmount,xlab="Avg. Noncash External Cost",ylab="Allocation for IM",
         type='l',col='blue',ylim=c(-1,yMax),bty='n',main="cost=5,liquidity=5")
    lines(x=resultPref3$avgNoncashExternal,y=resultPref3$imNoncashAmount,col='red')
    
    plot(x=resultPref4$avgNoncashExternal,y=resultPref4$imCashAmount,xlab="Avg. Noncash External Cost",ylab="Allocation for IM",
         type='l',col='blue',ylim=c(-1,yMax),bty='n',main="cost=2,liquidity=8")
    lines(x=resultPref4$avgNoncashExternal,y=resultPref4$imNoncashAmount,col='red')
    legend(min(resultPref4$avgCashExternal),yMax/1.6,c("Cash IM","Noncash IM"),col=c('blue','red'),lty=c(1,1))
  }
  
}
