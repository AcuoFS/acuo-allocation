# visualize the algo test results

library("XLConnect")

setwd("E://ACUO/projects/acuo-allocation/")
filePath <- "test/testIntegerRatio/integerRatioPerformance1.xlsx"

worksheet <- readWorksheetFromFile(filePath,sheet="Results")
worksheet$integerRatio <- as.numeric(worksheet$integerRatio)
worksheet$dailyCost <- as.numeric(worksheet$dailyCost)
worksheet$RLRatio <- as.numeric(worksheet$RLRatio)

integerRatio <- worksheet$integerRatio
dailyCost <- worksheet$dailyCost
RLRatio <- worksheet$RLRatio

aggrDailyCost <- aggregate(dailyCost~integerRatio,data=worksheet,mean)
aggrLiquidity <- aggregate(RLRatio~integerRatio,data=worksheet,mean)


layout(matrix(c(1,2),nrow=1), widths=c(1,1))
plot(x=aggrDailyCost$integerRatio,y=aggrDailyCost$dailyCost,type='l',xaxt="n",
     xlab='integerRatio',ylab='daily cost ($)')
axis(side=1,at=c(0,2,5,8,10),labels=c("(0,10)","(2,8)","(5,5)","(8,2)","(10,0)"))


plot(x=aggrRLRatio$integerRatio,y=aggrRLRatio$RLRatio*100,type='l',xaxt="n",
     xlab='integerRatio',ylab='RLRatio (%)')
axis(side=1,at=c(0,2,5,8,10),labels=c("(0,10)","(2,8)","(5,5)","(8,2)","(10,0)"))
