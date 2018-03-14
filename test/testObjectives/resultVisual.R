# visualize the algo test results

library("XLConnect")

filePath <- "test/testObjectives/objectivesPerformance.xlsx"

worksheet <- readWorksheetFromFile(filePath,sheet="Results")
worksheet[,6] <- as.numeric(worksheet[,6])
worksheet[,7] <- as.numeric(worksheet[,7])

costObj <- worksheet$costObj
liquidityObj <- worksheet$liquidityOBj
dailyCost <- worksheet$dailyCost
RLRatio <- worksheet$RLRatio

aggrDailyCost <- aggregate(dailyCost~costObj+liquidityObj,data=worksheet,mean)
aggrDailyCost <- aggrDailyCost[order(as.numeric(aggrDailyCost$costObj)),]
aggrRLRatio <- aggregate(RLRatio~costObj+liquidityObj,data=worksheet,mean)
aggrRLRatio <- aggrRLRatio[order(as.numeric(aggrRLRatio$liquidityObj)),]

layout(matrix(c(1,2),nrow=1), widths=c(1,1))
plot(x=aggrDailyCost$costObj,y=aggrDailyCost$dailyCost,type='l',xaxt="n",
     xlab='Objs:(cost,liquidity)',ylab='daily cost ($)')
axis(side=1,at=c(0,2,5,8,10),labels=c("(0,10)","(2,8)","(5,5)","(8,2)","(10,0)"))


plot(x=aggrRLRatio$costObj,y=aggrRLRatio$RLRatio*100,type='l',xaxt="n",
     xlab='Objs:(cost,liquidity)',ylab='RLRatio (%)')
axis(side=1,at=c(0,2,5,8,10),labels=c("(0,10)","(2,8)","(5,5)","(8,2)","(10,0)"))
