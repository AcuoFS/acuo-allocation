# visualize the algo test results

library("XLConnect")

filePath <- "test/testNumberInGroup/numberInGroupPerformance.xlsx"

worksheet <- readWorksheetFromFile(filePath,sheet="Results")
worksheet$dailyCost <- as.numeric(worksheet$dailyCost)
worksheet$RLRatio <- as.numeric(worksheet$RLRatio)

costObj <- worksheet$costObj
liquidityObj <- worksheet$liquidityOBj
dailyCost <- worksheet$dailyCost
RLRatio <- worksheet$RLRatio

dataSource <- unique(worksheet$Rdata)


layout(matrix(c(1:6),nrow=3,byrow=T) )
for(i in 1:length(dataSource)){
  subSet <- worksheet[which(worksheet$Rdata==dataSource[i]),]
  aggrDailyCost <- aggregate(dailyCost~msNumInGroup,data=subSet,mean)
  aggrDailyCost <- aggrDailyCost[order(as.numeric(aggrDailyCost$msNumInGroup)),]
  
  aggrRLRatio <- aggregate(RLRatio~msNumInGroup,data=subSet,mean)
  aggrRLRatio <- aggrRLRatio[order(as.numeric(aggrRLRatio$msNumInGroup)),]
  
  plot(x=aggrDailyCost$msNumInGroup,y=aggrDailyCost$dailyCost,type='p',
       xlab='MS Number In A Group',ylab='daily cost ($)')
  plot(x=aggrRLRatio$msNumInGroup,y=aggrRLRatio$RLRatio*100,type='p',
       xlab='MS Number In A Group',ylab='RLRatio (%)')
}

aggrDailyCost <- aggregate(dailyCost~Rdata+msNumInGroup,data=worksheet,mean)
aggrDailyCost <- aggrDailyCost[order(as.numeric(aggrDailyCost$costObj)),]
aggrRLRatio <- aggregate(dailyCost~Rdata+msNumInGroup,data=worksheet,mean)
aggrRLRatio <- aggrRLRatio[order(as.numeric(aggrRLRatio$liquidityObj)),]

layout(matrix(c(1,2),nrow=1), widths=c(1,1))
plot(x=aggrDailyCost$costObj,y=aggrDailyCost$dailyCost,type='l',xaxt="n",
     xlab='Objs:(cost,liquidity)',ylab='daily cost ($)')
axis(side=1,at=c(0,2,5,8,10),labels=c("(0,10)","(2,8)","(5,5)","(8,2)","(10,0)"))


plot(x=aggrRLRatio$costObj,y=aggrRLRatio$RLRatio*100,type='l',xaxt="n",
     xlab='Objs:(cost,liquidity)',ylab='RLRatio (%)')
axis(side=1,at=c(0,2,5,8,10),labels=c("(0,10)","(2,8)","(5,5)","(8,2)","(10,0)"))
