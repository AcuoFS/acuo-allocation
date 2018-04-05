# visualize the algo test results

library("XLConnect")

setwd("E://ACUO/projects/acuo-allocation/")
filePath <- "test/testNumberInGroup/numberInGroupPerformance.xlsx"

worksheet <- readWorksheetFromFile(filePath,sheet="Results")
worksheet$msNumInGroup <- as.numeric(worksheet$msNumInGroup)
worksheet$dailyCost <- as.numeric(worksheet$dailyCost)
worksheet$RLRatio <- as.numeric(worksheet$RLRatio)

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
  axis(side=3,at=max(aggrDailyCost$msNumInGroup),labels=paste("Data Set",i))
  plot(x=aggrRLRatio$msNumInGroup,y=aggrRLRatio$RLRatio*100,type='p',
       xlab='MS Number In A Group',ylab='RLRatio (%)')
}
