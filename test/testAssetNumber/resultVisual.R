# visualize the algo test results

library("XLConnect")

setwd("E://ACUO/projects/acuo-allocation/")
filePath <- "test/testAssetNumber/assetNumberPerformance.xlsx"

worksheet <- readWorksheetFromFile(filePath,sheet="Results")


layout(matrix(c(1,2),nrow=1), widths=c(1,1))
for(callNum in c(1,3)){
  subSet <- worksheet[which(worksheet$callNumber==callNum),]

  assetNum <- as.numeric(subSet$assetNumber)
  runTime <- as.numeric(subSet$runTime)
  
  plot(x=assetNum,y=runTime,type='p',cex.lab=0.9, cex.axis=0.8,
       xlab='Asset Number ',ylab='Algo Run Time (sec)')
  axis(side=3,at=12,labels=paste(callNum,"call"),tick=F)
 
}

