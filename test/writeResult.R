
filePath <- "test/algoPerformanceAmountWithTimeLimit.xlsx"

assetAmountFiles <- c("test/testAssetAmount/callNumber10AssetNumberS20M20L10ResultList.RData",
                      "test/testAssetAmount/callNumber10AssetNumberS10M35L5ResultList.RData",
                      "test/testAssetAmount/callNumber10AssetNumberS10M32L5SL3ResultList.RData")

for(m in 1:3){
  worksheet <- readWorksheetFromFile(filePath,sheet="result")
  data <- load(assetAmountFiles[m])
  for(i in 1:5){
    result <- get(data[1])[[i]]
    runTime1 <- get(data[2])[i]
    
    callNumber <- length(result$checkCall_mat[,1])
    totalCallAmount <- sum(result$checkCall_mat[,1])
    timeLimit	<- 10*i^2
    movementPerMS	<- 2
    fungible <- FALSE
    dailyCost	<- result$resultAnalysis$dailyCost
    RLRatio	<- result$resultAnalysis$reservedLiquidityRatio
    movements <- result$resultAnalysis$movements
    rdata <- assetAmountFiles[m]
    output <- cbind(callNumber,totalCallAmount,timeLimit,movementPerMS,fungible,runTime1,dailyCost,RLRatio,movements,rdata)
    writeWorksheetToFile(filePath,output,sheet="result",startRow=length(worksheet[,1])+i+1,startCol=1,header=F)
  }
}





