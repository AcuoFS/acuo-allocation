
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.3")
source("src/generalFunctions.R")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")
library("testthat")
library("XLConnect")

filePath <- "opportunityCost/externalPerformance.xlsx"
loadFolder <- "opportunityCost/RData"
saveFolder <- "opportunityCost/Result"
data1 <- "callNumber10AssetNumberS20M20L10"
#data2 <- "callNumber10AssetNumberS10M35L5"
loadFile <- data1
externalCostRange <- c(0.07,0.42)/10000
interval <- 0.02/10000

# preference
pref1 <- c(10,0)
pref2 <- c(8,2)
pref3 <- c(5,5)
pref4 <- c(2,8)
pref5 <- c(0,10)

## scenario 1: 
# internal cost cash: 0.3bps/day
internalCostCash <- 0.3/10000
# internal cost noncash: 0.3bps/day
internalCostNoncash <- 0.3/10000
# test external cost range: 0.07~0.42bps/day
####

testExternalCptyScenario1Data1Pref1 <- function(){
  # load data
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
  # increase the amount of assets
  resource_df$qtyOri=resource_df$qtyOri*10
  resource_df$qtyMin=resource_df$qtyMin*10
  
  # change the internal cost
  availAsset_df$internalCost[which(availAsset_df$cashOrNoncash=="cash")] <- internalCostCash
  availAsset_df$internalCost[which(availAsset_df$cashOrNoncash=="noncash")] <- internalCostNoncash
  
  # variables
  sheetName <- "CptyS1"
  cpty1 <- "JPMorgan"
  cpty2 <- "MorganStanley"
  cpty1CashOrNoncash <- "cash"
  cpty2CashOrNoncash <- "noncash"
  
  # preference setting
  pref_vec <- pref1
  pref <- "Pref1"
  
  # load excel sheet to store result later
  worksheet <- readWorksheetFromFile(filePath,sheet=sheetName)
  
  # derive the external cost ranges
  cpty1ChangeIdx_vec <- which(availAsset_df$cashOrNoncash==cpty1CashOrNoncash & availAsset_df$Cpty==cpty1)
  cpty2ChangeIdx_vec <- which(availAsset_df$cashOrNoncash==cpty2CashOrNoncash & availAsset_df$Cpty==cpty2)
  
  cpty1Low <- floor((min(availAsset_df$externalCost[cpty1ChangeIdx_vec]) - externalCostRange[1])/interval)
  cpty1Up <- floor((externalCostRange[2]-max(availAsset_df$externalCost[cpty1ChangeIdx_vec]))/interval)
  if(cpty1Low<=0){
    cpty1Changes <- c(0,1:cpty1Up)
  } else{
    cpty1Changes <- c(-(cpty1Low:1),0,1:cpty1Up)
  }
  
  cpty2Low <- floor((min(availAsset_df$externalCost[cpty2ChangeIdx_vec]) - externalCostRange[1])/interval)
  cpty2Up <- floor((externalCostRange[2]-max(availAsset_df$externalCost[cpty2ChangeIdx_vec]))/interval)
  if(cpty2Low<=0){
    cpty2Changes <- c(0,1:cpty2Up)
  } else{
    cpty2Changes <- c(-(cpty2Low:1),0,1:cpty2Up)
  }
  
  if(length(cpty1Changes)<length(cpty2Changes)){
    cpty2Changes <- cpty2Changes[1:length(cpty1Changes)]
  } else {
    cpty1Changes <- cpty1Changes[1:length(cpty2Changes)]
  }
  
  # resultList to store all the results
  resultList <- list()
  
  # oriAvailAsset_df to keep the original values of external cost 
  oriAvailAsset_df <- availAsset_df
  for(i in 1:length(cpty1Changes)){
    cpty1Change <- cpty1Changes[i]
    cpty2Change <- cpty2Changes[i]
    availAsset_df$externalCost[cpty1ChangeIdx_vec] <- oriAvailAsset_df$externalCost[cpty1ChangeIdx_vec] + interval*cpty1Change
    availAsset_df$externalCost[cpty2ChangeIdx_vec] <- oriAvailAsset_df$externalCost[cpty2ChangeIdx_vec] + interval*cpty2Change
    
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                      pref_vec,operLimit,operLimitMs_vec,fungible,
                                      ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    
    avgExternal_df <- aggregate(externalCost~cashOrNoncash,data=availAsset_df,mean)
    avgCashExternal <- avgExternal_df$externalCost[which(avgExternal_df$cashOrNoncash=='cash')]
    avgNoncashExternal <- avgExternal_df$externalCost[which(avgExternal_df$cashOrNoncash=='noncash')]
    result_df <- ResultList2Df(resultList[[i]]$callOutput,callInfo_df$id)
    vmAssets <- unique(as.character(result_df$Asset[which(result_df$marginType=="Variation")]))
    imAssets <- unique(as.character(result_df$Asset[which(result_df$marginType=="Initial")]))
    imCashAmount <- sum(result_df$`Amount(USD)`[which(result_df$marginType=="Initial" & as.character(result_df$Asset)==as.character(result_df$Currency))])
    imNoncashAmount <- sum(result_df$`Amount(USD)`[which(result_df$marginType=="Initial" & as.character(result_df$Asset)!=as.character(result_df$Currency))])
    
    output <- cbind(paste(loadFile,"RData",sep='.'),pref_vec[1],pref_vec[2],avgCashExternal,avgNoncashExternal,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements,
                    paste(vmAssets, collapse=" "),paste(imAssets, collapse=" "),imCashAmount,imNoncashAmount)
    writeWorksheetToFile(filePath,data=output,sheet=sheetName,startRow=length(worksheet[,1])+2+i-1,startCol=1,header=F)
  }
  availAsset_df <- oriAvailAsset_df
  
  # save the result to RData
  saveFile <- paste(loadFile,sheetName,pref,"ResultList.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save(resultList,callInfo_df,resource_df,availAsset_df,changes,pref_vec,file=savePath)
  
  for(m in 1:length(changes)){
    print(as.character(ResultList2Df(resultList[[m]]$callOutput,callInfo_df$id)$Asset))
  }
}
testExternalCptyScenario1Data1Pref2 <- function(){
  # load data
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
  # increase the amount of assets
  resource_df$qtyOri=resource_df$qtyOri*10
  resource_df$qtyMin=resource_df$qtyMin*10
  
  # change the internal cost
  availAsset_df$internalCost[which(availAsset_df$cashOrNoncash=="cash")] <- internalCostCash
  availAsset_df$internalCost[which(availAsset_df$cashOrNoncash=="noncash")] <- internalCostNoncash
  
  # variables
  sheetName <- "CptyS1"
  cpty1 <- "JPMorgan"
  cpty2 <- "MorganStanley"
  cpty1CashOrNoncash <- "cash"
  cpty2CashOrNoncash <- "noncash"
  
  # preference setting
  pref_vec <- pref2
  pref <- "Pref2"
  
  # load excel sheet to store result later
  worksheet <- readWorksheetFromFile(filePath,sheet=sheetName)
  
  # derive the external cost ranges
  cpty1ChangeIdx_vec <- which(availAsset_df$cashOrNoncash==cpty1CashOrNoncash & availAsset_df$Cpty==cpty1)
  cpty2ChangeIdx_vec <- which(availAsset_df$cashOrNoncash==cpty2CashOrNoncash & availAsset_df$Cpty==cpty2)
  
  cpty1Low <- floor((min(availAsset_df$externalCost[cpty1ChangeIdx_vec]) - externalCostRange[1])/interval)
  cpty1Up <- floor((externalCostRange[2]-max(availAsset_df$externalCost[cpty1ChangeIdx_vec]))/interval)
  if(cpty1Low<=0){
    cpty1Changes <- c(0,1:cpty1Up)
  } else{
    cpty1Changes <- c(-(cpty1Low:1),0,1:cpty1Up)
  }
  
  cpty2Low <- floor((min(availAsset_df$externalCost[cpty2ChangeIdx_vec]) - externalCostRange[1])/interval)
  cpty2Up <- floor((externalCostRange[2]-max(availAsset_df$externalCost[cpty2ChangeIdx_vec]))/interval)
  if(cpty2Low<=0){
    cpty2Changes <- c(0,1:cpty2Up)
  } else{
    cpty2Changes <- c(-(cpty2Low:1),0,1:cpty2Up)
  }
  
  if(length(cpty1Changes)<length(cpty2Changes)){
    cpty2Changes <- cpty2Changes[1:length(cpty1Changes)]
  } else {
    cpty1Changes <- cpty1Changes[1:length(cpty2Changes)]
  }
  
  # resultList to store all the results
  resultList <- list()
  
  # oriAvailAsset_df to keep the original values of external cost 
  oriAvailAsset_df <- availAsset_df
  for(i in 1:length(cpty1Changes)){
    cpty1Change <- cpty1Changes[i]
    cpty2Change <- cpty2Changes[i]
    availAsset_df$externalCost[cpty1ChangeIdx_vec] <- oriAvailAsset_df$externalCost[cpty1ChangeIdx_vec] + interval*cpty1Change
    availAsset_df$externalCost[cpty2ChangeIdx_vec] <- oriAvailAsset_df$externalCost[cpty2ChangeIdx_vec] + interval*cpty2Change
    
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                      pref_vec,operLimit,operLimitMs_vec,fungible,
                                      ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    
    avgExternal_df <- aggregate(externalCost~cashOrNoncash,data=availAsset_df,mean)
    avgCashExternal <- avgExternal_df$externalCost[which(avgExternal_df$cashOrNoncash=='cash')]
    avgNoncashExternal <- avgExternal_df$externalCost[which(avgExternal_df$cashOrNoncash=='noncash')]
    result_df <- ResultList2Df(resultList[[i]]$callOutput,callInfo_df$id)
    vmAssets <- unique(as.character(result_df$Asset[which(result_df$marginType=="Variation")]))
    imAssets <- unique(as.character(result_df$Asset[which(result_df$marginType=="Initial")]))
    imCashAmount <- sum(result_df$`Amount(USD)`[which(result_df$marginType=="Initial" & as.character(result_df$Asset)==as.character(result_df$Currency))])
    imNoncashAmount <- sum(result_df$`Amount(USD)`[which(result_df$marginType=="Initial" & as.character(result_df$Asset)!=as.character(result_df$Currency))])
    
    output <- cbind(paste(loadFile,"RData",sep='.'),pref_vec[1],pref_vec[2],avgCashExternal,avgNoncashExternal,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements,
                    paste(vmAssets, collapse=" "),paste(imAssets, collapse=" "),imCashAmount,imNoncashAmount)
    writeWorksheetToFile(filePath,data=output,sheet=sheetName,startRow=length(worksheet[,1])+2+i-1,startCol=1,header=F)
  }
  availAsset_df <- oriAvailAsset_df
  
  # save the result to RData
  saveFile <- paste(loadFile,sheetName,pref,"ResultList.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save(resultList,callInfo_df,resource_df,availAsset_df,changes,pref_vec,file=savePath)
  
  for(m in 1:length(changes)){
    print(as.character(ResultList2Df(resultList[[m]]$callOutput,callInfo_df$id)$Asset))
  }
}
testExternalCptyScenario1Data1Pref3 <- function(){
  # load data
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
  # increase the amount of assets
  resource_df$qtyOri=resource_df$qtyOri*10
  resource_df$qtyMin=resource_df$qtyMin*10
  
  # change the internal cost
  availAsset_df$internalCost[which(availAsset_df$cashOrNoncash=="cash")] <- internalCostCash
  availAsset_df$internalCost[which(availAsset_df$cashOrNoncash=="noncash")] <- internalCostNoncash
  
  # variables
  sheetName <- "CptyS1"
  cpty1 <- "JPMorgan"
  cpty2 <- "MorganStanley"
  cpty1CashOrNoncash <- "cash"
  cpty2CashOrNoncash <- "noncash"
  
  # preference setting
  pref_vec <- pref3
  pref <- "Pref3"
  
  # load excel sheet to store result later
  worksheet <- readWorksheetFromFile(filePath,sheet=sheetName)
  
  # derive the external cost ranges
  cpty1ChangeIdx_vec <- which(availAsset_df$cashOrNoncash==cpty1CashOrNoncash & availAsset_df$Cpty==cpty1)
  cpty2ChangeIdx_vec <- which(availAsset_df$cashOrNoncash==cpty2CashOrNoncash & availAsset_df$Cpty==cpty2)
  
  cpty1Low <- floor((min(availAsset_df$externalCost[cpty1ChangeIdx_vec]) - externalCostRange[1])/interval)
  cpty1Up <- floor((externalCostRange[2]-max(availAsset_df$externalCost[cpty1ChangeIdx_vec]))/interval)
  if(cpty1Low<=0){
    cpty1Changes <- c(0,1:cpty1Up)
  } else{
    cpty1Changes <- c(-(cpty1Low:1),0,1:cpty1Up)
  }
  
  cpty2Low <- floor((min(availAsset_df$externalCost[cpty2ChangeIdx_vec]) - externalCostRange[1])/interval)
  cpty2Up <- floor((externalCostRange[2]-max(availAsset_df$externalCost[cpty2ChangeIdx_vec]))/interval)
  if(cpty2Low<=0){
    cpty2Changes <- c(0,1:cpty2Up)
  } else{
    cpty2Changes <- c(-(cpty2Low:1),0,1:cpty2Up)
  }
  
  if(length(cpty1Changes)<length(cpty2Changes)){
    cpty2Changes <- cpty2Changes[1:length(cpty1Changes)]
  } else {
    cpty1Changes <- cpty1Changes[1:length(cpty2Changes)]
  }
  
  # resultList to store all the results
  resultList <- list()
  
  # oriAvailAsset_df to keep the original values of external cost 
  oriAvailAsset_df <- availAsset_df
  for(i in 1:length(cpty1Changes)){
    cpty1Change <- cpty1Changes[i]
    cpty2Change <- cpty2Changes[i]
    availAsset_df$externalCost[cpty1ChangeIdx_vec] <- oriAvailAsset_df$externalCost[cpty1ChangeIdx_vec] + interval*cpty1Change
    availAsset_df$externalCost[cpty2ChangeIdx_vec] <- oriAvailAsset_df$externalCost[cpty2ChangeIdx_vec] + interval*cpty2Change
    
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                      pref_vec,operLimit,operLimitMs_vec,fungible,
                                      ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    
    avgExternal_df <- aggregate(externalCost~cashOrNoncash,data=availAsset_df,mean)
    avgCashExternal <- avgExternal_df$externalCost[which(avgExternal_df$cashOrNoncash=='cash')]
    avgNoncashExternal <- avgExternal_df$externalCost[which(avgExternal_df$cashOrNoncash=='noncash')]
    result_df <- ResultList2Df(resultList[[i]]$callOutput,callInfo_df$id)
    vmAssets <- unique(as.character(result_df$Asset[which(result_df$marginType=="Variation")]))
    imAssets <- unique(as.character(result_df$Asset[which(result_df$marginType=="Initial")]))
    imCashAmount <- sum(result_df$`Amount(USD)`[which(result_df$marginType=="Initial" & as.character(result_df$Asset)==as.character(result_df$Currency))])
    imNoncashAmount <- sum(result_df$`Amount(USD)`[which(result_df$marginType=="Initial" & as.character(result_df$Asset)!=as.character(result_df$Currency))])
    
    output <- cbind(paste(loadFile,"RData",sep='.'),pref_vec[1],pref_vec[2],avgCashExternal,avgNoncashExternal,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements,
                    paste(vmAssets, collapse=" "),paste(imAssets, collapse=" "),imCashAmount,imNoncashAmount)
    writeWorksheetToFile(filePath,data=output,sheet=sheetName,startRow=length(worksheet[,1])+2+i-1,startCol=1,header=F)
  }
  availAsset_df <- oriAvailAsset_df
  
  # save the result to RData
  saveFile <- paste(loadFile,sheetName,pref,"ResultList.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save(resultList,callInfo_df,resource_df,availAsset_df,changes,pref_vec,file=savePath)
  
  for(m in 1:length(changes)){
    print(as.character(ResultList2Df(resultList[[m]]$callOutput,callInfo_df$id)$Asset))
  }
}
testExternalCptyScenario1Data1Pref4 <- function(){
  # load data
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
  # increase the amount of assets
  resource_df$qtyOri=resource_df$qtyOri*10
  resource_df$qtyMin=resource_df$qtyMin*10
  
  # change the internal cost
  availAsset_df$internalCost[which(availAsset_df$cashOrNoncash=="cash")] <- internalCostCash
  availAsset_df$internalCost[which(availAsset_df$cashOrNoncash=="noncash")] <- internalCostNoncash
  
  # variables
  sheetName <- "CptyS1"
  cpty1 <- "JPMorgan"
  cpty2 <- "MorganStanley"
  cpty1CashOrNoncash <- "cash"
  cpty2CashOrNoncash <- "noncash"
  
  # preference setting
  pref_vec <- pref4
  pref <- "Pref4"
  
  # load excel sheet to store result later
  worksheet <- readWorksheetFromFile(filePath,sheet=sheetName)
  
  # derive the external cost ranges
  cpty1ChangeIdx_vec <- which(availAsset_df$cashOrNoncash==cpty1CashOrNoncash & availAsset_df$Cpty==cpty1)
  cpty2ChangeIdx_vec <- which(availAsset_df$cashOrNoncash==cpty2CashOrNoncash & availAsset_df$Cpty==cpty2)
  
  cpty1Low <- floor((min(availAsset_df$externalCost[cpty1ChangeIdx_vec]) - externalCostRange[1])/interval)
  cpty1Up <- floor((externalCostRange[2]-max(availAsset_df$externalCost[cpty1ChangeIdx_vec]))/interval)
  if(cpty1Low<=0){
    cpty1Changes <- c(0,1:cpty1Up)
  } else{
    cpty1Changes <- c(-(cpty1Low:1),0,1:cpty1Up)
  }
  
  cpty2Low <- floor((min(availAsset_df$externalCost[cpty2ChangeIdx_vec]) - externalCostRange[1])/interval)
  cpty2Up <- floor((externalCostRange[2]-max(availAsset_df$externalCost[cpty2ChangeIdx_vec]))/interval)
  if(cpty2Low<=0){
    cpty2Changes <- c(0,1:cpty2Up)
  } else{
    cpty2Changes <- c(-(cpty2Low:1),0,1:cpty2Up)
  }
  
  if(length(cpty1Changes)<length(cpty2Changes)){
    cpty2Changes <- cpty2Changes[1:length(cpty1Changes)]
  } else {
    cpty1Changes <- cpty1Changes[1:length(cpty2Changes)]
  }
  
  # resultList to store all the results
  resultList <- list()
  
  # oriAvailAsset_df to keep the original values of external cost 
  oriAvailAsset_df <- availAsset_df
  for(i in 1:length(cpty1Changes)){
    cpty1Change <- cpty1Changes[i]
    cpty2Change <- cpty2Changes[i]
    availAsset_df$externalCost[cpty1ChangeIdx_vec] <- oriAvailAsset_df$externalCost[cpty1ChangeIdx_vec] + interval*cpty1Change
    availAsset_df$externalCost[cpty2ChangeIdx_vec] <- oriAvailAsset_df$externalCost[cpty2ChangeIdx_vec] + interval*cpty2Change
    
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                      pref_vec,operLimit,operLimitMs_vec,fungible,
                                      ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    
    avgExternal_df <- aggregate(externalCost~cashOrNoncash,data=availAsset_df,mean)
    avgCashExternal <- avgExternal_df$externalCost[which(avgExternal_df$cashOrNoncash=='cash')]
    avgNoncashExternal <- avgExternal_df$externalCost[which(avgExternal_df$cashOrNoncash=='noncash')]
    result_df <- ResultList2Df(resultList[[i]]$callOutput,callInfo_df$id)
    vmAssets <- unique(as.character(result_df$Asset[which(result_df$marginType=="Variation")]))
    imAssets <- unique(as.character(result_df$Asset[which(result_df$marginType=="Initial")]))
    imCashAmount <- sum(result_df$`Amount(USD)`[which(result_df$marginType=="Initial" & as.character(result_df$Asset)==as.character(result_df$Currency))])
    imNoncashAmount <- sum(result_df$`Amount(USD)`[which(result_df$marginType=="Initial" & as.character(result_df$Asset)!=as.character(result_df$Currency))])
    
    output <- cbind(paste(loadFile,"RData",sep='.'),pref_vec[1],pref_vec[2],avgCashExternal,avgNoncashExternal,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements,
                    paste(vmAssets, collapse=" "),paste(imAssets, collapse=" "),imCashAmount,imNoncashAmount)
    writeWorksheetToFile(filePath,data=output,sheet=sheetName,startRow=length(worksheet[,1])+2+i-1,startCol=1,header=F)
  }
  availAsset_df <- oriAvailAsset_df
  
  # save the result to RData
  saveFile <- paste(loadFile,sheetName,pref,"ResultList.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save(resultList,callInfo_df,resource_df,availAsset_df,changes,pref_vec,file=savePath)
  
  for(m in 1:length(changes)){
    print(as.character(ResultList2Df(resultList[[m]]$callOutput,callInfo_df$id)$Asset))
  }
}
testExternalCptyScenario1Data1Pref5 <- function(){
  # load data
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
  # increase the amount of assets
  resource_df$qtyOri=resource_df$qtyOri*10
  resource_df$qtyMin=resource_df$qtyMin*10
  
  # change the internal cost
  availAsset_df$internalCost[which(availAsset_df$cashOrNoncash=="cash")] <- internalCostCash
  availAsset_df$internalCost[which(availAsset_df$cashOrNoncash=="noncash")] <- internalCostNoncash
  
  # variables
  sheetName <- "CptyS1"
  cpty1 <- "JPMorgan"
  cpty2 <- "MorganStanley"
  cpty1CashOrNoncash <- "cash"
  cpty2CashOrNoncash <- "noncash"
  
  # preference setting
  pref_vec <- pref5
  pref <- "Pref5"
  
  # load excel sheet to store result later
  worksheet <- readWorksheetFromFile(filePath,sheet=sheetName)
  
  # derive the external cost ranges
  cpty1ChangeIdx_vec <- which(availAsset_df$cashOrNoncash==cpty1CashOrNoncash & availAsset_df$Cpty==cpty1)
  cpty2ChangeIdx_vec <- which(availAsset_df$cashOrNoncash==cpty2CashOrNoncash & availAsset_df$Cpty==cpty2)
  
  cpty1Low <- floor((min(availAsset_df$externalCost[cpty1ChangeIdx_vec]) - externalCostRange[1])/interval)
  cpty1Up <- floor((externalCostRange[2]-max(availAsset_df$externalCost[cpty1ChangeIdx_vec]))/interval)
  if(cpty1Low<=0){
    cpty1Changes <- c(0,1:cpty1Up)
  } else{
    cpty1Changes <- c(-(cpty1Low:1),0,1:cpty1Up)
  }
  
  cpty2Low <- floor((min(availAsset_df$externalCost[cpty2ChangeIdx_vec]) - externalCostRange[1])/interval)
  cpty2Up <- floor((externalCostRange[2]-max(availAsset_df$externalCost[cpty2ChangeIdx_vec]))/interval)
  if(cpty2Low<=0){
    cpty2Changes <- c(0,1:cpty2Up)
  } else{
    cpty2Changes <- c(-(cpty2Low:1),0,1:cpty2Up)
  }
  
  if(length(cpty1Changes)<length(cpty2Changes)){
    cpty2Changes <- cpty2Changes[1:length(cpty1Changes)]
  } else {
    cpty1Changes <- cpty1Changes[1:length(cpty2Changes)]
  }
  
  # resultList to store all the results
  resultList <- list()
  
  # oriAvailAsset_df to keep the original values of external cost 
  oriAvailAsset_df <- availAsset_df
  for(i in 1:length(cpty1Changes)){
    cpty1Change <- cpty1Changes[i]
    cpty2Change <- cpty2Changes[i]
    availAsset_df$externalCost[cpty1ChangeIdx_vec] <- oriAvailAsset_df$externalCost[cpty1ChangeIdx_vec] + interval*cpty1Change
    availAsset_df$externalCost[cpty2ChangeIdx_vec] <- oriAvailAsset_df$externalCost[cpty2ChangeIdx_vec] + interval*cpty2Change
    
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                      pref_vec,operLimit,operLimitMs_vec,fungible,
                                      ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    
    avgExternal_df <- aggregate(externalCost~cashOrNoncash,data=availAsset_df,mean)
    avgCashExternal <- avgExternal_df$externalCost[which(avgExternal_df$cashOrNoncash=='cash')]
    avgNoncashExternal <- avgExternal_df$externalCost[which(avgExternal_df$cashOrNoncash=='noncash')]
    result_df <- ResultList2Df(resultList[[i]]$callOutput,callInfo_df$id)
    vmAssets <- unique(as.character(result_df$Asset[which(result_df$marginType=="Variation")]))
    imAssets <- unique(as.character(result_df$Asset[which(result_df$marginType=="Initial")]))
    imCashAmount <- sum(result_df$`Amount(USD)`[which(result_df$marginType=="Initial" & as.character(result_df$Asset)==as.character(result_df$Currency))])
    imNoncashAmount <- sum(result_df$`Amount(USD)`[which(result_df$marginType=="Initial" & as.character(result_df$Asset)!=as.character(result_df$Currency))])
    
    output <- cbind(paste(loadFile,"RData",sep='.'),pref_vec[1],pref_vec[2],avgCashExternal,avgNoncashExternal,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements,
                    paste(vmAssets, collapse=" "),paste(imAssets, collapse=" "),imCashAmount,imNoncashAmount)
    writeWorksheetToFile(filePath,data=output,sheet=sheetName,startRow=length(worksheet[,1])+2+i-1,startCol=1,header=F)
  }
  availAsset_df <- oriAvailAsset_df
  
  # save the result to RData
  saveFile <- paste(loadFile,sheetName,pref,"ResultList.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save(resultList,callInfo_df,resource_df,availAsset_df,changes,pref_vec,file=savePath)
  
  for(m in 1:length(changes)){
    print(as.character(ResultList2Df(resultList[[m]]$callOutput,callInfo_df$id)$Asset))
  }
}
