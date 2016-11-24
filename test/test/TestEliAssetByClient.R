library('RUnit')


test.OW168_1 <- function(){
  source('examples/OW168_1.R')
  assetId <- result$assetId
  
  checkTrue(is.element('USD',assetId))
  checkEquals(result$internalCost[which(assetId=='USD')],0.03)
  checkEquals(result$opptCost[which(assetId=='USD')],0.04)
  checkEquals(result$value[which(assetId=='USD')],1)
  checkEquals(result$currency[which(assetId=='USD')],'USD')
  checkEquals(result$quantity[which(assetId=='USD')],20000000)
  checkEquals(result$availableQuantities[which(assetId=='USD')],20000000)
  
  checkTrue(is.element('GBP',assetId))
  checkEquals(result$internalCost[which(assetId=='GBP')],0.01)
  checkEquals(result$opptCost[which(assetId=='GBP')],0.05)
  checkEquals(result$quantity[which(assetId=='GBP')],100000)
  checkEquals(result$availableQuantities[which(assetId=='GBP')],100000)
  
  checkTrue(is.element('JPY',assetId))
  checkEquals(result$internalCost[which(assetId=='JPY')],0.01)
  checkEquals(result$value[which(assetId=='JPY')],1)
  checkEquals(result$currency[which(assetId=='JPY')],'JPY')
  checkEquals(result$quantity[which(assetId=='JPY')],5000000)
  checkEquals(result$availableQuantities[which(assetId=='JPY')],5000000)
  
  checkTrue(is.element('CAD',assetId))
  checkEquals(result$internalCost[which(assetId=='CAD')],0.01)
  checkEquals(result$currency[which(assetId=='CAD')],'CAD')
  checkEquals(result$quantity[which(assetId=='CAD')],40000000)
  checkEquals(result$availableQuantities[which(assetId=='CAD')],40000000)
  
  checkTrue(is.element('46625H100',assetId))
  checkEquals(result$internalCost[which(assetId=='46625H100')],0.02)
  checkEquals(result$value[which(assetId=='46625H100')],65)
  checkEquals(result$quantity[which(assetId=='46625H100')],200000)
  checkEquals(result$availableQuantities[which(assetId=='46625H100')],200000)
  
  checkTrue(is.element('37833100',assetId))
  checkEquals(result$internalCost[which(assetId=='37833100')],0.02)
  checkEquals(result$opptCost[which(assetId=='37833100')],0.02)
  checkEquals(result$quantity[which(assetId=='37833100')],500000)
  checkEquals(result$availableQuantities[which(assetId=='37833100')],500000)
  
  checkTrue(is.element('US912796HW25',assetId))
  checkEquals(result$internalCost[which(assetId=='US912796HW25')],0.03)
  checkEquals(result$opptCost[which(assetId=='US912796HW25')],0.01)
  checkEquals(result$value[which(assetId=='US912796HW25')],100)
  checkEquals(result$currency[which(assetId=='US912796HW25')],'USD')
  checkEquals(result$quantity[which(assetId=='US912796HW25')],500000)
  checkEquals(result$availableQuantities[which(assetId=='US912796HW25')],500000)
  
}


test.OW168_2 <- function(){
  source('examples/OW168_2.R')
  assetId <- result$assetId
  
  checkTrue(is.element('USD',assetId))
  checkEquals(result$internalCost[which(assetId=='USD')],0.03)
  checkEquals(result$opptCost[which(assetId=='USD')],0.04)
  checkEquals(result$value[which(assetId=='USD')],1)
  checkEquals(result$currency[which(assetId=='USD')],'USD')
  checkEquals(result$quantity[which(assetId=='USD')],3000000)
  checkEquals(result$availableQuantities[which(assetId=='USD')],3000000)
  
  checkTrue(is.element('Gold',assetId))
  checkEquals(result$internalCost[which(assetId=='Gold')],0.03)
  checkEquals(result$opptCost[which(assetId=='Gold')],0.01)
  checkEquals(result$currency[which(assetId=='Gold')],'GLD')
  checkEquals(result$value[which(assetId=='Gold')],10)
  checkEquals(result$quantity[which(assetId=='Gold')],500000)
  checkEquals(result$availableQuantities[which(assetId=='Gold')],500000)
}

test.OW168_3 <- function(){
  source('src/eliAssetByClient.R')
  clientId <- c('c1','c2')
  result <- eliAssetByClient(clientId)
  checkEquals(length(result),0)
}


