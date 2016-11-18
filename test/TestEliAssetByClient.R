library('RUnit')

source('R/eliAssetByClient.R')


test.OW168.1 <- function(){
  result <- eliAssetByClient('c1')
  write.csv(file='/Result/eliAssetByClient.csv',x=result)
  result$assetID
}


