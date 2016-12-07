library('RNeo4j')

source('src/functionsOfDBRequestByExecutingCypher.R')

#### CONSTANTS, PLASE DO NOT CHANGE #####
assetId1 <- c('USD','JP1023621G33')
assetId2 <- c("US912796HW25","US5801351017","GBP","NOK","SG1W45939194","CAD","JPY","GB00B1VWPC84")

#### EXAMPLE FUNCTIONS ##################
assetInfoByAssetIdEx1<-function(){
  assetInfoByAssetId(assetId1)
}
assetInfoByAssetIdEx2<-function(){
  assetInfoByAssetId(assetId2)
}

#### EXAMPLES RESULTS ####################
assetInfoByAssetIdEx1()
assetInfoByAssetIdEx2()


