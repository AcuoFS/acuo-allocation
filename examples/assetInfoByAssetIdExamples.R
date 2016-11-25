library('RNeo4j')

source('src/functionsOfDBRequestByExecutingCypher.R')

#### CONSTANTS, PLASE DO NOT CHANGE #####
assetId1 <- c('USD','Gold')
assetId2 <- c("US912796HW25","37833100","GBP","USD","46625H100","CAD","JPY","Gold")

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


