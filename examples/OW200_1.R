library('RNeo4j')

source('src/assetInfoById.R')

assetId <- c("US912796HW25","37833100","GBP","USD","46625H100","CAD","JPY")

result <- assetInfoById(assetId)
