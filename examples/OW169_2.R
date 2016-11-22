library('RNeo4j')

source('src/availAssetByCall.R')

callId <- c('mc2','mc3')
order <- 'assetId'

result <- availAssetByCall(callId,order)
