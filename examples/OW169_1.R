library('RNeo4j')

source('../src/availAssetByCall.cql')

callId <- c('c1','c2','c3')
order <- 'isdaType'

result <- availAssetByCall(callId,order)