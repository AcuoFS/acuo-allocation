library('RNeo4j')

neo4jUrl <- 'http://neo4j:7474/db/data'
neo4jLocalUrl <- 'http://localhost:7474/db/data/'

callInfoByCallIdCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/src/Cypher/callInfoByCallId.cql'
availAssetByCallIdAndClientIdCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/src/Cypher/availAssetByCallIdAndClientId.cql'
eliAssetByClientIdCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/src/Cypher/eliAssetByClientId.cql'
assetInfoByAssetIdCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/src/Cypher/assetInfoByAssetId.cql'


executeCypher <- function(path,...){
  params <- list(...)
  query = paste(readLines(path), collapse="\n")
  #graph = startGraph(neo4jUrl)
  graph = startGraph(neo4jLocalUrl,username='neo4j',password='neo4j')
  cypher(graph,query,params)
}
  
callInfoByCallId <- function(callId){
  executeCypher(path=callInfoByCallIdCypherPath,callId=callId)
}

assetInfoByAssetId <- function(assetId){
  executeCypher(path=assetInfoByAssetIdCypherPath,assetId=assetId)
}

eliAssetByClientId <- function(clientId){
  executeCypher(path=eliAssetByClientIdCypherPath,clientId=clientId)
}

availAssetByCallIdAndClientId <- function(callId,clientId,order='assetId') {
  executeCypher(path=availAssetByCallIdAndClientIdCypherPath, callId=callId,clientId=clientId, order=order)
}
