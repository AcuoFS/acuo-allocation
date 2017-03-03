library('RNeo4j')

#neo4jUrl <- 'http://neo4j.acuo.com:7474/db/data/'
neo4jUrl <-"http://neo4j:7474/db/data"
neo4jLocalUrl = "http://localhost:7474/db/data/"

callInfoByCallIdCypherPath <- "https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/src/Cypher/callInfoByCallId.cql"
availAssetByCallIdAndClientIdCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/src/Cypher/availAssetByCallIdAndClientId.cql'
assetInfoByAssetIdCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/src/Cypher/assetInfoByAssetId.cql'
eliAssetByClientIdCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/src/Cypher/eliAssetByClientId.cql'
resAssetByClientIdCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/src/Cypher/resAssetByClientId.cql'


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

availAssetByCallIdAndClientId <- function(callId,clientId) {
  executeCypher(path=availAssetByCallIdAndClientIdCypherPath, callId=callId,clientId=clientId)
}

eliAssetByClientId <- function(clientId){
  executeCypher(path=eliAssetByClientIdCypherPath,clientId=clientId)
}

resAssetByClientId <- function(clientId){
  executeCypher(path=resAssetByClientIdCypherPath,clientId=clientId)
}
