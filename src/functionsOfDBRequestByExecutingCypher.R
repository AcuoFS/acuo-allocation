library('RNeo4j')

neo4jUrl <-"http://neo4j:7474/db/data"
neo4jDevUrl <- "http://dev.acuo.com:7474/db/data"
neo4jLocalUrl = "http://localhost:7474/db/data/"

callInfoByCallIdCypherPath <- "https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/Cypher/callInfoByCallId.cql"
availAssetByCallIdAndClientIdCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/Cypher/availAssetByCallIdAndClientId.cql'
assetInfoByAssetIdCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/Cypher/assetInfoByAssetId.cql'
eliAssetByClientIdCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/Cypher/eliAssetByClientId.cql'
resAssetByClientIdCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/Cypher/resAssetByClientId.cql'
callIdByAgreementIdPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/Cypher/callIdByAgreementId.cql'
callIdByMsIdPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/Cypher/callIdByMsId.cql'

ExecuteCypher <- function(path,...){
  params <- list(...)
  query = paste(readLines(path), collapse="\n")
 # graph = startGraph(neo4jLocalUrl,username='neo4j',password='neo4j')
  graph = startGraph(neo4jDevUrl)
  #graph = startGraph(neo4jUrl)
  cypher(graph,query,params)
}
  
CallInfoByCallId <- function(callId){
  callId <- c(callId,'nonexist')
  ExecuteCypher(path=callInfoByCallIdCypherPath,callId=callId)
}

AssetInfoByAssetId <- function(assetId){
  assetId <- c(assetId,'nonexist')
  ExecuteCypher(path=assetInfoByAssetIdCypherPath,assetId=assetId)
}

AvailAssetByCallIdAndClientId <- function(callId,clientId) {
  callId <- c(callId,'nonexist')
  ExecuteCypher(path=availAssetByCallIdAndClientIdCypherPath, callId=callId,clientId=clientId)
}

EliAssetByClientId <- function(clientId){
  ExecuteCypher(path=eliAssetByClientIdCypherPath,clientId=clientId)
}

ResAssetByClientId <- function(clientId){
  ExecuteCypher(path=resAssetByClientIdCypherPath,clientId=clientId)
}

CallIdByAgreementId <- function(agreementId){
  agreementId <- c(agreementId,'nonexist')
  ExecuteCypher(path=callIdByAgreementIdPath,agreementId=agreementId)
}

CallIdByMsId <- function(msId){
  msId <- c(msId,'nonexist')
  ExecuteCypher(path=callIdByMsIdPath,msId=msId)
}
