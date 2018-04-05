library('RNeo4j')

neo4jUrl <-"http://neo4j:7474/db/data"
neo4jDevUrl <- "http://dev.acuo.com:7474/db/data"
neo4jLocalUrl <- "http://localhost:7474/db/data/"
neo4jUATUrl <- "http://10.0.1.6:7474/db/data"
neo4jQAUrl <- "http://10.0.1.5:7474/db/data"

callInfoByCallIdCypherPath <- "https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/Cypher/callInfoByCallId.cql"
availAssetByCallIdAndClientIdCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/Cypher/availAssetByCallIdAndClientId.cql'
#assetInfoByAssetIdCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/Cypher/assetInfoByAssetId.cql'
assetInfoByAssetIdCypherPath <- 'src/Cypher/assetInfoByAssetId.cql'

eliAssetByClientIdCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/Cypher/eliAssetByClientId.cql'
resAssetByClientIdCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/Cypher/resAssetByClientId.cql'
callIdByAgreementIdPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/Cypher/callIdByAgreementId.cql'
callIdByMsIdPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/Cypher/callIdByMsId.cql'
#fxRateByCurrencyPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/Cypher/fxRateByCurrency.cql'
fxRateByCurrencyPath <- 'E:/ACUO/projects/acuo-allocation/src/Cypher/fxRateByCurrency.cql'

ExecuteCypher <- function(path,...){
  params <- list(...)
  query <- paste(readLines(path), collapse="\n")
  graph <- startGraph(neo4jLocalUrl,username='neo4j',password='neo4j')
  #graph <- startGraph(neo4jDevUrl)
  #graph <- startGraph(neo4jUrl)
  #graph <- startGraph(neo4jUATUrl)
  #graph <- startGraph(neo4jQAUrl)
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
  return(ExecuteCypher(path=callIdByMsIdPath,msId=msId))
}

FxRateByCurrency <- function(currency){
  currencies <- c(currency,'nonexist')
  ExecuteCypher(path=fxRateByCurrencyPath,currencies=currencies)
}
