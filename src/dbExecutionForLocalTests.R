library('RNeo4j')

neo4jUrl <-"http://neo4j:7474/db/data"
neo4jDevUrl <- "http://dev.acuo.com:7474/db/data"
neo4jLocalUrl = "http://localhost:7474/db/data/"

marginCallsPath <- 'E:/ACUO/projects/acuo-allocation/src/ow-919/marginCalls.cql'
assetTransfersPath <- 'E:/ACUO/projects/acuo-allocation/src/ow-919/assetTransfers.cql'
settledCollateralsPath <- 'E:/ACUO/projects/acuo-allocation/src/ow-919/settledCollaterals.cql'
fxRatesPath <- 'E:/ACUO/projects/acuo-allocation/src/ow-919/fxRates.cql'
deleteDataPath <- 'E:/ACUO/projects/acuo-allocation/src/ow-919/deleteData.cql'
fetchSettledCollateralsPath <- 'E:/ACUO/projects/acuo-allocation/src/ow-919/fetchSettledCollaterals.cql'

readLoad <- function(path) {
  query = paste(readLines(path), collapse="\n")
  return (query)
}

executeCypher <- function(path,...){

  params <- list(...)
  query = paste(readLines(path), collapse="\n")
  #graph = startGraph(neo4jUrl)
  #graph = startGraph(neo4jDevUrl)
  graph = startGraph(neo4jLocalUrl,username='neo4j',password='neo4j')
  cypher(graph,query,params)
}

buildTestData <- function(){
  executeCypher(path=marginCallsPath)
  executeCypher(path=assetTransfersPath)
  executeCypher(path=settledCollateralsPath)
  executeCypher(path=fxRatesPath)
}

deleteTestData <- function(){
  executeCypher(path=deleteDataPath)
}

settledCollaterals <- function(){
  executeCypher(path=fetchSettledCollateralsPath)
}
