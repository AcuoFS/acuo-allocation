library('RNeo4j')

availAssetByCall <- function(callId,order){
  readLoad <- function(path) {
    query = paste(readLines(path), collapse="\n")
    return (query)
  }
  
  # graph = startGraph("http://neo4j:7474/db/data")
  graph = startGraph("http://localhost:7474/db/data/")
  
  file.url <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/Cypher/availAssetByCall.cql'
file.url <-   'availAssetByCall.cql'
  query <- readLoad(file.url)
  
  cypher(graph,query,callId=callId,order=order)
}

callId <- c('mc1','mc2','mc3')
order <- 'callId'
result <- availAssetByCall(callId,order)
write.csv(file='../Result/availAssetByCall.csv',x=result)

