library('RNeo4j')

eliAssetByClient <- function(clientId){
  readLoad <- function(path) {
    query = paste(readLines(path), collapse="\n")
    return (query)
  }
  
  # graph = startGraph("http://neo4j:7474/db/data")
  graph = startGraph("http://localhost:7474/db/data/")
  
  file.url <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/src/Cypher/eliAssetByClient.cql'
  
  query <- readLoad(file.url)
  
  cypher(graph,query,clientId=clientId)
}
