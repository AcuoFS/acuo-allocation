library("RNeo4j")

OW171.ex2 <- function(){
  source('src/opti.R')
  
  callId <- c('mc1','mc2','mc3','mc4','mc5','mc6','mc7','mc8')
  
  ############## Modify the quantity in DB to simulate the scenario OW-171 #################
  readLoad <- function(path) {
    query = paste(readLines(path), collapse="\n")
    return (query)
  }
  
  graph = startGraph("http://neo4j:7474/db/data")
  #graph = startGraph("http://localhost:7474/db/data/")
  
  file.url <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/OW171_custodianAsset.load'
  query <- readLoad(file.url)
  cypher(graph,query)
  
  ############### Run allocation algorithm ################################
  allocation.result<-allocation(callId)
  
  ############### Restore the DB to the status before modification ##################
  file.url <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/OW171_custodianAsset_restore.load'
  query <- readLoad(file.url)
  cypher(graph,query)
  
  return(allocation.result)
}

OW171.ex2()
