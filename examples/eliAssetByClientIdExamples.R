library('RNeo4j')
source('src/functionsOfDBRequestByExecutingCypher.R')

#### CONSTANTS, PLASE DO NOT CHANGE #####
clientId1 <- 'c1'
clientId2 <- 'c2'

#### EXAMPLE FUNCTIONS ##################
eleAssetByClientIdEx1<-function(){
  eliAssetByClientId(clientId1)
}
eleAssetByClientIdEx2<-function(){
  eliAssetByClientId(clientId2)
}

#### EXAMPLES RESULTS ####################
eleAssetByClientIdEx1()
eleAssetByClientIdEx2()