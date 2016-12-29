library('RNeo4j')
source('src/functionsOfDBRequestByExecutingCypher.R')

#### CONSTANTS, PLASE DO NOT CHANGE #####
clientId1 <- '999'

#### EXAMPLE FUNCTIONS ##################
eliAssetByClientIdEx1<-function(){
  eliAssetByClientId(clientId1)
}

#### EXAMPLES RESULTS ####################
eliAssetByClientIdEx1()
