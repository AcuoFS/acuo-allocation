library('RNeo4j')
source('src/functionsOfDBRequestByExecutingCypher.R')

#### CONSTANTS, PLASE DO NOT CHANGE #####
clientId1 <- '999'

#### EXAMPLE FUNCTIONS ##################
resAssetByClientIdEx1<-function(){
  resAssetByClientId(clientId1)
}

#### EXAMPLES RESULTS ####################
resAssetByClientIdEx1()
