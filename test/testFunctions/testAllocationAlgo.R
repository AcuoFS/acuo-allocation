library('RUnit')
source('src/allocationFunction.R')

#### CONSTANTS INPUT FOR TESTING, PLEASE DO NOT CHANGE ############
emptyList <- c()

nonexistentClientId1 <- 'c001'
nonexistentClientId2 <- 'adhknj'
existentClientId1 <- 'c1'
existentClientId2 <- 'c2'

nonexistentCallIdGroup1 <- c('asdd','null','adhajk')
nonexistentCallIdGroup2 <- c('mc001d','mc01010')
existentCallIdToClient1Group1 <- c("mc1","mc2")
existentCallIdToClient1Group2 <- c('mc1','mc2','mc3','mc5')
existentCallIdToClient1Group3 <- c("mc1","mc2","mc3","mc4","mc5","mc6","mc7","mc8","mc9","mc10","mc11")

prefForCostOnlyNoConstraintAllocationAlgo <- c(0,0,1,0)


###### TEST FUNCTIONS ##############################################
testCostOnlyNoConstraintAllocationAlgoByPassingAExistentClientIdAndAListOfExistentCallIdsWhichDirectToTheClient<-function(){
  # test input: a existent client id; a list of existent margin call ids, which direct to the client
  callId <- existentCallIdToClient1Group2
  clientId <- existentClientId1
  pref <- prefForCostOnlyNoConstraintAllocationAlgo
  
  # test function: allocationAlgo(callId,clientId,pref)
  result<- allocationAlgo(callId=callId,clientId=clientId,pref=pref)
  
  # test output:
  # check whether each margin call has been fulfilled with the correct asset and amount
  checkEquals(result$output[[existentCallIdToClient1Group2[1]]]$Asset,'CAD')
  checkEquals(result$output[[existentCallIdToClient1Group2[1]]]$Name,'Canadian dollar')
  checkEquals(result$output[[existentCallIdToClient1Group2[1]]]$NetAmount,10000)
  checkEquals(round(result$output[[existentCallIdToClient1Group2[1]]]$Amount,0),10526)
  
  checkEquals(result$output[[existentCallIdToClient1Group2[2]]]$Asset,'JPY')
  checkEquals(result$output[[existentCallIdToClient1Group2[2]]]$Name,'Japanese yen')
  checkEquals(result$output[[existentCallIdToClient1Group2[2]]]$NetAmount,15000)
  checkEquals(round(result$output[[existentCallIdToClient1Group2[2]]]$Amount,0),15000)
  
  checkEquals(result$output[[existentCallIdToClient1Group2[3]]]$Asset,'JPY')
  checkEquals(result$output[[existentCallIdToClient1Group2[3]]]$Name,'Japanese yen')
  checkEquals(result$output[[existentCallIdToClient1Group2[3]]]$NetAmount,20000)
  checkEquals(round(result$output[[existentCallIdToClient1Group2[3]]]$Amount,0),20000)
  
  checkEquals(result$output[[existentCallIdToClient1Group2[4]]]$Asset,'37833100')
  checkEquals(result$output[[existentCallIdToClient1Group2[4]]]$Name,'APPL INC')
  checkEquals(result$output[[existentCallIdToClient1Group2[4]]]$NetAmount,30000)
  checkEquals(round(result$output[[existentCallIdToClient1Group2[4]]]$Amount,0),42857)
  
}


