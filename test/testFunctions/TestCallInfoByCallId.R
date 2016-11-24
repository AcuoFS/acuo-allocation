library('RUnit')

testFunctionCallInfoByCallId <- function(){ 
  
  # test input: a list of margin call ids
  callId <- c("mc1","mc2","mc3","mc4","mc5","mc6","mc7","mc8","mc9","mc10","mc11")
  
  # test function: callInfoByCallId(callId)
  source('src/callInfoByCallId.R')
  result <- callInfoByCallId(callId) # function output

  # check whether all info from the output is correct
  checkTrue(is.element('mc1',callId))
  checkEquals(result$callAmount[callId=='mc1'],10000)
  
  checkTrue(is.element('mc2',callId))
  checkEquals(result$callAmount[callId=='mc2'],15000)
  
  checkTrue(is.element('mc3',callId))
  checkEquals(result$callAmount[callId=='mc3'],20000)
  
  checkTrue(is.element('mc4',callId))
  checkEquals(result$callAmount[callId=='mc4'],250)
  
  checkTrue(is.element('mc5',callId))
  checkEquals(result$callAmount[callId=='mc5'],30000)
  
  checkTrue(is.element('mc6',callId))
  checkEquals(result$callAmount[callId=='mc6'],15000)
  
  checkTrue(is.element('mc7',callId))
  checkEquals(result$callAmount[callId=='mc7'],5000)
  
  checkTrue(is.element('mc8',callId))
  checkEquals(result$callAmount[callId=='mc8'],10000)
  
  checkTrue(is.element('mc9',callId))
  checkEquals(result$callAmount[callId=='mc9'],10000)
  
  checkTrue(is.element('mc10',callId))
  checkEquals(result$callAmount[callId=='mc10'],10000)
  
  checkTrue(is.element('mc11',callId))
  checkEquals(result$callAmount[callId=='mc11'],10000)
  
}