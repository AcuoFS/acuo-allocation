library('RUnit')

test.OW201_1 <- function(){
  source('examples/OW201_1.R')
  callId <- result$id
  
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