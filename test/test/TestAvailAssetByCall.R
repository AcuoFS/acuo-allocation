library('RUnit')

test.OW169_1 <- function(){
  source('examples/OW169_1.R')
  assetId <- result[callId=='mc1']$assetId
  custAccount<- result$CustodianAccount
  
  # In order to compare the vectors, a new function(setequal) is brought in.
  # Two vectors with same elements(exclude dulplicate elements) will return TRUE.
  
  checkTrue(is.element('USD',assetId))
  checkTrue(setequal(result$CustodianAccount[ (assetId=='USD')],c('custac5')))
  checkEquals(result$opptCost[ (assetId=='USD'& custAccount=='custac5')],0.04)
  checkEquals(result$value[ (assetId=='USD'& custAccount=='custac5')],1)
  checkEquals(result$quantity[ (assetId=='USD'& custAccount=='custac5')],20000000)
  checkEquals(result$internalCost[ (assetId=='USD'& custAccount=='custac5')],0.03)
  checkEquals(result$externalCost[ (assetId=='USD'& custAccount=='custac5')],0.05)
  checkEquals(result$haircut[ (assetId=='USD'& custAccount=='custac5')],0)
  checkEquals(result$FXHaircut[ (assetId=='USD'& custAccount=='custac5')],0)
  checkEquals(result$interestRate[ (assetId=='USD'& custAccount=='custac5')],0.05)
  checkEquals(result$FXRate[ (assetId=='USD'& custAccount=='custac5')],1)
              
  
  checkTrue(is.element('GBP',assetId))
  checkTrue(setequal(result$CustodianAccount[ (assetId=='GBP')],c('custac1', 'custac2')))
  checkEquals(result$value[ (assetId=='GBP'& custAccount=='custac1')],1)
  checkEquals(result$quantity[ (assetId=='GBP'& custAccount=='custac1')],50000)
  checkEquals(result$quantity[ (assetId=='GBP'& custAccount=='custac2')],50000)
  checkEquals(result$internalCost[ (assetId=='GBP'& custAccount=='custac1')],0.01)
  checkEquals(result$externalCost[ (assetId=='GBP'& custAccount=='custac2')],0.01)
  checkEquals(result$haircut[ (assetId=='GBP'& custAccount=='custac1')],0)
  checkEquals(result$FXHaircut[ (assetId=='GBP'& custAccount=='custac2')],0.05)
  checkEquals(result$interestRate[ (assetId=='GBP'& custAccount=='custac1')],0.02)
  checkEquals(result$FXRate[ (assetId=='GBP'& custAccount=='custac2')],0.8)
  
  checkTrue(is.element('JPY',assetId))
  checkTrue(setequal(result$CustodianAccount[ (assetId=='JPY')],c('custac5')))
  checkEquals(result$value[ (assetId=='JPY'& custAccount=='custac5')],1)
  checkEquals(result$quantity[ (assetId=='JPY'& custAccount=='custac5')],5000000)
  checkEquals(result$internalCost[ (assetId=='JPY'& custAccount=='custac5')],0.01)
  checkEquals(result$opptCost[ (assetId=='JPY'& custAccount=='custac5')],0.02)
  checkEquals(result$externalCost[ (assetId=='JPY'& custAccount=='custac5')],0.04)
  checkEquals(result$haircut[ (assetId=='JPY'& custAccount=='custac5')],0)
  checkEquals(result$FXHaircut[ (assetId=='JPY'& custAccount=='custac5')],0.05)
  checkEquals(result$interestRate[ (assetId=='JPY'& custAccount=='custac5')],0.01)
  checkEquals(result$FXRate[ (assetId=='JPY'& custAccount=='custac5')],103)
  
  
  checkTrue(is.element('CAD',assetId))
  checkTrue(setequal(result$CustodianAccount[ (assetId=='CAD')],c('custac6')))
  checkEquals(result$value[ (assetId=='CAD'& custAccount=='custac6')],1)
  checkEquals(result$quantity[ (assetId=='CAD'& custAccount=='custac6')],40000000)
  checkEquals(result$internalCost[ (assetId=='CAD'& custAccount=='custac6')],0.01)
  checkEquals(result$opptCost[ (assetId=='CAD'& custAccount=='custac6')],0.01)
  checkEquals(result$externalCost[ (assetId=='CAD'& custAccount=='custac6')],0.05)
  checkEquals(result$haircut[ (assetId=='CAD'& custAccount=='custac6')],0)
  checkEquals(result$FXHaircut[ (assetId=='CAD'& custAccount=='custac6')],0.05)
  checkEquals(result$interestRate[ (assetId=='CAD'& custAccount=='custac6')],0.03)
  checkEquals(result$FXRate[ (assetId=='CAD'& custAccount=='custac6')],1.34)
  
  checkTrue(is.element('46625H100',assetId))
  checkTrue(setequal(result$CustodianAccount[ (assetId=='46625H100')],c('custac4')))
  checkEquals(result$value[ (assetId=='46625H100'& custAccount=='custac4')],65)
  checkEquals(result$quantity[ (assetId=='46625H100'& custAccount=='custac4')],200000)
  checkEquals(result$internalCost[ (assetId=='46625H100'& custAccount=='custac4')],0.02)
  checkEquals(result$opptCost[ (assetId=='46625H100'& custAccount=='custac4')],0.02)
  checkEquals(result$externalCost[ (assetId=='46625H100'& custAccount=='custac4')],0.03)
  checkEquals(result$haircut[ (assetId=='46625H100'& custAccount=='custac4')],0.3)
  checkEquals(result$FXHaircut[ (assetId=='46625H100'& custAccount=='custac4')],0)
  checkEquals(result$interestRate[ (assetId=='46625H100'& custAccount=='custac4')],0)
  checkEquals(result$FXRate[ (assetId=='46625H100'& custAccount=='custac4')],1)
  
  checkTrue(is.element('37833100',assetId))
  checkTrue(setequal(result$CustodianAccount[ (assetId=='37833100')],c('custac3')))
  checkEquals(result$value[ (assetId=='37833100'& custAccount=='custac3')],117)
  checkEquals(result$quantity[ (assetId=='37833100'& custAccount=='custac3')],500000)
  checkEquals(result$internalCost[ (assetId=='37833100'& custAccount=='custac3')],0.02)
  checkEquals(result$opptCost[ (assetId=='37833100'& custAccount=='custac3')],0.02)
  checkEquals(result$externalCost[ (assetId=='37833100'& custAccount=='custac3')],0.03)
  checkEquals(result$haircut[ (assetId=='37833100'& custAccount=='custac3')],0.3)
  checkEquals(result$FXHaircut[ (assetId=='37833100'& custAccount=='custac3')],0)
  checkEquals(result$interestRate[ (assetId=='37833100'& custAccount=='custac3')],0)
  checkEquals(result$FXRate[ (assetId=='37833100'& custAccount=='custac3')],1)
  
  checkTrue(is.element('US912796HW25',assetId))
  checkTrue(setequal(result$CustodianAccount[ (assetId=='US912796HW25')],c('custac1')))
  checkEquals(result$value[ (assetId=='US912796HW25'& custAccount=='custac1')],100)
  checkEquals(result$quantity[ (assetId=='US912796HW25'& custAccount=='custac1')],500000)
  checkEquals(result$internalCost[ (assetId=='US912796HW25'& custAccount=='custac1')],0.03)
  checkEquals(result$opptCost[ (assetId=='US912796HW25'& custAccount=='custac1')],0.01)
  checkEquals(result$externalCost[ (assetId=='US912796HW25'& custAccount=='custac1')],0.01)
  checkEquals(result$haircut[ (assetId=='US912796HW25'& custAccount=='custac1')],0.005)
  checkEquals(result$FXHaircut[ (assetId=='US912796HW25'& custAccount=='custac1')],0)
  checkEquals(result$interestRate[ (assetId=='US912796HW25'& custAccount=='custac1')],0)
  checkEquals(result$FXRate[ (assetId=='US912796HW25'& custAccount=='custac1')],1)
}


test.OW169_2 <- function(){
  source('examples/OW169_2.R')
  
  callId <- result$callId
  assetId <- result$assetId
  custAccount<- result$CustodianAccount
  
  # check whether all available assets and custodian accounts for each margin call are returned
  checkTrue(setequal(callId,c('mc2','mc3')))
  checkTrue(setequal(result$assetId[which(callId=='mc2')],c('USD','GBP','JPY','CAD')))
  checkTrue(setequal(result$assetId[callId=='mc3'],c('USD','GBP','JPY','CAD')))
  checkTrue(setequal(result$CustodianAccount[callId=='mc3'],c('custac1','custac2','custac5','custac6')))
  checkTrue(setequal(result$CustodianAccount[callId=='mc3'],c('custac1','custac2','custac5','custac6')))
  
  # check whether the same asset from the same custodian account 
  # and available for different margin calls has the same quantity and other properties
  checkTrue(setequal(result$quantity[assetId=='USD' & custAccount=='custac5'],20000000))
  checkTrue(setequal(result$internalCost[assetId=='USD' & custAccount=='custac5'],0.03))
  checkTrue(setequal(result$opptCost[assetId=='USD' & custAccount=='custac5'],0.04))
  checkTrue(setequal(result$currency[assetId=='USD' & custAccount=='custac5'],'USD'))
  
  checkTrue(setequal(result$quantity[assetId=='GBP' & custAccount=='custac1'],50000))
  checkTrue(setequal(result$internalCost[assetId=='GBP' & custAccount=='custac1'],0.01))
  checkTrue(setequal(result$opptCost[assetId=='GBP' & custAccount=='custac1'],0.05))
  checkTrue(setequal(result$currency[assetId=='GBP' & custAccount=='custac1'],'GBP'))
  checkTrue(setequal(result$quantity[assetId=='GBP' & custAccount=='custac2'],50000))
  checkTrue(setequal(result$internalCost[assetId=='GBP' & custAccount=='custac2'],0.01))
  checkTrue(setequal(result$opptCost[assetId=='GBP' & custAccount=='custac2'],0.05))
  checkTrue(setequal(result$currency[assetId=='GBP' & custAccount=='custac2'],'GBP'))
  
  checkTrue(setequal(result$quantity[assetId=='JPY' & custAccount=='custac5'],5000000))
  checkTrue(setequal(result$internalCost[assetId=='JPY' & custAccount=='custac5'],0.01))
  checkTrue(setequal(result$opptCost[assetId=='JPY' & custAccount=='custac5'],0.02))
  checkTrue(setequal(result$currency[assetId=='JPY' & custAccount=='custac5'],'JPY'))
  
  checkTrue(setequal(result$quantity[assetId=='CAD' & custAccount=='custac6'],40000000))
  checkTrue(setequal(result$internalCost[assetId=='CAD' & custAccount=='custac6'],0.01))
  checkTrue(setequal(result$opptCost[assetId=='CAD' & custAccount=='custac6'],0.01))
  checkTrue(setequal(result$currency[assetId=='CAD' & custAccount=='custac6'],'CAD'))
  
  # check haircut, FXHaircut, externalCost, interestRate, FXRate
  # those value will rely on the agreement(different FCM and CTPY), thus 
  # even a same asset may have different value
  
  checkEquals(result$externalCost[assetId=='USD'& custAccount=='custac5'&callId=='mc2'],0)
  checkEquals(result$externalCost[assetId=='USD'& custAccount=='custac5'&callId=='mc3'],0)
  
  checkEquals(result$haircut[assetId=='USD'& custAccount=='custac5'&callId=='mc2'],0)
  checkEquals(result$FXHaircut[assetId=='USD'& custAccount=='custac5'&callId=='mc3'],0)
  checkEquals(result$interestRate[assetId=='USD'& custAccount=='custac5'&callId=='mc2'],0.05)
  checkEquals(result$FXRate[assetId=='USD'& custAccount=='custac5'&callId=='mc3'],1)
  
  
  checkEquals(result$externalCost[assetId=='GBP'& custAccount=='custac2'&callId=='mc2'],0)
  checkEquals(result$haircut[assetId=='GBP'& custAccount=='custac1'& callId=='mc3'],0)
  checkEquals(result$FXHaircut[assetId=='GBP'& custAccount=='custac2'& callId=='mc2'],0)
  checkEquals(result$interestRate[assetId=='GBP'& custAccount=='custac1'& callId=='mc3'],0.03)
  checkEquals(result$FXRate[assetId=='GBP'& custAccount=='custac2'& callId=='mc2'],0.8)
  
  checkEquals(result$externalCost[assetId=='JPY'& custAccount=='custac5'& callId=='mc2'],0)
  checkEquals(result$haircut[assetId=='JPY'& custAccount=='custac5'&callId=='mc3'],0)
  checkEquals(result$FXHaircut[assetId=='JPY'& custAccount=='custac5'&callId=='mc2'],0)
  checkEquals(result$interestRate[assetId=='JPY'& custAccount=='custac5'&callId=='mc2'],0.01)
  checkEquals(result$FXRate[assetId=='JPY'& custAccount=='custac5'&callId=='mc3'],103)
  
  
  checkEquals(result$externalCost[assetId=='CAD'& custAccount=='custac6'&callId=='mc2'],0.05)
  checkEquals(result$haircut[assetId=='CAD'& custAccount=='custac6'& callId=='mc2'],0)
  checkEquals(result$FXHaircut[assetId=='CAD'& custAccount=='custac6'& callId=='mc3'],0.08)
  checkEquals(result$interestRate[assetId=='CAD'& custAccount=='custac6'& callId=='mc3'],0.03)
  checkEquals(result$FXRate[assetId=='CAD'& custAccount=='custac6'&callId=='mc2'],1.34)
}


test.OW169_3 <- function(){
  source('examples/OW169_3.R')
  
  callId <- result$callId
  assetId <- result$assetId
  custAccount<- result$CustodianAccount
  
  # check whether all available assets and custodian accounts for each margin call are returned
  checkTrue(setequal(callId,c('mc1','mc2','mc4','mc5','mc6','mc8','mc9','mc10','mc11')))
  checkTrue(setequal(result$assetId[callId=='mc2'],c('USD','GBP','JPY','CAD')))
  checkTrue(setequal(result$assetId[callId=='mc4'],c('USD','GBP','JPY')))
  checkTrue(setequal(result$assetId[callId=='mc5'],c('37833100','46625H100')))
  checkTrue(setequal(result$assetId[callId=='mc6'],c('USD','GBP','JPY','CAD')))
  checkTrue(setequal(toupper(result$assetId[callId=='mc8']),c('USD')))
  checkTrue(setequal(result$assetId[callId=='mc9'],c('USD')))
  checkTrue(setequal(result$assetId[callId=='mc10'],c('USD')))
  checkTrue(setequal(result$assetId[callId=='mc11'],c('USD')))
  
  
  checkTrue(setequal(result$CustodianAccount[callId=='mc4'],c('custac2','custac5','custac1')))
  checkTrue(setequal(result$CustodianAccount[callId=='mc5'],c('custac3','custac4')))
  checkTrue(setequal(result$CustodianAccount[callId=='mc8'],c('custac5')))

  # check whether the same asset from the same custodian account 
  # and available for different margin calls has the same quantity and other properties
  checkTrue(setequal(result$quantity[assetId=='USD' & custAccount=='custac5'],20000000))
  checkTrue(setequal(result$internalCost[assetId=='USD' & custAccount=='custac5'],0.03))
  checkTrue(setequal(result$opptCost[assetId=='USD' & custAccount=='custac5'],0.04))
  checkTrue(setequal(result$currency[assetId=='USD' & custAccount=='custac5'],'USD'))
  
  checkTrue(setequal(result$quantity[assetId=='GBP' & custAccount=='custac1'],50000))
  checkTrue(setequal(result$internalCost[assetId=='GBP' & custAccount=='custac1'],0.01))
  checkTrue(setequal(result$opptCost[assetId=='GBP' & custAccount=='custac1'],0.05))
  checkTrue(setequal(result$currency[assetId=='GBP' & custAccount=='custac1'],'GBP'))
  checkTrue(setequal(result$quantity[assetId=='GBP' & custAccount=='custac2'],50000))
  checkTrue(setequal(result$internalCost[assetId=='GBP' & custAccount=='custac2'],0.01))
  checkTrue(setequal(result$opptCost[assetId=='GBP' & custAccount=='custac2'],0.05))
  checkTrue(setequal(result$currency[assetId=='GBP' & custAccount=='custac2'],'GBP'))
  
  checkTrue(setequal(result$quantity[assetId=='JPY' & custAccount=='custac5'],5000000))
  checkTrue(setequal(result$internalCost[assetId=='JPY' & custAccount=='custac5'],0.01))
  checkTrue(setequal(result$opptCost[assetId=='JPY' & custAccount=='custac5'],0.02))
  checkTrue(setequal(result$currency[assetId=='JPY' & custAccount=='custac5'],'JPY'))
  
  checkTrue(setequal(result$quantity[assetId=='CAD' & custAccount=='custac6'],40000000))
  checkTrue(setequal(result$internalCost[assetId=='CAD' & custAccount=='custac6'],0.01))
  checkTrue(setequal(result$opptCost[assetId=='CAD' & custAccount=='custac6'],0.01))
  checkTrue(setequal(result$currency[assetId=='CAD' & custAccount=='custac6'],'CAD'))
  
  checkTrue(setequal(result$quantity[assetId=='46625H100' & custAccount=='custac4'],200000))
  checkTrue(setequal(result$internalCost[assetId=='46625H100' & custAccount=='custac4'],0.02))
  checkTrue(setequal(result$opptCost[assetId=='46625H100' & custAccount=='custac4'],0.02))
  checkTrue(setequal(result$currency[assetId=='46625H100' & custAccount=='custac4'],'USD'))
  
  checkTrue(setequal(result$quantity[assetId=='37833100' & custAccount=='custac3'],500000))
  checkTrue(setequal(result$internalCost[assetId=='37833100' & custAccount=='custac3'],0.02))
  checkTrue(setequal(result$opptCost[assetId=='37833100' & custAccount=='custac3'],0.02))
  checkTrue(setequal(result$currency[assetId=='37833100' & custAccount=='custac3'],'USD'))
  
  checkTrue(setequal(result$quantity[assetId=='US912796HW25' & custAccount=='custac1'],500000))
  checkTrue(setequal(result$internalCost[assetId=='US912796HW25' & custAccount=='custac1'],0.03))
  checkTrue(setequal(result$opptCost[assetId=='US912796HW25' & custAccount=='custac1'],0.01))
  checkTrue(setequal(result$currency[assetId=='US912796HW25' & custAccount=='custac1'],'USD'))
  
}


test.OW169_4 <- function(){
  source('src/availAssetByCall.R')

  checkEquals(availAssetByCall('c21','assetId'),NULL)
}


