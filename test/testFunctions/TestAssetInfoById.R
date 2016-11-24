library('RUnit')

test.OW200_1 <- function(){
  source('examples/OW200_1.R')
  assetId <- result$id
  
  checkTrue(is.element('USD',assetId))
  checkEquals(result$name[assetId=='USD'],'US dollar')
  checkEquals(result$value[assetId=='USD'],1)
  checkEquals(result$isdaType[assetId=='USD'],'Cash')
  checkEquals(result$currency[assetId=='USD'],'USD')
  checkEquals(result$type[assetId=='USD'],'Cash')
  
  checkTrue(is.element('GBP',assetId))
  checkEquals(result$name[assetId=='GBP'],'British Pound')
  checkEquals(result$value[assetId=='GBP'],1)
  checkEquals(result$isdaType[assetId=='GBP'],'Cash')
  checkEquals(result$currency[assetId=='GBP'],'GBP')
  checkEquals(result$type[assetId=='GBP'],'Cash')
  
  checkTrue(is.element('JPY',assetId))
  checkEquals(result$name[assetId=='JPY'],'Japanese yen')
  checkEquals(result$value[assetId=='JPY'],1)
  checkEquals(result$isdaType[assetId=='JPY'],'Cash')
  checkEquals(result$currency[assetId=='JPY'],'JPY')
  checkEquals(result$type[assetId=='JPY'],'Cash')
  
  checkTrue(is.element('CAD',assetId))
  checkEquals(result$name[assetId=='CAD'],'Canadian dollar')
  checkEquals(result$value[assetId=='CAD'],1)
  checkEquals(result$isdaType[assetId=='CAD'],'Cash')
  checkEquals(result$currency[assetId=='CAD'],'CAD')
  checkEquals(result$type[assetId=='CAD'],'Cash')
  
  checkTrue(is.element('37833100',assetId))
  checkEquals(result$name[assetId=='37833100'],'APPL INC')
  checkEquals(result$value[assetId=='37833100'],117)
  checkEquals(result$isdaType[assetId=='37833100'],'Corporate Equity')
  checkEquals(result$currency[assetId=='37833100'],'USD')
  checkEquals(result$type[assetId=='37833100'],'Equity')
  
  checkTrue(is.element('46625H100',assetId))
  checkEquals(result$name[assetId=='46625H100'],'JPM US Equity')
  checkEquals(result$value[assetId=='46625H100'],65)
  checkEquals(result$isdaType[assetId=='46625H100'],'Corporate Equity')
  checkEquals(result$currency[assetId=='46625H100'],'USD')
  checkEquals(result$type[assetId=='46625H100'],'Equity')
  
  checkTrue(is.element('US912796HW25',assetId))
  checkEquals(result$name[assetId=='US912796HW25'],'U.S. Treasury Bills')
  checkEquals(result$value[assetId=='US912796HW25'],100)
  checkEquals(result$isdaType[assetId=='US912796HW25'],'MM Instruments')
  checkEquals(result$currency[assetId=='US912796HW25'],'USD')
  checkEquals(result$type[assetId=='US912796HW25'],'Tbill')
  
  checkTrue(is.element('Gold',assetId))
  checkEquals(result$name[assetId=='Gold'],'Gold')
  checkEquals(result$value[assetId=='Gold'],10)
  checkEquals(result$isdaType[assetId=='Gold'],'Gold')
  checkEquals(result$currency[assetId=='Gold'],'GLD')
  checkEquals(result$type[assetId=='Gold'],'Gold')
}