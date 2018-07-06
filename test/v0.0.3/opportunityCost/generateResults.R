# generate results
library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.3/")

source("opportunityCost/externalScenario1CashTests.R")
testExternalCashScenario1Data1Pref1()
testExternalCashScenario1Data1Pref2()
testExternalCashScenario1Data1Pref3()
testExternalCashScenario1Data1Pref4()
testExternalCashScenario1Data1Pref5()

source("externalScenario1CptySuffTests.R")
testExternalCashSuffScenario1Data1Pref1()
testExternalCashSuffScenario1Data1Pref2()
testExternalCashSuffScenario1Data1Pref3()
testExternalCashSuffScenario1Data1Pref4()
testExternalCashSuffScenario1Data1Pref5()

source("externalScenario1NoncashTests.R")
testExternalNoncashScenario1Data1Pref1()
testExternalNoncashScenario1Data1Pref2()
testExternalNoncashScenario1Data1Pref3()
testExternalNoncashScenario1Data1Pref4()
testExternalNoncashScenario1Data1Pref5()

source("externalScenario1NoncashSuffTests.R")
testExternalNoncashSuffScenario1Data1Pref1()
testExternalNoncashSuffScenario1Data1Pref2()
testExternalNoncashSuffScenario1Data1Pref3()
testExternalNoncashSuffScenario1Data1Pref4()
testExternalNoncashSuffScenario1Data1Pref5()






