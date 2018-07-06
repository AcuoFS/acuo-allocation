library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.3")

test.suite = defineTestSuite("example",
                             dirs = file.path("opportunityCost"),
                             testFileRegexp = "externalScenario1CashTests.R")

test.result <- runTestSuite(test.suite) 

ExternalCostTestVisual("CashS1","cash")
  
printTextProtocol(test.result)
