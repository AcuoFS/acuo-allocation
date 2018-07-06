library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.3")

test.suite = defineTestSuite("example",
                             dirs = file.path("opportunityCost"),
                             testFileRegexp = "externalScenario1CashSuffTests.R")

test.result <- runTestSuite(test.suite) 

ExternalCostTestVisual("CashS1Suff","cash")

printTextProtocol(test.result)
