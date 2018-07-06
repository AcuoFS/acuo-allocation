library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.3")

test.suite = defineTestSuite("example",
                             dirs = file.path("opportunityCost"),
                             testFileRegexp = "externalScenario1CptyTests.R")

test.result <- runTestSuite(test.suite) 

ExternalCostTestVisual("CptyS1","cash")
ExternalCostTestVisual("CptyS1","noncash")

printTextProtocol(test.result)
