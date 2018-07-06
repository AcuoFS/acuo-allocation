library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.3")

test.suite = defineTestSuite("example",
                             dirs = file.path("opportunityCost"),
                             testFileRegexp = "externalScenario1CptySuffTests.R")

test.result <- runTestSuite(test.suite) 

ExternalCostTestVisual("CptyS1Suff","cash")
ExternalCostTestVisual("CptyS1Suff","noncash")

printTextProtocol(test.result)
