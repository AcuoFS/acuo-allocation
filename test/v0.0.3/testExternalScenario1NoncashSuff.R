library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.3")

test.suite = defineTestSuite("example",
                             dirs = file.path("opportunityCost"),
                             testFileRegexp = "externalScenario1NoncashSuffTests.R")

test.result <- runTestSuite(test.suite) 

source("opportunityCost/externalCostVisual.R")
ExternalCostTestVisual("NoncashS1Suff","noncash")

printTextProtocol(test.result)
