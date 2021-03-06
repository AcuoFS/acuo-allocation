library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.3")

test.suite = defineTestSuite("example",
                             dirs = file.path("opportunityCost"),
                             testFileRegexp = "externalScenario1NoncashTests.R")

test.result <- runTestSuite(test.suite) 

source("opportunityCost/externalCostVisual.R")
ExternalCostTestVisual("NoncashS1","noncash")

printTextProtocol(test.result)
