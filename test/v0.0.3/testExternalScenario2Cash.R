library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.3")

test.suite = defineTestSuite("example",
                             dirs = file.path("opportunityCost"),
                             testFileRegexp = "externalScenario2CashTests.R")

test.result <- runTestSuite(test.suite) 


printTextProtocol(test.result)
