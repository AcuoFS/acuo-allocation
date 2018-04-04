library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.2")

test.suite1 = defineTestSuite("example",
                             dirs = file.path("testAssetAmount"),
                             testFileRegexp = 'assetAmountTests.R')
test.suite2 = defineTestSuite("example",
                             dirs = file.path("testAssetAmount"),
                             testFileRegexp = 'assetAmountTests2.R')

test.result1 <- runTestSuite(test.suite1)
test.result2 <- runTestSuite(test.suite2)


printTextProtocol(test.result1)
printTextProtocol(test.result2)
