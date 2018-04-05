library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.2")

test.suite = defineTestSuite("example",
                             dirs = file.path("testAssetNumber"),
                             testFileRegexp = 'assetNumberTests.R')

test.result <- runTestSuite(test.suite) # all passed 20180404

printTextProtocol(test.result)
