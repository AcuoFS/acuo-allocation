library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.2")
test.suite = defineTestSuite("example",
                             dirs = file.path("testCallAmount"),
                             testFileRegexp = 'callAmountTests.R')

test.result <- runTestSuite(test.suite) # all passed 20180404

printTextProtocol(test.result)
