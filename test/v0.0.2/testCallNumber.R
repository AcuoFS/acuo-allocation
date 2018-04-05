library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.2")

test.suite = defineTestSuite("example",
                             dirs = file.path("testCallNumber"),
                             testFileRegexp = 'callNumberTests.R')

test.result <- runTestSuite(test.suite) # two tests failed 20180404

printTextProtocol(test.result)
