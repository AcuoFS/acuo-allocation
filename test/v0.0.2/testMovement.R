library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.2")

test.suite = defineTestSuite("example",
                             dirs = file.path("testMovement"),
                             testFileRegexp = 'movementTests.R')

test.result <- runTestSuite(test.suite) # 4 tests failed 20180405

printTextProtocol(test.result)
