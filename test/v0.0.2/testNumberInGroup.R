library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.2")
test.suite1 = defineTestSuite("example",
                              dirs = file.path("testNumberInGroup"),
                              testFileRegexp = 'numberInGroupTests.R')
test.suite2 = defineTestSuite("example",
                             dirs = file.path("testNumberInGroup"),
                             testFileRegexp = 'numberInGroupTests2.R')
test.suite3 = defineTestSuite("example",
                              dirs = file.path("testNumberInGroup"),
                              testFileRegexp = 'numberInGroupTests3.R')

test.result1 <- runTestSuite(test.suite1) # all passed 20180405
test.result2 <- runTestSuite(test.suite2) # all passed 20180405
test.result3 <- runTestSuite(test.suite3) # all passed 20180405

printTextProtocol(test.result1)
printTextProtocol(test.result2)
printTextProtocol(test.result3)
