library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.2")

test.suite = defineTestSuite("example",
                             dirs = file.path("testIntegerRatio"),
                             testFileRegexp = 'integerRatioTests')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
