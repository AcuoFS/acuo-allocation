library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/")

test.suite = defineTestSuite("example",
                             dirs = file.path("test/testIntegerRatioWithoutMovementConstraint"),
                             testFileRegexp = 'integerRatioWithoutMovementConstraintTest.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
