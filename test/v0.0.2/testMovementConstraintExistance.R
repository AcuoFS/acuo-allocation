library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.2")

test.suite = defineTestSuite("example",
                             dirs = file.path("testMovementConstraintExistence/"),
                             testFileRegexp = 'movementConstraintExistenceTests.R')

test.result <- runTestSuite(test.suite) # all passed 20180405

printTextProtocol(test.result)
