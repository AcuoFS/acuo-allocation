library('RUnit')

test.suite = defineTestSuite("example",
                             dirs = file.path("test/testFunctions"),
                             testFileRegexp = 'testCallLpSolve.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
