library('RUnit')
library('lpSolveAPI')
test.suite = defineTestSuite("example",
                             dirs = file.path("test/testCallLpSolve"),
                             testFileRegexp = 'R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
