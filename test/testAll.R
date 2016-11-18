library('RUnit')

test.suite = defineTestSuite("example",
                             dirs = file.path("test/test"),
                             testFileRegexp = 'R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
