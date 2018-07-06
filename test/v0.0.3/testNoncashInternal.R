library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.3")

test.suite = defineTestSuite("example",
                             dirs = file.path("testNoncashInternal"),
                             testFileRegexp = 'noncashInternalTests')

test.result <- runTestSuite(test.suite) 


printTextProtocol(test.result)
