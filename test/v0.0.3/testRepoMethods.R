library('RUnit')
setwd("E://ACUO/projects/acuo-allocation/test/v0.0.3")

test.suite = defineTestSuite("example",
                             dirs = file.path("opportunityCost"),
                             testFileRegexp = "repoMethods.R")

test.result <- runTestSuite(test.suite) 

source('opportunityCost/repoMethodsVisual.R')
RepoMethodsTestVisual("CashS1","cash")
  
printTextProtocol(test.result)
