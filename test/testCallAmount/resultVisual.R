# visualize the algo test results

library("XLConnect")

filePath <- "test/testCallAmount/callAmountPerformance.xlsx"

worksheet <- readWorksheetFromFile(filePath,sheet="Results")

callAmount <- as.numeric(worksheet$CallAmount)
runTime <- as.numeric(worksheet$RunTime)

plot(x=callAmount,y=runTime,type='l',
     xlab='Call Amount ($)',ylab='Algo Run Time (sec)')
