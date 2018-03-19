# visualize the algo test results

library("XLConnect")

filePath <- "test/testCallNumber/callNumberPerformance.xlsx"

worksheet <- readWorksheetFromFile(filePath,sheet="Results")

callNumber <- as.numeric(worksheet$callNumber)
runTime <- as.numeric(worksheet$runTime)

plot(x=callNumber,y=runTime,type='p',
     xlab='Call Number ',ylab='Algo Run Time (sec)')
