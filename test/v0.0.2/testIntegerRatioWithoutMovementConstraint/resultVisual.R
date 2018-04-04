# visualize the algo test results

library("XLConnect")

setwd("E://ACUO/projects/acuo-allocation/")
filePath <- "test/testIntegerRatioWithoutMovementConstraint/integerRatioWithoutMovementConstraintPerformance.xlsx"
pref1 <- c(10,0)
pref3 <- c(5,5)

# all columns except the first one are numeric
worksheet <- readWorksheetFromFile(filePath,sheet="Results")


# compare the run time and objectives between two integer ratio 
# use the average of the two data sets
pref1.index <- which(pref1[1]==worksheet$costObj & pref1[2] ==worksheet$liquidityObj)
pref3.index <- which(pref3[1]==worksheet$costObj & pref3[2] ==worksheet$liquidityObj)

pref1.df <- worksheet[pref1.index,]
pref3.df <- worksheet[pref3.index,]

pref1.result <- aggregate(.~integerRatio,data=pref1.df[,-1],mean)
pref3.result <- aggregate(.~integerRatio,data=pref3.df[,-1],mean)

