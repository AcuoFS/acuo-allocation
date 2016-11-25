# acuo-allocation

## Getting started

In order to run the allocation algorithm, please clone the repo to your local machine or 
directly go to Rstudio server: https://rstudio.acuo.com/

## Run the Allocation Algo

The algorithm is at testing stage. To see some sample allocation result, go to the folder 'examples', run '*AllocationAlgoExamples.R'.

## Unit Test

To do unit testing, please go to the folder 'test', run 'testAll.R'


## Layers
From inner to outer
###1. Cypher query, resquest the database, return the data.
* src/Cypher/availAssetByCallIdAndClient.cql 
* src/Cypher/assetInfoByAssetId.cql 
* src/Cypher/callInfoByCallId.cql 

###2. R file, contains several R functions, call the Cypher query(1), generate raw data, return raw data.
* src/functionsOfDBRequestByExecutingCypher.R 

###3. R file, contains allocation function, call the R function(2), convert raw data to certain format, return formalized data.
* src/allocationInputData.R 

###4. R function(file), call the R function(3), run the allocation algorithm, return allocation result.
* src/allocationFunction.R 

###5. R file, contain several allocation examples functions, call the R function(4), allocation exaples given sample input parameters, return allocation result.
* examples/costOnlyNoConstraintAllocationAlgoExamples.R 


###6. R file, test all examples by processing each step.
* test/testAll.R 


