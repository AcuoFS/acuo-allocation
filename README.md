# acuo-allocation

## Getting started

In order to run the allocation algorithm, please clone the repo to your local machine or 
directly go to Rstudio server: https://rstudio.acuo.com/

## Run the Allocation Algo

The algorithm is at testing stage. To see some sample allocation result, go to the folder 'examples', run '*AllocationAlgoExamples.R'.

## Unit Test

To do unit testing, please go to the folder 'test', run 'testAll.R'


## Layers(From inner to outer)

1. Cypher query, resquest the database, return the data.
  1. Location:
    1. src/Cypher/availAssetByCallIdAndClient.cql 
    2. src/Cypher/assetInfoByAssetId.cql 
    3. src/Cypher/callInfoByCallId.cql 

2. R file, contains several R functions, call the Cypher query(1), generate raw data, return raw data.
  1. Location:
    1. src/functionsOfDBRequestByExecutingCypher.R 
  2. Functions:
    1. executeCypher(path,...)
    2. callInfoByCallId(callId)
    3. assetInfoByAssetId(assetId)
    4. eliAssetByClientId(clientId)
    5. availAssetByCallIdAndClientId(callId,clientId)

3. R file, contains allocation function, call the R function(2), convert raw data to certain format, return formalized data.
  1. Location:
    1. src/allocationInputData.R
  2. Functions:
    1. allocationInputData(callId,clientId)

4. R file, call the R function(3), run the allocation algorithm, return allocation result.
  1. Location:
    1. src/allocationFunction.R
  2. Functions:
    1. allocationAlgo(callId,clientId)

5. R file, contain several allocation examples functions, call the R function(4), allocation examples given sample input parameters, return allocation result.
  1. Location:
    1. examples/costOnlyNoConstraintAllocationAlgoExamples.R 
    2. examples/costOnlyQuantityLimitAllocationAlgoExamples.R
    3. examples/liquidityOnlyNoConstraintAllocationAlgoExamples.R 
    4. examples/liquidityOnlyQuantityLimitAllocationAlgoExamples.R
    5. examples/operationOnlyNoConstraintAllocationAlgoExamples.R 
  2. Functions:
    1. costOnlyNoConstraintAllocationAlgoEx*()
    2. costOnlyQuantityLimitAllocationAlgoEx*()
    3. liquidityOnlyNoConstraintAllocationAlgoEx*()
    4. liquidityOnlyQuantityLimitAllocationAlgoEx*()
    5. operationOnlyNoConstraintAllocationAlgoEx*()

6. R file, test all examples by processing each step.
  1. Location:
    1. test/testAll.R 
  2. Test Functions Location:
    1. test/testFunctions/testAllocationAlgo.R
    2. test/testFunctions/testDBRequestFunction.R

