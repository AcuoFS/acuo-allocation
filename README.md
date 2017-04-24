# acuo-allocation

## Getting started

In order to run the allocation algorithm, please clone the repo to your local machine or 
directly go to Rstudio server: https://rstudio.acuo.com/

## Run the Allocation Algo

The algorithm is at testing stage. To see some sample allocation result, go to the folder 'examples', run 'callAlgoExamples.R'.

## Unit Test

To do unit testing, please go to the folder 'test', run 'testAll.R'(not available currently)


## Layers(From inner to outer)

- Cypher query, resquest the database, return the data.
1. Location:
    1. src/Cypher/availAssetByCallIdAndClient.cql 
    2. src/Cypher/assetInfoByAssetId.cql 
    3. src/Cypher/callInfoByCallId.cql 

- R file, contains several R functions, call the Cypher query(1), generate raw data, return raw data.
1. Location:
    1. src/functionsOfDBRequestByExecutingCypher.R 
2. Functions:
    1. executeCypher(path,...)
    2. callInfoByCallId(callId)
    3. assetInfoByAssetId(assetId)
    4. eliAssetByClientId(clientId)
    5. availAssetByCallIdAndClientId(callId,clientId)

- R files, contains allocation function, modelling, calling the solver, general functions.
1. Location:
    1. src/allocationFunction.R
    2. src/coreAlgo.R
    3. src/callLpSolve.R
    4. src/generalFunctions.R
    5. src/secondAllocationFunction.R
 2. Functions:
    1. src/functions.xlsx
