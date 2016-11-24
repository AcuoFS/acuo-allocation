# acuo-allocation

## Getting started

In order to run the allocation algorithm, please clone the repo to your local machine or 
directly go to Rstudio server: https://rstudio.acuo.com/

## Run the Allocation Algo

The algorithm is at testing stage. To see some sample allocation result, go to the folder 'examples', run "OW171_*.R".

## Unit Test

To do unit testing, please go to the folder 'test', run 'testAll.R'


## Layers
From inner to outer
###1. Cypher query, resquest the database, return the data.
* src/Cypher/availAssetByCall.cql 
* src/Cypher/assetInfoById.cql 
* src/Cypher/callInfoById.cql 

###2. R function(file), call the Cypher query(1), generate raw data, return raw data.
* src/availAssetByCall.R 
* src/assetInfoById.R 
* src/callInfoById.R 

###3. R function(file), call the R function(2), convert raw data to certain format, return formalized data.
* src/modelInput.R 

###4. R function(file), call the R function(3), run the allocation algorithm, return allocation result.
* src/opti.R 

###5. R function(file), call the R function(4), allocation exaples given sample input parameters, return allocation result.
* examples/OW171_1.R 
* examples/OW171_1.R 

###6. R function(file), test all examples by processing each step.
* test/testAll.R 


