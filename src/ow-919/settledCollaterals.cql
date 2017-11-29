// 3. create settledCollateral
LOAD CSV WITH HEADERS FROM 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/ow-919/createSettledCollaterals.csv' 
as line with line
match (ms:MarginStatement {id:line.statementId})
merge (ms)<-[:BALANCE]-(collateral:Collateral)-[:VALUE]->(value:CollateralValue{amount:line.collateralValue})
set collateral.marginType = line.marginType,
	collateral.assetType = line.assetType,
	collateral.status = line.status
merge (collateral)-[:LATEST]->(value)

