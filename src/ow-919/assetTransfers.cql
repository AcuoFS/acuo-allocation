// 2. create assetTransfer
LOAD CSV WITH HEADERS FROM 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/ow-919/createAssetTransfers.csv' 
as line with line
MATCH (from:CustodianAccount {id:line.clientCustodianAccountId}),
	  (to:CustodianAccount {id:line.counterpartyCustodianAccountId}),
	  (mc:MarginCall {id:line.callId}),
	  (asset:Asset {id:line.assetId})
merge (asset)<-[:OF]-(transfer:AssetTransfer{id:line.id})-[:GENERATED_BY]->(mc)
set transfer.quantities = TOFLOAT(line.quantities),	
	transfer.value = TOFLOAT(line.value),	
	transfer.status = line.status,
	transfer.subStatus = line.subStatus,
	transfer.pledgeDate = line.pledgeDate
MERGE (transfer)-[:FROM]->(from)
MERGE (transfer)-[:TO]->(to)
