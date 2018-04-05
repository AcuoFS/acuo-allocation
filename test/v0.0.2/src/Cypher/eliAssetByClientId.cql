//query for asset in 'Available' status

MATCH (client:Client {id:{clientId}})-[:MANAGES]->(entity:LegalEntity)-[:CLIENT_SIGNS]->(agreement:Agreement)-[:IS_COMPOSED_OF]->(rule:Rule)-[:APPLIES_TO]->(asset:Asset) 

WITH asset, client, rule

MATCH h=(cu:Custodian)-[:MANAGES]->(ca:CustodianAccount)-[holds:HOLDS]->(asset) 
//MATCH v=(asset)<-[:VALUATED]-(:AssetValuation)-[:VALUE]->(:AssetValue) 

WHERE holds.availableQuantity > 0

WITH DISTINCT asset,cu,ca,holds

RETURN
	  asset.ACUOCategory AS ACUOCategory,
 	  asset.id AS assetId,
    asset.idType AS idType,
    asset.name AS assetName,
    asset.currency AS currency,
    asset.rating AS rating,
    asset.maturityDate AS maturityDate,
    cu.countryShortName AS venue,
    ca.id AS custodianAccountId,
    holds.deliveryTime AS deliveryTime,
    'Available' AS status,
    holds.subStatus AS subStatus, //no subStatus in [p], value is NULL, 
    asset.parValue AS unitValue,
    holds.availableQuantity AS quantity,
    holds.internalCost AS internalCost,
    holds.opptCost AS opptCost


UNION 

//query for asset in 'Available' status

MATCH (client:Client {id:{clientId}})-[:MANAGES]->(entity:LegalEntity)-[:CLIENT_SIGNS]->(agreement:Agreement)-[:IS_COMPOSED_OF]->(rule:Rule)-[:APPLIES_TO]->(asset:Asset) 

WITH asset, client, rule

MATCH h=(cu:Custodian)-[:MANAGES]->(ca:CustodianAccount)-[holds:HOLDS]->(asset) 
//MATCH v=(asset)<-[:VALUATED]-(:AssetValuation)-[:VALUE]->(:AssetValue) 

MATCH (asset)<-[:OF]-(at:AssetTransfer)-[:FROM|TO]->(:CustodianAccount)<-[:HAS]-(client) 

WITH DISTINCT at,asset,cu,ca,holds

RETURN
	  asset.ACUOCategory AS ACUOCategory,
 	  asset.id AS assetId,
    asset.idType AS idType,
    asset.name AS assetName,
    asset.currency AS currency,
    asset.rating AS rating,
    asset.maturityDate AS maturityDate,
    cu.countryShortName AS venue,
    ca.id AS custodianAccountId,
    at.deliveryTime AS deliveryTime,
    at.status AS status,
    at.subStatus AS subStatus,
    asset.parValue AS unitValue,
    holds.availableQuantity AS quantity,
    holds.internalCost AS internalCost,
    holds.opptCost AS opptCost


