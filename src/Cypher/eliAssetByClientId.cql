//query for asset in 'Available' status
MATCH path=(e:LegalEntity)<-[:MANAGES]-(c:Client)-[:CLIENT_ASSET]->(:ClientAsset)-[p:POSSESSES]->(a:Asset)-[:IS_AVAILABLE_FOR]->(ag:Agreement)
WHERE (e)-[:CLIENT_SIGNS]->(ag)
AND c.id = {clientId}
AND p.quantities>p.reservedQuantities

WITH *
MATCH (c)-->(e)-->(ta:TradingAccount)-->(ca:CustodianAccount)-[:HOLDS]->(a)

WITH *
MATCH (cu:Custodian)-[:MANAGES]->(ca)

WITH DISTINCT a,p,ca,cu

RETURN
	a.ACUOCategory AS ACUOCategory,
    a.type AS type,
 	a.id AS assetId,
    a.idType AS idType,
    a.name AS assetName,
    a.currency AS currency,
    a.rating AS rating,
    a.maturityDate AS maturityDate,
    cu.countryShortName AS venue,
    ca.id AS custodianAccountId,
    p.deliveryTime AS deliveryTime,
    'Available' AS status,
    p.subStatus AS subStatus, //no subStatus in [p], value is NULL, 
    a.parValue AS unitValue,
    a.minUnit AS minUnit,
    a.minUnitValue AS minUnitValue,
    (p.quantities-p.reservedQuantities) AS quantity,
    //p.reservedQuantities AS reservedQuantities,
    p.internalCost AS internalCost,
    p.opptCost AS opptCost

UNION 

//query asset in status other than 'Available'
MATCH (c:Client)-->(e:LegalEntity)-->(ta:TradingAccount)-->(ca:CustodianAccount)-[:RECEIVES|SENDS]-(at:AssetTransfer)
WHERE c.id = {clientId}

WITH DISTINCT at, ca
MATCH (ca)-[:HOLDS]->(a:Asset)<-[p:POSSESSES]-(:ClientAsset)<-[:CLIENT_ASSET]-(c)
WHERE a.id = at.assetId

WITH *
MATCH (cu:Custodian)-[:MANAGES]->(ca:CustodianAccount)-[:HOLDS]->(a)


WITH DISTINCT at,a,p,ca,cu

RETURN
    a.ACUOCategory AS ACUOCategory,
    a.type AS type,
    a.id AS assetId,
    a.idType AS idType,
    a.name AS assetName,
    a.currency AS currency,
    a.rating AS rating,
    a.maturityDate AS maturityDate,
    cu.countryShortName AS venue,
    ca.id AS custodianAccountId,
    at.deliveryTime AS deliveryTime,
    at.status AS status,
    at.subStatus AS subStatus,
    a.parValue AS unitValue,
    a.minUnit AS minUnit,
    a.minUnitValue AS minUnitValue,
    (p.quantities-p.reservedQuantities) AS quantity,
    p.internalCost AS internalCost,
    p.opptCost AS opptCost

