//query for asset in 'Available' status
MATCH path=(e:LegalEntity)<-[:MANAGES]-(c:Client)-[:CLIENT_ASSET]->(:ClientAsset)-[p:POSSESSES]->(a:Asset)-[:IS_AVAILABLE_FOR]->(ag:Agreement)
WHERE (e)-[:CLIENT_SIGNS]->(ag)
AND c.id = {clientId}
AND p.reservedQuantities>0

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
    'Available' AS status,
    a.parValue AS unitValue,
    a.minUnit AS minUnit,
    a.minUnitValue AS minUnitValue,
    p.reservedQuantities AS quantity,
    p.internalCost AS internalCost,
    p.opptCost AS opptCost


