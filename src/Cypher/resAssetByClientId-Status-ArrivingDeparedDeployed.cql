MATCH (c:Client)-->(e:LegalEntity)-->(ta:TradingAccount)-->(ca:CustodianAccount)-[:RECEIVES]-(at:AssetTransfer)
WHERE c.id = {clientId}
AND at.status = 'Arriving'
AND at.reservedQuantities >0 

WITH DISTINCT at, ca
MATCH (ca)-[:HOLDS]->(a:Asset)<-[p:POSSESSES]-(c)
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
    at.status AS status,
    at.subStatus AS subStatus,
    a.parValue AS unitValue,
    a.minUnit AS minUnit,
    a.minUnitValue AS minUnitValue,
    at.reservedQuantities AS quantity,
    p.internalCost AS internalCost,
    p.opptCost AS opptCost

