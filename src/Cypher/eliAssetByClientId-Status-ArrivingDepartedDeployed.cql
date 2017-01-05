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
    at.status AS status,
    at.subStatus AS subStatus,
    a.parValue AS unitValue,
    a.minUnit AS minUnit,
    a.minUnitValue AS minUnitValue,
    (p.quantities-p.reservedQuantities) AS quantity,
    p.internalCost AS internalCost,
    p.opptCost AS opptCost
