MATCH path=(e:LegalEntity)<-[:MANAGES]-(c:Client)-[p:POSSESSES]->(a:Asset)-[:IS_AVAILABLE_FOR]->(ag:Agreement)
WHERE (e)-[:CLIENT_SIGNS]->(ag)
AND c.id = {clientId}

WITH DISTINCT a,p
RETURN
	a.ACUOCategory AS ACUOCategory,
    a.type AS type,
 	a.id AS assetId,
    a.idType AS idType,
    a.name AS assetName,
    a.currency AS currency,
    a.parValue AS unitValue,
    a.minUnit AS minUnit,
    a.minUnitValue AS minUnitValue,
    p.quantities AS totalQuantity,
    p.availableQuantities AS availableQuantities,
//    p.deployedQuantities AS deployedQuantities,
//    p.departingQuantities AS departingQuantities,
//    p.arrivingQuantities AS arrivingQuantities,
    p.earmarkedQuantities AS earmarkedQuantities,
    p.internalCost AS internalCost,
    p.opptCost AS opptCost


