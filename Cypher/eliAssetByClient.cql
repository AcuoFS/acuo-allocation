MATCH (e:LegalEntity)<-[:MANAGES]-(c:Client)-[:POSSESSES]->(a:Asset)-[:IS_AVAILABLE_FOR]->(ag:Agreement)
WHERE (e)-[:SIGNS]->(ag)
AND c.id = {clientId}

RETURN 
 	a.id AS assetID,
    a.name AS assetName,
    a.value AS value,
    a.currency AS currency,
    p.quantities AS quantity,
    p.availableQuantities AS availableQuantities,
    p.pledgedQuantities AS pledgedQuantities,
    p.settledQuantities AS settledQuantities,
    p.internalCost AS internalCost,
    p.opptCost AS opptCost
