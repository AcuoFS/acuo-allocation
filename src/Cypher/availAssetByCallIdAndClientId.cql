MATCH (ca:CustodianAccount)-[h:HOLDS]-(a:Asset)-[is:IS_AVAILABLE_FOR]->(ag:Agreement)<-[*1..2]-(mc:MarginCall)-[:DIRECTED_TO]->
    (c:Client)-[:MANAGES]-(e:LegalEntity)-[:HAS]-(acc:Account)
WHERE mc.id IN {callId}
AND c.id = {clientId}
AND mc.callType IN is.callType
AND (acc)-[:ACCESSES]->(ca)
AND (e)-[:CLIENT_SIGNS]-(ag)

WITH *
MATCH (c)-[p:POSSESSES]->(a)

RETURN DISTINCT
//    ag.id AS agreementId,
    mc.id AS callId,
    mc.callType AS callType,
    mc.callAmount AS callAmount,
    ca.id AS CustodianAccount,
    a.id AS assetId,
    a.ACUOCategory AS ACUOCategory,
//    a.name AS assetName,
    a.parValue AS unitValue,
    a.minUnit AS minUnit,
    a.minUnitValue AS minUnitValue,
    a.currency AS currency,
 //   h.quantities AS quantity,
    h.availableQuantities AS quantity,
    h.quantities AS totalQuantity,
    p.internalCost AS internalCost,
    p.opptCost AS opptCost,
    p.yield AS yield,
    is.haircut AS haircut,
    is.FXHaircut AS FXHaircut,
    is.externalCost AS externalCost,
    is.interestRate AS interestRate,
    is.FXRate AS FXRate

ORDER BY
CASE 
WHEN 'assetId' IN {order} THEN assetId
WHEN 'ACUOCategory' IN {order} THEN ACUOCategory
WHEN 'callId' IN {order} THEN callId
WHEN 'callType' IN {order} THEN callType
END
