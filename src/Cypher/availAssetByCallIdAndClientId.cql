MATCH (ca:CustodianAccount)-[h:HOLDS]-(a:Asset)-[is:IS_AVAILABLE_FOR]->(ag:Agreement)<-[*1..2]-(mc:MarginCall)-[:DIRECTED_TO]->
    (c:Client)-[:MANAGES]-(e:LegalEntity)-[:HAS]-(acc:Account)
WHERE mc.id IN {callId}
AND c.id = {clientId}
AND (acc)-[:ACCESSES]->(ca)
AND (e)-[:SIGNS]-(ag)

WITH *
MATCH (c)-[p:POSSESSES]->(a)

RETURN DISTINCT
//    c.id AS client,
//    ag.id AS agreementId,
    mc.id AS callId,
    mc.callType AS callType,
    mc.callAmount AS callAmount,
    ca.id AS CustodianAccount,
    a.id AS assetId,
    a.isdaType AS isdaType,
//    a.name AS assetName,
    a.value AS value,
    a.currency AS currency,
 //   h.quantities AS quantity,
    h.availableQuantities AS quantity,
 //   h.pledgedQuantities AS pledgedQuantities,
 //   h.settledQuantities AS settledQuantities,
    p.internalCost AS internalCost,
    p.opptCost AS opptCost,
    is.haircut AS haircut,
    is.FXHaircut AS FXHaircut,
    is.externalCost AS externalCost,
    is.interestRate AS interestRate,
    is.FXRate AS FXRate

ORDER BY
CASE 
WHEN 'assetId' IN {order} THEN assetId
WHEN 'isdaType' IN {order} THEN isdaType
WHEN 'callId' IN {order} THEN callId
WHEN 'callType' IN {order} THEN callType
END
