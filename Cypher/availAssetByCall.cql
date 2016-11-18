MATCH (ca:CustodianAccount)-[h:HOLDS]-(a:Asset)-[is:IS_AVAILABLE_FOR]->(ag:Agreement)<-[:STEMS_FROM]-(mc:MarginCall)-[:DIRECTED_TO]->(c:Client)
WHERE mc.id IN {callId}

WITH *
MATCH (c)-[p:POSSESSES]->(a)

RETURN 
//    c.id AS client,
//    ag.id AS agreementId,
    mc.id AS callId,
    mc.callType AS callType,
    mc.callAmount AS callAmount,
    a.id AS assetID,
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
WHEN 'assetType' IN {order} THEN a.type
WHEN 'isdaType' IN {order} THEN a.isdaType
WHEN 'callId' IN {order} THEN mc.id
WHEN 'callType' IN {order} THEN mc.type
END
