MATCH (cu:Custodian)-[:MANAGES]->(ca:CustodianAccount)-[h:HOLDS]->(a:Asset)<-[:APPLIES_TO]-(rule:Rule)<-[:IS_COMPOSED_OF]-(ag:Agreement)<-
[:STEMS_FROM]-(ms:MarginStatement)-[:DIRECTED_TO]->(e:LegalEntity)<-[:MANAGES]-(c:Client)
MATCH (mc:MarginCall)-[*1..2]->(ms:MarginStatement)

WHERE mc.id IN {callId}
AND c.id = {clientId}
AND mc.marginType IN rule.marginType
AND (e)-[:CLIENT_SIGNS]-(ag)
AND NOT (a)-[:EXCLUDED]->(mc)

WITH *
MATCH (e)-[:HAS]-(acc:TradingAccount)
WHERE (acc)-[:ACCESSES]->(ca)


RETURN DISTINCT
    mc.id AS callId,
    a.id AS assetId,
    h.availableQuantity AS quantity,
    a.parValue AS unitValue,
    a.minUnit AS minUnit,
    a.minUnitValue AS minUnitValue,
    a.currency AS currency,
    ca.id AS CustodianAccount,
    cu.countryShortName AS venue,
    h.internalCost AS internalCost,
    h.opptCost AS opptCost,
    a.yield AS yield,
    rule.haircut AS haircut,
    rule.FXHaircut AS FXHaircut,
    rule.externalCost AS externalCost,
    rule.interestRate AS interestRate
