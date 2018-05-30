MATCH (cu:Custodian)-[:MANAGES]->(ca:CustodianAccount)-[h:HOLDS]->(a:Asset)<-[:APPLIES_TO]-(rule:Rule)<-[:IS_COMPOSED_OF]-(ag:Agreement)<-[:STEMS_FROM]-(ms:MarginStatement)<-[*1..2]-(mc:MarginCall)
MATCH (e:LegalEntity)<-[:MANAGES]-(c:Client)

WHERE mc.id IN {callId}
AND c.id = {clientId}
AND mc.marginType IN rule.marginType
AND (e)-[:CLIENT_SIGNS]-(ag)
AND NOT (a)-[:EXCLUDED]->(mc)

WITH *
MATCH (e)-[:HAS]-(acc:TradingAccount)
WHERE (acc)-[:ACCESSES]->(ca)

WITH *
MATCH (a)<-[:VALUATED]-(:Valuation)-[:LATEST]->(value:AssetValue)


RETURN DISTINCT
    mc.id AS callId,
    a.id AS assetId,
    rule.haircut AS haircut,
    rule.FXHaircut AS FXHaircut,
    rule.externalCost AS externalCost,
    rule.interestRate AS interestRate,
    h.availableQuantity AS quantity,
    h.internalCost AS internalCost,
    h.opptCost AS opptCost,
    cu.countryShortName AS venue,
    ca.id AS CustodianAccount
