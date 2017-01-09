MATCH (cu:Custodian)-[:MANAGES]->(ca:CustodianAccount)-[h:HOLDS]-(a:Asset)-[is:IS_AVAILABLE_FOR]->(ag:Agreement)<-[:STEMS_FROM]-(mc:MarginCall)-[:DIRECTED_TO]->
    (e:LegalEntity)<-[:MANAGES]-(c:Client)
WHERE mc.id IN {callId}
AND c.id = {clientId}
AND mc.marginType IN is.marginType
AND (e)-[:CLIENT_SIGNS]-(ag)

WITH *
MATCH (e)-[:HAS]-(acc:TradingAccount)
WHERE (acc)-[:ACCESSES]->(ca)

WITH *
MATCH (c)-[:CLIENT_ASSET]->(:ClientAsset)-[p:POSSESSES]->(a)

RETURN DISTINCT
//    ag.id AS agreementId,
    mc.id AS callId,
    mc.marginType AS marginType,
    mc.excessAmount AS callAmount,
    ca.id AS CustodianAccount,
    cu.countryShortName AS venue,
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
    a.yield AS yield,
    is.haircut AS haircut,
    is.FXHaircut AS FXHaircut,
    is.externalCost AS externalCost,
    is.interestRate AS interestRate,
    is.FXRate AS FXRate
