MATCH (cu:Custodian)-[:MANAGES]->(ca:CustodianAccount)-[h:HOLDS]-(a:Asset)-[is:IS_AVAILABLE_FOR]->(ag:Agreement)<-
[:STEMS_FROM]-(mc:MarginCall)-[*1..2]->(ms:MarginStatement)-[:DIRECTED_TO]->(e:LegalEntity)<-[:MANAGES]-(c:Client)
WHERE mc.id IN {callId}
AND c.id = {clientId}
AND mc.marginType IN is.marginType
AND (e)-[:CLIENT_SIGNS]-(ag)
AND NOT (a)-[:EXCLUDED]->(mc)

WITH *
MATCH (e)-[:HAS]-(acc:TradingAccount)
WHERE (acc)-[:ACCESSES]->(ca)

WITH *
MATCH (ccy1:Currency {id:a.currency})-[fx1:FX_RATE]->(usd:Currency {id:'USD'})
WITH *
MATCH (ccy2:Currency {id:mc.currency})-[fx2:FX_RATE]->(usd:Currency {id:'USD'})

RETURN DISTINCT
//    ag.id AS agreementId,
    mc.id AS callId,
    //ABS(mc.excessAmount/fx2.fxRate) AS callAmountUSD,
    a.id AS assetId,
    //a.name AS assetName,
    h.availableQuantity AS quantity,
    //mc.marginType AS marginType,
    //mc.currency AS callCurrency,
    //ABS(mc.excessAmount) AS callAmount,
    //a.ACUOCategory AS ACUOCategory,
    a.parValue AS unitValue,
    a.minUnit AS minUnit,
    a.minUnitValue AS minUnitValue,
    a.currency AS currency,
    ca.id AS CustodianAccount,
    cu.countryShortName AS venue,
    h.internalCost AS internalCost,
    h.opptCost AS opptCost,
    a.yield AS yield,
    is.haircut AS haircut,
    is.FXHaircut AS FXHaircut,
    is.externalCost AS externalCost,
    is.interestRate AS interestRate,
    fx1.fxRate AS FXRate
