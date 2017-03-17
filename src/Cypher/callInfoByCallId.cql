MATCH (mc:MarginCall)-[:PART_OF]->(ms:MarginStatement)
WHERE mc.id IN {callId}

WITH *
MATCH (ccy:Currency {id:mc.currency})-[fx:FX_RATE]->(usd:Currency {id:'USD'})

RETURN  mc.id AS id,
		mc.marginType AS marginType,
		ms.id AS marginStatement,
		ABS(mc.marginAmount) AS callAmount,
		ABS(mc.marginAmount/fx.fxRate) AS callAmountUSD,
		mc.currency AS currency,
		mc.callDate AS callDate
