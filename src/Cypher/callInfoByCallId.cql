MATCH (mc:MarginCall)
WHERE mc.id IN {callId}

WITH *
MATCH (ccy:Currency {id:mc.currency})-[fx:FX_RATE]->(usd:Currency {id:'USD'})

RETURN  mc.id AS id,
		mc.marginType AS marginType,
		ABS(mc.excessAmount) AS callAmount,
		ABS(mc.excessAmount/fx.fxRate) AS callAmountUSD,
		mc.currency AS currency,
		mc.callDate AS callDate
