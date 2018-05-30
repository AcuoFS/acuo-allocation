MATCH (mc:MarginCall)-[:PART_OF]->(ms:MarginStatement)
WHERE mc.id IN {callId}

WITH *
OPTIONAL MATCH (ccy:Currency)<-[r1]-(fxrate:FXRate)-[r2]->(usd:Currency {id:'USD'})
where ccy.id = mc.currency

WITH *
OPTIONAL match (fxrate)<-[:OF]-(fx)
where (fxrate)-[:LAST]->(fx)

RETURN  mc.id AS id,
		ms.id AS marginStatement,
		mc.marginType AS marginType,
		ABS(mc.marginAmount) AS callAmount,
		mc.currency AS currency,
		mc.callDate AS callDate,
		CASE WHEN mc.currency = 'USD' THEN 1
		     WHEN mc.currency <> 'USD' THEN fx.value
		END
		AS FXRate,
		CASE WHEN mc.currency = 'USD' THEN 'USD'
		     WHEN type(r1)='FROM' THEN ccy.id
		     WHEN type(r1)='TO' THEN usd.id
		END
		AS from,
		CASE WHEN mc.currency = 'USD' THEN 'USD'
		     WHEN type(r1)='FROM' THEN usd.id
		     WHEN type(r1)='TO' THEN ccy.id
		END
		AS to
