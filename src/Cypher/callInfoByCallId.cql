MATCH (mc:MarginCall)-[:PART_OF]->(ms:MarginStatement)
WHERE mc.id IN {callId}

WITH *
OPTIONAL MATCH (ccy:Currency)<-[r1]-(fxrate:FXRate)-[r2]->(usd:Currency {id:'USD'})
where ccy.id = mc.currency

WITH *
OPTIONAL match (fxrate)<-[:OF]-(fx)
where (fxrate)-[:LAST]->(fx)

RETURN  mc.id AS id,
		mc.marginType AS marginType,
		ms.id AS marginStatement,
		ABS(mc.marginAmount) AS callAmount,
		mc.currency AS currency,
		mc.callDate AS callDate,
		mc.direction AS direction,
		CASE WHEN a.currency = 'USD' THEN 1
         WHEN a.currency <> 'USD THEN fx.value
	  END
		AS FXRate,
		CASE WHEN type(r1)="FROM" THEN ccy.id
         WHEN type(r1)="TO" THEN usd.id
	  END
	  AS from,
	  CASE WHEN type(r1)="FROM" THEN usd.id
         WHEN type(r1)="TO" THEN ccy.id
	  END
	  AS to
		
		