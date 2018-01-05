MATCH (mc:MarginCall)-[:PART_OF]->(ms:MarginStatement)
WHERE mc.id IN {callId}

WITH mc,ms

MATCH (c:FXRate)
WHERE c.id = mc.currency

RETURN  mc.id AS id,
		mc.marginType AS marginType,
		ms.id AS marginStatement,
		ABS(mc.marginAmount) AS callAmount,
		ABS(mc.marginAmount) AS callAmountUSD,
		mc.currency AS currency,
		mc.callDate AS callDate,
		mc.direction AS direction,
    c.value AS FXRate