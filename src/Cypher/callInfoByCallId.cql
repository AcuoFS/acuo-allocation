MATCH (mc:MarginCall)-[:PART_OF]->(ms:MarginStatement)
WHERE mc.id IN {callId}

RETURN  mc.id AS id,
		mc.marginType AS marginType,
		ms.id AS marginStatement,
		ABS(mc.marginAmount) AS callAmount,
		mc.currency AS currency,
		mc.callDate AS callDate,
		mc.direction AS direction
