MATCH (mc:MarginCall)
WHERE mc.id IN {callId}


RETURN  mc.id AS id,
		mc.marginType AS marginType,
		ABS(mc.excessAmount) AS callAmount,
		mc.currency AS currency,
		mc.callDate AS callDate

