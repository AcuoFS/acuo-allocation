MATCH (mc:MarginCall)
WHERE mc.id IN {callId}


RETURN  mc.id AS id,
		mc.callType AS callType,
		mc.callAmount AS callAmount,
		mc.currency AS currency,
		mc.callDate AS callDate

