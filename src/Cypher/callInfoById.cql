MATCH (mc:MarginCall)
WHERE mc.id IN {callId}


RETURN  mc.id AS id,
		mc.name AS name,
		mc.callType AS callType,
		mc.callAmount AS callAmount,
		mc.callCurrency AS currency,
		mc.callDate AS callDate

