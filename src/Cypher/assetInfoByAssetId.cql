MATCH (a:Asset)
WHERE a.id IN {assetId}


RETURN  a.id AS id,
		a.name AS name,
		a.type AS type,
		a.isdaType AS isdaType,
		a.currency AS currency,
		a.value AS value
