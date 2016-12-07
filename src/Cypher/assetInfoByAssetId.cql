MATCH (a:Asset)
WHERE a.id IN {assetId}


RETURN  a.id AS id,
		a.idType AS idType,
		a.name AS name,
		a.type AS type,
		a.ICADCode AS ICADCode,
		a.ACUOCategory AS ACUOCategory,
		a.rating AS rating,
		a.currency AS currency,
		a.parValue AS unitValue,
		a.minUnit AS minUnit,
		a.minUnitValue AS minUnitValue

