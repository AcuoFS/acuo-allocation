MATCH (a:Asset)<-[:VALUATED]-(:Valuation)-[:LATEST]->(value:AssetValue)
WHERE a.id IN {assetId}

WITH *
OPTIONAL MATCH (ccy:Currency)<-[r1]-(fxrate:FXRate)-[r2]->(usd:Currency {id:'USD'})
where ccy.id = a.currency

WITH *
OPTIONAL match (fxrate)<-[:OF]-(fx)
where (fxrate)-[:LAST]->(fx)

RETURN  a.id AS id,
		a.idType AS idType,
		a.name AS name,
		a.type AS type,
		a.ICADCode AS ICADCode,
		a.ACUOCategory AS ACUOCategory,
		a.rating AS rating,
		a.currency AS currency,
    value.unitValue AS unitValue,
		a.minUnit AS minUnit,
    a.minUnit*value.unitValue AS minUnitValue,
		CASE WHEN a.currency = 'USD' THEN 1
         WHEN type(r1)="FROM" THEN fx.value 
         WHEN type(r1)="TO" THEN 1/fx.value 
	  END
		AS FXRate
