MATCH (a:Asset)
WHERE a.id IN {assetId}

WITH *
OPTIONAL MATCH (ccy:Currency)<-[r1]-(fx:FXRate)-[r2]->(usd:Currency {id:'USD'})
where ccy.id = a.id 
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
		a.minUnitValue AS minUnitValue,
		CASE WHEN a.id= 'USD' THEN 1
         WHEN type(r1)="FROM" THEN fx.value 
         WHEN type(r1)="TO" THEN 1/fx.value 
	  END
		AS FXRate
