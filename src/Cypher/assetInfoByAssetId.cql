MATCH (a:Asset)<-[:VALUATED]-(:Valuation)-[:LATEST]->(value:AssetValue)
WHERE a.id IN {assetId}

WITH *
OPTIONAL MATCH (ccy:Currency)<-[r1]-(fxrate:FXRate)-[r2]->(usd:Currency {id:'USD'})
where ccy.id = a.currency

WITH *
OPTIONAL match (fxrate)<-[:OF]-(fx)
where (fxrate)-[:LAST]->(fx)

RETURN  a.id AS id,
		a.name AS name,
		a.rating AS rating,
		a.currency AS currency,
		value.unitValue AS unitValue,
		a.minUnit AS minUnit,
		value.yieldToMaturity AS yield,
		CASE WHEN a.currency = 'USD' THEN 1
		     WHEN a.currency <> 'USD' THEN fx.value
		END
		AS FXRate,
		CASE WHEN a.currency = 'USD' THEN 'USD'
		     WHEN type(r1)='FROM' THEN ccy.id
		     WHEN type(r1)='TO' THEN usd.id
		END
		AS from,
		CASE WHEN a.currency = 'USD' THEN 'USD'
		     WHEN type(r1)='FROM' THEN usd.id
		     WHEN type(r1)='TO' THEN ccy.id
		END
		AS to
