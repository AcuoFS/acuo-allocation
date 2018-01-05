MATCH (ccy:Currency)
WHERE ccy.id in {currencies}

OPTIONAL MATCH (ccy:Currency)<-[r1]-(fxrate:FXRate)-[r2]->(usd:Currency {id:'USD'})

WITH *
OPTIONAL match (fxrate)<-[:OF]-(fx)
where (fxrate)-[:LAST]->(fx)


RETURN 
    ccy.id AS currency,
		CASE WHEN ccy.id = 'USD' THEN 1
		     WHEN type(r1)="FROM" THEN fx.value 
         WHEN type(r1)="TO" THEN 1/fx.value 
	  END
		AS FXRate
