MATCH (ccy:Currency)
WHERE ccy.id in {currencies}

OPTIONAL MATCH (ccy)<-[r1]-(fxrate:FXRate)-[r2]->(usd:Currency {id:'USD'})

WITH *
OPTIONAL match (fxrate)<-[:OF]-(fx)
where (fxrate)-[:LAST]->(fx)


RETURN 
    ccy.id AS currency,
    CASE WHEN ccy.id = 'USD' THEN 1
         WHEN fx IS NOT NULL THEN fx.value
    END
    AS FXRate,
		CASE WHEN type(r1)="FROM" THEN ccy.id 
         WHEN type(r1)="TO" THEN 'USD'
	  END
		AS from,
		CASE WHEN type(r1)="FROM" THEN ccy.id 
         WHEN type(r1)="TO" THEN 'USD'
	  END
		AS to
		
