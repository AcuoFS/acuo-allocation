MATCH (c1:Currency)<-[:FROM]-(fx:FXRate)-[:TO]->(c2:Currency)
OPTIONAL MATCH (fx)-[:LAST]->(fxValue:FXValue)
WHERE c1.id="USD" 
AND c2.id in {currencies}
RETURN 
  c2.id as currency, 
  1/fxValue.value as FxRate

UNION
  
MATCH (c1:Currency)<-[:FROM]-(fx:FXRate)-[:TO]->(c2:Currency)
OPTIONAL MATCH (fx)-[:LAST]->(fxValue:FXValue)
WHERE c2.id="USD"
AND c1.id in {currencies}
RETURN 
  c1.id as currency, 
  1/fxValue.value as FxRate
  
UNION
  
MATCH (c1:Currency)
WHERE c1.id="USD"
AND c1.id in {currencies}
RETURN 
  c1.id as currency, 
  1 as FxRate
