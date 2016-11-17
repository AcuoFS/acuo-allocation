MATCH (c:Client)-[:MANAGES]->(e:LegalEntity)-[:HAS]->(acc:Account)-[:ACCESSES]->(a:Asset)
WHERE c.id IN {clientId}


WITH DISTINCT a, c

MATCH (c)-[p:POSSESSES]->(a)

RETURN  a.id AS id,
		a.name AS name,
		a.type AS type,
		a.currency AS currency,
		p.quantities AS quantity,
		p.pledgedQuantities AS pledgedQuantities,
		p.settledQuantities AS settledQuantities,
		a.value AS value
ORDER BY
CASE
WHEN 'assetName' IN {order} THEN a.name
WHEN 'assetType' IN {order} THEN a.type
END
