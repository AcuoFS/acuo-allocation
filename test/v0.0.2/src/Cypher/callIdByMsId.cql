MATCH (s:Step)<-[:LAST]-(mc:MarginCall)-[:PART_OF]->(ms:MarginStatement)
WHERE ms.id IN {msId}
AND s.status = 'Reconciled'

RETURN  mc.id AS callId
