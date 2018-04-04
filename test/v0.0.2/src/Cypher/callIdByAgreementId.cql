match (status)<-[:LAST]-(mc:MarginCall)-[:PART_OF]->(ms:MarginStatement)-[:STEMS_FROM]->(a:Agreement)
where a.id in {agreementId}
and not status.status='Expected'
return mc.id
