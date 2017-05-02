match (mc:MarginCall)-[:PART_OF]->(ms:MarginStatement)-[:STEMS_FROM]->(a:Agreement)
where a.id in {agreementId}
return mc.id