// 1. create 10 margin statements(4 legacy, 4 regulatory, 2 cleared)
LOAD CSV WITH HEADERS FROM 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/ow-919/createMarginCalls.csv' 
as line 
with line where line.agreement is not null
match (a:Agreement {id:line.agreement})
merge (a)<-[:STEMS_FROM]-(ms: MarginStatement{id:line.marginStatement})
merge (ms)<-[:PART_OF]-(mc:MarginCall{id:line.marginCall})
set mc.marginType = line.marginType


