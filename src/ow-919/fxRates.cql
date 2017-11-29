// 4. fxRate
LOAD CSV WITH HEADERS FROM 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/develop/src/ow-919/fxRates.csv'
AS line

MATCH (c1:Currency {id:line.CurrencyISO})
merge (:FXRate {id:c1.id,value:line.ValueToUSD})
with *
match (c2:Currency {id:"USD"})
merge (:FXRate {id:c2.id,value:1})
