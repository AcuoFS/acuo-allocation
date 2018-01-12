// fetch settled collateral
match (asset:Asset)<-[:OF]-(transfer:AssetTransfer)-[:GENERATED_BY]->(mc:MarginCall)-[:PART_OF]->(ms:MarginStatement)<-[:BALANCE]-(collateral:Collateral)-[:LATEST]->(cValue:CollateralValue)
where mc.marginType = collateral.marginType
and collateral.status = "settled"
and collateral.assetType = "cash"
with *
optional match (clientAccount:CustodianAccount)<-[:FROM]-(transfer)
with *
optional match (ms)-[:STEMS_FROM]->(ag:Agreement)
with *
match (fxRate:FXRate {id:asset.currency})
return  asset.id as asset,
		clientAccount.id as custodianAccount,
		transfer.quantities as quantity,
		transfer.value as unitValue,
		asset.currency as currency,
		TOFLOAT(fxRate.value) as fxRate,
		transfer.totalHaircut as haircut,
		toFloat(transfer.quantities)*toFloat(transfer.value)/toFloat(fxRate.value) as amount,
		mc.id as call,
		mc.marginType as marginType,
		ag.id as agreement,
		collateral.assetType as assetType,
		cValue.amount as collateralAmount
order by mc.id
