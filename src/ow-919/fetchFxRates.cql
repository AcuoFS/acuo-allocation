match (fx:FXRate)
where fx.id in {currencies}

return fx.id as currency, 
		fx.value as fxRate
