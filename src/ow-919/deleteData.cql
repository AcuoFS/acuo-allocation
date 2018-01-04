match (mc:MarginCall)
optional match (ms:MarginStatement)
optional match (at:AssetTransfer)
optional match (c:SettledCollateral)
optional match (fx:FXRate)

detach delete mc,ms,at,c,fx
