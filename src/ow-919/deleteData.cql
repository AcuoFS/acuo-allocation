match (mc:MarginCall), (ms:MarginStatement), (at:AssetTransfer), (c:SettledCollateral),(fx:FXRate)

detach delete mc,ms,at,c,fx
