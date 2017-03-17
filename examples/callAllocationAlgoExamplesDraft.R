
########### FUNCTION INPUT FROM JAVA LAYER ###########################
# call ids
# user id
# uesr preference
# callInfo <- callInfoByCallId(callIds)
# availAssets <- availAssetByCallIdAndClientId(callIds,clientId)
# assetIds
# assetInfo <- assetInfoByAssetId(assetId)
#######################################################################

# Data below should come from the Java, 
# but I need to test before the real connection built,
# so I use cypher queries to request DB to get data

callIds.all <- c('mcp1','mcp5','mcp7','mcp15','mcp19','mcp23','mcp25',                            # 7 lagacy VM
                 'mcp2','mcp4','mcp8','mcp10','mcp14','mcp16','mcp20','mcp22','mcp26','mcp30',    # 10 bilateral VM
                 'mcp31','mcp32','mcp33','mcp37','mcp38','mcp39','mcp42',#'mcp34','mcp35','mcp36', # 10 cleared VM
                 'mcp43','mcp44','mcp45','mcp49','mcp50','mcp51','mcp54')#,'mcp46','mcp47','mcp48') # 10 cleared IM
idx.VM.all <- c(1:24)#27)
idx.IM.all <- c(25:31)#c(28:37)
all.num <- length(callIds.all)

clientId <- '999'

# preference order: operation, liquidity, and cost
pref.all <- list(c(10,0,0),c(10,10,0),c(10,0,10),
                 c(0,10,0),c(0,10,10),
                 c(0,0,10),
                 c(10,5,7),c(4,10,2),c(3,5,10),c(9,8,7))


source('src/functionsOfDBRequestByExecutingCypher.R')
source("src/allocationFunction.R")
library("XLConnect")
file.path <- "Result/testCloudServerRunningTime.xlsx"
workbook <- loadWorkbook(file.path)
read.params <- readWorksheet(workbook,sheet = 'Sheet1',header=TRUE)

callIds = c("mcp1","mcp5","mcp7","mcp50")
callIds = c("mcp38","mcp20","mcp22","mcp15")
callIds = c("mcp32","mcp33","mcp37","mcp26","mcp39","mcp50");
pref = c(0,10,7);
clientId = '999';
index <- 0
for(i in 1:2){
  callIds.num <- i
  for(k in 1:sample.num){
    index <- (i-1)*10+k
    pref <- pref.all[[k]]
    idx.callIds <- sample(1:31,callIds.num)
    callIds <- callIds.all[idx.callIds]
    VM.num <- length(intersect(idx.callIds,idx.VM.all))
    
    # test
    callInfo <- callInfoByCallId(callIds)
    availAssets <- availAssetByCallIdAndClientId(callIds,clientId) # available asset for the margin call
    availAssets <- availAssets[order(availAssets$callId),]
    
    #### create the new identifier: assetId-custodianAccountId
    asset.custac.id <- paste(availAssets$assetId,availAssets$CustodianAccount,sep='-')
    availAssets$assetCustacId <- asset.custac.id
    
    assetCustacIds <- unique(asset.custac.id)
    #### end #######################
    
    assetIds <- as.character(data.frame(strsplit(assetIds.new,'-'))[1,])
    assetInfo <- assetInfoByAssetId(assetIds)
    assetInfo <- assetInfo[match(assetIds,assetInfo$id),]
    
    start.time <- proc.time()[3]
    ## CALL THE ALLOCATION FUNCTION ###########
    limit <- c(30,7,7); time.limit=1
    result <- allocationAlgo(callIds,assetCustacIds,clientId,callInfo,availAssets,assetInfo,pref,time.limit,limit)
    end.time <- proc.time()[3]
    run.time <- end.time-start.time
    
    output.write <- matrix(c(callIds.num,VM.num,pref[3],pref[2],pref[1],run.time,status),nrow=1)
    writeWorksheetToFile(file.path,data=output.write,sheet='Sheet1',startRow=index+1,startCol=1,header=F)
  }
}




