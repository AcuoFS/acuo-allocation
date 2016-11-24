library('RNeo4j')

source('src/callInfoById.R')

callId <- c("mc1","mc2","mc4","mc5","mc6","mc8","mc9","mc10","mc11")

result <- callInfoById(callId)
