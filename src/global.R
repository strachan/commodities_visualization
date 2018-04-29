
source('./helpers.R')

domainTables <- dbGetAllDomainTables(dbname = './commodities.sqlite')
commodities <- domainTables[[1]][order(commodity)] 
categories <- domainTables[[2]][order(category)]
flow_types <- c('Export', 'Import', 'Balance')
countries <- domainTables[[3]]
years <- domainTables[[4]]