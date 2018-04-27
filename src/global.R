
source('./helpers.R')

domainTables <- dbGetAllDomainTables(dbname = './commodities.sqlite')
commodities <- domainTables[[1]]
categories <- domainTables[[2]]