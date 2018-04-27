
source('./helpers.R')

domainTables <- dbGetAllDomainTables(dbname = './commodities.sqlite')
commodities <- domainTables[[1]][order(-commodity)] 
categories <- domainTables[[2]]