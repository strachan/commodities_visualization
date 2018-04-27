

conn <- dbConnector('./commodities.sqlite')

commodities <- dbGetCommodities(conn)
categories <- dbGetCategories(conn)