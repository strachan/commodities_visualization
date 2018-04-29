
dbGetCommodities <- function(conn) {
  require(data.table)
  # query to get all commodities from domain table
  query <- 'SELECT * FROM commodity'
  as.data.table(dbGetQuery(conn = conn,
                           statement = query))
}

dbGetCategories <- function(conn) {
  require(data.table)
  # query to get all categories from domain table
  query <- 'SELECT * FROM category'
  as.data.table(dbGetQuery(conn = conn,
                           statement = query))
}

dbGetCountries <- function(conn) {
  require(data.table)
  # query to get all countries from domain table
  query <- 'SELECT DISTINCT trade.country_or_area FROM trade'
  as.data.table(dbGetQuery(conn = conn,
                           statement = query))
}

dbGetYears <- function(conn) {
  require(data.table)
  # query to get all years from domain table
  query <- 'SELECT DISTINCT trade.year FROM trade'
  as.data.table(dbGetQuery(conn = conn,
                           statement = query))
}

dbGetAllDomainTables <- function(dbname) {
  require(RSQLite)
  # set up connection to a database
  conn <- dbConnect(drv = SQLite(),
                    dbname = dbname)
  # get the domain tables
  commodity <- dbGetCommodities(conn)
  category <- dbGetCategories(conn)
  countries <- dbGetCountries(conn)
  years <- dbGetYears(conn)
  # close the connection
  dbDisconnect(conn)
  # return the values
  list(commodity, category, countries, years)
}

dbConnector <- function(session, dbname) {
  require(RSQLite)
  # set up connection to a database
  conn <- dbConnect(drv = SQLite(),
                    dbname = dbname)
  ## disconnect database when session ends
  session$onSessionEnded(function() {
    dbDisconnect(conn)
  })
  # return conn
  conn
}

dbGetDataByCommodity <- function(conn, commodity_id) {
  require(data.table)
  # query to select all data filtering by commodity
  query <- paste('SELECT trade.*, commodity.commodity FROM trade',
                 'JOIN commodity ON trade.commodity_id = commodity.id',
                 'WHERE commodity.id =',
                 commodity_id)
  as.data.table(dbGetQuery(conn = conn,
                           statement = query))
}

dbGetDataByCategory <- function(conn, category_id) {
  require(data.table)
  # query to select all data filtering by category
  query <- paste('SELECT trade.* FROM trade',
                 'JOIN commodity ON trade.commodity_id = commodity.id',
                 'JOIN category ON commodity.category_id = category.id',
                 'WHERE category.id =',
                 category_id)
  as.data.table(dbGetQuery(conn = conn,
                           statement = query))
}

dbGetDataByCountry <- function(conn, country) {
  require(data.table)
  # query to select all data filtering by country
  query <- paste0('SELECT trade.*, commodity.commodity, category.category FROM trade ',
                  'JOIN commodity ON trade.commodity_id = commodity.id ',
                  'JOIN category ON commodity.category_id = category.id ',
                  "WHERE trade.country_or_area ='",
                  country,"'")
  as.data.table(dbGetQuery(conn = conn,
                           statement = query))
}