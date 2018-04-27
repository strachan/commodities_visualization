
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

dbGetAllDomainTables <- function(dbname) {
  require(RSQLite)
  # set up connection to a database
  conn <- dbConnect(drv = SQLite(),
                    dbname = dbname)
  # get the domain tables
  commodity <- dbGetCommodities(conn)
  category <- dbGetCategories(conn)
  # close the connection
  dbDisconnect(conn)
  # return the values
  list(commodity, category)
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

dbGetData <- function(conn, commodity_id) {
  require(data.table)
  # query to select all data filtering by commodity
  query <- paste('SELECT trade.*, commodity.commodity FROM trade',
                 'JOIN commodity ON trade.commodity_id = commodity.id',
                 'WHERE commodity.id =',
                 commodity_id)
  as.data.table(dbGetQuery(conn = conn,
                           statement = query))
}