
dbConnector <- function(dbname) {
  require(RSQLite)
  # set up connection to a database
  conn <- dbConnect(drv = SQLite(),
                    dbname = dbname)
  # return conn
  conn
}

dbGetData <- function(conn, commodity_id) {
  # query to select all data filtering by commodity
  query <- paste('SELECT trade.*, commodity.commodity FROM trade',
                 'JOIN commodity ON trade.commodity_id = commodity.id',
                 'WHERE commodity.id =',
                 commodity_id)
  as.data.table(dbGetQuery(conn = conn,
                           statement = query))
}

dbGetCommodities <- function(conn) {
  # query to get all commodities from domain table
  query <- 'SELECT * FROM commodity'
  as.data.table(dbGetQuery(conn = conn,
                           statement = query))
}

dbGetCategories <- function(conn) {
  # query to get all categories from domain table
  query <- 'SELECT * FROM category'
  as.data.table(dbGetQuery(conn = conn,
                           statement = query))
}