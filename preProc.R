library(RSQLite)
library(data.table)
library(dplyr)


csvpath = "./commodity_trade_statistics_data.csv"
dbname = "./commodities.sqlite"
tblname = "trade"
tblname2 = "commodity"
tblname3 = "category"

## read csv
data <- fread(input = csvpath,
              sep = ",",
              header = TRUE)

# select the unique values of commodity to create an id for each one
category = unique(data$category)
category_df = data.table(id = 1:length(category), category = category)

# include the category's ids in the original data table
# set the ON clause as keys of the tables:
setkey(data,category)
setkey(category_df,category)
# perform the join using the merge function
data = merge(data,category_df, all.x=TRUE)
# rename the column to be category_id instead of id
data = data %>% rename(category_id = id)

# select the unique values of commodity to create an id for each one
commodity = unique(data %>% select(commodity, category_id))
commodity_df = data.table(id = 1:nrow(commodity), commodity = commodity$commodity, category_id = commodity$category_id)

# include the commodity's ids in the original data table
# set the ON clause as keys of the tables:
setkey(data,commodity)
setkey(commodity_df,commodity)
# perform the join using the merge function
data = merge(data,commodity_df, all.x=TRUE)
# rename the column to be commodity_id instead of id
data = data %>% rename(commodity_id = id)

# remove the number from the categories
category_df[1:(nrow(category_df) - 1), 'category'] = sapply(category_df[1:(nrow(category_df) - 1), 'category'], substring, first = 4)

# select columns that will be used to store in the database
data = data %>% select(country_or_area, year, flow, trade_usd, commodity_id)

## connect to database
conn <- dbConnect(drv = SQLite(), 
                  dbname = dbname)

# write category table
dbWriteTable(conn = conn,
             name = tblname3,
             value = category_df,
             append = T)
# write commodity table
dbWriteTable(conn = conn,
             name = tblname2,
             value = commodity_df,
             append = T)
## write trade table
dbWriteTable(conn = conn,
             name = tblname,
             value = data,
             append = T)
## list tables
dbListTables(conn)
## disconnect
dbDisconnect(conn)
