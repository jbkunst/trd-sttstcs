library(tidyverse)
library(DBI)
library(RSQLite)

dbpath <- "data/db.sqlite"

data_yrpc <- readRDS("data/yrpc.rds")
data_yr   <- readRDS("data/yr.rds")
data_yrc  <- readRDS("data/yrc2018.rds")
data_yrpc_chl <- readRDS("data/yrpc_chl.rds")

try(file.remove(dbpath))

dblite <- dbConnect(RSQLite::SQLite(), dbpath)

dbWriteTable(dblite, "yr", data_yr)
dbWriteTable(dblite, "yrc", data_yrc)
dbWriteTable(dblite, "yrpc", data_yrpc)
dbWriteTable(dblite, "yrpc_chl", data_yrpc_chl)

dbListTables(dblite)

dbDisconnect(dblite)
