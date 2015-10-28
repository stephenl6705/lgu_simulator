library(sqldf)
library(RSQLite)

saveData <- function(data,table,dbpath=dbDir) {
        # Connect to the database
        setwd(dbpath)
        db <- dbConnect(SQLite(), "database")
        # Construct the update query by looping over the data fields
        for (i in 1:nrow(data)) {
                query <- sprintf(
                        "INSERT INTO %s (%s) VALUES ('%s')",
                        table, 
                        paste(names(data), collapse = ", "),
                        paste(data[i,],collapse = "', '")
                )
                dbGetQuery(db, query)
        }
        # Submit the update query and disconnect
        dbDisconnect(db)
}

loadData <- function(table,dbpath=dbDir) {
        # Connect to the database
        setwd(dbpath)
        db <- dbConnect(SQLite(), "database")
        # Construct the fetching query
        query <- sprintf("SELECT * FROM %s", table)
        # Submit the fetch query and disconnect
        data <- dbGetQuery(db, query)
        dbDisconnect(db)
        data
}

reset_history <- function() {
        
        #copy_datafile <- datafile; copy_calcfile <- calcfile
        
        setwd(dbDir)
        datafile <- copy_datafile; calcfile <- copy_calcfile
        datafile$date <- as.character(datafile$date); calcfile$date <- as.character(calcfile$date)
        db <- dbConnect(SQLite(), "database")
        dbRemoveTable(db,"datafile");dbRemoveTable(db,"calcfile")
        dbWriteTable(conn = db, name = "datafile", value = datafile, row.names = FALSE)
        dbWriteTable(conn = db, name = "calcfile", value = calcfile, row.names = FALSE)
        #dbListTables(db)
        dbDisconnect(db)
        
}

update_database <- function(device) {
        
        gs <- gs_title("LGU")
        gs_download(gs,ws = device,to = paste(device,".csv",sep=""),overwrite = T)
        gsdevice <- read.csv(paste(device,".csv",sep=""))
        gsdevice$date <- as.Date(gsdevice$date,format = "%m/%d/%Y")
        
        pgsdevice <- fadd_paramdata(gsdevice,paramfile)
        gscalcfile <- fcalc_model(pgsdevice)
        
        gsdevice$date <- as.character(gsdevice$date)
        gscalcfile$date <- as.character(gscalcfile$date)
        
        gsdevice <- convert_character(gsdevice)
        gscalcfile <- convert_character(gscalcfile)
        
        saveData(gsdevice,"datafile")
        saveData(gscalcfile,"calcfile")
        
        datafile <- loadData("datafile"); datafile$date <- as.Date(datafile$date,format = "%Y-%m-%d")
        calcfile <- loadData("calcfile"); calcfile$date <- as.Date(calcfile$date,format = "%Y-%m-%d")
        
        outfile <- list(datafile)
        outfile[[2]] <- calcfile
        
        outfile
        
}

reset_history()

