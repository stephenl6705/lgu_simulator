library(sqldf)
library(RSQLite)

saveData <- function(data,table,dbpath=dbDir) {
        setwd(dbpath)
        db <- dbConnect(SQLite(), "database")
        for (i in 1:nrow(data)) {
                query <- sprintf(
                        "INSERT INTO %s (%s) VALUES ('%s')",
                        table, 
                        paste(names(data), collapse = ", "),
                        paste(data[i,],collapse = "', '")
                )
                dbGetQuery(db, query)
        }
        dbDisconnect(db)
}

updateData <- function(data,table,dbpath=dbDir) {
        setwd(dbpath)
        db <- dbConnect(SQLite(), "database")
        if (table == "datafile") {
                for (i in 1:nrow(data)) {
                        query <- sprintf("SELECT * FROM %s WHERE (date='%s' and channel='%s' and region='%s'
                                        and company='%s' and Device='%s' and Plan='%s' and sub_type = '%s')",
                                       table,data[i,1],data[i,2],data[i,3],data[i,6],data[i,7],data[i,8],data[i,9])
                        getRow <- dbGetQuery(db, query)
                        if (nrow(getRow)>0) {
                                query <- sprintf("UPDATE %s SET MDDI='%s',PLC=%s,weekday=%s,DPEAK=%s,
                                                 Dum_Var=%s,max_mode_inc=%s,reb_max_mode=%s,Min_Rebate=%s,
                                                 sub34_15=%s,sub62_15=%s,sub69_15=%s,sub89_15=%s,
                                                 inventory=%s,sales=%s
                                                 WHERE (date='%s' and channel='%s' and region='%s' and company='%s'
                                                 and Device='%s' and Plan='%s' and sub_type='%s')",
                                                 table,
                                                 data[i,4],data[i,5],data[i,10],data[i,11],data[i,12],data[i,13],data[i,14],
                                                 data[i,15],data[i,16],data[i,17],data[i,18],data[i,19],data[i,20],data[i,21],
                                                 data[i,1],data[i,2],data[i,3],data[i,6],data[i,7],data[i,8],data[i,9])
                                
                        } else {
                                query <- sprintf(
                                        "INSERT INTO %s (%s) VALUES ('%s')",
                                        table, 
                                        paste(names(data), collapse = ", "),
                                        paste(data[i,],collapse = "', '")
                                )
                        }
                        dbGetQuery(db, query)
                }
        }
        else if (table == "calcfile") {
                #table <- "calcfile"; data <- gscalcfile
                #i<-1;
                for (i in 1:nrow(data)) {
                        query <- sprintf("SELECT * FROM %s WHERE (company= '%s' and channel= '%s' and region= '%s' 
                                         and Device= '%s' and Plan= '%s' and sub_type= '%s' and date = '%s')",
                                         table,data[i,1],data[i,2],data[i,3],data[i,5],data[i,6],data[i,7],data[i,10])
                        getRow <- dbGetQuery(db, query)
                        if (nrow(getRow)>0) {
                                query <- sprintf("UPDATE %s SET MDDI='%s',PLC=%s,sales=%s,PredSales=%s
                                                 WHERE (company='%s' and channel='%s' and region='%s' and Device='%s'
                                                 and Plan='%s' and sub_type='%s' and date='%s')",
                                                 table,data[i,4],data[i,9],data[i,11],data[i,12],data[i,1],data[i,2],
                                                 data[i,3],data[i,5],data[i,6],data[i,7],data[i,10])
                        } else {
                                query <- sprintf(
                                        "INSERT INTO %s (%s) VALUES ('%s')",
                                        table, 
                                        paste(names(data), collapse = ", "),
                                        paste(data[i,],collapse = "', '")
                                )
                        }
                        dbGetQuery(db, query)
                        #i <- i+1
                        #print(i)
                }
        }
        dbDisconnect(db)
        getRow
}

loadData <- function(table,dbpath=dbDir) {
        setwd(dbpath)
        db <- dbConnect(SQLite(), "database")
        query <- sprintf("SELECT * FROM %s", table)
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
        
        updateData(gsdevice,"datafile")
        updateData(gscalcfile,"calcfile")
        
}

load_datafile <- function() {
        
        datafile <- loadData("datafile"); datafile$date <- as.Date(datafile$date,format = "%Y-%m-%d")
        
        datafile
        
}

load_calcfile <- function() {
        
        calcfile <- loadData("calcfile"); calcfile$date <- as.Date(calcfile$date,format = "%Y-%m-%d")
        
        calcfile
        
}
