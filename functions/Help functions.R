################# FUNCTIONS        ###################################

ftime_agg <- function (file,agg_list,order_list,desc_vec,stat_vec,stat) {
        time_agg <-
                with(file,
                     aggregate(
                             agg_list
                             ,by=order_list
                             ,FUN=stat
                     )
                )
        
        max <- length(desc_vec)
        for (i in 1:max) {
                names(time_agg)[i]<-paste(desc_vec[i])
        }
        max2 <- length(stat_vec)
        for (i in 1:max2) {
                names(time_agg)[max+i]<-paste(stat_vec[i])
        }
        time_agg <- time_agg[order(time_agg[1]),]
        time_agg
}

fread_histfile <- function(histfile) {
        
        # Read History File
        # histfile <- "fin101_simul.csv"
        # rm(histfile,history)
        
        history <- histfile
        history <- read.csv(paste(inDir,history,sep=""),stringsAsFactors=FALSE)
        history[is.na(history)] <- 1
        names(history)[names(history)=="Model_name"] <- "Device"
        history$date <- as.Date(history$date,format = "%d/%m/%Y")

        history[history$channel=="HYPERMARKETS","channel"] <- "HM"
        history[history$channel=="RETAIL DEALER","channel"] <- "RD"
        history[history$channel=="WHOLESALE DEALER","channel"] <- "WD"
        history[history$channel=="RETAIL DEALER MANAGED BY LGU+","channel"] <- "RDM"
        history[history$channel=="WHOLESALE DEALER MANAGED BY LGU+","channel"] <- "WDM"
        
        history[history$region=="CHUNGCHEONG","region"] <- "CHG"
        history[history$region=="HONAM","region"] <- "HON"
        history[history$region=="KYUNGBUK","region"] <- "KYK"
        history[history$region=="KYUNGNAM","region"] <- "KYM"
        history[history$region=="SEOUL","region"] <- "SEL"
        
        history[history$Plan=="OVER62","Plan"] <- "62o"
        history[history$Plan=="BELOW62","Plan"] <- "62b"
        
        history
}

fadd_peakdata <- function(infile,peakfile) {
        
        # Add Peak Data
        
        dPeakFile <- peakfile
        dPeak <- read.csv(paste(inDir,dPeakFile,sep=""), stringsAsFactors=FALSE)
        dPeak$date <- as.Date(dPeak$date,format = "%d/%m/%Y")
        
        outfile <- merge(infile,dPeak,by=c("Device","channel","MDDI","Plan","sub_type","region","date"),all.x=T)
        outfile[is.na(outfile)] <- 0
        
        outfile
        
}

fadd_dumvardata <- function(infile,dumvarfile) {
        
        # Add dumvar Data
        
        dVarFile <- "dumvar_input.csv"
        dVar <- read.csv(paste(inDir,dVarFile,sep=""), stringsAsFactors=FALSE)
        dVar$Date <- as.Date(dVar$Date,format = "%d/%m/%Y")
        
        outfile <- merge(infile,dVar,by.x=c("date"),by.y=c("Date"),all.x=T)
        outfile[is.na(outfile)] <- 0
        
        outfile
        
}

fadd_PLCdata <- function(infile,PLCfile) {
        
        # Add PLC Data
        # infile <- datafile; PLCfile <- "plc_info.csv"
        # rm(infile,PLCfile,fillTheBlanks,outfile,devices,finout,d,temp,PLC)
        
        PLC <- read.csv(paste(inDir,PLCfile,sep=""), stringsAsFactors=FALSE)
        PLC$DATE <- as.Date(PLC$DATE,format = "%d/%m/%Y")
        
        outfile <- merge(infile,PLC,by.x=c("Device","date"),by.y=c("DEVICE","DATE"),all.x=T)
        outfile[is.na(outfile)] <- 0
        
        outfile[outfile$PLC.x==0 & outfile$PLC.y!=0,"PLC.x"] <- outfile[outfile$PLC.x==0 & outfile$PLC.y!=0,"PLC.y"]
        outfile[outfile$PLC.x!=0 & outfile$PLC.y==0,"PLC.y"] <- outfile[outfile$PLC.x!=0 & outfile$PLC.y==0,"PLC.x"]
        outfile <- outfile[,-(ncol(outfile)-3)]
        names(outfile)[names(outfile)=="PLC.y"] <- "PLC"
        
        fillTheBlanks <- function(x, missing=0){
                if (sum(x) > 0) {
                        rle <- rle(x)
                        empty <- which(rle$value==missing)
                        rle$values[empty] <- rle$values[empty-1]
                        inverse.rle(rle)
                } else {x}
        }
        
        outfile <- outfile[order(outfile$Device,outfile$date),]
        
        devices <- summary(as.factor(outfile$Device))
        for (d in 1:length(devices)) {
                temp <- outfile[outfile$Device==names(devices[d]),]
                if (sum(as.integer(temp$PLC)) > 0) {
                        PLC <- fillTheBlanks(temp$PLC,0)
                        finout <- rbind(finout,cbind(temp[,-ncol(temp)],as.data.frame(PLC)))
                } else {
                        if (d==1) {finout <- temp} else {finout <- rbind(finout,temp)}
                }
        }
        outfile <- finout[-1,]
        
        outfile

}

fadd_rebmindata <- function(infile,rebateminfile) {
        
        # Add Rebate Min File
        
        rebMinFile <- rebateminfile
        rebMin <- read.csv(paste(inDir,rebMinFile,sep=""), stringsAsFactors=FALSE)

        outfile <- merge(infile,rebMin[,c(3,4)],by.x=c("Device"),by.y=c("DEVICE"),all.x=T)
        
        outfile

}

fprep_estimates <- function(paraminput,devicefile) {
        
        # Prepare Estimates File
        # paramfile <- "estimates.csv"; devicefile <- "device_map.csv"
        # rm(paramfile,devicefile,paramFile,param,deviceMapFile,deviceMap,compDevice,compDevice_lr,compDevice_lss,targetDevice,trim.trailing)

        paramFile <- paraminput
        param <- read.csv(paste(inDir,paramFile,sep=""), stringsAsFactors=FALSE)
        param<-param[param$name!="",]
        
        deviceMapFile <- devicefile
        deviceMap <- read.csv(paste(inDir,deviceMapFile,sep=""), stringsAsFactors=FALSE)
        deviceMap <- deviceMap[,1:2]
        
        compDevice <- param[grepl("lr_mm",param$name),]
        compDevice$compName <- compDevice$name
        compDevice$name <- factor(compDevice$name)
        compDevice$name <- substr(compDevice$name,6,100)
        compDeviceLGU <- compDevice[substr(compDevice$name,1,3)=="LGU",]; compDeviceLGU$compCompany <- "LGU"; compDeviceLGU$name <- substr(compDeviceLGU$name,4,100)
        compDeviceSKT <- compDevice[substr(compDevice$name,1,3)=="SKT",]; compDeviceSKT$compCompany <- "SKT"; compDeviceSKT$name <- substr(compDeviceSKT$name,4,100)
        compDeviceKT  <- compDevice[substr(compDevice$name,1,2)=="KT",]; compDeviceKT$compCompany <- "KT"; compDeviceKT$name <- substr(compDeviceKT$name,3,100)
        compDevice <- rbind(compDeviceLGU,compDeviceSKT,compDeviceKT); rm(compDeviceLGU,compDeviceSKT,compDeviceKT)
        compDevice$compPlan <- substr(compDevice$name,nchar(x = compDevice$name)-2,nchar(x = compDevice$name))
        compDevice$name <- substr(compDevice$name,1,nchar(x = compDevice$name)-3)
        compDeviceMNP <- compDevice[substr(compDevice$name,nchar(x = compDevice$name)-2,nchar(x = compDevice$name))=="MNP",]; compDeviceMNP$compSubtype <- "MNP"; compDeviceMNP$name <- substr(compDeviceMNP$name,1,nchar(x = compDeviceMNP$name)-3)
        compDevice010 <- compDevice[substr(compDevice$name,nchar(x = compDevice$name)-2,nchar(x = compDevice$name))=="010",]; compDevice010$compSubtype <- "010"; compDevice010$name <- substr(compDevice010$name,1,nchar(x = compDevice010$name)-3)
        compDeviceDS  <- compDevice[substr(compDevice$name,nchar(x = compDevice$name)-1,nchar(x = compDevice$name))=="DS",]; compDeviceDS$compSubtype <- "DS"; compDeviceDS$name <- substr(compDeviceDS$name,1,nchar(x = compDeviceDS$name)-2)
        compDevice <- rbind(compDeviceMNP,compDevice010,compDeviceDS); rm(compDeviceMNP,compDevice010,compDeviceDS)
        compDevice <- merge(compDevice,deviceMap,by.x="name",by.y="MAP",all.x=T)
        names(compDevice)[names(compDevice)=="ORIG"] <- "compDevice"
        compDevice <- compDevice[,-1]
        compDevice$compName <- "lfrmaxmode"
        compDevice_lr <- compDevice
        
        compDevice <- param[grepl("LRSS",param$name),]
        compDevice$compName <- paste("LSSLGU",substr(compDevice$name,8,9), "15",sep="")
        compDevice$compCompany <- substr(compDevice$name,12,14)
        compDevice[compDevice$compCompany=="KT3","compCompany"] <- "KT"
        compDevice[compDevice$compCompany=="KT6","compCompany"] <- "KT"
        compDevice[compDevice$compCompany=="KT8","compCompany"] <- "KT"
        compDevice$compPlan <- compDevice$PLAN
        compDevice$compSubtype <- compDevice$SUBTYPE
        compDevice$compDevice <- compDevice$Device
        compDevice <- compDevice[,-which(names(compDevice)=="name")]
        compDevice_lss <- compDevice
        
        targetDevice <- param[!grepl("lr_mm",param$name) & !grepl("LRSS",param$name),]
        targetDevice$compName <- targetDevice$name
        targetDevice$compCompany <- "LGU"
        targetDevice$compPlan <- targetDevice$PLAN
        targetDevice$compSubtype <- targetDevice$SUBTYPE
        targetDevice$compDevice <- targetDevice$Device
        targetDevice <- targetDevice[,-which(names(targetDevice)=="name")]
        
        param <- rbind(targetDevice, compDevice_lr,compDevice_lss)
        
        param <- cast(data=param,
                     formula = modelname + Device + CHANNEL + MDDI + PLAN + PLC + SUBTYPE + REGION + 
                             compCompany + compPlan + compSubtype + compDevice ~ compName,
                     fun.aggregate = sum, value="Estimate")
        
        param$Intercept <- param$Intercept + param$D_Intercept
        param <- param[,-which(names(param)=="D_Intercept")]
        param[param$MDDI=="OF","MDDI"] <- "OFF"
        
        trim.trailing <- function (x) sub("\\s+$", "", x)
        
        param$Device <- trim.trailing(param$Device)
        
        param$company <- "LGU"
        
        param

}

fadd_paramdata <- function(infile,paramfile) {
        
        # infile <- datafile
        # rm(infile,outfile)
        
        outfile <- merge(paramfile,infile[,!(names(infile) %in% c("Series","reb_avg","avg_inc"))],
                         by.x=c("CHANNEL","REGION","MDDI","PLC","company","Device","PLAN","SUBTYPE"),
                         by.y=c("channel","region","MDDI","PLC","company","Device","Plan","sub_type"),
                         all.x=T)
        outfile <- outfile[!is.na(outfile$date),]

        outfile <- merge(outfile,infile[,!(names(infile) %in% c("Series","reb_avg","avg_inc"))],
                      by.x=c("CHANNEL","REGION","MDDI","PLC","date","compCompany","compDevice","compPlan","compSubtype"),
                      by.y=c("channel","region","MDDI","PLC","date","company"    ,"Device"    ,"Plan"    ,"sub_type"),
                      all.x=T)
        outfile[is.na(outfile)] <- 0
        
        outfile
        
}

fcalc_model <- function(infile) {
        
        # infile <- datafile
        # rm(infile,infile1,infile2)

        plot(infile1$sales.x,exp(infile1$lnpredsales))
        infile1 <- infile1[order(infile1$lnpredsales),]
        tail(infile1)
        
        infile$lnpredsales <- infile$Intercept
        infile$lnpredsales <- infile$lnpredsales + infile$Dpeak * infile$DPEAK.x
        infile$lnpredsales <- infile$lnpredsales + infile$I_dumvar * infile$Dum_Var.x
        infile$lnpredsales <- infile$lnpredsales + infile$Lmaxmodeinc * log(infile$max_mode_inc.x)
        
        infile1 <- infile[infile$reb_max_mode.x==0,]; infile2 <- infile[infile$reb_max_mode.x!=0,]
        infile1$reb_max_mode.x <- infile1$Min_Rebate.x
        infile <- rbind(infile1,infile2)

        infile1 <- infile[infile$company==infile$compCompany,]
        infile2 <- infile[infile$company!=infile$compCompany,]
        
        infile1$lnpredsales <- infile1$lnpredsales + infile1$lfrmaxmode * log(infile1$reb_max_mode.x / infile1$Min_Rebate.x)
        infile1$lnpredsales <- infile1$lnpredsales + infile1$LSSLGU3415 * log(infile1$sub34_15.x) + infile1$LSSLGU6215 * log(infile1$sub62_15.x)
        infile1$lnpredsales <- infile1$lnpredsales + infile1$LSSLGU6915 * log(infile1$sub69_15.x) + infile1$LSSLGU8915 * log(infile1$sub89_15.x)
        
        infile21 <- infile2[infile2$reb_max_mode.y!=0,]; infile22 <- infile2[infile2$reb_max_mode.y==0,]
        infile21$lnpredsales <- infile21$lnpredsales + infile21$lfrmaxmode * log(infile21$reb_max_mode.x / infile21$reb_max_mode.y)
        infile2 <- rbind(infile21,infile22)
        infile21 <- infile2[infile2$sub34_15.y!=0,]; infile22 <- infile2[infile2$sub34_15.y==0,]
        infile21$lnpredsales <- infile21$lnpredsales + infile21$LSSLGU3415 * log(infile21$sub34_15.x / infile21$sub34_15.y)
        infile2 <- rbind(infile21,infile22)
        infile21 <- infile2[infile2$sub62_15.y!=0,]; infile22 <- infile2[infile2$sub62_15.y==0,]
        infile21$lnpredsales <- infile21$lnpredsales + infile21$LSSLGU6215 * log(infile21$sub62_15.x / infile21$sub62_15.y)
        infile2 <- rbind(infile21,infile22)
        infile21 <- infile2[infile2$sub69_15.y!=0,]; infile22 <- infile2[infile2$sub69_15.y==0,]
        infile21$lnpredsales <- infile21$lnpredsales + infile21$LSSLGU6915 * log(infile21$sub69_15.x / infile21$sub69_15.y)
        infile2 <- rbind(infile21,infile22)
        infile21 <- infile2[infile2$sub89_15.y!=0,]; infile22 <- infile2[infile2$sub89_15.y==0,]
        infile21$lnpredsales <- infile21$lnpredsales + infile21$LSSLGU8915 * log(infile21$sub89_15.x / infile21$sub89_15.y)
        infile2 <- rbind(infile21,infile22)
        
        infile <- rbind(infile1,infile2)
        infile$predsales <- exp(infile$lnpredsales)
        
        infile <- ftime_agg(infile,cbind(infile$sales.x,infile$predsales),
                            list(infile$CHANNEL,infile$REGION,infile$MDDI,infile$Device,infile$PLAN,infile$SUBTYPE,
                                 infile$modelname,infile$PLC,infile$date,infile$suspension.x),
                                c("Channel","Region","MDDI","Device","Plan","Subtype","Modelname","PLC","Date","Suspension"),
                                c("Sales","PredSales"),
                                sum
                            )
 
        infile$Channel <- factor(infile$Channel)
        infile$Region <- factor(infile$Region)
        infile$MDDI <- factor(infile$MDDI)
        infile$Device <- factor(infile$Device)
        infile$Plan <- factor(infile$Plan)
        infile$Subtype <- factor(infile$Subtype)
        infile$Modelname <- factor(infile$Modelname)
        infile$PLC <- factor(infile$PLC)
        
        infile <- infile[infile$Suspension=="",]
        
        infile
}

fagg_calc <- function(infile) {

        infile <- ftime_agg(infile,cbind(infile$Sales,infile$PredSales),
                            list(infile$Device,infile$Date),
                            c("Device","Date"),
                            c("Sales","PredSales"),
                            sum
                                )
        infile
        
}

commentout <- function() {
        
        incFile <- "Incentive.xlsx"
        Incentive <- loadWorkbook(paste(inDir,incFile,sep=""))
        Incentive = readWorksheet(Incentive, sheet = getSheets(Incentive)[1],useCachedValues=FALSE,startRow=1, endRow=10000, startCol=1, endCol=17, header=T)
        Incentive <- Incentive[Incentive$company!="",]
        Incentive[is.na(Incentive)] <- 0
        Incentive <- melt(Incentive,id=(c("company","Device","CHANNEL","SUBTYPE","PLAN","REGION")))
        Incentive$variable <- strptime(substr(Incentive$variable,2,9),"%d.%m.%y")
        names(Incentive)[names(Incentive)=="variable"] <- "DATE"
        Incentive <- Incentive[Incentive$value!=0,]
        
        rebateFile <- "rebate.xlsx"
        rebateFile <- loadWorkbook(paste(inDir,rebateFile,sep=""))
        rebate <- ""
        for (i in 1:5) {
                rebatei <- readWorksheet(rebateFile, sheet = getSheets(rebateFile)[i],useCachedValues=FALSE,startRow=1, endRow=10000, startCol=1, endCol=17, header=T)
                rebate <- rbind(rebate,rebatei)
        }
        rebate <- rebate[rebate$company!="",]
        rebate[is.na(rebate)] <- 0
        rebate <- melt(rebate,id=(c("company","Device","CHANNEL","SUBTYPE","PLAN","REGION")))
        rebate$variable <- strptime(substr(rebate$variable,2,9),"%d.%m.%y")
        names(rebate)[names(rebate)=="variable"] <- "DATE"
        
        rebOwn <- rebate[rebate$company=="LGU",]
        rebOwn <- merge(rebOwn,targetDevice,c("Device"),all.y=T)
        
        rebComp <- merge(rebOwn,compDevice,by.x=c("Device","CHANNEL","SUBTYPE","PLAN"),by.y=c("Device","CHANNEL","SUBTYPE","compPlan"),all.y=T)
        rebComp <- merge(rebComp,rebate,by.x=c("compDeviceName","CHANNEL","compSubtype","compPlan","compCompany","REGION","DATE"),by.x=c("Device","CHANNEL","SUBTYPE","PLAN","company","REGION","DATE"),all.x=T)
        head(rebComp)
        
        subsidyFile <- "subsidy.xlsx"
        subsidyFile <- loadWorkbook(paste(inDir,subsidyFile,sep=""))
        subsidy <- ""
        for (i in 1:4) {
                subsidyi <- readWorksheet(subsidyFile, sheet = getSheets(subsidyFile)[i],useCachedValues=FALSE,startRow=1, endRow=10000, startCol=1, endCol=15, header=T)
                subsidyi[,"File"] <- getSheets(subsidyFile)[i]
                subsidy <- rbind(subsidy,subsidyi)
        }
        subsidy <- subsidy[subsidy$company!="",]
        subsidy[is.na(subsidy)] <- 0
        subsidy <- melt(subsidy,id=(c("company","Device","SUBTYPE","PLAN","File")))
        subsidy$variable <- strptime(substr(subsidy$variable,2,9),"%d.%m.%y")
        names(subsidy)[names(subsidy)=="variable"] <- "DATE"
}
