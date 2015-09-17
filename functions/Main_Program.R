options(java.parameters="-Xmx4g")

library(shiny)
library(ggplot2)
library(scales)
library(Hmisc)
library(reshape)
library(XLConnect)
library(reshape)

inDir <- "~/PROJECTS/LGU/R-Project/LGU/input/"
appDir <- "~/PROJECTS/LGU/R-Project/LGU/app/"

source('~/PROJECTS/LGU/R-Project/LGU/functions/Help functions.R', echo=FALSE)

# save.image("~/PROJECTS/LGU/R-Project/LGU/LGU/.RData")

########### STEP 1 - Load History  #########################
        
datafile <- fread_histfile("fin101_simul.csv")

datafile <- fadd_peakdata(datafile,"dpeak_input.csv")

datafile <- fadd_dumvardata(datafile,"dumvar_input.csv")

datafile <- fadd_PLCdata(datafile,"plc_info.csv")

datafile <- fadd_rebmindata(datafile,"rebate_min.csv")

paramfile <- fprep_estimates("estimates.csv","device_map.csv")
summary(paramfile)

datafile <- fadd_paramdata(datafile,paramfile)

calcfile <- fcalc_model(datafile)

write.csv(calcfile,paste(inDir,"/calcfile.csv",sep=""))

summary(calcfile)

nrow(calcfile)

aggcalcfile <- fagg_calc(calcfile)

with(calcfile,summary(calcfile),by=Channel)
with(calcfile,plot(Date,Sales))

################# READ INPUT FILES ###################################

Intercept <- param[grepl("D_",param$name) | param$name=="Intercept",]
Intercept <- time_agg(Intercept,Intercept$Estimate,
                      list(Intercept$Device,Intercept$CHANNEL,Intercept$MDDI,Intercept$PLAN,Intercept$PLC,Intercept$SUBTYPE,Intercept$REGION),
                      c("Device","CHANNEL","MDDI","PLAN","PLC","SUBTYPE","REGION"),
                      "DueTo",sum)
Intercept$DueTo <- exp(Intercept$DueTo)

Estimates_Exec <- param[!(substr(param$name,1,1)=="D" | substr(param$name,1,1)=="I"),]

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

