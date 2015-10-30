options(java.parameters="-Xmx4g")

install.packages("httpuv")

library(devtools)
install_github("shinyTable", "trestletech")

library(ggplot2)
library(scales)
library(Hmisc)
library(reshape)
library(XLConnect)
library(reshape)

inDir <- "~/PROJECTS/LGU/R-Project/LGU/input/"
inDir48 <- "~/PROJECTS/LGU/R-Project/LGU/input_v4.8/"
dbDir <- "~/PROJECTS/LGU/R-Project/LGU/database/"
exDir <- "~/PROJECTS/LGU/R-Project/LGU/extracts/"
appDir <- "~/PROJECTS/LGU/R-Project/LGU/app/"

source('~/PROJECTS/LGU/R-Project/LGU/functions/Help functions.R', echo=FALSE)
source('~/PROJECTS/LGU/R-Project/LGU/functions/sqldb.R', echo=FALSE)

# save.image("~/PROJECTS/LGU/R-Project/LGU/LGU/.RData")

########### SHINY DASHBOARD ################################

runApp(appDir='~/PROJECTS/LGU/R-Project/LGU/LGU_Simulator/',launch.browser=T)


########### STEP 1 - Load History  #########################
        
datafile <- fread_histfile("fin101_simul.csv")

datafile <- fadd_weekday(datafile)

datafile <- fadd_peakdata(datafile,"dpeak_input.csv")

datafile <- fadd_dumvardata(datafile,"dumvar_input.csv")

datafile <- fadd_PLCdata(datafile,"plc_info.csv")

datafile <- fadd_rebmindata(datafile,"rebate_min.csv")
head(datafile)

paramfile <- fprep_estimates("estimates.csv","device_map.csv")
head(paramfile)
pdatafile <- fadd_paramdata(datafile,paramfile)

calcfile <- fcalc_model(pdatafile)
head(calcfile)
#calcfile$PredSales <- calcfile$Sales * 0.95

write.csv(calcfile,paste(exDir,"/calcfile.csv",sep=""))
write.csv(datafile,paste(exDir,"/datafile.csv",sep=""))
write.csv(pdatafile,paste(exDir,"/pdatafile.csv",sep=""))
write.csv(paramfile,paste(exDir,"/paramfile.csv",sep=""))

aggcalcfile <- fagg_calc(calcfile)

with(calcfile,summary(calcfile),by=Channel)
with(calcfile,plot(Date,Sales))


############## Version 4.8 ############################

data48 <- fread_histfile(inDir48,"201510_rawdata_V3.csv","new")

data48 <- fadd_weekday(data48)

data48 <- fadd_peakdata(data48,"dpeak_input.csv")
data48$DPEAK <- 0

data48 <- fadd_dumvardata(data48,"dumvar_input.csv")
data48[data48$weekday==2,"Dum_Var"] <- 1

data48 <- fadd_PLCdata(data48,"plc_info.csv")

data48 <- fadd_rebmindata(data48,"rebate_min.csv")

param48 <- fprep_estimates(inDir48,"estimates.csv","device_map.csv")

pdata48 <- fadd_paramdata(data48,param48)

calc48 <- fcalc_model(pdata48)
