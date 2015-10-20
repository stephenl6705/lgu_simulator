# plot(,y=plotfile$sales,x=plotfile$weekday)

shinyServer(function(input, output, session) {
        
        output$avpplot <- renderPlot({    
                        if (file.exists(paste(inDir,"cscenario.csv",sep=""))) {
                                cscenario <- read.csv(paste(inDir,"cscenario.csv",sep=""), stringsAsFactors=FALSE)
                                infile <- rbind(calcfile,cscenario)
                        } else {
                                infile <- calcfile
                        }
                        plotfile <- infile[infile$Device==input$device
                                             & infile$Channel==input$channel
                                             & infile$Region==input$region
                                             & infile$Plan==input$plan
                                             & infile$Subtype==input$subtype,]
                        
                        tsplotfile <- zoo(cbind(plotfile$Sales,plotfile$PredSales), order.by=plotfile$Date)
                        
                        # tsRainbow <- rainbow(ncol(tsplotfile))
                        tsRainbow <- c("blue","red")
                        
                        if (nrow(tsplotfile)>0) {
                                plot(tsplotfile,main=input$device,xlab="Date",ylab="Sales",col=tsRainbow, screens=1)
                                
                                legend(x="topright", legend=c("Actual","Predicted"),lty=1,col=tsRainbow)
                        }
        })

        observe({
                input$btn
                session$sendCustomMessage(type = "resetFileInputHandler", "file1")
                output$results <- renderPrint({
                                return("File reset")
                })
        })
        

        output$tbl <- renderDataTable({

                setwd("~/PROJECTS/LGU/R-Project/LGU/input/")
                
                inFile <- input$file1
                
                if (is.null(inFile)) {
                        return(NULL)
                } else {

                        tbl <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
                        tbl$weekday <- as.POSIXlt(tbl$date)$wday + 1
                        tbl[tbl$weekday==2,"Dum_Var"] <- 0
                        
                        #pscenario <- fadd_paramdata(tbl,paramfile)
                        #cscenario <- fcalc_model(pscenario)
                        
                        #write.csv(cscenario,paste(inDir,"cscenario.csv",sep=""))
                        
                        tbl <- tbl[,c("Device","date","channel","Plan","sub_type","region","company","reb_max_mode","max_mode_inc"
                                      )]
                        #"sub89_15","sub69_15","sub62_15","sub34_15","inventory","DPEAK","Dum_Var","PLC","Min_Rebate"
                        
                        return(tbl)
                }
                
                
                # tbl$date <- as.Date(tbl$date)
                        
                #tbl <- pdatafile[pdatafile$Device==input$tbl_device
                #        & pdatafile$CHANNEL==input$tbl_channel
                #        & pdatafile$REGION==input$tbl_region
                #        & pdatafile$PLAN==input$tbl_plan
                #        & pdatafile$SUBTYPE==input$tbl_subtype,c("date","reb_max_mode.x")]
                
        },options=list(pageLength=10))
        
})