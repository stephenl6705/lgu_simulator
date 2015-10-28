require(shiny)
require(ggplot2)
require(reshape)
require(DT)
require(googlesheets)

ui <- fluidPage(
 
        sidebarPanel(
                dateRangeInput("daterange", label = h3("select a date range"),
                               start="2014-09-26",end="2015-01-31",min=NULL,max=NULL,language="kr"),
                selectInput("device", label = h3("Select device"),
                            choices = list("GALAXY_NOTE3","GALAXY_NOTE4","IPHONE6_16G","IPHONE6_64G",
                                           "IPHONE6+_16G","IPHONE6+_64G","LG_G2","LG_G3","LG_G3_CAT.6"),
                            selected = "GALAXY_NOTE4"),
                radioButtons("channel", label = h3("Select channel"),
                            choices = list("HM","RD","RDM","WD","WDM"),
                            selected = "RD",inline=T),
                radioButtons("region", label = h3("Select region"),
                            choices = list("CHG","HON","KYK","KYM","SEL"),
                            selected = "SEL",inline=T),
                radioButtons("plan", label = h3("Select subscription plan"),
                            choices = list("62b","62o"),
                            selected = "62o",inline=T),
                radioButtons("subtype", label = h3("Select subscription type"),
                            choices = list("010","DS","MNP"),
                            selected = "MNP",inline=T),
                selectInput("variable", label = h3("Select variable"),
                            choices = list("reb_max_mode","max_mode_inc","sub89_15","sub69_15","sub62_15","sub34_15","inventory"),
                            selected = "reb_max_mode"),
                actionButton("gsheet", label = h3("Copy to Google Sheet?")),
                actionButton("gsheet_update", label = h3("Update from Google Sheet?"))
                #,sliderInput("nrdays", label = h3("Select number of days"),
                #            min = 1, max = 31, value = 5)
        ),
        mainPanel(
                tabsetPanel(
                        tabPanel("plot",tags$h1("Actual vs. Predicted"),
                                 plotOutput("avpplot"),
                                 plotOutput("varplot")
                        ),
                        tabPanel("table",DT::dataTableOutput("vartable"))
##                        ,
##                        tabPanel("form",
##                                 tags$form(
##                                         tags$div(class="form-group",                                          
##                                                  tags$label("Email address"),
##                                                  tags$input(type="email",class="form-control",id="exampleInputEmail1",placeholder="Email")
##                                         ),
##                                         tags$div(class="form-group",
##                                                  tags$label("Password"),
##                                                  tags$input(type="password",class="form-control",id="exampleInputPassword1",placeholder="Password") 
##                                         ),
##                                         tags$div(class="form-group",
##                                                  tags$label("File input"),
##                                                  tags$input(type="file",id="exampleInputFile"),
##                                                  tags$p(class="help-block","Example block-level help text here.")
##                                         ),
##                                         tags$div(class="checkbox",
##                                                  tags$label(""),
##                                                  tags$input(type="checkbox","Check me out")
##                                         ),
##                                         tags$button(type="submit",class="btn btn-default","Submit")
##                                         )
##                                 )
                        )
                )
        )

server <- function(input,output) {
        
        output$avpplot <- renderPlot({    
                
                infile <- calcfile
                
                plotfile <- infile[infile$Device==input$device
                                   & infile$channel==input$channel
                                   & infile$region==input$region
                                   & infile$Plan==input$plan
                                   & infile$sub_type==input$subtype
                                   & infile$date >= input$daterange[1]
                                   & infile$date <= input$daterange[2]
                                   ,c("date","Sales","PredSales")]
                
                tsRainbow <- c("blue","red")
                
                if (nrow(plotfile)>0) {
                        
                        plotfile <- melt(plotfile,id=c("date"))
                        
                        j <- ggplot(plotfile) +
                                #ggtitle("Actual vs. Predicted") +
                                geom_line(aes(date,value,colour=variable)) +
                                scale_colour_manual(values=tsRainbow) +
                                theme(legend.position="bottom")
                        
                        #j <- j + theme(legend.position="bottom")
                        #j <- j + guides(col="blue")
                        
                        print(j)
                        
                }
        })
        
        sdatafile <- reactive({
                sdatafile <- datafile[datafile$Device==input$device
                                      & datafile$channel==input$channel
                                      & datafile$region==input$region
                                      & datafile$Plan==input$plan
                                      & datafile$sub_type==input$subtype
                                      & datafile$date >= input$daterange[1]
                                      & datafile$date <= input$daterange[2]
                                      ,]
                sdatafile
        })
        
        output$varplot <- renderPlot({    
                
                sdatafile <- sdatafile()
                
                sdatafile <- sdatafile[sdatafile$company=="LGU",c("date",input$variable)]
                
                sdatafile <- sdatafile[order(sdatafile$date),]
                
                #tsRainbow <- c("blue","red")
                
                if (nrow(sdatafile)>0) {
                        
                        plotfile <- melt(sdatafile,id=c("date"))
                        
                        j <- ggplot(plotfile) +
                                #ggtitle("Actual vs. Predicted") +
                                geom_line(aes(date,value,colour=variable)) +
                                theme(legend.position="bottom")
                        
                        #j <- j + theme(legend.position="bottom")
                        #j <- j + guides(col="blue")
                        
                        print(j)
                        
                }
        })
        
        observeEvent(input$gsheet, {
                
                sdatafile <- sdatafile()
                
                sdatafile <- sdatafile[,c("channel","region","MDDI","PLC","company","Device","Plan","sub_type","date",
                                          "weekday","DPEAK","Dum_Var","max_mode_inc","reb_max_mode","Min_Rebate",
                                          "sub34_15","sub62_15","sub69_15","sub89_15","sales")
                                       ]
                
                uniqdates <- unique(datafile$date)
                uniqdates <- data.frame(uniqdates)
                names(uniqdates)[1] <- "date"
                uniqdates <- uniqdates[order(uniqdates$date),]
                uniqdates <- data.frame(uniqdates)
                names(uniqdates)[1] <- "date"
                uniqdates <- uniqdates[uniqdates$date >= input$daterange[1]
                                       & uniqdates$date <= input$daterange[2],]
                uniqdates <- data.frame(uniqdates)
                names(uniqdates)[1] <- "date"
                sdatafile <- merge(sdatafile,uniqdates,by="date",all.y = T)
                
                gs <- gs_title("LGU"); 
                gsheets <- gs_ws_ls(gs)
                for (i in 1:length(gsheets)) {
                        if (gsheets[i] == input$device) {
                                gs_ws_delete(gs,ws = input$device)
                        }
                }
                gs <- gs_title("LGU"); gs_ws_new(gs,ws_title = input$device, input = sdatafile, trim = T)
        })
        
        updateData <- reactive({
                outfile <- update_database(input$device)
                outfile
        })
        
        observeEvent(input$gsheet_update, {
                
                outfile <- updateData()
                datafile <- outfile[[1]]
                calcfile <- outfile[[2]]
                
        })
        
        output$vartable <- DT::renderDataTable({
                
                sdatafile <- sdatafile()
                
                sdatafile <- sdatafile[sdatafile$company=="LGU",
                                       c("date","reb_max_mode","max_mode_inc","sub89_15","sub69_15","sub62_15","sub34_15","inventory")
                                      ]

                sdatafile <- sdatafile[order(sdatafile$date),]
                rownames(sdatafile) <- NULL
                
                sdatafile$date <- format(sdatafile$date, "%Y-%m-%d")
                sdatafile$day <- weekdays(as.Date(sdatafile$date))
                sdatafile <- sdatafile[,c(1,ncol(sdatafile),2:(ncol(sdatafile)-1))]
                
                sketch = htmltools::withTags(table(
                        class = 'display',
                        thead(
                                tr(
                                        th(rowspan = 2, 'date'),
                                        th(rowspan = 2, 'weekday'),
                                        th(rowspan = 2, 'rebate'),
                                        th(rowspan = 2, 'incentive'),
                                        th(colspan = 4, 'subsidy'),
                                        th(rowspan = 2, 'inventory')
                                ),
                                tr(
                                        lapply(rep(c('89', '69', "62", "34"), 1), th)
                                )
                        )
                ))
                
                datatable(sdatafile,
                          callback = JS('table.page("next").draw(false);'),
                          colnames = c("date","weekday","rebate","incentive","subsidy_89","subsidy_69","subsidy_62","subsidy_34","inventory"),
                          container = sketch,
                          #class = 'display',
                          #style = 'bootstrap',
                          rownames = F,
                          #caption = 'Table 1: This is a simple caption for the table.',
                          caption = htmltools::tags$caption(
                                  style = 'caption-side: bottom; text-align: center;',
                                  'Table 1: ', htmltools::em('This is a simple caption for the table.')),
                          options = list(
                                  pageLength = 5,
                                  autoWidth = T,
                                  #lengthMenu = c(5, 10, 15, 20),
                                  initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                          "}"),
                                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Korean.json')
                          )
                          )
        })
        
}

shinyApp(ui,server)
