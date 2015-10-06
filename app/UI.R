library(shinydashboard)
library(shiny)
library(zoo)

shinyUI(dashboardPage(
        dashboardHeader(disable = F, title = "LGU Simulator",
                        dropdownMenu(type = "messages",
                                     messageItem(
                                             from = "Innovation Team",
                                             message = "3 months of data loaded"
                                     ),
                                     messageItem(
                                             from = "Innovation Team",
                                             message = "Visualizing data in progress",
                                             icon = icon("question"),
                                             time = "13:45"
                                     ),
                                     messageItem(
                                             from = "Innovation Team",
                                             message = "The new server is ready.",
                                             icon = icon("life-ring"),
                                             time = "2014-12-01"
                                     )
                        ),
                        dropdownMenu(type = "notifications",
                                     notificationItem(
                                             text = "5 new users today",
                                             icon("users")
                                     ),
                                     notificationItem(
                                             text = "12 items delivered",
                                             icon("truck"),
                                             status = "success"
                                     ),
                                     notificationItem(
                                             text = "Server load at 86%",
                                             icon = icon("exclamation-triangle"),
                                             status = "warning"
                                     )
                        ),
                        dropdownMenu(type = "tasks", badgeStatus = "success",
                                     taskItem(value = 90, color = "green",
                                              "Documentation"
                                     ),
                                     taskItem(value = 17, color = "aqua",
                                              "Project X"
                                     ),
                                     taskItem(value = 75, color = "yellow",
                                              "Server deployment"
                                     ),
                                     taskItem(value = 80, color = "red",
                                              "Overall project"
                                     )
                        )
        ),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("AVP Plot", tabName = "avpplot", icon = icon("signal")),
                        menuItem("Add Scenario", tabName = "scenario", icon = icon("th"), badgeLabel = "new", badgeColor = "green")
                )
        ),
        dashboardBody(
                tabItems(
                        tabItem(tabName = "avpplot",
                                fluidRow(
                                        box(title="AVP",solidHeader=TRUE, background = "light-blue", width = 9, height = 500, 
                                            plotOutput("avpplot")),
                                        box(title="Inputs", status = "info",solidHeader=TRUE, collapsible = TRUE,
                                                collapsed = FALSE, width = 3,
                                                selectInput("device", label = h3("Select device"),
                                                            choices = list("GALAXY_NOTE3","GALAXY_NOTE4","IPHONE6_16G","IPHONE6_64G",
                                                                           "IPHONE6+_16G","IPHONE6+_64G","LG_G2","LG_G3","LG_G3_CAT.6"),
                                                            selected = "GALAXY_NOTE4"),
                                                selectInput("channel", label = h3("Select channel"),
                                                            choices = list("HM","RD","RDM","WD","WDM"),
                                                            selected = "RD"),
                                                selectInput("region", label = h3("Select region"),
                                                            choices = list("CHG","HON","KYK","KYM","SEL"),
                                                            selected = "SEL"),
                                                selectInput("plan", label = h3("Select subscription plan"),
                                                            choices = list("62b","62o"),
                                                            selected = "62o"),
                                                selectInput("subtype", label = h3("Select subscription type"),
                                                            choices = list("010","DS","MNP"),
                                                            selected = "MNP")
                                        )
                                )
                        ),
                        tabItem(tabName = "scenario",
                                fluidRow(
                                        box(width = 12,dataTableOutput("tbl")),
                                        box(title="Select File", width = 4,
                                            fileInput('file1', 'Choose CSV File',
                                                      accept=c('text/csv', 
                                                               'text/comma-separated-values,text/plain', 
                                                               '.csv')),
                                            tags$hr(),
                                            checkboxInput('header', 'Header', TRUE),
                                            radioButtons('sep', 'Separator',
                                                         c(Comma=',',
                                                           Semicolon=';',
                                                           Tab='\t'),
                                                         ','),
                                            radioButtons('quote', 'Quote',
                                                         c(None='',
                                                           'Double Quote'='"',
                                                           'Single Quote'="'"),
                                                         '"')
                                        )
                                )
                        )
                )
        )
))
