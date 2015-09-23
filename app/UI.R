
shinyUI(fluidPage(
        
        titlePanel("LGU +"),
        
        sidebarLayout(
                sidebarPanel(
                        selectInput("device", label = h3("Select device"),
                                    choices = list("GALAXY_NOTE3","GALAXY_NOTE4","IPHONE6_16G","IPHONE6_64G",
                                                   "IPHONE6+_16G","IPHONE6+_64G","LG_G2","LG_G3","LG_G3_CAT.6"),
                                    selected = "GALAXY_NOTE3"),
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
                ),
                mainPanel(plotOutput("avpplot"))
        )
))
