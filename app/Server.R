
shinyServer(function(input, output)
{
        output$avpplot <- 
        renderPlot(
                {
                        plotfile <- calcfile[calcfile$Device==input$device
                                             & calcfile$Channel==input$channel
                                             & calcfile$Region==input$region
                                             & calcfile$Plan==input$plan
                                             & calcfile$Subtype==input$subtype,]
                        tsplotfile= zoo(x=plotfile$Sales, order.by=plotfile$Date)
                        plot(tsplotfile,main=input$device,xlab="Date",ylab="Sales")
                }
        )
}
)
?plot
