library(shiny)

shinyServer(function(input, output) {

    output$plot <- renderPlot({
        plot(cars, type=input$plotType)
    })
    
    observe({
        toggleClass(condition = input$foo,
                    class = "disabled",
                    selector = ".navbar ul > li:nth-child(2)")
    })
    
    output$time <- renderUI({
        as.character(strptime(Sys.time(), "%Y-%m-%d", tz = "EET"))
    })
    

})
