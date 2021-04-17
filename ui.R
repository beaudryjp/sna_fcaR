
package_load()


jscode <- '
shinyjs.init = function() {
    $(".nav").on("click", ".disabled", function (e) {
        e.preventDefault();
        return false;
    });
}
'

shinyUI(
    ui <- fluidPage(
        tags$head(tags$style(HTML("
            .navbar-nav {
              float: none;
            }
            .navbar ul > li:nth-child(2) {
              float: right;
            }
            .navbar ul > li:nth-child(2) {
              color: black !important;
            }
        "))),
        useShinyjs(),
        extendShinyjs(text = jscode, functions = "init"),
        theme = shinytheme("cosmo"),
        #theme = shinytheme("darkly"),
        
        #shinythemes::themeSelector(),
        
        spsGoTop("up", right = "5%",  bottom= "5%", icon = icon("arrow-up"), color = "green"),
        
        tagList(
            navbarPage("Social Network Analysis",
                tabPanel("Network",
                         sidebarLayout(
                             sidebarPanel(
                                 "Posts",
                                 hr()
                             ),
                             mainPanel(
                                 plotOutput("plot")
                             )
                         )),
                tabPanel(tags$ul(class='nav navbar-nav',
                                 style = "padding-left: 5px; float: right;", htmlOutput("time")))
            )
        )

    )
)
