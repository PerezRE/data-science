
library(shiny)
library(class)
library(dplyr)
library(shinydashboard)
library(ggplot2)

#match.data <- read.csv(choose.files())  # Buscamos donde se encuentra data table
match.data <- read.csv("https://raw.githubusercontent.com/PerezRE/datascience/main/Postworks%20Lenguaje%20R/Postwork%20Sesi%C3%B3n%208/match.data.csv")

ui <- 
    
    pageWithSidebar(
        headerPanel("Postwork 8"),
        sidebarPanel(
            p("Crear plots con el DF 'auto'"), 
            selectInput("x", "Seleccione el valor de X",
                        choices = names(mtcars))
        ),
        mainPanel(
            
            
            # Agregando 4 pestañas
            tabsetPanel(
                tabPanel("Datos Momio",
                         img( src = "momios_1.png", 
                              height = 350, width = 450),
                         img( src = "momios_2.png", 
                              height = 350, width = 450)
                ),
                
                tabPanel("Postwork 3",
                         img( src = "marginal-home.png", 
                              height = 350, width = 450),
                         img( src = "marginal-away.png", 
                              height = 350, width = 450),
                         img( src = "heatmap-conjun.png", 
                              height = 350, width = 450)
                         
                ),             
                tabPanel("Match data", dataTableOutput("data_table"))
            )
        )
    )


#De aquí en adelante es la parte que corresponde al server

server <- function(input, output) {
    
    output$data_table <- renderDataTable({match.data}, 
                                         options = list(aLengthMenu = c(20,40,60),
                                                        iDisplayLength = 20))
}


shinyApp(ui, server)