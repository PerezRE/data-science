
library(shiny)
library(class)
library(dplyr)
library(shinydashboard)
library(ggplot2)

match.data <- read.csv(choose.files())  # Buscamos donde se encuentra data table

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
                
                tabPanel("Plots",                   
                         h3(textOutput("output_text")), 
                         plotOutput("output_plot"), 
                ),
                
                tabPanel("Postwork 3",
                         img( src = "marginal-home.png", 
                              height = 350, width = 450),
                         img( src = "marginal-away.png", 
                              height = 350, width = 450),
                         img( src = "heatmap-conjun.png", 
                              height = 350, width = 450)
                         
                ),
                
                tabPanel("Table", tableOutput("table")),                 
                tabPanel("Data Table", dataTableOutput("data_table"))
            )
        )
    )


#De aquí en adelante es la parte que corresponde al server

server <- function(input, output) {
    
    
    output$output_text <- renderText(paste("mpg~", input$x))
    
    # Gráficas
    output$output_plot <- renderPlot({ plot( as.formula(paste("mpg ~", input$x)),
                                             data = mtcars) })
    
    output$summary <- renderPrint({ summary(mtcars)})
    
    output$table <- renderTable({ 
        data.frame(mtcars)
    })
    
    output$data_table <- renderDataTable({mtcars}, 
                                         options = list(aLengthMenu = c(5,25,50),
                                                        iDisplayLength = 5))
}


shinyApp(ui, server)