library(shiny)
library(class)
library(dplyr)
library(shinydashboard)
library(ggplot2)

#match.data <- read.csv(choose.files())  # Buscamos donde se encuentra data table
match.data <- read.csv("https://raw.githubusercontent.com/PerezRE/datascience/main/Postworks/Postwork%20Sesi%C3%B3n%208/match.data.csv")

ui <- 
    pageWithSidebar(
        headerPanel("Postwork 8"),
        sidebarPanel(
            p("Seleccione el gráfico"), 
            selectInput("x", "Equipo",
                        c("Casa" = "casa",
                          "Visitante" = "visitante"))
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
                tabPanel("Gráficas", 
                         plotOutput("output_plot"),
                         plotOutput("output_plot_face")
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
    output$output_plot <- renderPlot({
        if(input$x == "casa"){
            home <- as.data.frame(table(match.data$home.score))
            data <- home
        }else{
            away <- as.data.frame(table(match.data$away.score))
            data <- away
        }
        a<- ggplot(data, aes(x = Var1, y = Freq, fill = Var1)) + 
            geom_bar(stat = "identity") +
            ggtitle(paste0("Equipo: ", input$x)) +
            labs(x = "Goles", y = "Frecuencia") +
            theme(
                legend.position="none"
            )
        return(a)
    })
    
    output$output_plot_face <- renderPlot({
        b<- ggplot(match.data, aes(x = home.score, y = away.score)) + 
            geom_bar(stat = "identity") +
            ggtitle("Equipo ") +
            labs(x = "Goles Local", y = "Goles Visitante") +
            theme(legend.position="none") +
            facet_wrap("away.team")
        return(b)
    })
    
}

shinyApp(ui, server)