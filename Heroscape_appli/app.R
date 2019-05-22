#
#

library(shiny)
library(shinyWidgets)
source("heroscape_fonction_dom.R")

ui <- fluidPage(
    
    titlePanel(title = h2("Distribution des dommages", align = "center")), br(),
    

    sidebarLayout(
        sidebarPanel(
            selectInput("nb_attaque", "Nombre d'attaque", selected = 1, choices = c("1" = 1, "2" = 2, "3" = 3, "4" = 4)), br(),
            sliderInput("nb_de_att", "nombre de des de l'attaquant", value = 1, min = 1, max = 15, step = 1), br(),
            sliderInput("nb_de_def", "nombre de des du defenseur", value = 1, min = 1, max = 15, step = 1), br(),
            sliderInput("nb_vie_def", "nombre de vie du defenseur", value = 1, min = 1, max = 10, step = 1), br(),
            checkboxInput("counter_attack", "Contre-attaque", value = FALSE)
            
        ),
        
        
        mainPanel(
            
            tabsetPanel(type = "tab", 
                        tabPanel('Sommaire', verbatimTextOutput("distr_dommages"), verbatimTextOutput("attaque_nec")),
                        tabPanel('Graphiques')
                        
                        )
            
        )
    )
)


server <- function(input, output) {
    
    output$distr_dommages <- renderText({
        
        ele_1 <- Dommage(input$nb_attaque, input$nb_de_att, input$nb_de_def, input$nb_vie_def)
        message_1 <- "probabilite de faire"
        text_1 <- ''
        
        for(i in 1:length(ele_1)){
            text_1 <- paste(paste(message_1, (i - 1), "dommages est de :", toString(format(ele_1[i], digits = 6)), sep = ' '), text_1, sep = '\n')
        }
        
        return(text_1)
    })
    
    output$attaque_nec <- renderText({
        
        message_2 <- "probabilite que le defenseur soit encore vivant apres"
        text_2 <- ''
        prob <- numeric(input$nb_attaque)
        
        for(j in 1:(input$nb_attaque)){
            prob[j] <-  1 - Dommage(j, input$nb_de_att, input$nb_de_def, input$nb_vie_def)[input$nb_vie_def + 1]
            text_2 <- paste(paste(message_2, j, "attaque(s) : ", toString(format(prob[j], digits = 6)), sep = ' '), text_2, sep = '\n')
        }        

        return(text_2)
    })
    
}


shinyApp(ui = ui, server = server)


