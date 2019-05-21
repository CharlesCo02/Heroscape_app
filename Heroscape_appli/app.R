#
#

library(shiny)
library(shinyWidgets)
source("heroscape_fonction_dom.R")

ui <- fluidPage(
    
    titlePanel("Distribution des dommages"),
    

    sidebarLayout(
        sidebarPanel(
            radioButtons("nb_attaque", "Nombre d'attaque",
                         choices = c("1" = 1, "2" = 2, "3" = 3, "4" = 4)),
            
            numericInput("nb_de_att", "nombre de des de l'attaquant", value = 1, min = 1, max = 15, step = 1),
            numericInput("nb_de_def", "nombre de des du defenseur", value = 1, min = 1, max = 15, step = 1),
            numericInput("nb_vie_def", "nombre de vie du defenseur", value = 1, min = 1, max = 10, step = 1),
            checkboxInput("counter_attack", "Contre-attaque", value = FALSE)
            
        ),
        
        
        mainPanel(
            verbatimTextOutput("Dommage")
        )
    )
)


server <- function(input, output) {
    
    output$Dommage <- reactive({
        ele <- Dommage(input$nb_attaque, input$nb_de_att, input$nb_de_def, input$nb_vie_def)
        vec <- ''
        for(i in 1:length(ele)){
            vec <- paste(paste("probabilite de faire", (i - 1), "dommages est de :", toString(format(ele[i], digits = 6)), sep = ' '), vec, sep = '\n')
        }
        return(vec)
    })
    
}


shinyApp(ui = ui, server = server)

