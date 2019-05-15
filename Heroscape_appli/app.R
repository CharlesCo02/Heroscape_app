#
#

library(shiny)
library(shinyWidgets)
source("heroscape_fonction_dom.R")

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Distribution des dommages"),

    setBackgroundImage(src = "wp_heroscape772_1280_original.jpg"),
    
    

    sidebarLayout(
        sidebarPanel(
            radioButtons("nb,attaque", "Nombre d'attaque",
                         choices = c("1" = 1, "2" = 2, "3" = 3, "4" = 4)),
            
            numericInput("nb_de_att", "nombre de des de l'attaquant", value = 1, min = 1, max = 15, step = 1),
            numericInput("nb_de_def", "nombre de des du defenseur", value = 1, min = 1, max = 15, step = 1),
            checkboxInput("counter_attack", "Contre-attaque", value = FALSE)
            
        ),

        
        mainPanel(
            
        )
    )
)

# Define server 
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
