
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
source("heroscape_fonction_dom.R")

header <- dashboardHeader(title = "Options", titleWidth = "200px")

sidebar <- dashboardSidebar(width = "250px",
                            sidebarMenu(
                                menuItem(text = "Distribution des dommages", tabName = "probs_dam"),
                                menuItem(text = "Table du défenseur", tabName = "probs_no_dam")
                            ))

body <- dashboardBody(
    tabItems(
        
        tabItem(tabName = "probs_dam",
                sidebarLayout(
                    sidebarPanel(width = 2,
                                 selectInput("nb_attaque", "Nombre d'attaque", selected = 1, choices = c("1" = 1, "2" = 2, "3" = 3, "4" = 4),
                                             width = "200px"), br(),
                                 selectInput("nb_de_att", "nombre de des de l'attaquant", choices = c(1:12),
                                             width = "200px"), br(),
                                 selectInput("nb_de_def", "nombre de des du defenseur", choices = c(1:15),
                                             width = "200px"), br(),
                                 selectInput("nb_vie", "nombre de vie du defenseur", choices = c(1:9),
                                             width = "200px"), br(),
                                 checkboxInput("counter_attack", "Contre-attaque", value = FALSE)
                                 
                    ),
                    
                    
                    mainPanel(width = 10,
                        verbatimTextOutput("distr_dommages"),
                        verbatimTextOutput("attaque_nec")
                    )
                )
                
        ),
        tabItem(tabName = "probs_no_dam",
            tableOutput("table_survie")
        )
    )
)

ui <- dashboardPage(header, sidebar, body)


server <- function(input, output) {
    
    nb_attaque <- reactive({
        as.numeric(input$nb_attaque)
    })
    
    nb_de_att <- reactive({
        as.numeric(input$nb_de_att)
    })
    
    nb_de_def <- reactive({
        as.numeric(input$nb_de_def)
    })
    
    nb_vie <- reactive({
        as.numeric(input$nb_vie)
    })
    
    output$distr_dommages <- renderText({

        ele_1 <- Dommage(nb_attaque(), nb_de_att(), nb_de_def(), nb_vie())
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
        prob <- numeric(nb_attaque())
        
        for(j in 1:(input$nb_attaque)){
            prob[j] <-  1 - Dommage(j, nb_de_att(), nb_de_def(), nb_vie())[nb_vie() + 1]
            text_2 <- paste(paste(message_2, j, "attaque(s) : ", toString(format(prob[j], digits = 6)), sep = ' '), text_2, sep = '\n')
        }        

        return(text_2)
    })
    
    output$table_survie <- renderTable({
        dtable <- as.data.frame(mat_prob)
        dtable <- cbind(' ' = rownames(dtable), dtable)
    })
}


shinyApp(ui = ui, server = server)


