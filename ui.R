#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinyTime)
library(shiny)
library(visNetwork)
# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel(title = span(img(src = "logodgfip.png", height = 80), "Mousqueton")),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("address","Adresse:"),
            numericInput(inputId = "min",label = "Valeur minimale des transactions:",value=0),
            numericInput(inputId = "max",label = "Valeur maximale des transactions:",value=50),
            selectInput("type","Type de transaction:",choices=c("Éxecution de contrat"="Éxecution de contrat","Création de contrat"="Création de contrat","Transaction"="Transaction"),selected ="Transaction"),
            checkboxGroupInput(inputId = "etat",
                               label = "Etat:",
                               choices = c("Succès" = "Succès", "Échec" = "Échec"), inline=TRUE, selected=c("Succès","Échec")),
            dateRangeInput("daterange", label="Plage journalière",start="2021-01-01",end="2022-12-31"),

actionButton("do", icon("refresh")),
actionButton("button", "Exporter l'historique")),
        

        # Show a plot of the generated distribution
        mainPanel(
          visNetworkOutput("mygraph",height = "700px"), 
          textOutput("mytext"),
          
          wellPanel(tableOutput("mytable"))
          
        )
    )
))
