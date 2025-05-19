#Installation des packages
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("bslib", quietly = TRUE)) install.packages("bslib")
if (!requireNamespace("shinymanager", quietly = TRUE)) install.packages("shinymanager")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("shinydashboard")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("rsconnect")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("leaflet")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("DT")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("shinythemes")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("shinydashboardPlus")

library(shiny)
library(bslib)
library(shinymanager)
library(sf)
library(httr)
library(jsonlite)
library(plotly)
library(shinydashboard)
library(readr)
library(leaflet)
library(DT)
library(shinythemes)
library(shinydashboardPlus)


# création de l'User interface de l'application
fluidPage(

  dashboardPage(
    
    dashboardHeader(title = "SAÉ : Analyse multivariée"),
    
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Contexte", tabName = "contexte", icon = icon("bookmark")),
        menuItem("Globale", tabName = "globale", icon = icon("dashboard")),
        menuItem("Carte", tabName = "carte", icon = icon("map")),
        menuItem("Régression Linéaire", tabName = "regression", icon = icon("chart-line"))
      )
    ),
    dashboardBody(
      
      #style CSS définition :
      
      tags$style(HTML("
      .custom-icon {
        font-size: 30px; /* Changez cette valeur pour ajuster la taille */
        vertical-align: middle; /* Aligne l'icône verticalement au centre */
      }")),
      
      
      tags$style(HTML("
      .big-text:not(.custom-icon) {
        font-size: 40px; /* Changez cette valeur pour ajuster la taille */
        text-align: center; /* Centre le texte */
      }")),
    
    )
  )
)