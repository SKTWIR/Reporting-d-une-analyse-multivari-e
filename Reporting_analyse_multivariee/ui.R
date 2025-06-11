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
          menuItem("Contexte de la SAÉ", tabName = "contexte", icon = icon("bookmark")),
          menuItem("Globale", tabName = "globale", icon = icon("dashboard")),
          menuItem("Carte", tabName = "carte", icon = icon("map")),
          menuItem("ACP ", tabName = "regression", icon = icon("chart-line"))
        )
      ),
      
      dashboardBody(
        tags$style(HTML("
        .custom-icon {
          font-size: 30px;
          vertical-align: middle;
        }
      ")),
        tags$style(HTML("
        .big-text:not(.custom-icon) {
          font-size: 40px;
          text-align: center;
        }
      ")),
        
        # Panneau de skin
        controlbar = dashboardControlbar(skinSelector()),
        
        tabItems(
          tabItem(tabName = "contexte",
                  h2("Contexte de la SAÉ"),
                  p("Cette section présente le contexte général du projet de SAÉ, ses objectifs pédagogiques, ainsi que les données utilisées.")
          ),
          tabItem(tabName = "globale",
                  h2("Analyse Globale"),
                  p("Ici, vous trouverez une vue d’ensemble de l’analyse multivariée, avec des graphiques et des résumés statistiques.")
          ),
          tabItem(tabName = "carte",
                  h2("Visualisation Cartographique"),
                  p("Une carte interactive permettant de visualiser les données géographiques associées à notre analyse."),
                  leafletOutput("carte_metro_interactive", height = "700px")
          ),
          tabItem(tabName = "regression",
                  h2("Analyse en Composantes Principales (ACP)"),
                  p("Exploration des résultats de l’ACP, représentation des variables et individus dans un plan factoriel."),
                  plotOutput("plot_acp", height = "500px")
          )
        )
      )
    )
  )