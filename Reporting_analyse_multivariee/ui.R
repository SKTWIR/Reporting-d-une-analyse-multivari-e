# Installation des packages si nécessaire
packages <- c("shiny", "bslib", "shinymanager", "sf", "plotly", "httr", "jsonlite", "shinydashboard",
              "readr", "leaflet", "DT", "shinythemes", "shinydashboardPlus")

installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)
if (length(to_install) > 0) install.packages(to_install)

# Chargement des bibliothèques
lapply(packages, library, character.only = TRUE)

# Interface utilisateur
fluidPage(
  dashboardPage(
    dashboardHeader(title = "SAÉ : Analyse multivariée"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Contexte de la SAÉ", tabName = "contexte", icon = icon("bookmark")),
        menuItem("Globale", tabName = "globale", icon = icon("dashboard")),
        menuItem("Carte", tabName = "carte", icon = icon("map")),
        menuItem("ACP", tabName = "regression", icon = icon("chart-line"))
      )
    ),
    
    dashboardBody(
      # Styles personnalisés
      tags$style(HTML("
        .custom-icon {
          font-size: 30px;
          vertical-align: middle;
        }
        .big-text:not(.custom-icon) {
          font-size: 40px;
          text-align: center;
        }
      ")),
      
      # Choix du thème via le controlbar
      controlbar = dashboardControlbar(skinSelector()),
      
      # Contenu des onglets
      tabItems(
        # Onglet Contexte
        tabItem(tabName = "contexte",
                h2("Contexte de la SAÉ"),
                p("Cette section présente le contexte général du projet de SAÉ, ses objectifs pédagogiques, 
                   ainsi que les données utilisées. Pour nos données nous avons utilisé les données des communes entre 10 et 20K habitants.")
        ),
        
        # Onglet Analyse Globale
        tabItem(tabName = "globale",
                h2("Analyse Globale"),
                p("Ici, vous trouverez une vue d’ensemble de l’analyse multivariée, avec des graphiques et des résumés statistiques.")
        ),
        
        # Onglet Carte
        tabItem(tabName = "carte",
                h2("Visualisation Cartographique"),
                p("Une carte interactive permettant de visualiser les données géographiques associées à notre analyse."),
                leafletOutput("carte_metro_interactive", height = "700px")
        ),
        
        # Onglet ACP
        tabItem(tabName = "regression",
                h2("Analyse en Composantes Principales (ACP)"),
                p("Exploration des résultats de l’ACP, représentation des variables et individus dans un plan factoriel."),
                
                # Filtre par typologie
                selectInput("selected_clusters", "Filtrer par typologie :", 
                            choices = c("Toutes", 
                                        "Tourisme et culture", 
                                        "Touristique sans culture", 
                                        "Culturel sans tourisme", 
                                        "Peu touristique et peu culturel"),
                            selected = "Toutes", 
                            multiple = TRUE),
                
                plotlyOutput("plot_acp", height = "500px")
        )
      )
    )
  )
)
