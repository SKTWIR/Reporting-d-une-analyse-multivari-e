library(shiny)
library(bslib)
library(fontawesome)

# Installation des packages si nécessaire (optionnel, à mettre en global.R idéalement)
packages <- c("shiny", "bslib", "sf", "plotly", "leaflet", "fontawesome")
installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)
if (length(to_install) > 0) install.packages(to_install)

# Thème Bootstrap moderne avec bslib
theme_moderne <- bs_theme(
  version = 5,
  bootswatch = "minty",    # Choix d'un thème clair et frais
  primary = "#2C3E50",     # Couleur principale sombre
  secondary = "#18BC9C",   # Couleur secondaire turquoise
  base_font = font_google("Poppins"), # Police moderne
  code_font = font_google("Fira Mono")
)

ui <- fluidPage(
  theme = theme_moderne,
  
  # Navbar principal pour la navigation
  navbarPage(
    title = div(tags$i(class = "fas fa-chart-pie me-2"), "SAÉ : Analyse multivariée"),
    id = "nav",
    
    tabPanel("Contexte de la SAÉ",
             fluidRow(
               column(
                 width = 8, offset = 2,
                 h1("Contexte de la SAÉ", class = "mb-4"),
                 p("Cette section présente le contexte général du projet de SAÉ, ses objectifs pédagogiques, ainsi que les données utilisées. Pour nos données nous avons utilisé les données des communes entre 10 et 20K habitants."),
                 tags$hr()
               )
             )
    ),
    
    tabPanel("Analyse Globale",
             fluidRow(
               column(
                 width = 10, offset = 1,
                 h1("Analyse Globale", class = "mb-4"),
                 p("Ici, vous trouverez une vue d’ensemble de l’analyse multivariée, avec des graphiques et des résumés statistiques."),
                 tags$hr()
               )
             )
    ),
    
    tabPanel("Visualisation Cartographique",
             fluidRow(
               column(
                 width = 12,
                 h1("Visualisation Cartographique", class = "mb-3"),
                 p("Une carte interactive permettant de visualiser les données géographiques associées à notre analyse."),
                 leafletOutput("carte_metro_interactive", height = "700px")
               )
             )
    ),
    
    tabPanel("Analyse en Composantes Principales (ACP)",
             fluidRow(
               column(
                 width = 10, offset = 1,
                 h1("Analyse en Composantes Principales (ACP)", class = "mb-4"),
                 p("Exploration des résultats de l’ACP, représentation des variables et individus dans un plan factoriel."),
                 
                 # Filtre par typologie avec un style modernisé
                 selectInput(
                   inputId = "selected_clusters", 
                   label = "Filtrer par typologie :", 
                   choices = c(
                     "Toutes", 
                     "Tourisme et culture", 
                     "Touristique sans culture", 
                     "Culturel sans tourisme", 
                     "Peu touristique et peu culturel"
                   ),
                   selected = "Toutes", 
                   multiple = TRUE,
                   width = "100%"
                 ),
                 tags$hr(),
                 plotlyOutput("plot_acp", height = "550px")
               )
             )
    )
  )
)
