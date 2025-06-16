library(shiny)
library(bslib)
library(fontawesome)
library(leaflet) 
library(plotly)  


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
                 p("Dans le cadre de notre SAÉ REPORTING D’UNE ANALYSE MULTIVARIÉE, nous avons travaillé sur un projet visant à établir une typologie des communes françaises selon les équipements publics disponibles (culturels, touristiques, sportifs, éducatifs, de santé, etc.). 
                 L’étude a porté plus particulièrement sur les communes de 10 000 à 20 000 habitants."),
                 
                p("Notre problématique s’est centrée sur l’étude des équipements touristiques et culturels afin de déterminer s’il existe un lien entre les communes très touristiques et celles disposant de nombreux équipements culturels. Nous avons ainsi cherché à répondre à la question suivante : les communes à fort attrait touristique sont-elles également riches en équipements culturels ?
                Notre démarche a suivi plusieurs étapes :"),
                 p("•	Construction d’une base de données à partir des fichiers de l’INSEE et de la Base Permanente des Équipements (BPE)."),
                 
                 p("•	Sélection et traitement des variables liées aux domaines touristique et culturel."),

                 p("•	Réalisation d’une Analyse en Composantes Principales (ACP) pour explorer les structures de données et identifier des axes de différenciation."),

                 p("•	Typologie par clustering afin de regrouper les communes selon des profils similaires."),

                 p("•	Développement d’une application R Shiny permettant d’automatiser et d’explorer ces résultats de manière interactive.
Cette application facilite la visualisation des résultats, met en évidence les liens entre tourisme et culture dans les communes étudiées, et permet d’envisager un suivi annuel automatisé."),
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
