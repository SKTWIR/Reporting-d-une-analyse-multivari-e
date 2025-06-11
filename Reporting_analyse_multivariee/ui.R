library(shiny)
library(bslib)
library(fontawesome)

# Thème sombre moderne avec "darkly"
theme_darkly <- bs_theme(
  version = 5,
  bootswatch = "lux",  # Thème sombre
  primary = "#00BCD4",    # Cyan lumineux pour les accents
  secondary = "#FF5722",  # Orange profond pour contraste
  base_font = font_google("Roboto"),
  code_font = font_google("Fira Code")
)

ui <- fluidPage(
  theme = theme_darkly,
  
  navbarPage(
    title = div(tags$i(class = "fas fa-city me-2"), "SAÉ : Analyse multivariée"),
    id = "nav",
    
    tabPanel("Contexte de la SAÉ",
             fluidRow(
               column(
                 width = 8, offset = 2,
                 h1("Contexte de la SAÉ"),
                 p("Présentation du contexte général du projet, objectifs pédagogiques, et jeux de données utilisés."),
                 tags$hr()
               )
             )
    ),
    
    tabPanel("Analyse Globale",
             fluidRow(
               column(
                 width = 10, offset = 1,
                 h1("Analyse Globale"),
                 p("Vue d’ensemble de l’analyse multivariée, incluant graphiques et statistiques clés."),
                 tags$hr()
               )
             )
    ),
    
    tabPanel("Carte interactive",
             fluidRow(
               column(
                 width = 12,
                 h1("Carte des typologies"),
                 p("Carte interactive pour visualiser les typologies géographiques."),
                 leafletOutput("carte_metro_interactive", height = "700px")
               )
             )
    ),
    
    tabPanel("Analyse ACP",
             fluidRow(
               column(
                 width = 10, offset = 1,
                 h1("Analyse en Composantes Principales (ACP)"),
                 p("Projection des communes selon leurs scores culture/tourisme."),
                 
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
