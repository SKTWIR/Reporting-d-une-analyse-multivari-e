library(shiny)
library(readxl)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(cluster)
library(stringr)
library(sf)
library(DT)  # Pour afficher des tableaux interactifs

# UI
ui <- fluidPage(
  titlePanel("Analyse touristique et socio-culturelle"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Sélectionnez les données"),
      fileInput("file", "Importer un fichier Excel", accept = ".xlsx"),
      hr(),
      
      # Sélection multiple des régions avec option par défaut
      selectInput("region", "Choisissez une ou plusieurs régions", choices = NULL, selected = NULL, multiple = TRUE),
      
      # Ajouter les boutons de génération et de téléchargement
      actionButton("generate", "Générer les résultats"),
      downloadButton("download_data", "Télécharger la base filtrée"),
      
      # Ajouter un bouton de téléchargement pour le fichier Word
      tags$hr(),
      tags$p("Télécharger le rapport d'analyse :"),
      tags$a(href = "https://www.dropbox.com/scl/fi/tmzkxxtnbnofbqjs2bgf0/SAE-4.docx?rlkey=acny2yeonatpt4faviyx3kihq&st=l2wdq4ls&dl=1", "Télécharger le fichier Word", target = "_blank")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Contexte", 
                 h3("Contexte de l'étude", style = "font-size: 24px;"),
                 p("Notre problématique s’est centrée sur l’étude des équipements touristiques et culturels afin de déterminer s’il existe un lien entre les communes très touristiques et celles disposant de nombreux équipements culturels. Nous avons ainsi cherché à répondre à la question suivante : les communes à fort attrait touristique sont-elles également riches en équipements culturels ?", style = "font-size: 18px; line-height: 1.8;"),
                 br(),
                 p("Notre démarche a suivi plusieurs étapes :", style = "font-size: 18px; line-height: 1.8;"),
                 tags$ul(
                   tags$li(style = "font-size: 18px; line-height: 1.8;", "Construction d’une base de données à partir des fichiers de l’INSEE et de la Base Permanente des Équipements (BPE)."),
                   tags$li(style = "font-size: 18px; line-height: 1.8;", "Sélection et traitement des variables liées aux domaines touristique et culturel."),
                   tags$li(style = "font-size: 18px; line-height: 1.8;", "Réalisation d’une Analyse en Composantes Principales (ACP) pour explorer les structures de données et identifier des axes de différenciation."),
                   tags$li(style = "font-size: 18px; line-height: 1.8;", "Typologie par clustering afin de regrouper les communes selon des profils similaires."),
                   tags$li(style = "font-size: 18px; line-height: 1.8;", "Développement d’une application R Shiny permettant d’automatiser et d’explorer ces résultats de manière interactive. Cette application facilite la visualisation des résultats, met en évidence les liens entre tourisme et culture dans les communes étudiées, et permet d’envisager un suivi annuel automatisé.")
                 )
        ),
        tabPanel("ACP - Graphique",
                 plotOutput("acp_typologie", height = "600px"),  # Scatter plot en haut
                 plotOutput("acp_biplot", height = "600px"),  # ACP biplot au milieu
                 plotOutput("scatter_plot", height = "600px")  # Typologie en bas
        ),
        tabPanel("Carte Métropole", plotOutput("carte_metro", height = "950px", width = "100%")),  # Prendre toute la largeur de la page
        tabPanel("Carte Mayotte", plotOutput("carte_mayotte", height = "800px")),
        tabPanel("Base de Données", DTOutput("data_table"))  # Ajout d'une page pour la base de données
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Données réactives pour la génération des résultats
  data_reactive <- eventReactive(input$generate, {
    req(input$file)
    df <- read_excel(input$file$datapath)
    df <- df[, c(3, 5, 14:29)]  # Sélectionner les colonnes nécessaires
    
    # Séparer les variables en tourisme vs culture
    tourisme_vars <- df %>%
      select(NB_G101, NB_G102, NB_G103, NB_G104) %>%
      mutate_all(~replace(., is.na(.), 0))
    
    culture_vars <- df %>% 
      select(NB_F303, NB_F303_NB_SALLES, NB_F305, NB_F307, NB_F315, NB_F315_NB_SALLES) %>%
      mutate_all(~replace(., is.na(.), 0))
    
    # Créer 2 scores normalisés
    df$score_tourisme <- rowSums(scale(tourisme_vars))
    df$score_culture  <- rowSums(scale(culture_vars))
    
    # Clustering
    vars_score <- df %>% select(score_tourisme, score_culture)
    set.seed(42)
    km <- kmeans(scale(vars_score), centers = 4, nstart = 25)
    df$cluster_num <- km$cluster
    
    # Attribution des libellés
    cluster_summary <- df %>%
      group_by(cluster_num) %>%
      summarise(tour = mean(score_tourisme), cult = mean(score_culture))
    
    ordre <- cluster_summary %>%
      mutate(cat = case_when(
        tour > 0 & cult > 0 ~ "Tourisme et culture",
        tour > 0 & cult <= 0 ~ "Touristique sans culture",
        tour <= 0 & cult > 0 ~ "Culturel sans tourisme",
        TRUE ~ "Peu touristique et peu culturel"
      ))
    
    label_map <- setNames(ordre$cat, ordre$cluster_num)
    df$cluster <- label_map[as.character(df$cluster_num)]
    
    # ACP
    acp <- PCA(vars_score, graph = FALSE)
    coord <- as.data.frame(acp$ind$coord[, 1:2])
    result <- cbind(df, coord)
    
    # Harmonisation des noms
    result$LIBGEO <- str_to_upper(result$LIBGEO)
    
    return(result)
  })
  
  # Mise à jour dynamique des régions disponibles
  observe({
    req(data_reactive())
    result <- data_reactive()
    
    # Exclure les îles dans la liste des régions
    regions <- unique(result$Région)
    excluded_regions <- c("Guadeloupe", "Martinique", "Guyane", "La Réunion", "Mayotte", "Saint-Pierre-et-Miquelon", "Saint-Barthélemy", "Saint-Martin")
    regions <- regions[!regions %in% excluded_regions]
    
    # Ajouter l'option "Tout sélectionner" en haut
    updateSelectInput(session, "region", choices = c("Tout sélectionner" = "", regions), selected = NULL)
  })
  
  # Scatter plot des individus avec filtre appliqué
  output$scatter_plot <- renderPlot({
    req(data_reactive())
    result <- data_reactive()
    
    # Filtrage selon la sélection des régions
    if(length(input$region) > 0 && !"" %in% input$region) {
      result <- result %>% filter(Région %in% input$region)
    }
    
    ggplot(result, aes(x = Dim.1, y = Dim.2, color = as.factor(Région))) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(title = "Répartition des communes selon les 2 premières composantes", 
           x = "Dimension 1", 
           y = "Dimension 2") +
      scale_color_manual(values = c(
        "Auvergne-Rhône-Alpes" = "#4B9CD3", 
        "Hauts-de-France" = "#56B870", 
        "Provence-Alpes-Côte d'Azur" = "#f07b6f", 
        "Occitanie" = "#56B870", 
        "Grand Est" = "#D55E00", 
        "Normandie" = "#E69F00", 
        "Nouvelle-Aquitaine" = "#009E73",
        "Bretagne" = "#F0E442",  
        "Île-de-France" = "#56B870"  
      )) +
      theme(legend.position = "bottom")
  })
  
  # Biplot ACP avec filtre appliqué
  output$acp_biplot <- renderPlot({
    req(data_reactive())
    result <- data_reactive()
    
    # Filtrage selon la sélection des régions
    if(length(input$region) > 0 && !"" %in% input$region) {
      result <- result %>% filter(Région %in% input$region)
    }
    
    # Graphique Biplot
    fviz_pca_biplot(PCA(result[, c("score_tourisme", "score_culture")], graph = FALSE), 
                    axes = c(1, 2), 
                    col.var = "blue", 
                    col.ind = result$cluster, 
                    palette = "jco", 
                    addEllipses = TRUE, 
                    label = "var")
  })
  
  # ACP - Typologie des communes selon attractivité touristique et socio-culturelle
  output$acp_typologie <- renderPlot({
    req(data_reactive())
    result <- data_reactive()
    
    ggplot(result, aes(x = Dim.1, y = Dim.2, color = cluster)) +
      geom_point(size = 2) +
      theme_minimal() +
      scale_color_manual(values = c("Tourisme et culture" = "#4B9CD3", 
                                    "Peu touristique et peu culturel" = "#56B870", 
                                    "Culturel sans tourisme" = "#f07b6f", 
                                    "Touristique sans culture" = "#56B870")) +
      labs(title = "ACP - Typologie des communes selon attractivité touristique et socio-culturelle")
  })
  
  # Carte Métropole avec filtrage et zoom sur la région
  output$carte_metro <- renderPlot({
    req(data_reactive())
    result <- data_reactive()
    
    # Filtrer pour la ou les régions sélectionnées
    if(length(input$region) > 0 && !"" %in% input$region) {
      result <- result %>% filter(Région %in% input$region)
    }
    
    # Charger les communes de métropole (exclure les îles)
    communes_sf <- st_read("https://france-geojson.gregoiredavid.fr/repo/communes.geojson", quiet = TRUE)
    
    # Exclure les îles : On ne garde que les communes de la métropole
    communes_sf <- communes_sf %>% filter(!str_detect(nom, "^(Guadeloupe|Martinique|Guyane|La Réunion|Mayotte|Saint-Pierre-et-Miquelon|Saint-Barthélemy|Saint-Martin)$"))
    
    # Harmonisation des noms pour la jointure
    communes_sf$nom <- str_to_upper(communes_sf$nom)
    
    # Effectuer la jointure entre communes_sf et result sur 'LIBGEO'
    carte_clusters <- communes_sf %>% 
      left_join(result, by = c("nom" = "LIBGEO"))
    
    # Recode des clusters : "Communes non sélectionnées" si les données sont manquantes
    carte_clusters$cluster_affichee <- case_when(
      is.na(carte_clusters$cluster) ~ "Communes non sélectionnées",
      carte_clusters$cluster == "Tourisme et culture" ~ "Tourisme et culture",
      carte_clusters$cluster == "Peu touristique et peu culturel" ~ "Peu touristique et peu culturel",
      TRUE ~ NA_character_
    )
    
    carte_clusters$cluster_affichee <- factor(
      carte_clusters$cluster_affichee,
      levels = c("Tourisme et culture", "Peu touristique et peu culturel", "Communes non sélectionnées")
    )
    
    # Définir les limites pour le zoom sur la carte
    if (nrow(carte_clusters) > 0) {
      xmin <- min(st_bbox(carte_clusters)$xmin, na.rm = TRUE)
      xmax <- max(st_bbox(carte_clusters)$xmax, na.rm = TRUE)
      ymin <- min(st_bbox(carte_clusters)$ymin, na.rm = TRUE)
      ymax <- max(st_bbox(carte_clusters)$ymax, na.rm = TRUE)
    } else {
      xmin <- xmax <- ymin <- ymax <- c(0, 0)  # Default in case no data
    }
    
    # Affichage de la carte avec zoom dynamique
    ggplot(carte_clusters) +
      geom_sf(aes(fill = cluster_affichee), color = NA, size = 0.1) +
      scale_fill_manual(
        name = "Typologie",
        values = c(
          "Tourisme et culture" = "#4B9CD3",
          "Peu touristique et peu culturel" = "#56B870",
          "Communes non sélectionnées" = "lightgrey"
        ),
        drop = FALSE,
        na.translate = FALSE
      ) +
      labs(
        title = paste("Carte des communes (métropole) selon attractivité touristique et socio-culturelle"),
        caption = "Source : INSEE + clustering (traitement personnel)"
      ) +
      theme_minimal() +
      coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), datum = NA)  # Centrer la carte et zoomer
  })
  
  # Carte Mayotte sans filtrage par région
  output$carte_mayotte <- renderPlot({
    req(data_reactive())
    result <- data_reactive()
    
    # Charger les communes de Mayotte sans appliquer le filtre de région
    communes_my <- st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions/mayotte/communes-mayotte.geojson", quiet = TRUE)
    communes_my$nom <- str_to_upper(communes_my$nom)
    
    # Effectuer la jointure pour la carte Mayotte
    carte_mayotte <- communes_my %>% 
      left_join(result, by = c("nom" = "LIBGEO"))
    
    # Recode des clusters : "Culturel sans tourisme" ou "Communes non sélectionnées"
    carte_mayotte$cluster_affichee <- case_when(
      carte_mayotte$cluster == "Culturel sans tourisme" ~ "Culturel sans tourisme",
      TRUE ~ "Communes non sélectionnées"
    )
    
    carte_mayotte$cluster_affichee <- factor(
      carte_mayotte$cluster_affichee,
      levels = c("Culturel sans tourisme", "Communes non sélectionnées")
    )
    
    ggplot(carte_mayotte) +
      geom_sf(aes(fill = cluster_affichee), color = "white", size = 0.2) +
      scale_fill_manual(
        name = "Typologie à Mayotte",
        values = c(
          "Culturel sans tourisme" = "#f07b6f",
          "Communes non sélectionnées" = "lightgrey"
        )
      ) +
      labs(
        title = "Typologie des communes de Mayotte",
        caption = "Source : INSEE + clustering (traitement personnel)"
      ) +
      theme_minimal()
  })
  
  # Table de la base de données filtrée
  output$data_table <- renderDT({
    req(data_reactive())
    result <- data_reactive()
    
    # Filtrage par région
    if(length(input$region) > 0 && !"" %in% input$region) {
      result <- result %>% filter(Région %in% input$region)
    }
    
    # Afficher la table avec DT
    datatable(result, options = list(pageLength = 10))
  })
  
  # Téléchargement de la base de données filtrée
  output$download_data <- downloadHandler(
    filename = function() {
      paste("base_de_donnees_filtrée_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(data_reactive())
      result <- data_reactive()
      
      # Filtrage par région avant téléchargement
      if(length(input$region) > 0 && !"" %in% input$region) {
        result <- result %>% filter(Région %in% input$region)
      }
      
      # Sauvegarder le fichier CSV
      write.csv(result, file, row.names = FALSE)
    }
  )
}

# Lancer l'application
shinyApp(ui = ui, server = server)
