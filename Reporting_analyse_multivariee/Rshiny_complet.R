library(shiny)
library(readxl)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(cluster)
library(stringr)
library(sf)
library(DT)

# UI
ui <- fluidPage(
  titlePanel("Analyse touristique et socio-culturelle"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Sélectionnez les données"),
      fileInput("file", "Importer un fichier Excel", accept = ".xlsx"),
      hr(),
      selectInput("region", "Choisissez une ou plusieurs régions", choices = NULL, selected = NULL, multiple = TRUE),
      actionButton("generate", "Générer les résultats"),
      downloadButton("download_data", "Télécharger la base filtrée"),
      tags$hr(),
      tags$p("Télécharger le rapport d'analyse :"),
      tags$a(href = "https://www.dropbox.com/scl/fi/mueypzcc23ij55vlmcpun/Groupe15_NKOGHE_HOUY_RAHMANI.pdf?rlkey=4k9qbaie0fwrs3bs7t4x4dnv9&st=ab2f61an&dl=1", "Télécharger le fichier PDF", target = "_blank")
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
        tabPanel("Synthèse Régionale", DTOutput("synthese_table")),
        tabPanel("ACP - Graphique",
                 plotOutput("acp_typologie", height = "600px"),
                 plotOutput("acp_biplot", height = "600px"),
                 plotOutput("scatter_plot", height = "600px")
        ),
        tabPanel("Carte Métropole", plotOutput("carte_metro", height = "950px", width = "100%")),
        tabPanel("Carte Mayotte", plotOutput("carte_mayotte", height = "800px")),
        tabPanel("Base de Données", DTOutput("data_table")),
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  data_reactive <- eventReactive(input$generate, {
    req(input$file)
    df <- read_excel(input$file$datapath)
    df <- df[, c(3, 5, 14:29)]
    
    tourisme_vars <- df %>%
      select(NB_G101, NB_G102, NB_G103, NB_G104) %>%
      mutate_all(~replace(., is.na(.), 0))
    
    culture_vars <- df %>% 
      select(NB_F303, NB_F303_NB_SALLES, NB_F305, NB_F307, NB_F315, NB_F315_NB_SALLES) %>%
      mutate_all(~replace(., is.na(.), 0))
    
    df$score_tourisme <- rowSums(scale(tourisme_vars))
    df$score_culture  <- rowSums(scale(culture_vars))
    
    vars_score <- df %>% select(score_tourisme, score_culture)
    set.seed(42)
    km <- kmeans(scale(vars_score), centers = 4, nstart = 25)
    df$cluster_num <- km$cluster
    
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
    
    acp <- PCA(vars_score, graph = FALSE)
    coord <- as.data.frame(acp$ind$coord[, 1:2])
    result <- cbind(df, coord)
    
    result$LIBGEO <- str_to_upper(result$LIBGEO)
    return(result)
  })
  
  observe({
    req(data_reactive())
    result <- data_reactive()
    regions <- unique(result$Région)
    excluded_regions <- c("Guadeloupe", "Martinique", "Guyane", "La Réunion", "Mayotte", "Saint-Pierre-et-Miquelon", "Saint-Barthélemy", "Saint-Martin")
    regions <- regions[!regions %in% excluded_regions]
    updateSelectInput(session, "region", choices = c("Tout sélectionner" = "", regions), selected = NULL)
  })
  
  output$scatter_plot <- renderPlot({
    req(data_reactive())
    result <- data_reactive()
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
        "Hauts-de-France" = "#009E73", 
        "Provence-Alpes-Côte d'Azur" = "#f07b6f", 
        "Occitanie" = "#CC79A7", 
        "Grand Est" = "#D55E00", 
        "Normandie" = "#E69F00", 
        "Nouvelle-Aquitaine" = "#009E73",
        "Bretagne" = "#F0E442",  
        "Île-de-France" = "#9966CC"  
      )) +
      theme(legend.position = "bottom")
  })
  
  output$acp_biplot <- renderPlot({
    req(data_reactive())
    result <- data_reactive()
    if(length(input$region) > 0 && !"" %in% input$region) {
      result <- result %>% filter(Région %in% input$region)
    }
    fviz_pca_biplot(PCA(result[, c("score_tourisme", "score_culture")], graph = FALSE), 
                    axes = c(1, 2), 
                    col.var = "blue", 
                    col.ind = result$cluster, 
                    palette = "jco", 
                    addEllipses = TRUE, 
                    label = "var")
  })
  
  output$acp_typologie <- renderPlot({
    req(data_reactive())
    result <- data_reactive()
    
    # Appliquer le filtre de région
    if (length(input$region) > 0 && !"" %in% input$region) {
      result <- result %>% filter(Région %in% input$region)
    }
    
    ggplot(result, aes(x = Dim.1, y = Dim.2, color = cluster)) +
      geom_point(size = 2) +
      theme_minimal() +
      scale_color_manual(values = c("Tourisme et culture" = "#4B9CD3", 
                                    "Peu touristique et peu culturel" = "#56B870", 
                                    "Culturel sans tourisme" = "#f07b6f", 
                                    "Touristique sans culture" = "#56B870")) +
      labs(title = "ACP - Typologie des communes selon attractivité touristique et socio-culturelle") +
      theme(legend.position = "bottom")
  })
  
  output$carte_metro <- renderPlot({
    req(data_reactive())
    result <- data_reactive()
    if(length(input$region) > 0 && !"" %in% input$region) {
      result <- result %>% filter(Région %in% input$region)
    }
    communes_sf <- st_read("https://france-geojson.gregoiredavid.fr/repo/communes.geojson", quiet = TRUE)
    communes_sf <- communes_sf %>% filter(!str_detect(nom, "^(Guadeloupe|Martinique|Guyane|La Réunion|Mayotte|Saint-Pierre-et-Miquelon|Saint-Barthélemy|Saint-Martin)$"))
    communes_sf$nom <- str_to_upper(communes_sf$nom)
    carte_clusters <- communes_sf %>% left_join(result, by = c("nom" = "LIBGEO"))
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
      theme_minimal()
  })
  
  output$carte_mayotte <- renderPlot({
    req(data_reactive())
    result <- data_reactive()
    communes_my <- st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions/mayotte/communes-mayotte.geojson", quiet = TRUE)
    communes_my$nom <- str_to_upper(communes_my$nom)
    carte_mayotte <- communes_my %>% left_join(result, by = c("nom" = "LIBGEO"))
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
  
  output$data_table <- renderDT({
    req(data_reactive())
    result <- data_reactive()
    if(length(input$region) > 0 && !"" %in% input$region) {
      result <- result %>% filter(Région %in% input$region)
    }
    datatable(result, options = list(pageLength = 10))
  })
  
  output$synthese_table <- renderDT({
    req(data_reactive())
    result <- data_reactive()
    if (length(input$region) > 0 && !"" %in% input$region) {
      result <- result %>% filter(Région %in% input$region)
    }
    vars_interet <- c("NB_G101", "NB_G102", "NB_G103", "NB_G104", 
                      "NB_F303", "NB_F303_NB_SALLES", "NB_F305", "NB_F307", 
                      "NB_F311", "NB_F312", "NB_F313", "NB_F314", 
                      "NB_F315", "NB_F315_NB_SALLES")
    tableau <- result %>%
      select(Région, all_of(vars_interet)) %>%
      group_by(Région) %>%
      summarise(
        `Nombre de communes` = n(),
        across(all_of(vars_interet), ~sum(.x, na.rm = TRUE))
      ) %>%
      arrange(Région)
    datatable(tableau, options = list(pageLength = 10))
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("base_de_donnees_filtrée_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(data_reactive())
      result <- data_reactive()
      if(length(input$region) > 0 && !"" %in% input$region) {
        result <- result %>% filter(Région %in% input$region)
      }
      write.csv(result, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
