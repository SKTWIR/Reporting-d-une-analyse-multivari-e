library(shiny)
library(readxl)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(cluster)
library(stringr)
library(sf)
library(leaflet)
library(plotly)

function(input, output, session) {
  
  data_processed <- reactive({
    url <- "https://github.com/SKTWIR/Reporting-d-une-analyse-multivari-e/raw/main/Donn%C3%A9es%20ville2.xlsx"
    temp_file <- tempfile(fileext = ".xlsx")
    download.file(url, destfile = temp_file, mode = "wb")
    df <- read_excel(temp_file)
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
    df$LIBGEO <- str_to_upper(df$LIBGEO)
    
    acp <- PCA(vars_score, graph = FALSE)
    coord <- as.data.frame(acp$ind$coord[, 1:2])
    result <- cbind(df, coord)
    
    return(result)
  })
  
  # ACP interactive filtrée par cluster
  output$plot_acp <- renderPlotly({
    result <- data_processed()
    
    # Appliquer le filtre si sélection multiple ou simple
    filtered_data <- if (is.null(input$selected_clusters) || "Toutes" %in% input$selected_clusters) {
      result
    } else {
      result %>% filter(cluster %in% input$selected_clusters)
    }
    
    p <- ggplot(filtered_data, aes(x = Dim.1, y = Dim.2, color = cluster, text = LIBGEO)) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(
        title = "ACP - Typologie des communes",
        x = "Composante principale 1",
        y = "Composante principale 2",
        color = "Typologie"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$carte_metro_interactive <- renderLeaflet({
    result <- data_processed()
    
    communes_sf <- st_read("https://france-geojson.gregoiredavid.fr/repo/communes.geojson", quiet = TRUE)
    communes_sf$nom <- str_to_upper(communes_sf$nom)
    
    carte_clusters <- communes_sf %>%
      left_join(result, by = c("nom" = "LIBGEO"))
    
    carte_clusters$cluster_affichee <- case_when(
      carte_clusters$cluster == "Tourisme et culture" ~ "Tourisme et culture",
      carte_clusters$cluster == "Peu touristique et peu culturel" ~ "Peu touristique et peu culturel",
      TRUE ~ NA_character_
    )
    
    carte_clusters <- carte_clusters %>% filter(!is.na(cluster_affichee))
    
    palette <- colorFactor(
      palette = c(
        "Tourisme et culture" = "#4B9CD3",
        "Peu touristique et peu culturel" = "#56B870"
      ),
      domain = carte_clusters$cluster_affichee
    )
    
    leaflet(carte_clusters) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~palette(cluster_affichee),
        weight = 0.3,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0(nom, " - ", cluster_affichee),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = palette,
        values = ~cluster_affichee,
        opacity = 0.7,
        title = "Typologie",
        position = "bottomright"
      )
  })
}
