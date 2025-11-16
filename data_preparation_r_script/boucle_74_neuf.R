# --- Installation et chargement des packages ---
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("dplyr")) install.packages("dplyr")

library(httr)
library(jsonlite)
library(dplyr)

# --- Dossier de travail ---
setwd("C:/Users/UR82707255/Documents/R")

# --- Lecture du CSV ---
df_hs <- read.csv(
  file = "data_HauteSavoie.csv",
  header = TRUE,
  sep = ";",
  dec = ".",
  fileEncoding = "UTF-8"
)

# --- Codes INSEE uniques ---
codes_insee <- unique(df_hs$code_commune_insee)

# --- URL de base ---
base_url_neuf <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe02neuf/lines"

# --- DataFrame final ---
df_final_neufs_hs <- data.frame()

# --- Liste des années à parcourir ---
years <- 2021:2025

# --- Boucle principale ---
for (code_commune_insee in codes_insee) {
  
  message("\n⏳ Traitement du code INSEE : ", code_commune_insee)
  
  for (year in years) {
    
    message("📅 Année : ", year)
    
    page <- 1
    has_data <- TRUE
    
    while (has_data) {
      
      # --- Filtrage par code INSEE + année ---
      qs_filter <- paste0(
        'code_insee_ban:"', code_commune_insee, 
        '" AND date_reception_dpe:[', year, '-01-01 TO ', year, '-12-31]'
      )
      
      # --- Liste des champs à récupérer (mêmes que pour "existant", sauf ceux absents du dataset neuf) ---
      select_fields <- paste(
        "numero_dpe",
        "code_postal_ban",
        "code_insee_ban",
        "etiquette_dpe",
        "etiquette_ges",
        "cout_total_5_usages",
        "cout_chauffage",
        "cout_ecs",
        "cout_refroidissement",
        "cout_eclairage",
        "cout_auxiliaires",
        "emission_ges_chauffage",
        "emission_ges_ecs",
        "emission_ges_5_usages",
        "surface_habitable_logement",
        "type_batiment",
        "type_installation_chauffage",
        "type_energie_principale_chauffage",
        "date_reception_dpe",
        sep = ","
      )
      
      # --- Construction de l’URL ---
      url_encoded <- paste0(
        base_url_neuf,
        "?page=", page,
        "&size=9999",
        "&select=", URLencode(select_fields),
        "&qs=", URLencode(qs_filter)
      )
      
      # --- Requête API ---
      response <- GET(url_encoded, timeout(30))
      
      if (status_code(response) == 200) {
        content <- fromJSON(rawToChar(response$content), flatten = TRUE)
        
        if (!is.null(content$results) && is.data.frame(content$results) && nrow(content$results) > 0) {
          df <- as.data.frame(content$results)
          
          # Ajout du code INSEE et de l'année pour suivi
          df$code_insee_source <- code_commune_insee
          df$annee_recherche <- year
          
          df_final_neufs_hs <- bind_rows(df_final_neufs_hs, df)
          
          message("✅ ", nrow(df), " lignes récupérées pour ", code_commune_insee, " en ", year)
          page <- page + 1
          Sys.sleep(0.3)
        } else {
          message("ℹ️ Aucune donnée supplémentaire pour ", code_commune_insee, " en ", year)
          has_data <- FALSE
        }
        
      } else {
        # --- Gestion des erreurs HTTP ---
        if (status_code(response) != 400) {
          warning("⚠️ Erreur HTTP ", status_code(response), " pour ", code_commune_insee, " en ", year)
        } else {
          message("ℹ️ Fin des données pour ", code_commune_insee, " en ", year, " (erreur 400 ignorée)")
        }
        has_data <- FALSE
      }
    }
  }
  
  message("✅ Fin du code INSEE : ", code_commune_insee, 
          " | Total cumulé : ", nrow(df_final_neufs_hs), " lignes.")
  
  # --- Sauvegarde intermédiaire ---
  write.csv(df_final_neufs_hs, "resultats_dpe_HauteSavoie_neufs.csv", row.names = FALSE)
}

# --- Résumé final ---
message("🟢 Extraction terminée : ", nrow(df_final_neufs_hs), " lignes au total.")
View(df_final_neufs_hs)
