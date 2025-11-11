# --- Installation et chargement des packages ---
install.packages(c("httr", "jsonlite", "dplyr"))
library(httr)
library(jsonlite)
library(dplyr)

# --- Dossier de travail ---
setwd("C:/Users/UR82707255/Documents/R")

# --- Lecture du CSV ---
df_savoie <- read.csv(
  file = "data_Savoie.csv",
  header = TRUE,
  sep = ";",
  dec = ".",
  fileEncoding = "UTF-8"
)

# --- Codes INSEE uniques ---
codes_insee <- unique(df_savoie$code_commune_insee)

# --- URL de base ---
base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/lines"

# --- DataFrame final ---
df_final_savoie <- data.frame()

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
      
      # --- Paramètres de la requête ---
      params <- list(
        page = page,
        size = 9999,
        select = paste(
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
          "periode_construction",
          "annee_construction",
          "type_installation_chauffage",
          "type_energie_principale_chauffage",
          "date_reception_dpe",
          sep = ","
        ),
        qs = qs_filter
      )
      
      # --- Construction manuelle de l’URL ---
      url_encoded <- paste0(
        base_url,
        "?page=", params$page,
        "&size=", params$size,
        "&select=", URLencode(params$select),
        "&qs=", URLencode(params$qs)
      )
      
      # --- Requête API avec timeout de sécurité ---
      response <- GET(url_encoded, timeout(30))
      
      # --- Vérification du code HTTP ---
      if (status_code(response) == 200) {
        content <- fromJSON(rawToChar(response$content), flatten = TRUE)
        
        if (!is.null(content$result) && is.data.frame(content$result) && nrow(content$result) > 0) {
          df <- as.data.frame(content$result)
          df_final_savoie <- bind_rows(df_final_savoie, df)
          
          message("✅ ", nrow(df), " lignes récupérées pour ", year)
          page <- page + 1
          Sys.sleep(0.3)
        } else {
          message("Aucune donnée supplémentaire ou format inattendu pour ", code_commune_insee, " en ", year)
          has_data <- FALSE
        }
        
      } else {
        # --- Gestion des erreurs HTTP ---
        if (status_code(response) != 400) {
          warning("⚠️ Erreur ", status_code(response), " pour ", code_commune_insee, " en ", year)
        } else {
          message("ℹ️ Fin des données pour ", code_commune_insee, " en ", year, " (erreur 400 ignorée)")
        }
        has_data <- FALSE  # indispensable pour casser la boucle
      }
    }
  }
  
  message("✅ Fin du code INSEE : ", code_commune_insee, 
          " | Total cumulé : ", nrow(df_final_savoie), " lignes.")
  
  # --- Sauvegarde intermédiaire pour éviter la perte de données ---
  write.csv(df_final_savoie, "resultats_dpe_savoie_existants.csv", row.names = FALSE)
}

# --- Résumé final ---
message("🟢 Extraction terminée : ", nrow(df_final_savoie), " lignes au total.")
View(df_final_savoie)
