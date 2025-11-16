# --- Installation et chargement des packages ---
# install.packages(c("httr", "jsonlite", "dplyr"))
library(httr)
library(jsonlite)
library(dplyr)

# --- URL de base de l'API ADEME ---
base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/lines"

# --- DataFrame pour stocker tous les résultats ---
df_final <- data.frame()

# --- Paramètres de la boucle ---
years <- 2021:2025         # Années à parcourir
departments <- c("73", "74")  # 73 = Savoie, 74 = Haute-Savoie

# --- Boucle principale par département ---
for (dept in departments) {
  
  message("\n=============================================")
  message("⏳ Traitement du département : ", dept)
  message("=============================================")
  
  # --- Boucle imbriquée par année ---
  for (year in years) {
    
    message("📅 Année : ", year)
    
    page <- 1
    has_data <- TRUE
    
    # --- Boucle de pagination ---
    while (has_data) {
      
      # --- Filtrage par département et année ---
      qs_filter <- paste0(
        'code_departement_ban:"', dept, '"',
        ' AND date_reception_dpe:[', year, '-01-01 TO ', year, '-12-31]'
      )
      
      # --- Paramètres de la requête ---
      params <- list(
        page = page,
        size = 9999,  # Taille maximale pour limiter le nombre d’appels
        select = paste(
          "numero_dpe",
          "identifiant_ban",
          sep = ","
        ),
        qs = qs_filter
      )
      
      # --- Construction manuelle de l’URL (gère bien les caractères spéciaux) ---
      url_encoded <- paste0(
        base_url,
        "?page=", params$page,
        "&size=", params$size,
        "&select=", URLencode(params$select),
        "&qs=", URLencode(params$qs)
      )
      
      # --- Requête API avec timeout ---
      response <- GET(url_encoded, timeout(30))
      
      # --- Vérification du code HTTP ---
      if (status_code(response) == 200) {
        content <- fromJSON(rawToChar(response$content), flatten = TRUE)
        
        if (!is.null(content$result) && is.data.frame(content$result) && nrow(content$result) > 0) {
          
          df <- as.data.frame(content$result)
          df_final <- bind_rows(df_final, df)
          
          message("✅ ", nrow(df), " lignes récupérées pour ", dept, " / ", year, " (page ", page, ")")
          page <- page + 1
          Sys.sleep(0.3)  # Petite pause pour ne pas surcharger l’API
          
        } else {
          message("ℹ️ Aucune donnée supplémentaire pour ", dept, " en ", year, " (page ", page, ")")
          has_data <- FALSE
        }
        
      } else {
        # --- Gestion des erreurs HTTP ---
        if (status_code(response) != 400) {
          warning("⚠️ Erreur ", status_code(response), " pour ", dept, " en ", year, " à la page ", page)
        } else {
          message("ℹ️ Fin des données (erreur 400) pour ", dept, " en ", year, " à la page ", page)
        }
        has_data <- FALSE
      }
    } # fin while
  } # fin for (year)
  
  message("✅ Fin du département : ", dept,
          " | Total cumulé : ", nrow(df_final), " lignes.")
  
  # --- Sauvegarde intermédiaire après chaque département ---
  write.csv(
    df_final, 
    "resultats_dpe_identifiant_ban_73_74.csv", 
    row.names = FALSE, 
    fileEncoding = "UTF-8"
  )
}

# --- Résumé final ---
message("🟢 Extraction terminée : ", nrow(df_final), " lignes au total.")
message("Fichier sauvegardé : resultats_dpe_identifiant_ban_73_74.csv")

# --- Affichage des résultats ---
View(df_final)
