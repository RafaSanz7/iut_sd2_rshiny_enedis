install.packages(c("httr", "jsonlite", "dplyr"))
library(httr)
library(jsonlite)
library(dplyr)

# --- Dossier de travail ---

setwd("L:/BUT/SD/Promo 2024/rsanz/but_2/R")

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
base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/lines"

# --- DataFrame final ---
df_final <- data.frame()

# --- Boucle principale ---

for (code_commune_insee in codes_insee) {
  
  message("⏳ Traitement du code INSEE : ", code_commune_insee)
  
  page <- 1
  has_data <- TRUE
  
  while (has_data) {
    
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
        "conso_5_usages_ep",
        "conso_5_usages_ef",
        "cout_total_5_usages",
        "cout_chauffage",
        "surface_habitable_logement",
        "type_batiment",
        "periode_construction",
        "annee_construction",
        "type_installation_chauffage",
        "type_energie_principale_chauffage",
        "date_reception_dpe",
        sep = ","
      ),
      qs = paste0('code_insee_ban:"', code_commune_insee, '"')
    )
    
    # --- Construction de l'URL ---
    url_encoded <- paste0(
      base_url,
      "?page=", params$page,
      "&size=", params$size,
      "&select=", URLencode(params$select),
      "&qs=", URLencode(params$qs)
    )
    
    # --- Requête API ---
    response <- GET(url_encoded)
    
    # --- Vérification du code HTTP ---
    if (status_code(response) == 200) {
      content <- fromJSON(rawToChar(response$content), flatten = TRUE)
      
      if (!is.null(content$result) && nrow(content$result) > 0) {
        df <- as.data.frame(content$result)
        df_final <- bind_rows(df_final, df)
        
        message("✅ Page ", page, " récupérée (", nrow(df), " lignes)")
        page <- page + 1
        Sys.sleep(0.3)
      } else {
        message("📭 Aucune donnée supplémentaire pour ", code_commune_insee)
        has_data <- FALSE
      }
    } else {
      warning("⚠️ Erreur ", status_code(response), " pour ", code_commune_insee)
      has_data <- FALSE
    }
  }
  
  message("✅ Fin du code INSEE : ", code_commune_insee)
}

# --- Visualiser et sauvegarder ---
View(df_final)
write.csv(df_final, "resultats_dpe_hautesavoie.csv", row.names = FALSE)
