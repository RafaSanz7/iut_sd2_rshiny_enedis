# 1. Charger le fichier BAN complet

setwd(dir = "C:/Users/UR82707255/Documents/GitHub/iut_sd2_rshiny_enedis/")

df <- read.csv(
  "data/adresses_73-74.csv",
  sep = ";",
  dec = ".",
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)

# 2. Filtrer uniquement les départements 73 et 74
df <- subset(df, code_postal >= 73000 & code_postal < 75000)

# 3. Correction des encodages (facultatif mais propre)
df$type_position <- enc2utf8(df$type_position)
df$nom_voie <- enc2utf8(df$nom_voie)
df$nom_commune <- enc2utf8(df$nom_commune)

# 4. Échantillonner 10 000 adresses
set.seed(123)
n <- min(10000, nrow(df))  # sécurité
df_sample <- df[sample(nrow(df), n), ]

# 5. Sauvegarde de l'échantillon
write.csv(df_sample,
          "data/adresses_echantillon_10000.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

cat("✅ Fichier créé : data/adresses_echantillon_10000.csv\n")
cat("Nombre de lignes dans l'échantillon :", n, "\n")
