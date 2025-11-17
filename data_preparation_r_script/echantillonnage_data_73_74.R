set.seed(123)  # Pour avoir toujours le même échantillon

setwd("C:/Users/UR82707255/Documents/GitHub/iut_sd2_rshiny_enedis")

# Chargement des données
df_existants <- read.csv("data/data_dpe_2_savoies_existants.csv", sep = ";", dec = ".", fileEncoding = "UTF-8")
df_neufs      <- read.csv("data/data_dpe_2_savoies_neufs.csv",     sep = ";", dec = ".", fileEncoding = "UTF-8")

# Taille de l’échantillon (ex : 10%)
n_existants <- round(0.10 * nrow(df_existants))
n_neufs     <- round(0.10 * nrow(df_neufs))

# Échantillonnage
ech_existants <- df_existants[sample(nrow(df_existants), n_existants), ]
ech_neufs     <- df_neufs[sample(nrow(df_neufs), n_neufs), ]

# Sauvegarde dans data/
write.csv(ech_existants, "data/echantillon_existants.csv", row.names = FALSE)
write.csv(ech_neufs,     "data/echantillon_neufs.csv",     row.names = FALSE)
