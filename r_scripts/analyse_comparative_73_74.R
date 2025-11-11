install.packages("ggplot2")
library(ggplot2)
library(scales)

install.packages("corrplot")
library("corrplot")


setwd(dir = "C:/Users/UR82707255/Documents/R")

df_existants = read.csv(file = "data_dpe_2_savoies_existants.csv",
                        header = TRUE,
                        sep = ";",
                        dec = ".",
                        fileEncoding = "UTF-8")


df_neufs = read.csv(file = "data_dpe_2_savoies_neufs.csv",
                    header = TRUE,
                    sep = ";",
                    dec = ".",
                    fileEncoding = "UTF-8")

dim(df_existants)
dim(df_neufs)



df_neufs$logement <- "neuf"
df_neufs$anne_construction <- Sys.Date()
df_neufs$annee_construction <- as.numeric(format(Sys.Date(), "%Y"))
df_existants$logement <- "ancien"


colnames_neufs = colnames(df_neufs)
colnames_existants = colnames(df_existants)

colonnes_communes = intersect(colnames_neufs, colnames_existants)

df = rbind(df_neufs[ , colonnes_communes],
           df_existants[ , colonnes_communes])

dim(df)
class(df$date_reception_dpe)
df$date_reception_dpe <- as.Date(df$date_reception_dpe, format = "%d/%m/%Y")
df$date_reception_dpe <- as.numeric(format(df$date_reception_dpe, "%Y"))


df$departement <- ifelse(substr(df$code_insee_ban, 1, 2) == "73", "73", 
                           ifelse(substr(df$code_insee_ban, 1, 2) == "74", "74", NA))

summary(df)

df$cout_chauffage <- as.numeric(df$cout_chauffage)
df$cout_ecs <- as.numeric(df$cout_ecs)
df$cout_refroidissement <- as.numeric(df$cout_refroidissement)
df$cout_eclairage <- as.numeric(df$cout_eclairage)
df$cout_auxiliaires <- as.numeric(df$cout_auxiliaires)
df$cout_total_5_usages <- as.numeric(df$cout_total_5_usages)
df$emission_ges_chauffage <- as.numeric(df$emission_ges_chauffage)
df$emission_ges_ecs <- as.numeric(df$emission_ges_ecs)
df$emission_ges_5_usages <- as.numeric(df$emission_ges_5_usages)

df$sum_cout_tot_5_usages = df$cout_chauffage + df$cout_eclairage + df$cout_ecs + df$cout_refroidissement + df$cout_auxiliaires

df$ecart_cout_tot_5_usages = df$cout_total_5_usages - df$sum_cout_tot_5_usages
df$ecart_cout_tot_5_usages <- NULL 



# comparaison chiffrée des 2 Savoies 


df$cout_ecs_pourcentage <- round(df$cout_ecs / df$cout_total_5_usages*100,2)
df$cout_chauffage_pourcentage <- round(df$cout_chauffage / df$cout_total_5_usages* 100,2)
df$cout_refroidissement_pourcentage <- round(df$cout_refroidissement / df$cout_total_5_usages * 100, 2)
df$cout_eclairage_pourcentage <- round(df$cout_eclairage / df$cout_total_5_usages * 100, 2)
df$cout_auxiliaires_pourcentage <- round(df$cout_auxiliaires / df$cout_total_5_usages * 100, 2)

moyenne_part_refroidissement_par_dept <- aggregate(cout_refroidissement_pourcentage ~ departement, data = df, FUN = mean, na.rm = TRUE)
moyenne_part_eclairage_par_dept <- aggregate(cout_eclairage_pourcentage ~ departement, data = df, FUN = mean, na.rm = TRUE)
moyenne_part_auxiliaires_par_dept <- aggregate(cout_auxiliaires_pourcentage ~ departement, data = df, FUN = mean, na.rm = TRUE)
moyenne_part_chauffage_par_dept = aggregate(cout_chauffage_pourcentage ~ departement,data = df, FUN = mean, na.rm = TRUE)
moyenne_part_ecs_par_dept = aggregate(cout_ecs_pourcentage ~ departement,data = df, FUN = mean, na.rm = TRUE)

moyenne_part_chauffage_par_dept
moyenne_part_ecs_par_dept
moyenne_part_refroidissement_par_dept
moyenne_part_eclairage_par_dept
moyenne_part_auxiliaires_par_dept


df$ges_chauffage_pourcentage <- round(df$emission_ges_chauffage / df$emission_ges_5_usages * 100, 2)
df$ges_ecs_pourcentage <- round(df$emission_ges_ecs / df$emission_ges_5_usages * 100, 2)
moyenne_part_ges_chauffage_par_dept <- aggregate(ges_chauffage_pourcentage ~ departement, data = df, FUN = mean, na.rm = TRUE)
moyenne_part_ges_ecs_par_dept <- aggregate(ges_ecs_pourcentage ~ departement, data = df, FUN = mean, na.rm = TRUE)

moyenne_part_ges_chauffage_par_dept
moyenne_part_ges_ecs_par_dept

moyenne_couts_totaux <- aggregate(cout_total_5_usages ~ departement,
                                  data = df,
                                  FUN = mean,
                                  na.rm = TRUE)

#   Graphique comparatif
bar_positions <- barplot(
  moyenne_couts_totaux$cout_total_5_usages,
  names.arg = moyenne_couts_totaux$departement,
  col = c("steelblue", "darkseagreen3"),
  main = "Comparaison du coût énergétique moyen par département",
  ylab = "Coût total moyen (€)",
  xlab = "Département",
  ylim = c(0, max(moyenne_couts_totaux$cout_total_5_usages) * 1.2)
)

#  Ajouter les valeurs au-dessus des barres
text(
  x = bar_positions,
  y = moyenne_couts_totaux$cout_total_5_usages,
  labels = round(moyenne_couts_totaux$cout_total_5_usages, 1),
  pos = 3,
  cex = 0.9,
  font = 2,
  col = c("navy", "darkgreen")  # texte lisible sur fond clair/foncé
)


df$periode_construction = cut(x = df$annee_construction,
                              breaks = c(0,1960,1970,1980,1990,2000,2010,2050),
                              labels = c("Avant 1960",
                                         "1961 - 1970",
                                         "1971 - 1980",
                                         "1981 - 1990",
                                         "1991 - 2000",
                                         "2001 - 2010",
                                         "Après 2010"))

moyenne_periode_construction <- aggregate(periode_construction ~ departement, 
                                          data = df, 
                                          FUN = mean, 
                                          na.rm = TRUE)




# REPARTION MOYENNE DES POSTES DE CONSOMMATION (73 / 74)

# Calcul des moyennes par département pour les 4 postes choisis
moyenne_postes <- aggregate(cbind(cout_chauffage_pourcentage,
                                  cout_ecs_pourcentage,
                                  cout_eclairage_pourcentage,
                                  cout_auxiliaires_pourcentage) ~ departement,
                            data = df,
                            FUN = mean,
                            na.rm = TRUE)

# Noms des postes
postes <- c("Chauffage", "ECS", "Éclairage", "Auxiliaires")

# Préparation pour le graphique
matrice_postes <- t(as.matrix(moyenne_postes[ , 2:5]))
colnames(matrice_postes) <- moyenne_postes$departement
rownames(matrice_postes) <- postes

matrice_postes

# Graphique avec une échelle jusqu'à 100 %
barplot_postes <- barplot(matrice_postes,
                         beside = TRUE,
                         col = c("steelblue", "darkorange", "gold", "darkseagreen3"),
                         legend.text = TRUE,
                         args.legend = list(title = "Département", x = "topright"),
                         main = "Répartition moyenne des postes de consommation",
                         ylab = "Pourcentage du coût total (%)",
                         ylim = c(0, max(matrice_postes) * 1.1))

# Ajout des valeurs au-dessus des barres
text(x = barplot_postes,
     y = matrice_postes,
     labels = round(matrice_postes, 1),
     pos = 3,
     cex = 0.8,
     font = 2,
     col = "black")




moyenne_ges <- aggregate(cbind(ges_chauffage_pourcentage,
                               ges_ecs_pourcentage) ~ departement,
                         data = df,
                         FUN = mean,
                         na.rm = TRUE)

matrice_ges <- t(as.matrix(moyenne_ges[ , 2:3]))
colnames(matrice_ges) <- moyenne_ges$departement
rownames(matrice_ges) <- c("Chauffage", "ECS")

barplot_ges <- barplot(matrice_ges,
        beside = TRUE,
        col = c("indianred3", "orange"),
        legend.text = TRUE,
        args.legend = list(title = "Département", x = "topright"),
        main = "Répartition moyenne des émissions de GES",
        ylab = "Pourcentage des émissions (%)", 
        ylim = c(0, max(matrice_ges)*1.1))


text(x = barplot_ges,
     y = matrice_ges,
     labels = round(matrice_ges, 1),
     pos = 3,
     cex = 0.8,
     font = 2,
     col = "black")


################################################################################

summary(df)


df$etiquette_dpe = as.character(df$etiquette_dpe)

proportion_DPE = prop.table(table(df$etiquette_dpe)) *100
print(proportion_DPE)

################################################################################

# DPE

class(df$etiquette_dpe)
class(df$etiquette_ges)
proportion_DPE = prop.table(table(df$etiquette_dpe)) * 100 

proportion_DPE <- sort(proportion_DPE, decreasing = TRUE)
round(proportion_DPE,2)

df$etiquette_dpe <- as.factor(df$etiquette_dpe)
df$departement <- as.factor(df$departement)

## création d'un tableau DPE 

table_dpe_dept <- as.data.frame(table(df$departement, df$etiquette_dpe))
colnames(table_dpe_dept) <- c("departement", "etiquette_dpe", "effectif")


table_dpe_dept$pourcentage <- ave(
  table_dpe_dept$effectif,
  table_dpe_dept$departement,
  FUN = function(x) round(x / sum(x) * 100, 2)
)
class(table_dpe_dept)

ggplot(table_dpe_dept, aes(x = etiquette_dpe, y = pourcentage, fill = departement)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(pourcentage, "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3) +
  labs(
    title = "Comparaison des étiquettes DPE entre les départements 73 et 74",
    x = "Étiquette DPE",
    y = "Pourcentage (%)",
    fill = "Département"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("steelblue", "darkseagreen3")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# -------------------------------------------------------------------------------------

# Répartition des logements par date_reception_DPE

class(df$date_reception_dpe)

df$date_reception_DPE = NULL
proportion_date_deliv_dpe = round(prop.table(table(df$date_reception_dpe)) * 100, 2)

# 0. Répartition : nombre d'accurences par département / etiquette DPE

table_dpe <- as.data.frame(table(df$departement, df$etiquette_dpe))
colnames(table_dpe) <- c("departement", "etiquette_dpe", "effectif")

table_dpe$pourcentage <- ave(
  table_dpe$effectif, 
  table_dpe$departement,
  FUN = function(x) round(x / sum(x) * 100,2)
)

## Graphe associé 

ggplot(table_dpe, aes(x = etiquette_dpe, y = pourcentage, fill = etiquette_dpe)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ departement) +  # 73 / 74 côte à côte
  geom_text(aes(label = paste0(pourcentage, "%")),
            vjust = -0.3,
            size = 3,
            fontface = "bold",
            color = "black") +
  labs(
    title = "Répartition des étiquettes DPE par département",
    x = "Étiquette DPE",
    y = "Proportion (%)",
    fill = "Étiquette DPE"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c(
    "A" = "#00B050",
    "B" = "#92D050",
    "C" = "#FFFF00",
    "D" = "#FFC000",
    "E" = "#FF9900",
    "F" = "#FF0000",
    "G" = "#C00000"
  )) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 3, hjust = 0.5)
  ) +
  ylim(0, max(table_dpe$pourcentage, na.rm = TRUE) * 1.1)



# Même chose mais avec les etiquettes GES 

table_ges <- as.data.frame(table(df$departement, df$etiquette_ges))
colnames(table_ges) <- c("departement", "etiquette_ges", "effectif")

# 2️⃣ Calculer le pourcentage par département
table_ges$pourcentage <- ave(
  table_ges$effectif,
  table_ges$departement,
  FUN = function(x) round(x / sum(x) * 100, 2)
)

# 3️⃣ Graphique associé
ggplot(table_ges, aes(x = etiquette_ges, y = pourcentage, fill = etiquette_ges)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ departement) +  # Séparation 73 / 74
  geom_text(aes(label = paste0(pourcentage, "%")),
            vjust = -0.3,
            size = 3,
            fontface = "bold",
            color = "black") +
  labs(
    title = "Répartition des étiquettes GES par département",
    x = "Étiquette GES",
    y = "Proportion (%)",
    fill = "Étiquette GES"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c(
    "A" = "#00B050",
    "B" = "#92D050",
    "C" = "#FFFF00",
    "D" = "#FFC000",
    "E" = "#FF9900",
    "F" = "#FF0000",
    "G" = "#C00000"
  )) +
  ylim(0, max(table_ges$pourcentage, na.rm = TRUE) * 1.1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 3, hjust = 0.5)
  )

# -------------------------------------------------------------------------------------
# 1. Répartition : nombre d'accurences par département et type de logement (neuf / ancien)

table_logement <- as.data.frame(table(df$departement, df$logement))
colnames(table_logement) <- c("departement", "logement", "effectif")

table_logement$pourcentage <- ave(
  table_logement$effectif,
  table_logement$departement,
  FUN = function(x) round(x / sum(x) * 100, 2)
)

View(table_logement)


## Graphique associé 

ggplot(table_logement, aes(x = logement, y = pourcentage, fill = departement)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(pourcentage, "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3,
            fontface = "bold",
            color = "black") +
  labs(
    title = "Répartition du type de logement par département",
    x = "Type de logement",
    y = "Proportion (%)",
    fill = "Département"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("steelblue", "darkseagreen3")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # 🔹 Titre centré
    axis.text.x = element_text(angle = 0, vjust = 3, hjust = 0.5)
  ) +
  ylim(0, max(table_logement$pourcentage, na.rm = TRUE) * 1.1)
 

# ---------------------------------------------------------------------------------




# 2. répartition : nombre d'occurrences par département et type d'installation chauffage 
table_chauffage <- as.data.frame(table(df$departement, df$type_installation_chauffage))
colnames(table_chauffage) <- c("departement", "type_installation_chauffage", "effectif")

# Calcul du pourcentage par département
table_chauffage$pourcentage <- ave(
  table_chauffage$effectif,
  table_chauffage$departement,
  FUN = function(x) round(x / sum(x) * 100, 2)
)

View(table_chauffage)



## graphique associé 
ggplot(table_chauffage, aes(x = type_installation_chauffage,
                            y = pourcentage,
                            fill = departement)) +
  geom_bar(stat = "identity", position = "dodge") +
  
  # Ajout des étiquettes au-dessus des barres
  geom_text(aes(label = paste0(pourcentage, "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3,
            fontface = "bold",
            color = "black") +
  
  # Titre et axes
  labs(
    title = "Répartition du type d'installation de chauffage par département",
    x = "Type d'installation",
    y = "Proportion (%)",
    fill = "Département"
  ) +
  
  # Style cohérent avec tes autres graphes
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("steelblue", "darkseagreen3")) +
  
  # Rotation lisible des étiquettes et centrage du titre
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 3, hjust = 0.5)
  ) +
  ylim(0, max(table_chauffage$pourcentage, na.rm = TRUE) * 1.1)


# --------------------------------------------------------------------------------
# 2.BIS Répartition : nb d'occurence par type d'energie par département 


table_energie_chauffage <- as.data.frame(table(df$departement, df$type_energie_principale_chauffage))
colnames(table_energie_chauffage) <- c("departement", "type_energie_chauffage", "effectif")

table_energie_chauffage$pourcentage <- ave(
  table_energie_chauffage$effectif, 
  table_energie_chauffage$departement,
  FUN = function(x) round(x / sum(x) * 100, 2)
)

# Filtrer le Top 5 des énergies les plus utilisées
table_top5 <- subset(
  table_energie_chauffage,
  type_energie_chauffage %in% names(sort(tapply(effectif, type_energie_chauffage, sum), decreasing = TRUE)[1:5])
)

View(table_top5)



# 3. répartition :  nombre d'occurrences par département et type (maison / appartement / immeuble)

table_batiment <- as.data.frame(table(df$departement, df$type_batiment))
colnames(table_batiment) <- c("departement", "type_batiment", "effectif")

table_batiment$pourcentage <- ave(
  table_batiment$effectif,
  table_batiment$departement,
  FUN = function(x) round(x / sum(x) * 100, 2)
)

View(table_batiment)


## GRAPHE ASSOCIE

ggplot(table_batiment, aes(x = type_batiment, y = pourcentage, fill = departement)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(pourcentage, "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3,
            fontface = "bold",
            color = "black") +
  labs(
    title = "Répartition du type de bâtiment par département",
    x = "Type de bâtiment",
    y = "Proportion (%)",
    fill = "Département"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("steelblue", "darkseagreen3")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # 🔹 Titre centré
    axis.text.x = element_text(angle = 0, vjust = 3, hjust = 0.5)
  ) +
  ylim(0, max(table_batiment$pourcentage, na.rm = TRUE) * 1.1)


# --------------------------------------------------------------------------------

# 4. répartition :  nombre d'occurrences par département et année de construction


table_annee_const <- as.data.frame(table(df$departement, df$periode_construction))
colnames(table_annee_const) <- c("departement", "periode_construction", "effectif")

table_annee_const

table_annee_const$pourcentage <- ave(
  table_annee_const$effectif,
  table_annee_const$departement,
  FUN = function(x) round(x / sum(x) * 100, 2)
)

# ---------------------------FONCTION Graphiques----------------------------------

graph_comparatif <- function(data, variable, titre, xlabel) {
  ggplot(data, aes_string(x = variable, y = "pourcentage", fill = "departement")) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) + 
    geom_text(aes(label = paste0(pourcentage, "%")),
              position = position_dodge(width = 0.9),
              vjust = -0.3,
              size = 3,
              fontface = "bold",
              color = "black") +
    labs(
      title = titre,
      x = xlabel,
      y = "Proportion (%)",
      fill = "Département"
    ) +
    theme_minimal(base_size = 14) +
    scale_fill_manual(values = c("steelblue", "darkseagreen3")) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 0, vjust = 3, hjust = 0.5)
    ) +
    ylim(0, max(data$pourcentage, na.rm = TRUE) * 1.1)
}


graph_comparatif(table_top5,"type_energie_chauffage","Top 5 des types d'énergie de chauffage par département","Type d'énergie de chauffage")
graph_comparatif(table_logement, "logement", "Répartition du type de logement par département", "Type de logement")
graph_comparatif(table_batiment, "type_batiment", "Répartition du type de bâtiment par département", "Type de bâtiment")
graph_comparatif(table_chauffage, "type_installation_chauffage", "Répartition du type d'installation de chauffage par département", "Type d'installation")
graph_comparatif(table_annee_const, "periode_construction", "Répartition des périodes des constructions des logements", "Période")

# --------------------------------------------------------------------------------

cout_chauffage_par_dept <- aggregate(cout_chauffage ~ departement, df, mean, na.rm = TRUE)

library(ggplot2)

# Calcul du coût moyen du chauffage par département
cout_chauffage_par_dept <- aggregate(cout_chauffage ~ departement, df, mean, na.rm = TRUE)

# Graphique comparatif
ggplot(cout_chauffage_par_dept, aes(x = factor(departement), y = cout_chauffage, fill = factor(departement))) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(cout_chauffage, 1), " €")),
            vjust = -0.3,
            size = 4,
            fontface = "bold") +
  labs(
    title = "Coût moyen du chauffage par département",
    x = "Département",
    y = "Coût moyen (€)",
    fill = "Département"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("steelblue", "darkseagreen3")) +
  ylim(0, max(cout_chauffage_par_dept$cout_chauffage) * 1.1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

# Corrélation ------------------------------------------------------------------------

# ---- Département 73 ----
df_73 <- subset(df, departement == "73")[, variables]

# ---- Département 74 ----
df_74 <- subset(df, departement == "74")[, variables]

##  Coefficient de corrélation entre la surface habitable du logement et le coût du chauffage

matrice_chauffage_surface_73 <-  cor(df_73$surface_habitable_logement, df_73$cout_chauffage, use = "complete.obs")
matrice_chauffage_surface_74 <- cor(df_74$surface_habitable_logement, df_74$cout_chauffage, use = "complete.obs")


variables <- c("cout_total_5_usages",
               "cout_chauffage",
               "cout_eclairage",
               "cout_ecs",
               "cout_refroidissement",
               "cout_auxiliaires",
               "surface_habitable_logement",
               "emission_ges_5_usages")


matrice_73 <- cor(df_73, use = "complete.obs")

matrice_74 <- cor(df_74, use = "complete.obs")

# ---- affichage côte à côte ----
par(mfrow = c(1, 2))  # 1 ligne, 2 colonnes

corrplot(matrice_73, 
         method = "color", 
         title = "Corrélations - Département 73",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         mar = c(0,0,2,0),
         col = colorRampPalette(c("red", "white", "blue"))(200))

corrplot(matrice_74, 
         method = "color", 
         title = "Corrélations - Département 74",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         mar = c(0,0,2,0),
         col = colorRampPalette(c("red", "white", "blue"))(200))


## corrélation gaz à effet de serre (GES)

vars_ges_focus <- c("emission_ges_5_usages",    # total des émissions
                    "emission_ges_chauffage",    # GES chauffage
                    "emission_ges_ecs",          # GES eau chaude
                    "cout_chauffage",            # coût du chauffage
                    "cout_ecs",                  # coût ECS
                    "surface_habitable_logement",# taille du logement
                    "periode_construction",      # période (convertie en numérique)
                    "type_energie_principale_chauffage")  # type d’énergie

# --- Copie du jeu de données ---
df_ges <- df

# --- Conversion des variables catégorielles en numériques pour la corrélation ---
df_ges$periode_construction <- as.numeric(as.factor(df_ges$periode_construction))
df_ges$type_energie_principale_chauffage <- as.numeric(as.factor(df_ges$type_energie_principale_chauffage))

# --- Création de la matrice ---
mat_ges_focus <- cor(df_ges[, vars_ges_focus], use = "complete.obs")

# --- Corrplot compact et propre ---
corrplot(mat_ges_focus,
         method = "color",
         addCoef.col = "black",
         tl.col = "black",
         tl.cex = 0.8,
         tl.srt = 45,
         mar = c(0, 0, 2, 0),
         col = colorRampPalette(c("firebrick3", "white", "steelblue"))(200),
         title = "Corrélations principales autour des émissions de GES")

# -------------------------------------------------------------------------------------------


# 1️⃣ Filtrer les logements énergivores (D, E, F, G)
df_DPE_etiquettes <- subset(df, etiquette_dpe %in% c("D", "E", "F", "G"))

# 2️⃣ Créer la table par département et étiquette DPE
table_dpe_energie <- as.data.frame(table(df_DPE_etiquettes$departement, df_DPE_etiquettes$etiquette_dpe))
colnames(table_dpe_energie) <- c("departement", "etiquette_dpe", "effectif")

# 3️⃣ Calcul du pourcentage interne à chaque département
table_dpe_energie$pourcentage <- ave(
  table_dpe_energie$effectif,
  table_dpe_energie$departement,
  FUN = function(x) round(x / sum(x) * 100, 2)
)

# 4️⃣ Supprimer toute ligne à 0 (pour enlever A, B, C)
table_dpe_energie <- subset(table_dpe_energie, pourcentage > 0)

# 5️⃣ Graphique propre et cohérent
ggplot(table_dpe_energie, aes(x = etiquette_dpe, y = pourcentage, fill = departement)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(pourcentage, "%")),
            position = position_dodge(width = 0.8),
            vjust = -0.3,
            size = 3.5,
            fontface = "bold",
            color = "black") +
  labs(
    title = "Répartition des logements énergivores (D à G) par département",
    x = "Étiquette DPE",
    y = "Proportion (%)",
    fill = "Département"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("steelblue", "darkseagreen3")) +
  ylim(0, max(table_dpe_energie$pourcentage, na.rm = TRUE) * 1.1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)
  )



# ---------------------------------------------------------

#  Coût moyen du chauffage par étiquette DPE et par département


# 1️⃣ Calcul du coût moyen du chauffage par DPE et par département
q1_4_dept <- aggregate(cout_chauffage ~ etiquette_dpe + departement,
                       data = df,
                       FUN = mean,
                       na.rm = TRUE)

# 2️⃣ Graphique comparatif
ggplot(q1_4_dept, aes(x = etiquette_dpe, y = cout_chauffage, fill = departement)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(cout_chauffage, 1), " €")),
            position = position_dodge(width = 0.8),
            vjust = -0.3,
            size = 3.5,
            fontface = "bold",
            color = "black") +
  labs(
    title = "Coût moyen du chauffage par étiquette DPE et par département",
    x = "Étiquette DPE",
    y = "Coût moyen (€)",
    fill = "Département"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("steelblue", "darkseagreen3")) +
  ylim(0, max(q1_4_dept$cout_chauffage, na.rm = TRUE) * 1.1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)
  )

# ----------------------------------------------------------

# Coût moyen de l’ECS (Eau Chaude Sanitaire) par étiquette DPE et par département


# 1️⃣ Calcul du coût moyen de l’ECS par DPE et département
q_ecs_dept <- aggregate(cout_ecs ~ etiquette_dpe + departement,
                        data = df,
                        FUN = mean,
                        na.rm = TRUE)

# 2️⃣ Graphique comparatif
ggplot(q_ecs_dept, aes(x = etiquette_dpe, y = cout_ecs, fill = departement)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(cout_ecs, 1), " €")),
            position = position_dodge(width = 0.8),
            vjust = -0.3,
            size = 3.5,
            fontface = "bold",
            color = "black") +
  labs(
    title = "Coût moyen de l’ECS (Eau Chaude Sanitaire) par étiquette DPE et par département",
    x = "Étiquette DPE",
    y = "Coût moyen (€)",
    fill = "Département"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("steelblue", "darkseagreen3")) +
  ylim(0, max(q_ecs_dept$cout_ecs, na.rm = TRUE) * 1.1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)
  )

# ------------------------------------------------------------------------------

# Coût total moyen (5 usages) par étiquette DPE et par département


q_total5_dept <- aggregate(cout_total_5_usages ~ etiquette_dpe + departement,
                           data = df,
                           FUN = mean,
                           na.rm = TRUE)

# 2️⃣ Graphique comparatif
ggplot(q_total5_dept, aes(x = etiquette_dpe, y = cout_total_5_usages, fill = departement)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(cout_total_5_usages, 0), "€")),
            position = position_dodge(width = 0.8),
            vjust = -0.3,
            size = 3.5,
            fontface = "bold",
            color = "black") +
  labs(
    title = "Coût total moyen (5 usages) par étiquette DPE et par département",
    x = "Étiquette DPE",
    y = "Coût moyen total (€)",
    fill = "Département"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("steelblue", "darkseagreen3")) +
  ylim(0, max(q_total5_dept$cout_total_5_usages, na.rm = TRUE) * 1.1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)
  )


# ------------------------------------------------------------------------------


# Coût total moyen (5 usages) par période de construction et par département

# 1️⃣ Calcul du coût total moyen par période de construction et par département
q_total5_periode <- aggregate(cout_total_5_usages ~ periode_construction + departement,
                              data = df,
                              FUN = mean,
                              na.rm = TRUE)

# 2️⃣ Graphique comparatif
ggplot(q_total5_periode, aes(x = periode_construction, y = cout_total_5_usages, fill = departement)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(cout_total_5_usages, 1), " €")),
            position = position_dodge(width = 0.8),
            vjust = -0.3,
            size = 3.5,
            fontface = "bold",
            color = "black") +
  labs(
    title = "Coût total moyen (5 usages) par période de construction et par département",
    x = "Période de construction",
    y = "Coût moyen total (€)",
    fill = "Département"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("steelblue", "darkseagreen3")) +
  ylim(0, max(q_total5_periode$cout_total_5_usages, na.rm = TRUE) * 1.1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )



# ------------------------------------------------------------------------------

# Coût moyen du chauffage par période de construction et par département

# 1️⃣ Calcul du coût moyen du chauffage par période de construction et département
q_chauffage_periode <- aggregate(cout_chauffage ~ periode_construction + departement,
                                 data = df,
                                 FUN = mean,
                                 na.rm = TRUE)

# 2️⃣ Graphique comparatif
ggplot(q_chauffage_periode, aes(x = periode_construction, y = cout_chauffage, fill = departement)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(cout_chauffage, 1), " €")),
            position = position_dodge(width = 0.8),
            vjust = -0.3,
            size = 3.5,
            fontface = "bold",
            color = "black") +
  labs(
    title = "Coût moyen du chauffage par période de construction et par département",
    x = "Période de construction",
    y = "Coût moyen du chauffage (€)",
    fill = "Département"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("steelblue", "darkseagreen3")) +
  ylim(0, max(q_chauffage_periode$cout_chauffage, na.rm = TRUE) * 1.1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    
    
  )
    
# -------------------------------------------------------------------------------

# BOXPLOT GES par dept


boxplot(emission_ges_5_usages ~ departement,
        data = df,
        outline = FALSE,
        main = "Distribution des émissions de GES (5 usages) par département",
        ylab = "Émissions de GES (kgCO₂/m²/an)",
        xlab = "Département",
        col = c("steelblue", "darkseagreen3"),
        border = "black")


# -------------------------------------------------------------------------------

# 1️⃣ Nuage de points (surface habitable < 300 m² et coût chauffage < 5000 €)
plot(df$surface_habitable_logement[df$surface_habitable_logement < 300 & df$cout_chauffage < 5000], 
     df$cout_chauffage[df$surface_habitable_logement < 300 & df$cout_chauffage < 5000],
     main = "Relation entre la surface habitable et le coût du chauffage",
     xlab = "Surface habitable (m²)",
     ylab = "Coût du chauffage (€)",
     pch = 16,
     col = "steelblue")

# 2️⃣ Ajout de la droite de régression
abline(lm(cout_chauffage ~ surface_habitable_logement,
          data = df[df$surface_habitable_logement < 300 & df$cout_chauffage < 5000, ]),
       col = "red",
       lwd = 3)

# 3️⃣ Calcul du coefficient de corrélation
cor(df$cout_chauffage, df$surface_habitable_logement, use = "complete.obs")

# ------------------------------------------------------------------------------


# adresse & cartographie 
setwd("C:/Users/UR82707255/Documents/R/adresses")

df_adresses <- read.csv(file = "adresses_73-74.csv",
                        header = TRUE,
                        sep = ";",
                        dec = ".",
                        fileEncoding = "UTF-8")
View(df_adresses)


df_id_ban <- read.csv(file = "resultats_dpe_identifiant_ban_73_74.csv",
                      header = TRUE,
                      sep = ";",
                      dec = ".",
                      fileEncoding = "UTF-8")

View(df_id_ban)


df_final <- merge(df,           
                  df_id_ban,        
                  by = "numero_dpe",
                  all.x = TRUE)     


nrow(df_final)
head(df_final)

View(df_final)

df_join = merge(df_final,
                df_adresses,
                by.x = "identifiant_ban",
                by.y = "id_ban",
                all.x = TRUE)

View(df_join)

write.csv(
  df_join, 
  "df_join", 
  row.names = FALSE, 
  fileEncoding = "UTF-8"
)

# Carte de base

carte <- leaflet(df_carto) %>%
  addTiles() %>%  # Fond de carte OpenStreetMap
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    color = ~palette_dpe(etiquette_dpe),
    radius = 5,
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1,
    popup = ~paste0(
      "<b>Étiquette DPE:</b> ", etiquette_dpe, "<br>",
      "<b>Type:</b> ", type_batiment, "<br>",
      "<b>Année:</b> ", annee_construction, "<br>",
      "<b>Coût total:</b> ", round(cout_total_5_usages, 0), " €<br>",
      "<b>Commune:</b> ", nom_commune
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = palette_dpe,
    values = ~etiquette_dpe,
    title = "Étiquette DPE"
  )

# print carte
carte


# ️ CARTE AVEC FILTRES PAR ÉTIQUETTE DPE (version simplifiée)

df_join$departement <- substr(df_join$code_insee_ban, 1, 2)
unique(df_join$departement)

library(leaflet)

# 1. Filtrer les données avec lon/lat valides
df_carto <- df_join[!is.na(df_join$lon) & !is.na(df_join$lat), ]

cat("✓ Nombre total de logements avec coordonnées :", nrow(df_carto), "\n")

# 2. Filtrer par département
df_73 <- df_carto[df_carto$departement == "73" & !is.na(df_carto$departement), ]
df_74 <- df_carto[df_carto$departement == "74" & !is.na(df_carto$departement), ]

cat("Nombre de logements Savoie (73) :", nrow(df_73), "\n")
cat("Nombre de logements Haute-Savoie (74) :", nrow(df_74), "\n")

# 3. Créer la carte avec filtres par DPE
carte_dpe_filtrable <- leaflet() %>%
  addTiles() %>%
  # DPE A
  addCircleMarkers(
    data = df_73[df_73$etiquette_dpe == "A" & !is.na(df_73$etiquette_dpe), ],
    lng = ~lon, lat = ~lat, color = "#00B050", radius = 5, fillOpacity = 0.7, 
    stroke = TRUE, weight = 1, group = "DPE A",
    popup = ~paste0("<b>73 - DPE A</b><br>", type_batiment, "<br>", annee_construction, "<br>", round(cout_total_5_usages, 0), " €<br>", nom_commune)
  ) %>%
  addCircleMarkers(
    data = df_74[df_74$etiquette_dpe == "A" & !is.na(df_74$etiquette_dpe), ],
    lng = ~lon, lat = ~lat, color = "#00B050", radius = 5, fillOpacity = 0.7, 
    stroke = TRUE, weight = 1, group = "DPE A",
    popup = ~paste0("<b>74 - DPE A</b><br>", type_batiment, "<br>", annee_construction, "<br>", round(cout_total_5_usages, 0), " €<br>", nom_commune)
  ) %>%
  # DPE B
  addCircleMarkers(
    data = df_73[df_73$etiquette_dpe == "B" & !is.na(df_73$etiquette_dpe), ],
    lng = ~lon, lat = ~lat, color = "#92D050", radius = 5, fillOpacity = 0.7, 
    stroke = TRUE, weight = 1, group = "DPE B",
    popup = ~paste0("<b>73 - DPE B</b><br>", type_batiment, "<br>", annee_construction, "<br>", round(cout_total_5_usages, 0), " €<br>", nom_commune)
  ) %>%
  addCircleMarkers(
    data = df_74[df_74$etiquette_dpe == "B" & !is.na(df_74$etiquette_dpe), ],
    lng = ~lon, lat = ~lat, color = "#92D050", radius = 5, fillOpacity = 0.7, 
    stroke = TRUE, weight = 1, group = "DPE B",
    popup = ~paste0("<b>74 - DPE B</b><br>", type_batiment, "<br>", annee_construction, "<br>", round(cout_total_5_usages, 0), " €<br>", nom_commune)
  ) %>%
  # DPE C
  addCircleMarkers(
    data = df_73[df_73$etiquette_dpe == "C" & !is.na(df_73$etiquette_dpe), ],
    lng = ~lon, lat = ~lat, color = "#FFFF00", radius = 5, fillOpacity = 0.7, 
    stroke = TRUE, weight = 1, group = "DPE C",
    popup = ~paste0("<b>73 - DPE C</b><br>", type_batiment, "<br>", annee_construction, "<br>", round(cout_total_5_usages, 0), " €<br>", nom_commune)
  ) %>%
  addCircleMarkers(
    data = df_74[df_74$etiquette_dpe == "C" & !is.na(df_74$etiquette_dpe), ],
    lng = ~lon, lat = ~lat, color = "#FFFF00", radius = 5, fillOpacity = 0.7, 
    stroke = TRUE, weight = 1, group = "DPE C",
    popup = ~paste0("<b>74 - DPE C</b><br>", type_batiment, "<br>", annee_construction, "<br>", round(cout_total_5_usages, 0), " €<br>", nom_commune)
  ) %>%
  # DPE D
  addCircleMarkers(
    data = df_73[df_73$etiquette_dpe == "D" & !is.na(df_73$etiquette_dpe), ],
    lng = ~lon, lat = ~lat, color = "#FFC000", radius = 5, fillOpacity = 0.7, 
    stroke = TRUE, weight = 1, group = "DPE D",
    popup = ~paste0("<b>73 - DPE D</b><br>", type_batiment, "<br>", annee_construction, "<br>", round(cout_total_5_usages, 0), " €<br>", nom_commune)
  ) %>%
  addCircleMarkers(
    data = df_74[df_74$etiquette_dpe == "D" & !is.na(df_74$etiquette_dpe), ],
    lng = ~lon, lat = ~lat, color = "#FFC000", radius = 5, fillOpacity = 0.7, 
    stroke = TRUE, weight = 1, group = "DPE D",
    popup = ~paste0("<b>74 - DPE D</b><br>", type_batiment, "<br>", annee_construction, "<br>", round(cout_total_5_usages, 0), " €<br>", nom_commune)
  ) %>%
  # DPE E
  addCircleMarkers(
    data = df_73[df_73$etiquette_dpe == "E" & !is.na(df_73$etiquette_dpe), ],
    lng = ~lon, lat = ~lat, color = "#FF9900", radius = 5, fillOpacity = 0.7, 
    stroke = TRUE, weight = 1, group = "DPE E",
    popup = ~paste0("<b>73 - DPE E</b><br>", type_batiment, "<br>", annee_construction, "<br>", round(cout_total_5_usages, 0), " €<br>", nom_commune)
  ) %>%
  addCircleMarkers(
    data = df_74[df_74$etiquette_dpe == "E" & !is.na(df_74$etiquette_dpe), ],
    lng = ~lon, lat = ~lat, color = "#FF9900", radius = 5, fillOpacity = 0.7, 
    stroke = TRUE, weight = 1, group = "DPE E",
    popup = ~paste0("<b>74 - DPE E</b><br>", type_batiment, "<br>", annee_construction, "<br>", round(cout_total_5_usages, 0), " €<br>", nom_commune)
  ) %>%
  # DPE F
  addCircleMarkers(
    data = df_73[df_73$etiquette_dpe == "F" & !is.na(df_73$etiquette_dpe), ],
    lng = ~lon, lat = ~lat, color = "#FF0000", radius = 5, fillOpacity = 0.7, 
    stroke = TRUE, weight = 1, group = "DPE F",
    popup = ~paste0("<b>73 - DPE F</b><br>", type_batiment, "<br>", annee_construction, "<br>", round(cout_total_5_usages, 0), " €<br>", nom_commune)
  ) %>%
  addCircleMarkers(
    data = df_74[df_74$etiquette_dpe == "F" & !is.na(df_74$etiquette_dpe), ],
    lng = ~lon, lat = ~lat, color = "#FF0000", radius = 5, fillOpacity = 0.7, 
    stroke = TRUE, weight = 1, group = "DPE F",
    popup = ~paste0("<b>74 - DPE F</b><br>", type_batiment, "<br>", annee_construction, "<br>", round(cout_total_5_usages, 0), " €<br>", nom_commune)
  ) %>%
  # DPE G
  addCircleMarkers(
    data = df_73[df_73$etiquette_dpe == "G" & !is.na(df_73$etiquette_dpe), ],
    lng = ~lon, lat = ~lat, color = "#C00000", radius = 5, fillOpacity = 0.7, 
    stroke = TRUE, weight = 1, group = "DPE G",
    popup = ~paste0("<b>73 - DPE G</b><br>", type_batiment, "<br>", annee_construction, "<br>", round(cout_total_5_usages, 0), " €<br>", nom_commune)
  ) %>%
  addCircleMarkers(
    data = df_74[df_74$etiquette_dpe == "G" & !is.na(df_74$etiquette_dpe), ],
    lng = ~lon, lat = ~lat, color = "#C00000", radius = 5, fillOpacity = 0.7, 
    stroke = TRUE, weight = 1, group = "DPE G",
    popup = ~paste0("<b>74 - DPE G</b><br>", type_batiment, "<br>", annee_construction, "<br>", round(cout_total_5_usages, 0), " €<br>", nom_commune)
  ) %>%
  # Contrôles et légende
  addLayersControl(
    overlayGroups = c("DPE A", "DPE B", "DPE C", "DPE D", "DPE E", "DPE F", "DPE G"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("#00B050", "#92D050", "#FFFF00", "#FFC000", "#FF9900", "#FF0000", "#C00000"),
    labels = c("A", "B", "C", "D", "E", "F", "G"),
    title = "Étiquette DPE",
    opacity = 1
  )

# Afficher la carte
carte_dpe_filtrable

