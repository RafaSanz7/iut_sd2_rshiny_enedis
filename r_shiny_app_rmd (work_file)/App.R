# --- 1. CHARGEMENT DES BIBLIOTHÈQUES ---

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(scales)
library(shinycssloaders)
library(reshape2) 
library(leaflet) 
library(here) # Pour une gestion saine des chemins de fichiers

# ==============================================================================
# --- 2. OPTIMISATION : CHARGEMENT ET PRÉPARATION DES DONNÉES GLOBALES ---
#
# Ces données sont chargées UNE SEULE FOIS au démarrage de l'application.
# ==============================================================================

# --- Fonction de chargement sécurisée ---
load_data <- function() {
  # Utilise here::here() pour trouver les fichiers depuis la racine du projet
  files_to_load <- c(
    "data_dpe_2_savoies_existants.csv",
    "data_dpe_2_savoies_neufs.csv",
    "adresses_73-74.csv",
    "resultats_dpe_identifiant_ban_73_74.csv"
  )
  
  # Vérifie si tous les fichiers existent avant de continuer
  files_exist <- sapply(files_to_load, function(f) file.exists(here::here(f)))
  
  if (!all(files_exist)) {
    missing_files <- paste(files_to_load[!files_exist], collapse = ", ")
    # stopApp arrête l'application et affiche un message clair
    stopApp(paste("Erreur critique : Fichier(s) manquant(s) :", missing_files,
                  "/nVérifiez que les fichiers sont bien dans :", here::here()))
  }
  
  # Affiche un message dans la console R
  message("Optimisation : Chargement des 4 fichiers CSV...")
  
  # Lecture des fichiers
  df_existants <- read.csv(file = here::here("data", "data_dpe_2_savoies_existants.csv"),
                           header = TRUE, sep = ";", dec = ".", fileEncoding = "UTF-8")
  df_neufs <- read.csv(file = here::here("data", "data_dpe_2_savoies_neufs.csv"),
                       header = TRUE, sep = ";", dec = ".", fileEncoding = "UTF-8")
  df_adresses <- read.csv(file = here::here("data","adresses_73-74.csv"), 
                          header = TRUE, sep = ";", dec = ".", fileEncoding = "UTF-8")
  df_id_ban <- read.csv(file = here::here("data", "resultats_dpe_identifiant_ban_73_74.csv"), 
                        header = TRUE, sep = ";", dec = ".", fileEncoding = "UTF-8")
  
  # --- Pré-traitement et fusion (identique à avant) ---
  message("Optimisation : Fusion et nettoyage des données...")
  
  df_neufs$logement <- "neuf"
  df_neufs$annee_construction <- as.numeric(format(Sys.Date(), "%Y"))
  df_existants$logement <- "ancien"
  
  colonnes_communes <- intersect(colnames(df_neufs), colnames(df_existants))
  
  df <- rbind(df_neufs[, colonnes_communes],
              df_existants[, colonnes_communes])
  
  df$date_reception_dpe <- as.Date(df$date_reception_dpe, format = "%d/%m/%Y")
  df$date_reception_dpe <- as.numeric(format(df$date_reception_dpe, "%Y"))
  
  df$departement <- ifelse(substr(df$code_insee_ban, 1, 2) == "73", "73", 
                           ifelse(substr(df$code_insee_ban, 1, 2) == "74", "74", NA))
  
  numeric_cols <- c("cout_chauffage", "cout_ecs", "cout_refroidissement", "cout_eclairage",
                    "cout_auxiliaires", "cout_total_5_usages", "emission_ges_chauffage",
                    "emission_ges_ecs", "emission_ges_5_usages", "surface_habitable_logement")
  
  df[, numeric_cols] <- lapply(df[, numeric_cols], function(x) as.numeric(gsub(" ", "", x)))
  
  df$periode_construction <- cut(x = df$annee_construction,
                                 breaks = c(0,1960,1970,1980,1990,2000,2010,2050),
                                 labels = c("Avant 1960", "1961 - 1970", "1971 - 1980", "1981 - 1990", "1991 - 2000", "2001 - 2010", "Après 2010"),
                                 right = FALSE)
  
  dpe_levels <- c("A", "B", "C", "D", "E", "F", "G")
  df$etiquette_dpe <- factor(df$etiquette_dpe, levels = dpe_levels, ordered = TRUE)
  df$etiquette_ges <- factor(df$etiquette_ges, levels = dpe_levels, ordered = TRUE)
  
  df <- df %>%
    filter(!is.na(departement), !is.na(logement), !is.na(etiquette_dpe))
  
  # --- Fusions pour la carte ---
  df_final <- merge(df, df_id_ban, by = "numero_dpe", all.x = TRUE)
  df_join <- merge(df_final, df_adresses, by.x = "identifiant_ban", by.y = "id_ban", all.x = TRUE)
  
  # --- OPTIMISATION : Pré-calcul des colonnes ---
  message("Optimisation : Pré-calcul des pourcentages et des popups...")
  df_join <- df_join %>%
    mutate(
      # Pour l'onglet "Analyses Comparatives"
      cout_chauffage_pourcentage = round(cout_chauffage / cout_total_5_usages * 100, 2),
      cout_ecs_pourcentage = round(cout_ecs / cout_total_5_usages * 100, 2),
      cout_refroidissement_pourcentage = round(cout_refroidissement / cout_total_5_usages * 100, 2),
      cout_eclairage_pourcentage = round(cout_eclairage / cout_total_5_usages * 100, 2),
      cout_auxiliaires_pourcentage = round(cout_auxiliaires / cout_total_5_usages * 100, 2),
      ges_chauffage_pourcentage = round(emission_ges_chauffage / emission_ges_5_usages * 100, 2),
      ges_ecs_pourcentage = round(emission_ges_ecs / emission_ges_5_usages * 100, 2),
      
      # Pour l'onglet "Cartographie"
      popup_html = paste0(
        "<b>", departement, " - DPE ", etiquette_dpe, "</b><br>", 
        "Type: ", type_batiment, "<br>", 
        "Année: ", annee_construction, "<br>", 
        "Coût: ", round(cout_total_5_usages, 0), " €<br>", 
        "Commune: ", nom_commune
      )
    ) %>%
    # Remplacer les NaN/Inf par 0 (résultat de division par zéro)
    mutate(across(ends_with("_pourcentage"), ~ifelse(is.finite(.x), .x, 0)))
  
  message("Données chargées et optimisées.")
  return(df_join)
}

# --- Exécution du chargement ---
data_full <- load_data()

# --- OPTIMISATION : Pré-calcul des matrices de corrélation ---
message("Optimisation : Pré-calcul des matrices de corrélation...")
variables_corr <- c("cout_total_5_usages", "cout_chauffage", "cout_eclairage", 
                    "cout_ecs", "cout_refroidissement", "cout_auxiliaires", 
                    "surface_habitable_logement", "emission_ges_5_usages")

matrice_73 <- cor(data_full[data_full$departement == "73", variables_corr], use = "complete.obs")
matrice_74 <- cor(data_full[data_full$departement == "74", variables_corr], use = "complete.obs")

# Nous "fondons" (melt) les matrices ici pour que ggplot n'ait pas à le faire
melted_matrice_73 <- reshape2::melt(matrice_73)
melted_matrice_74 <- reshape2::melt(matrice_74)

# --- OPTIMISATION : Définition de la palette de couleurs de la carte ---
pal_dpe <- colorFactor(
  palette = c("#00B050", "#92D050", "#FFFF00", "#FFC000", "#FF9900", "#FF0000", "#C00000"),
  domain = levels(data_full$etiquette_dpe)
)
message("Pré-calculs terminés. Lancement de l'application.")

# ==============================================================================
#
# --- 3. INTERFACE UTILISATEUR (UI) ---
#
# ==============================================================================
ui <- fluidPage(
  useShinyjs(), 
  
  tags$head(
    # --- MODIFICATION : CSS réintégré dans l'application ---
    tags$style(HTML("
          /* --- MODIFIÉ : Correction taille du titre --- */
          .main-header .logo {
            font-size: 17px; /* Default is 20px */
            font-weight: 600;
          }

          /* --- Configuration générale (Light Mode) --- */
          .content-wrapper { 
            background-color: #f4f6f9;
            min-height: calc(100vh - 50px); /* Force la hauteur minimale */
          }
          .box {
            border: 1px solid #ced4da; 
            border-radius: 8px; 
            box-shadow: 0 2px 4px rgba(0,0,0,0.05);
            overflow: hidden; /* Empêche le header de déborder */
          }
          
          /* --- Correction des 'box-header' (rectangle bleu) --- */
          .box-solid[status='primary'] > .box-header {
              background: #007bff !important;
              color: #ffffff;
          }
          .box-solid[status='info'] > .box-header {
              background: #17a2b8 !important;
              color: #ffffff;
          }
          .box-solid[status='warning'] > .box-header {
              background: #ffc107 !important;
              color: #343a40; 
          }
          .box-solid > .box-header {
              border-bottom: none; 
          }

          /* --- MODIFIÉ : Centrer tous les titres de 'box' --- */
          .box-title {
              font-weight: 600;
              color: #0056B3; 
              float: none !important; /* Annule le 'float: left' par défaut */
              text-align: center;
              display: block; /* S'assurer qu'il prend toute la largeur */
              width: 100%;
          }
          /* Repositionne les outils (bouton collapse) pour ne pas gêner le titre centré */
          .box-header .box-tools {
             position: absolute;
             right: 10px;
             top: 5px;
          }

          .box-solid > .box-header > .box-title {
              color: #ffffff !important; 
          }
          .box-solid[status='warning'] > .box-header > .box-title {
              color: #343a40 !important; 
          }
    
          /* --- CORRECTION FINALE 'valueBox' (KPIs) --- */
          .small-box {
              border-radius: 8px;
              overflow: hidden; 
              height: 120px; /* Hauteur fixe */
              position: relative; /* Contexte pour l'icône */
          }
          .small-box > .inner {
              padding: 15px; /* Padding interne pour le texte */
              text-align: left; /* Alignement du texte à gauche */
          }
          .small-box > .inner h3 { /* Cible la valeur (le 'h3') */
              font-size: 38px !important; 
              font-weight: bold !important;
              color: #ffffff !important; 
              margin-top: 0; 
              margin-bottom: 5px; 
              white-space: nowrap; /* Empêche le retour à la ligne */
          }
          .small-box > .inner p { /* Cible le sous-titre (le 'p') */
              font-size: 15px !important;
              color: rgba(255, 255, 255, 0.9) !important; 
              margin-bottom: 0; 
          }
          .small-box .icon {
              font-size: 70px !important; /* Taille de l'icône */
              position: absolute !important; /* Positionnement absolu */
              top: 15px !important; /* Ajuste la position verticale */
              right: 15px !important; /* Ajuste la position horizontale */
              color: rgba(255, 255, 255, 0.3) !important; 
              transition: all 0.3s ease-in-out;
          }
          /* Effet au survol */
          .small-box:hover .icon {
              font-size: 75px !important;
              color: rgba(255, 255, 255, 0.5) !important;
          }
    
          /* Couleurs spécifiques pour les ValueBox (D'ORIGINE) */
          .bg-blue { background-color: #007bff !important; }
          .bg-green { background-color: #28a745 !important; }
          .bg-orange { background-color: #fd7e14 !important; }
          .bg-purple { background-color: #6f42c1 !important; }
    
          /* Bouton de connexion (D'ORIGINE) */
          #login_page .btn-primary { 
            background-color: #007bff; 
            border-color: #007bff;
          }

          /* --- Styles Dark Mode (D'ORIGINE) --- */
          .dark-mode-body {
            color: #f8f9fa !important;
          }
          .dark-mode-body .content-wrapper, 
          .dark-mode-body .main-sidebar, 
          .dark-mode-body .navbar {
            background-color: #212529 !important;
          }
          /* Force la hauteur minimale en mode sombre */
          .dark-mode-body .content-wrapper {
             min-height: calc(100vh - 50px);
          }
          .dark-mode-body .sidebar-menu > li > a {
            color: #f1f1f1;
          }
          .dark-mode-body .sidebar-menu > li.active > a {
            background-color: #0056B3 !important;
            color: #ffffff !important;
          }
          .dark-mode-body .box {
            background-color: #343a40 !important;
            color: #f8f9fa !important;
            border-color: #495057;
          }
          
          /* Dark mode pour les titres de box */
          .dark-mode-body .box-solid[status='primary'] > .box-header,
          .dark-mode-body .box-solid[status='info'] > .box-header {
              background-color: #0056B3 !important; /* Bleu Savoie */
          }
          .dark-mode-body .box-solid[status='warning'] > .box-header {
              background-color: #ffc107 !important;
          }
          .dark-mode-body .box-solid > .box-header > .box-title {
              color: #ffffff !important;
          }
          .dark-mode-body .box-solid[status='warning'] > .box-header > .box-title {
              color: #343a40 !important; 
          }
    
          /* Styles des 'ValueBox' en Dark Mode */
          .dark-mode-body .small-box, .dark-mode-body .value-box {
              background-color: #3d444a !important;
              border: 1px solid #495057;
          }
          .dark-mode-body .small-box > .inner h3,
          .dark-mode-body .small-box > .inner p {
              color: #f8f9fa !important;
          }
          .dark-mode-body .small-box .icon {
              color: rgba(255, 255, 255, 0.2) !important;
          }
          
          /* Inputs et DT */
          .dark-mode-body .form-control, .dark-mode-body .selectize-input {
            background-color: #495057 !important;
            color: #f8f9fa !important;
            border-color: #6c757d !important;
          }
          .dark-mode-body .dataTables_wrapper,
          .dark-mode-body .dataTables_wrapper th,
          .dark-mode-body .dataTables_wrapper td,
          .dark-mode-body .dataTables_wrapper .dataTables_info,
          .dark-mode-body .dataTables_wrapper .dataTables_paginate {
            color: #f8f9fa !important;
          }
        "))
  ),
  
  # Conteneur principal qui sera rempli par le serveur
  uiOutput("app_ui")
)

# ==============================================================================
#
# --- 4. SERVEUR (Logique de l'application) ---
#
# ==============================================================================
server <- function(input, output, session) {
  
  # --- 4.1. Gestion Login & Thème ---
  
  # Interface de Connexion
  login_ui <- div(
    id = "login_page",
    style = "width: 450px; margin: 100px auto; padding: 30px; background: #fff; border-radius: 10px; box-shadow: 0 4px 12px rgba(0,0,0,0.1);",
    
    div(style = "text-align: center; margin-bottom: 20px;",
        tags$img(src = "drapeau.png", 
                 height = "100px", 
                 alt = "Drapeau Savoyard")
    ),
    
    h3(icon("lock"), "Authentification Requise", align = "center"),
    br(),
    textInput("userName", "Nom d'utilisateur:", value = "admin"),
    passwordInput("password", "Mot de passe:", value = "admin"),
    actionButton("loginButton", "Se connecter", class = "btn-primary btn-block")
  )
  
  # Interface de l'Application Principale
  main_app_ui <- dashboardPage(
    skin = "black",
    
    dashboardHeader(
      title = tags$span(
        tags$img(src = "drapeau.png", 
                 height = "25px", 
                 style = "margin-top:-5px; margin-right:5px;"), 
        "DPE 73 & 74"
      ),
      titleWidth = 250, 
      
      tags$li(class = "dropdown",
              style = "padding: 8px;",
              actionButton("theme_toggle", "Thème", icon = icon("sun"), class = "btn-default")
      )
    ),
    
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "main_nav",
        menuItem("Dashboard Principal", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Contexte et Données", tabName = "tab_contexte", icon = icon("table")),
        menuItem("Analyses Comparatives", tabName = "tab_comparaison", icon = icon("chart-bar")),
        menuItem("Coûts & Performance", tabName = "tab_couts", icon = icon("euro-sign")),
        menuItem("Exploration & Corrélation", tabName = "tab_correl", icon = icon("search-plus")),
        menuItem("Cartographie DPE", tabName = "tab_map", icon = icon("map-marked-alt"))
      )
    ),
    
    dashboardBody(
      tabItems(
        
        # --- Dashboard Principal ---
        tabItem(
          tabName = "dashboard",
          h2(icon("dashboard"), " Dashboard Principal"),
          p("Aperçu des indicateurs clés pour les DPE des deux Savoies."),
          
          tags$head(
            tags$style(HTML("
              .row { margin-bottom: 0 !important; }
              .col-sm-12 { padding-top: 3px !important; padding-bottom: 3px !important; }
              .value-box, .small-box, .box { margin-bottom: 6px !important; }
              .content h2 { margin-bottom: 10px !important; }
            "))
          ),
          
          fluidRow(
            column(
              width = 6,
              fluidRow(
                valueBoxOutput("kpi_total_logements", width = 12),
                valueBoxOutput("kpi_cout_moyen", width = 12),
                valueBoxOutput("kpi_ges_moyen", width = 12),
                valueBoxOutput("kpi_surface_moyenne", width = 12)
              )
            ),
            column(
              width = 6,
              style = "margin-top:-10px;",
              fluidRow(
                box(
                  title = "Répartition Globale des Étiquettes DPE",
                  width = 12, solidHeader = TRUE, status = "primary",
                  height = 270,
                  withSpinner(plotlyOutput("dashboard_plot_dpe", height = "220px"))
                )
              ),
              fluidRow(
                box(
                  title = "Répartition Globale des Étiquettes GES",
                  width = 12, solidHeader = TRUE, status = "primary",
                  height = 270,
                  withSpinner(plotlyOutput("dashboard_plot_ges", height = "220px"))
                )
              )
            )
          )
        ),
        
        # --- Onglet 1: Contexte et Données ---
        tabItem(tabName = "tab_contexte",
                h2(icon("table"), "Contexte et Données"),
                box(
                  title = "Contexte du Projet", 
                  width = 12, solidHeader = TRUE, status = "info",
                  tags$p("Cet outil interactif est conçu pour vous aider à explorer, analyser et comprendre les Diagnostics de Performance Énergétique (DPE) des logements en Savoie (73) et Haute-Savoie (74)."),
                  tags$p("Les informations présentées dans cette application proviennent directement de l'API Data ADEME, la source publique et officielle de référence pour les DPE en France."),
                  tags$p("L'ADEME (Agence de la transition écologique) collecte et centralise l'ensemble des diagnostics réalisés sur le territoire. Les données que vous explorez ici sont un instantané consolidé de cette base, spécifiquement filtré pour les deux Savoies et combinant les logements neufs et existants"),
                  tags$p("Ce tableau de bord est organisé en plusieurs onglets thématiques pour vous guider dans votre analyse :"),
                  tags$p("Dashboard Principal : Votre point de départ. Il offre un aperçu immédiat des indicateurs clés (KPIs) — comme le coût moyen ou la surface moyenne — et de la répartition globale des étiquettes DPE et GES."),
                  tags$p("Contexte et Données (Vous êtes ici) : Cette page vous explique le projet. Juste en dessous de ce texte, vous trouverez un explorateur de données vous permettant de visualiser et même de télécharger l'intégralité des données brutes (.csv) utilisées dans l'application."),
                  tags$p("Analyses Comparatives : Explorez les différences et les similarités entre les départements (73 vs 74) ou le type de logement (Neuf vs Ancien) sur divers critères (répartition des coûts, étiquettes, période de construction)."),
                  tags$p("Coûts & Performance : Cet onglet se concentre sur l'impact financier. Visualisez comment les coûts énergétiques (total, chauffage) évoluent en fonction de l'étiquette DPE ou de l'ancienneté du bâtiment."),
                  tags$p("Exploration & Corrélation : Pour une analyse plus poussée. Créez vos propres nuages de points pour analyser la relation between deux variables (par exemple, Surface habitable vs Coût du chauffage) et consultez les matrices de corrélation globales."),
                  tags$p("Cartographie DPE : Un nouvel onglet permettant de visualiser la localisation géographique des DPE sur une carte interactive.")
                ),
                box(
                  title = "Explorateur de Données", 
                  width = 12, solidHeader = TRUE, status = "primary",
                  p("Le tableau ci-dessous contient l'intégralité des données nettoyées et fusionnées (Neuf + Ancien + Coordonnées GPS)."),
                  downloadButton("export_data_csv", "Exporter en .csv", class = "btn-success"),
                  hr(),
                  DTOutput("table_donnees_brutes")
                )
        ),
        
        # --- Onglet 2: Analyses Comparatives ---
        tabItem(tabName = "tab_comparaison",
                h2(icon("chart-bar"), "Analyses Comparatives"),
                
                fluidRow(
                  box(
                    title = "Filtres et Exports", 
                    icon = icon("filter"), width = 12, solidHeader = TRUE, status = "warning", collapsible = TRUE,
                    fluidRow(
                      column(width = 3,
                             checkboxGroupInput("filter_logement_comp", "Type de Logement:",
                                                choices = c("Ancien" = "ancien", "Neuf" = "neuf"),
                                                selected = c("ancien", "neuf"))
                      ),
                      column(width = 3,
                             radioButtons("comp_plot_type", "Analyse de répartition:",
                                          choices = c(
                                            "Coûts (5 usages)" = "couts_usages",
                                            "Émissions GES (Chauffage/ECS)" = "ges_usages"
                                          ),
                                          selected = "couts_usages")
                      ),
                      column(width = 3,
                             style = "padding-top: 25px;",
                             downloadButton("export_plot_comp_png", "Exporter Graphique .png")
                      )
                    )
                  )
                ),
                fluidRow(
                  box(
                    title = "Répartition des Postes",
                    width = 12, solidHeader = TRUE, status = "primary",
                    withSpinner(plotlyOutput("plot_repartition_postes"))
                  )
                ),
                fluidRow(
                  box(
                    title = "Répartition des Étiquettes DPE",
                    width = 6, solidHeader = TRUE, status = "primary",
                    withSpinner(plotlyOutput("plot_distrib_dpe"))
                  ),
                  box(
                    title = "Répartition des Étiquettes GES",
                    width = 6, solidHeader = TRUE, status = "primary",
                    withSpinner(plotlyOutput("plot_distrib_ges"))
                  )
                ),
                fluidRow(
                  box(
                    title = "Répartition par Type de Bâtiment",
                    width = 6, solidHeader = TRUE, status = "primary",
                    withSpinner(plotlyOutput("plot_distrib_batiment"))
                  ),
                  box(
                    title = "Répartition par Période de Construction",
                    width = 6, solidHeader = TRUE, status = "primary",
                    withSpinner(plotlyOutput("plot_distrib_periode"))
                  )
                )
        ),
        
        # --- Onglet 3: Coûts & Performance ---
        tabItem(tabName = "tab_couts",
                h2(icon("euro-sign"), "Coûts & Performance"),
                
                fluidRow(
                  box(
                    title = "Filtres et Exports",
                    icon = icon("filter"), width = 12, solidHeader = TRUE, status = "warning", collapsible = TRUE,
                    fluidRow(
                      column(width = 4,
                             selectInput("filter_dept_cout", "Choisir un département:",
                                         choices = c("Tous" = "tous", "Savoie (73)" = "73", "Haute-Savoie (74)" = "74"),
                                         selected = "tous")
                      ),
                      column(width = 4,
                             sliderInput("filter_surface_cout", "Filtrer par Surface Habitable (m²):",
                                         min = 0, max = 500,
                                         value = c(0, 500))
                      ),
                      column(width = 4,
                             style = "padding-top: 25px;",
                             downloadButton("export_plot_cout_dpe_png", "Exporter Graph. (DPE) .png"),
                             " ", 
                             downloadButton("export_plot_cout_periode_png", "Exporter Graph. (Période) .png")
                      )
                    )
                  )
                ),
                
                fluidRow(
                  box(
                    title = "Analyse des Coûts vs Étiquette DPE",
                    width = 12, solidHeader = TRUE, status = "primary",
                    fluidRow(
                      column(6, withSpinner(plotlyOutput("plot_cout_total_dpe"))),
                      column(6, withSpinner(plotlyOutput("plot_cout_chauffage_dpe")))
                    )
                  )
                ),
                fluidRow(
                  box(
                    title = "Analyse des Coûts vs Période de Construction",
                    width = 12, solidHeader = TRUE, status = "primary",
                    fluidRow(
                      column(6, withSpinner(plotlyOutput("plot_cout_total_periode"))),
                      column(6, withSpinner(plotlyOutput("plot_cout_chauffage_periode")))
                    )
                  )
                )
        ),
        
        # --- Onglet 4: Exploration & Corrélation ---
        tabItem(tabName = "tab_correl",
                h2(icon("search-plus"), "Exploration & Corrélation"),
                box(
                  title = "Corrélation & Régression Linéaire",
                  width = 12, solidHeader = TRUE, status = "primary",
                  p("Sélectionnez deux variables (X et Y) et un département pour calculer le coefficient de corrélation de Pearson et visualiser la régression linéaire."),
                  fluidRow(
                    column(3,
                           selectInput("cor_x", "Variable X (Numérique):",
                                       choices = c("Surface Habitable" = "surface_habitable_logement", "Coût Total (5 usages)" = "cout_total_5_usages", "Coût Chauffage" = "cout_chauffage", "Coût ECS" = "cout_ecs", "Émission GES (5 usages)" = "emission_ges_5_usages", "Émission GES Chauffage" = "emission_ges_chauffage"),
                                       selected = "surface_habitable_logement")
                    ),
                    column(3,
                           selectInput("cor_y", "Variable Y (Numérique):",
                                       choices = c("Surface Habitable" = "surface_habitable_logement", "Coût Total (5 usages)" = "cout_total_5_usages", "Coût Chauffage" = "cout_chauffage", "Coût ECS" = "cout_ecs", "Émission GES (5 usages)" = "emission_ges_5_usages", "Émission GES Chauffage" = "emission_ges_chauffage"),
                                       selected = "cout_chauffage")
                    ),
                    column(3,
                           selectInput("cor_dept", "Filtrer par Département:",
                                       choices = c("Tous" = "tous", "Savoie (73)" = "73", "Haute-Savoie (74)" = "74"),
                                       selected = "tous")
                    ),
                    column(3, style = "margin-top: 25px;",
                           checkboxInput("show_regression", "Afficher la droite de régression", value = TRUE)
                    )
                  ),
                  hr(),
                  fluidRow(
                    column(8,
                           withSpinner(plotlyOutput("correlation_plot"))
                    ),
                    column(4, 
                           h5("Coefficient de Corrélation (Pearson)"),
                           verbatimTextOutput("correlation_text"),
                           hr(),
                           downloadButton("export_correlation_plot_png", "Exporter Graph. .png")
                    )
                  )
                ),
                box(
                  title = "Matrices de Corrélation",
                  width = 12, solidHeader = TRUE, status = "primary",
                  p("Ces graphiques montrent les corrélations globales pour les variables clés, séparées par département."),
                  fluidRow(
                    column(6, withSpinner(plotlyOutput("corrplot_73"))),
                    column(6, withSpinner(plotlyOutput("corrplot_74")))
                  )
                )
        ),
        
        # --- Onglet 5: Cartographie (FILTRES SUPPRIMÉS) ---
        tabItem(tabName = "tab_map",
                h2(icon("map-marked-alt"), "Cartographie des DPE"),
                p("Visualisation de l'ensemble des DPE géolocalisés. Zoomez pour explorer et cliquez sur un point pour plus de détails."),
                
                # --- MODIFICATION UI : Suppression de la rangée de filtres ---
                
                fluidRow(
                  box(
                    title = "Carte des DPE (Savoie & Haute-Savoie)",
                    width = 12, solidHeader = TRUE, status = "primary",
                    # Hauteur remise à 700px
                    withSpinner(leafletOutput("map_dpe", height = "700px"))
                  )
                )
        )
        
      ) # Fin tabItems
    ) # Fin dashboardBody
  ) 
  
  
  # --- 4.2. Logique de connexion et rendu de l'UI ---
  
  login_status <- reactiveVal(FALSE)
  
  observeEvent(input$loginButton, {
    if (input$userName == "admin" && input$password == "admin") {
      login_status(TRUE)
    } else {
      showNotification("Nom d'utilisateur ou mot de passe incorrect.", type = "error")
    }
  })
  
  output$app_ui <- renderUI({
    if (login_status() == FALSE) {
      return(login_ui)
    } else {
      return(main_app_ui)
    }
  })
  
  theme_dark <- reactiveVal(FALSE)
  
  observeEvent(input$theme_toggle, {
    theme_dark(!theme_dark()) 
    
    if (theme_dark()) {
      shinyjs::addClass(selector = "body", class = "dark-mode-body")
      updateActionButton(session, "theme_toggle", icon = icon("moon"))
    } else {
      shinyjs::removeClass(selector = "body", class = "dark-mode-body")
      updateActionButton(session, "theme_toggle", icon = icon("sun"))
    }
  })
  
  # Thème ggplot réactif (DRY)
  reactive_ggplot_theme <- reactive({
    if (theme_dark()) {
      theme_minimal(base_size = 14) +
        theme(
          plot.background = element_rect(fill = "#343a40", color = NA),
          panel.background = element_rect(fill = "#343a40", color = NA),
          text = element_text(color = "#f8f9fa"),
          title = element_text(color = "#58a6ff"),
          axis.text = element_text(color = "#f8f9fa"),
          axis.title = element_text(color = "#f8f9fa"),
          legend.background = element_rect(fill = "#343a40", color = NA),
          legend.text = element_text(color = "#f8f9fa"),
          legend.title = element_text(color = "#f8f9fa"),
          legend.position = "top", 
          panel.grid.major = element_line(color = "#495057"),
          panel.grid.minor = element_line(color = "#495057"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          strip.background = element_rect(fill="#495057"),
          strip.text = element_text(color="#f8f9fa", face="bold")
        )
    } else {
      theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          strip.background = element_rect(fill="#e0e0e0"),
          strip.text = element_text(color="black", face="bold"),
          legend.position = "top"
        )
    }
  })
  
  # --- 4.3. Logique des Onglets ---
  
  # Note : Il n'y a plus de reactive `data_full()`. 
  # Nous utilisons directement l'objet global `data_full` (sans parenthèses).
  
  # --- Onglet "Dashboard Principal" ---
  
  output$kpi_total_logements <- renderValueBox({
    valueBox(
      value = prettyNum(nrow(data_full), big.mark = " "),
      subtitle = "Logements Analysés",
      icon = icon("home"),
      color = "blue"
    )
  })
  
  output$kpi_cout_moyen <- renderValueBox({
    cout_moy <- mean(data_full$cout_total_5_usages, na.rm = TRUE)
    valueBox(
      value = paste(round(cout_moy, 0), "€"),
      subtitle = "Coût Annuel Moyen (5 usages)",
      icon = icon("euro-sign"),
      color = "green"
    )
  })
  
  output$kpi_ges_moyen <- renderValueBox({
    ges_moy <- mean(data_full$emission_ges_5_usages, na.rm = TRUE)
    valueBox(
      value = paste(round(ges_moy, 1), " kgCO₂/m²"),
      subtitle = "Émissions GES Moyennes (5 usages)",
      icon = icon("smog"),
      color = "orange"
    )
  })
  
  output$kpi_surface_moyenne <- renderValueBox({
    surf_moy <- mean(data_full$surface_habitable_logement, na.rm = TRUE)
    valueBox(
      value = paste(round(surf_moy, 1), "m²"),
      subtitle = "Surface Habitable Moyenne",
      icon = icon("ruler-combined"),
      color = "purple"
    )
  })
  
  output$dashboard_plot_dpe <- renderPlotly({
    df_plot <- data_full %>%
      count(etiquette_dpe) %>%
      filter(!is.na(etiquette_dpe)) %>%
      mutate(pourcentage = round(n / sum(n) * 100, 1))
    
    g <- ggplot(df_plot, aes(x = etiquette_dpe, y = pourcentage, fill = etiquette_dpe)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(pourcentage, "%")), vjust = -0.3, size = 3.5) +
      labs(x = "Étiquette DPE", y = NULL) +
      scale_fill_manual(values = c("A"="#00B050","B"="#92D050","C"="#FFFF00","D"="#FFC000","E"="#FF9900","F"="#FF0000","G"="#C00000"), drop=FALSE) +
      reactive_ggplot_theme() +
      theme(legend.position = "none")
    
    ggplotly(g, tooltip = c("x", "y"))
  })
  
  output$dashboard_plot_ges <- renderPlotly({
    df_plot <- data_full %>%
      count(etiquette_ges) %>%
      filter(!is.na(etiquette_ges)) %>%
      mutate(pourcentage = round(n / sum(n) * 100, 1))
    
    g <- ggplot(df_plot, aes(x = etiquette_ges, y = pourcentage, fill = etiquette_ges)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(pourcentage, "%")), vjust = -0.3, size = 3.5) +
      labs(x = "Étiquette GES", y = NULL) +
      scale_fill_manual(values = c("A"="#00B050","B"="#92D050","C"="#FFFF00","D"="#FFC000","E"="#FF9900","F"="#FF0000","G"="#C00000"), drop=FALSE) +
      reactive_ggplot_theme() +
      theme(legend.position = "none")
    
    ggplotly(g, tooltip = c("x", "y"))
  })
  
  # --- Onglet "Contexte et Données" ---
  
  output$table_donnees_brutes <- renderDT({
    cols_to_show <- c("numero_dpe", "departement", "logement", "annee_construction", "periode_construction", 
                      "type_batiment", "surface_habitable_logement", 
                      "etiquette_dpe", "etiquette_ges", "cout_total_5_usages", "cout_chauffage",
                      "emission_ges_5_usages", "nom_commune", "code_insee_ban")
    
    cols_existantes <- intersect(cols_to_show, names(data_full))
    
    datatable(data_full[, cols_existantes],
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE,
              caption = "Données DPE fusionnées (Neufs, Existants et Adresses) pour 73 & 74.")
  })
  
  output$export_data_csv <- downloadHandler(
    filename = function() {
      paste("export_dpe_2savoies_complet_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_full, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # --- Onglet "Analyses Comparatives" ---
  
  data_filtered_comp <- reactive({
    req(input$filter_logement_comp)
    data_full %>% 
      filter(logement %in% input$filter_logement_comp)
  })
  
  plot_repartition_data <- reactive({
    df_comp <- data_filtered_comp()
    req(df_comp)
    
    # OPTIMISATION : Les calculs de % sont déjà faits globalement. 
    # On se contente d'agréger.
    if (input$comp_plot_type == "couts_usages") {
      moyenne_postes <- aggregate(cbind(cout_chauffage_pourcentage,
                                        cout_ecs_pourcentage,
                                        cout_eclairage_pourcentage,
                                        cout_auxiliaires_pourcentage,
                                        cout_refroidissement_pourcentage) ~ departement,
                                  data = df_comp, FUN = mean, na.rm = TRUE)
      
      data_plot <- moyenne_postes %>%
        tidyr::pivot_longer(cols = -departement, names_to = "Poste", values_to = "Pourcentage") %>%
        mutate(Poste = factor(Poste, levels = c("cout_chauffage_pourcentage", "cout_ecs_pourcentage", 
                                                "cout_eclairage_pourcentage", "cout_auxiliaires_pourcentage", "cout_refroidissement_pourcentage"),
                              labels = c("Chauffage", "ECS", "Éclairage", "Auxiliaires", "Refroidissement")))
      
      return(list(data = data_plot, title = "Répartition moyenne des postes de consommation", ylab = "Pourcentage du coût total (%)", fill_lab = "Poste de Coût"))
      
    } else { 
      moyenne_ges <- aggregate(cbind(ges_chauffage_pourcentage,
                                     ges_ecs_pourcentage) ~ departement,
                               data = df_comp, FUN = mean, na.rm = TRUE)
      
      data_plot <- moyenne_ges %>%
        tidyr::pivot_longer(cols = -departement, names_to = "Poste", values_to = "Pourcentage") %>%
        mutate(Poste = factor(Poste, levels = c("ges_chauffage_pourcentage", "ges_ecs_pourcentage"),
                              labels = c("GES Chauffage", "GES ECS")))
      
      return(list(data = data_plot, title = "Répartition moyenne des émissions de GES", ylab = "Pourcentage des émissions (%)", fill_lab = "Poste de GES"))
    }
  })
  
  output$plot_repartition_postes <- renderPlotly({
    plot_data <- plot_repartition_data()
    req(plot_data$data)
    
    g <- ggplot(plot_data$data, aes(x = departement, y = Pourcentage, fill = Poste)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = paste0(round(Pourcentage, 1), "%")),
                position = position_dodge(width = 0.9),
                vjust = -0.3, size = 3.5, fontface = "bold") +
      labs(title = NULL,
           x = "Département", y = plot_data$ylab, fill = plot_data$fill_lab) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      reactive_ggplot_theme()
    
    last_plot_comp(g)
    ggplotly(g) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "bottom", y = 1.2))
  })
  
  last_plot_comp <- reactiveVal()
  
  output$export_plot_comp_png <- downloadHandler(
    filename = function() { paste0("export_repartition_", input$comp_plot_type, ".png") },
    content = function(file) {
      req(last_plot_comp())
      ggsave(file, plot = last_plot_comp(), device = "png", width = 10, height = 7)
    }
  )
  
  output$plot_distrib_dpe <- renderPlotly({
    df_plot <- data_filtered_comp()
    req(df_plot)
    
    table_dpe <- as.data.frame(table(df_plot$departement, df_plot$etiquette_dpe))
    colnames(table_dpe) <- c("departement", "etiquette_dpe", "effectif")
    
    table_dpe <- table_dpe %>%
      group_by(departement) %>%
      mutate(pourcentage = round(effectif / sum(effectif) * 100, 1))
    
    g <- ggplot(table_dpe, aes(x = etiquette_dpe, y = pourcentage, fill = etiquette_dpe)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~ departement) +
      geom_text(aes(label = paste0(pourcentage, "%")),
                vjust = -0.3, size = 3, fontface = "bold") +
      labs(title = NULL, x = "Étiquette DPE", y = NULL, fill = "Étiquette DPE") +
      scale_fill_manual(values = c("A"="#00B050","B"="#92D050","C"="#FFFF00","D"="#FFC000","E"="#FF9900","F"="#FF0000","G"="#C00000"), drop=FALSE) +
      ylim(0, max(table_dpe$pourcentage, na.rm = TRUE) * 1.15) +
      reactive_ggplot_theme() +
      theme(legend.position = "none")
    
    ggplotly(g)
  })
  
  output$plot_distrib_ges <- renderPlotly({
    df_plot <- data_filtered_comp()
    req(df_plot)
    
    table_ges <- as.data.frame(table(df_plot$departement, df_plot$etiquette_ges))
    colnames(table_ges) <- c("departement", "etiquette_ges", "effectif")
    
    table_ges <- table_ges %>%
      group_by(departement) %>%
      mutate(pourcentage = round(effectif / sum(effectif) * 100, 1))
    
    g <- ggplot(table_ges, aes(x = etiquette_ges, y = pourcentage, fill = etiquette_ges)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~ departement) +
      geom_text(aes(label = paste0(pourcentage, "%")),
                vjust = -0.3, size = 3, fontface = "bold") +
      labs(title = NULL, x = "Étiquette GES", y = NULL, fill = "Étiquette GES") +
      scale_fill_manual(values = c("A"="#00B050","B"="#92D050","C"="#FFFF00","D"="#FFC000","E"="#FF9900","F"="#FF0000","G"="#C00000"), drop=FALSE) +
      ylim(0, max(table_ges$pourcentage, na.rm = TRUE) * 1.15) +
      reactive_ggplot_theme() +
      theme(legend.position = "none")
    
    ggplotly(g)
  })
  
  graph_comparatif_shiny <- function(data, variable, xlabel) {
    ggplot(data, aes_string(x = variable, y = "pourcentage", fill = "departement")) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) + 
      geom_text(aes(label = paste0(pourcentage, "%")),
                position = position_dodge(width = 0.9),
                vjust = -0.3, size = 3.5, fontface = "bold") +
      labs(title = NULL, x = xlabel, y = NULL, fill = "Département") +
      theme_minimal(base_size = 14) +
      scale_fill_manual(values = c("73" = "steelblue", "74" = "darkseagreen3")) +
      ylim(0, max(data$pourcentage, na.rm = TRUE) * 1.15) +
      reactive_ggplot_theme() +
      theme(axis.text.x = element_text(angle = 15, vjust = 1, hjust = 1))
  }
  
  output$plot_distrib_batiment <- renderPlotly({
    df_plot <- data_filtered_comp()
    req(df_plot)
    
    table_batiment <- as.data.frame(table(df_plot$departement, df_plot$type_batiment))
    colnames(table_batiment) <- c("departement", "type_batiment", "effectif")
    
    table_batiment <- table_batiment %>%
      group_by(departement) %>%
      mutate(pourcentage = round(effectif / sum(effectif) * 100, 1)) %>%
      filter(effectif > 0) 
    
    g <- graph_comparatif_shiny(table_batiment, "type_batiment", "Type de Bâtiment")
    ggplotly(g) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "bottom", y = 1.2))
  })
  
  output$plot_distrib_periode <- renderPlotly({
    df_plot <- data_filtered_comp()
    req(df_plot)
    
    table_annee_const <- as.data.frame(table(df_plot$departement, df_plot$periode_construction))
    colnames(table_annee_const) <- c("departement", "periode_construction", "effectif")
    
    table_annee_const <- table_annee_const %>%
      group_by(departement) %>%
      mutate(pourcentage = round(effectif / sum(effectif) * 100, 1)) %>%
      filter(effectif > 0)
    
    g <- graph_comparatif_shiny(table_annee_const, "periode_construction", "Période de Construction")
    g <- g + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    ggplotly(g) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "bottom", y = 1.2))
  })
  
  
  # --- Onglet "Coûts & Performance" ---
  
  data_filtered_cout <- reactive({
    req(input$filter_dept_cout, input$filter_surface_cout)
    
    df_filtered <- data_full %>%
      filter(surface_habitable_logement >= input$filter_surface_cout[1] &
               surface_habitable_logement <= input$filter_surface_cout[2])
    
    if (input$filter_dept_cout != "tous") {
      df_filtered <- df_filtered %>%
        filter(departement == input$filter_dept_cout)
    }
    return(df_filtered)
  })
  
  plot_cost_vs_dpe_shiny <- function(data, y_var) { 
    
    agg_data <- aggregate(as.formula(paste(y_var, "~ etiquette_dpe + departement")),
                          data = data, FUN = mean, na.rm = TRUE)
    
    last_plot_cout_dpe(agg_data)
    
    g <- ggplot(agg_data, aes(x = .data[["etiquette_dpe"]], y = .data[[y_var]], fill = .data[["departement"]])) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_text(aes(label = paste0(round(.data[[y_var]], 0), ' €')), 
                position = position_dodge(width = 0.8),
                vjust = -0.3, size = 3.5, fontface = "bold") +
      labs(title = NULL, x = "Étiquette DPE", y = NULL, fill = "Département") +
      scale_fill_manual(values = c("73" = "steelblue", "74" = "darkseagreen3")) +
      ylim(0, max(agg_data[[y_var]], 0, na.rm = TRUE) * 1.15) + 
      reactive_ggplot_theme()
    
    if (length(unique(agg_data$departement)) == 1) {
      g <- g + theme(legend.position = "none")
    }
    return(g)
  }
  
  last_plot_cout_dpe <- reactiveVal()
  last_plot_cout_periode <- reactiveVal()
  
  output$plot_cout_total_dpe <- renderPlotly({
    g <- plot_cost_vs_dpe_shiny(data_filtered_cout(), "cout_total_5_usages")
    ggplotly(g) %>% layout(
      legend = list(orientation = "v", x = 1.02, y = 0.5, xanchor = "left", yanchor = "middle"),
      margin = list(t = 30)
    )
  })
  output$plot_cout_chauffage_dpe <- renderPlotly({
    g <- plot_cost_vs_dpe_shiny(data_filtered_cout(), "cout_chauffage")
    ggplotly(g) %>% layout(
      legend = list(orientation = "v", x = 1.02, y = 0.5, xanchor = "left", yanchor = "middle"),
      margin = list(t = 30)
    )
  })
  
  plot_cost_vs_periode_shiny <- function(data, y_var) { 
    agg_data <- aggregate(as.formula(paste(y_var, "~ periode_construction + departement")),
                          data = data, FUN = mean, na.rm = TRUE)
    last_plot_cout_periode(agg_data)
    g <- ggplot(agg_data, aes(x = .data[["periode_construction"]], y = .data[[y_var]], fill = .data[["departement"]])) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_text(aes(label = paste0(round(.data[[y_var]], 0), ' €')),
                position = position_dodge(width = 0.8),
                vjust = -0.3, size = 3.5, fontface = "bold") +
      labs(title = NULL, x = "Période de construction", y = NULL, fill = "Département") +
      scale_fill_manual(values = c("73" = "steelblue", "74" = "darkseagreen3")) +
      ylim(0, max(agg_data[[y_var]], 0, na.rm = TRUE) * 1.15) + 
      reactive_ggplot_theme() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    
    if (length(unique(agg_data$departement)) == 1) {
      g <- g + theme(legend.position = "none")
    }
    return(g)
  }
  
  output$plot_cout_total_periode <- renderPlotly({
    g <- plot_cost_vs_periode_shiny(data_filtered_cout(), "cout_total_5_usages")
    ggplotly(g) %>% layout(
      legend = list(orientation = "v", x = 1.02, y = 0.5, xanchor = "left", yanchor = "middle"),
      margin = list(t = 30)
    )
  })
  output$plot_cout_chauffage_periode <- renderPlotly({
    g <- plot_cost_vs_periode_shiny(data_filtered_cout(), "cout_chauffage")
    ggplotly(g) %>% layout(
      legend = list(orientation = "v", x = 1.02, y = 0.5, xanchor = "left", yanchor = "middle"),
      margin = list(t = 30)
    )
  })
  
  output$export_plot_cout_dpe_png <- downloadHandler(
    filename = "export_cout_vs_dpe.png",
    content = function(file) {
      p2 <- plot_cost_vs_dpe_shiny(data_filtered_cout(), "cout_chauffage")
      ggsave(file, plot = p2, device = "png", width = 8, height = 6)
    }
  )
  output$export_plot_cout_periode_png <- downloadHandler(
    filename = "export_cout_vs_periode.png",
    content = function(file) {
      p2 <- plot_cost_vs_periode_shiny(data_filtered_cout(), "cout_chauffage")
      ggsave(file, plot = p2, device = "png", width = 8, height = 6)
    }
  )
  
  
  # --- Onglet "Exploration & Corrélation" ---
  
  data_filtered_cor <- reactive({
    req(input$cor_dept)
    
    if (input$cor_dept != "tous") {
      df_filtered <- data_full %>% filter(departement == input$cor_dept)
    } else {
      df_filtered <- data_full
    }
    return(df_filtered)
  })
  
  correlation_calc <- reactive({
    df_cor <- data_filtered_cor()
    req(input$cor_x, input$cor_y)
    
    cor_value <- cor(df_cor[[input$cor_x]], df_cor[[input$cor_y]], use = "complete.obs")
    return(cor_value)
  })
  
  output$correlation_text <- renderPrint({
    val <- correlation_calc()
    req(val)
    print(round(val, 4))
  })
  
  plot_correlation_reactive <- reactive({
    df_cor <- data_filtered_cor()
    x_var <- input$cor_x
    y_var <- input$cor_y
    req(df_cor, x_var, y_var)
    
    df_clean <- df_cor %>%
      filter(!is.na(.data[[x_var]]) & !is.na(.data[[y_var]]))
    
    x_q <- quantile(df_clean[[x_var]], 0.99, na.rm = TRUE)
    y_q <- quantile(df_clean[[y_var]], 0.99, na.rm = TRUE)
    
    df_plot_filtered <- df_clean %>%
      filter(.data[[x_var]] <= x_q & .data[[y_var]] <= y_q)
    
    if (nrow(df_plot_filtered) > 10000) {
      df_plot <- sample_n(df_plot_filtered, 10000)
    } else {
      df_plot <- df_plot_filtered 
    }
    
    g <- ggplot(df_plot, aes(x = .data[[x_var]], y = .data[[y_var]])) +
      geom_point(alpha = 0.3, color = "steelblue") +
      labs(title = NULL, x = x_var, y = y_var) +
      reactive_ggplot_theme()
    
    if (input$show_regression) {
      g <- g + geom_smooth(method = "lm", color = "firebrick", se = TRUE, fill = "firebrick", alpha = 0.1) 
    }
    return(g)
  })
  
  output$correlation_plot <- renderPlotly({
    ggplotly(plot_correlation_reactive())
  })
  
  output$export_correlation_plot_png <- downloadHandler(
    filename = function() { paste0("export_correl_", input$cor_x, "_vs_", input$cor_y, ".png") },
    content = function(file) {
      ggsave(file, plot = plot_correlation_reactive(), device = "png", width = 8, height = 6)
    }
  )
  
  # OPTIMISATION : Les données (melted_matrice_73) sont globales.
  # Les reactives `corr_data_73` et `final_corr_plot_73` sont supprimées.
  # Le ggplot est construit directement dans le renderPlotly.
  
  output$corrplot_73 <- renderPlotly({
    g <- ggplot(melted_matrice_73, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
      scale_fill_gradient2(low = "firebrick", high = "steelblue", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Corrélation") +
      labs(title = "Département 73", x = NULL, y = NULL) +
      reactive_ggplot_theme() + 
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = "none"
      )
    
    ggplotly(g, tooltip = c("x", "y", "fill")) %>%
      layout(title = list(x = 0.5, xanchor = "center"))
  })
  
  output$corrplot_74 <- renderPlotly({
    g <- ggplot(melted_matrice_74, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
      scale_fill_gradient2(low = "firebrick", high = "steelblue", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Corrélation") +
      labs(title = "Département 74", x = NULL, y = NULL) +
      reactive_ggplot_theme() + 
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = "none"
      )
    
    ggplotly(g, tooltip = c("x", "y", "fill")) %>%
      layout(title = list(x = 0.5, xanchor = "center"))
  })
  
  
  # --- Onglet "Cartographie" (METHODE SIMPLE SANS FILTRE) ---
  
  # 1. Filtre les données globales UNE SEULE FOIS pour la carte
  data_carto_finale <- reactive({
    data_full %>%
      filter(!is.na(lon) & !is.na(lat) & !is.na(etiquette_dpe))
  })
  
  # 2. Rendu de la carte
  output$map_dpe <- renderLeaflet({
    
    # Utilise les données filtrées une fois
    df_carto <- data_carto_finale()
    
    leaflet(data = df_carto) %>%
      addTiles() %>%
      setView(lng = 6.5, lat = 45.7, zoom = 9) %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat, 
        color = ~pal_dpe(etiquette_dpe), # Palette de couleurs globale
        radius = 5, fillOpacity = 0.7, 
        stroke = TRUE, weight = 1,
        popup = ~popup_html, # Popup global
        # Cluster unique pour tous les points
        clusterOptions = markerClusterOptions() 
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("#00B050", "#92D050", "#FFFF00", "#FFC000", "#FF9900", "#FF0000", "#C00000"),
        labels = c("A", "B", "C", "D", "E", "F", "G"),
        title = "Étiquette DPE",
        opacity = 1
      )
  })
  
} 

# ==============================================================================
# --- 5. LANCEMENT DE L'APPLICATION ---
# ==============================================================================
shinyApp(ui = ui, server = server)