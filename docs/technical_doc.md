# Documentation Technique : Dashboard DPE 73 & 74

### 1. Introduction

Cette documentation décrit l'architecture technique, les dépendances et la procédure d'installation de l'application Shiny `AppV2.R`.

L'application est un tableau de bord interactif (`shinydashboard`) conçu pour l'analyse des Diagnostics de Performance Énergétique (DPE) des logements en Savoie (73) et Haute-Savoie (74). Elle fusionne des données de logements neufs et existants, les enrichit avec des données de géolocalisation, et propose plusieurs onglets d'analyse :
  
  * **Dashboard Principal** : Indicateurs clés (KPIs) et répartition globale.
* **Contexte et Données** : Texte explicatif et explorateur des données brutes (`DT`).
* **Analyses Comparatives** : Comparaison 73/74 et Neuf/Ancien.
* **Coûts & Performance** : Analyse des coûts énergétiques par DPE et période.
* **Exploration & Corrélation** : Nuages de points et matrices de corrélation.
* **Cartographie DPE** : Carte interactive (`leaflet`) des DPE géolocalisés.

L'application inclut également une page d'authentification basique et un sélecteur de thème (Light/Dark).

---
  
  ### 2. Schéma de l'Architecture
  
  L'application suit une structure `Shiny` classique, mais tout est contenu dans un seul fichier `AppV2.R`. Le flux d'exécution est le suivant :
  
  > **1. Phase Globale (Démarrage de l'App)**
> * Chargement des 12 bibliothèques R.
> * Exécution de la fonction `load_data()` :
>     * Vérification de l'existence des 4 fichiers CSV sources.
                        >     * Lecture des 4 CSV en mémoire.
                        >     * Fusion, nettoyage et enrichissement (création de `periode_construction`, `popup_html`, etc.).
                        >     * Stockage du jeu de données final dans `data_full`.
                        > * Pré-calcul des matrices de corrélation (`matrice_73`, `matrice_74`).
                        > * Pré-définition de la palette de couleurs (`pal_dpe`).
                        >
                          > **2. Interface Utilisateur (UI)**
                          > * Utilise `shinyjs` pour le CSS dynamique (thème sombre).
                        > * Affiche `uiOutput("app_ui")` qui pointe vers :
                          >     * `login_ui` (si non authentifié)
                        >     * `main_app_ui` (si authentifié)
                        > * `main_app_ui` est un `dashboardPage` contenant :
                          >     * Un `dashboardHeader` (avec titre, logo et bouton de thème).
                        >     * Un `dashboardSidebar` (avec les 6 `menuItem`).
                        >     * Un `dashboardBody` (avec les 6 `tabItem` et leurs `plotlyOutput`, `DTOutput`, `leafletOutput`, etc.).
                        >
                          > **3. Serveur (Logique Réactive)**
                          > * **Gestion de session** :
                          >     * `observeEvent(input$loginButton)` : Vérifie "admin"/"admin" et met à jour `login_status(TRUE)`.
                        >     * `observeEvent(input$theme_toggle)` : Alterne le `reactiveVal(theme_dark)` et modifie le CSS du `body`.
                        > * **Logique de Thème** :
                          >     * `reactive_ggplot_theme` : Renvoie un thème `ggplot` différent (clair ou sombre) selon `theme_dark()`.
                        > * **Logique des Onglets** :
                          >     * **Données filtrées** : Plusieurs `reactive()` préparent les données selon les `input` des filtres (ex: `data_filtered_comp()`, `data_filtered_cout()`).
                        >     * **Rendus** :
                          >         * `renderValueBox` : Calcule les KPIs (Total, Coût moyen...).
                        >         * `renderPlotly` : Génère les graphiques `ggplot` (passés dans `ggplotly`) en utilisant `reactive_ggplot_theme()`.
                        >         * `renderDT` : Affiche la table `data_full` avec filtrage de colonnes.
                        >         * `renderLeaflet` : Génère la carte en utilisant `data_carto_finale()` et la palette `pal_dpe`.
                        >         * `downloadHandler` : Exporte les données (`write.csv`) ou les graphiques (`ggsave`).
                        
                        ---
                          
                          ### 3. Packages R Nécessaires
                          
                          Avant de lancer l'application, assurez-vous que les packages R suivants sont installés.

* `shiny` (Framework principal)
* `shinydashboard` (Mise en page du tableau de bord)
* `shinythemes` (Thèmes additionnels)
* `shinyjs` (Actions JavaScript, ex: toggle thème)
* `ggplot2` (Moteur de graphiques)
* `plotly` (Graphiques interactifs)
* `DT` (Tableaux de données interactifs)
* `dplyr` (Manipulation de données)
* `scales` (Mise à l'échelle des axes graphiques)
* `shinycssloaders` (Indicateurs de chargement, via `withSpinner`)
* `reshape2` (Utilisé pour `melt` les matrices de corrélation)
* `leaflet` (Cartographie interactive)

Vous pouvez les installer tous en une seule fois avec la commande R suivante :
  
  ```r
install.packages(c("shiny", "shinydashboard", "shinythemes", "shinyjs", 
                   "ggplot2", "plotly", "DT", "dplyr", "scales", 
                   "shinycssloaders", "reshape2", "leaflet"))

### 4. Guide d'Installation et de Lancement

Suivez ces étapes pour exécuter l'application sur votre poste de travail.

#### 1. Prérequis
* Un environnement R fonctionnel (ex: RStudio).
* Tous les packages listés ci-dessus installés.

#### 2. Structure des Fichiers
L'application dépend de **5 fichiers externes** (4 CSV et 1 image) qui doivent être placés au bon endroit.

1.  Créez un dossier principal pour votre projet (ex: `Mon_Projet_DPE`).
2.  Placez le fichier `AppV2.R` à la racine de ce dossier.
3.  Créez un sous-dossier nommé `www` (obligatoire pour que Shiny trouve l'image).
    * Placez l'image `drapeau.png` dans ce dossier `www`.
                                       4.  Placez les **4 fichiers de données CSV** suivants à la racine de votre dossier `Mon_Projet_DPE` :
                                         * `data_dpe_2_savoies_existants.csv`
                                       * `data_dpe_2_savoies_neufs.csv`
                                       * `adresses_73-74.csv`
                                       * `resultats_dpe_identifiant_ban_73_74.csv`
#### 3. Modification du Chemin d'Accès (Étape Critique)
                                       
Le script `AppV2.R` contient un chemin d'accès **codé en dur (hardcoded)**. Vous **devez** le modifier pour qu'il pointe vers votre dossier `Mon_Projet_DPE`.
                                       
Ouvrez `AppV2.R` et modifiez la ligne 18 :
                                         
```R
 # Ligne 18 :
path_projet <- "C:/Users/utilisateur/OneDrive - univ-lyon2.fr/Espace Travail/Projet-r"
path_projet <- "C:/Chemin/Vers/Mon_Projet_DPE"

Exmple :
path_projet <- "/Users/MonNom/Documents/Mon_Projet_DPE"

4. Lancement
Ouvrez AppV2.R dans RStudio.

Cliquez sur le bouton "Run App" en haut à droite de l'éditeur de script.

L'application se lancera dans votre navigateur (ou le visualiseur RStudio).

Sur la page de connexion, utilisez les identifiants par défaut :

Nom d'utilisateur : admin

Mot de passe : admin

Cliquez sur "Se connecter" pour accéder au tableau de bord.