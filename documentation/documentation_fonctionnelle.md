# Documentation Fonctionnelle : Dashboard DPE 73 & 74

Ce document présente les objectifs et les fonctionnalités de l'application d'analyse des DPE (Diagnostics de Performance Énergétique) pour la Savoie et la Haute-Savoie.

## 1. Objectif de l'application

L'objectif de ce tableau de bord est de permettre une exploration visuelle et interactive des données publiques des DPE. Il est conçu pour aider les utilisateurs (étudiants, analystes, ou particuliers) à :

* **Comprendre** la répartition des performances énergétiques des logements dans les deux Savoies.
* **Comparer** les coûts et les émissions de GES en fonction de divers critères (ancienneté, département, type de logement).
* **Identifier** les relations entre les variables (ex: lien entre surface et coût).
* **Visualiser** la localisation géographique des DPE.

## 2. Fonctionnalités Majeures de l'Application

Ces fonctionnalités sont présentes à travers toute l'application pour améliorer l'expérience utilisateur.

### Authentification
Pour des raisons de contrôle d'accès, l'application est protégée par une **page d'authentification**.
* L'utilisateur doit saisir un nom d'utilisateur et un mot de passe pour accéder au tableau de bord.
* Pour la version de développement, les identifiants sont :
    * **Utilisateur :** `admin`
    * **Mot de passe :** `admin`

### Thème Visuel (Mode Clair / Sombre)
* Un bouton "Thème" (☀️ / 🌙) est disponible en haut à droite de l'application.
* Il permet à l'utilisateur de basculer à tout moment entre un **thème clair** (par défaut) et un **thème sombre** pour un meilleur confort visuel.

### Export de Données
* L'application permet d'exporter les données brutes au format `.csv` depuis l'onglet "Contexte et Données".
* Elle permet également d'exporter certains graphiques clés (analyses comparatives, corrélation) au format `.png`.

### Performance
* L'application est optimisée pour la performance : les 4 fichiers de données sources sont chargés, nettoyés et fusionnés **une seule fois** au démarrage de l'application. Toutes les sessions utilisateur accèdent à ces données pré-calculées, rendant les filtrages et l'affichage quasi-instantanés.

## 3. Intérêt de Chaque Page

L'application est divisée en 6 onglets (pages) accessibles depuis le menu de gauche.

### Page 1 : Dashboard Principal
* **Intérêt :** Fournir une vue d'ensemble synthétique ("à vol d'oiseau") de l'état du parc immobilier des deux Savoies.
* **Fonctionnalités Clés :**
    * **4 Indicateurs Clés (KPIs) :**
        1.  Nombre total de logements analysés.
        2.  Coût énergétique annuel moyen.
        3.  Émissions GES moyennes.
        4.  Surface habitable moyenne.
    * **2 Graphiques de Répartition :** Affiche la distribution en pourcentage de tous les logements par étiquette DPE (A-G) et par étiquette GES (A-G).

### Page 2 : Contexte et Données
* **Intérêt :** Assurer la transparence sur les données utilisées et permettre une exploration brute.
* **Fonctionnalités Clés :**
    * **Texte de Contexte :** Présente le projet, l'objectif et la source des données (ADEME).
    * **Explorateur de Données :** Un tableau interactif (`DT`) affichant l'intégralité des données nettoyées. L'utilisateur peut trier, rechercher et paginer les données.
    * **Export CSV :** Un bouton "Exporter en .csv" permet de télécharger l'intégralité de la base de données.

### Page 3 : Analyses Comparatives
* **Intérêt :** Comparer directement les départements 73 (Savoie) et 74 (Haute-Savoie) sur plusieurs axes.
* **Fonctionnalités Clés :**
    * **Filtres Interactifs :**
        * `Type de Logement` : Permet de n'inclure que "Ancien", "Neuf" ou les deux.
        * `Analyse de répartition` : Permet de basculer le premier graphique entre l'analyse des **Coûts** et celle des **Émissions GES**.
    * **Graphiques Comparatifs (Diagrammes en barres) :**
        * `Répartition des Postes` : Compare la part moyenne du chauffage, de l'ECS, etc., dans la facture totale.
        * `Répartition DPE / GES` : Compare les distributions d'étiquettes côte à côte.
        * `Type de Bâtiment` / `Période de Construction` : Compare la composition structurelle du parc immobilier.

### Page 4 : Coûts & Performance
* **Intérêt :** Analyser en détail l'impact financier de la performance énergétique (étiquette DPE) et de l'ancienneté du bâtiment.
* **Fonctionnalités Clés :**
    * **Filtres Interactifs :**
        * `Choisir un département` : Permet d'isoler le 73, le 74, ou de voir les deux.
        * `Filtrer par Surface` : Un slider permet de restreindre l'analyse à une plage de surface spécifique (ex: petits logements de 20 à 50 m²).
    * **Graphiques d'Analyse :**
        * `Analyse vs Étiquette DPE` : Montre l'évolution du coût total et du coût de chauffage moyen pour chaque étiquette.
        * `Analyse vs Période de Construction` : Montre l'évolution de ces mêmes coûts en fonction de l'âge du bâtiment.

### Page 5 : Exploration & Corrélation
* **Intérêt :** Fournir un outil d'analyse avancé (type "data science") pour trouver des relations entre les variables numériques.
* **Fonctionnalités Clés :**
    * **Nuage de Points Dynamique :**
        * L'utilisateur choisit deux variables (X et Y) dans les menus (ex: "Surface Habitable" vs "Coût Chauffage").
        * Un nuage de points s'affiche, avec une case à cocher pour ajouter/retirer la **droite de régression linéaire**.
    * **Calcul de Corrélation :** Le coefficient de corrélation de Pearson (r) entre les deux variables choisies est calculé et affiché en temps réel.
    * **Matrices de Corrélation :** Deux "heatmaps" (cartes de chaleur) pré-calculées montrent l'ensemble des corrélations entre les variables clés, une pour chaque département.

### Page 6 : Cartographie DPE
* **Intérêt :** Visualiser la distribution géographique réelle des DPE sur le territoire.
* **Fonctionnalités Clés :**
    * **Carte Interactive :** Affiche une carte (fond OpenStreetMap) centrée sur les deux Savoies.
    * **Clustering (Regroupement) :** Pour garantir la performance, les milliers de points sont regroupés en "clusters" (bulles). Le chiffre sur la bulle indique le nombre de DPE dans cette zone.
    * **Zoom :** En zoomant sur une bulle, celle-ci se divise pour révéler des clusters plus petits, jusqu'à afficher les points individuels.
    * **Popups :** En cliquant sur un point individuel, une fenêtre affiche les détails du DPE (étiquette, commune, année, coût).
    * **Légende :** Une légende fixe en bas à droite rappelle la correspondance entre les couleurs et les étiquettes DPE.
    
    