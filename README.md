# DPE – Analyse énergétique des 2 Savoies (73 & 74)

Application **R/Shiny** pour l’analyse et la visualisation des Diagnostics de Performance Énergétique (DPE).

## 1. Présentation du projet

Cette application Shiny a été développée dans le cadre du **BUT Science des Données** (IUT Lyon 2).

### Objectif

Explorer, visualiser et analyser les DPE des départements suivants :

- Savoie (73)  
- Haute-Savoie (74)

### Sources de données

- **API ADEME** – Diagnostics de Performance Énergétique  
- **BAN** – Base Adresse Nationale (données géographiques)

### Note importante sur le déploiement

Pour respecter les limitations de mémoire du service gratuit [shinyapps.io](http://shinyapps.io/), la version en ligne de l’application charge uniquement les adresses géolocalisées du département **74** pour l’affichage de la carte **Leaflet**.

Toutes les autres analyses restent basées sur les deux départements **73 & 74**.

---

## 2. Prérequis

**Packages R nécessaires** :

```
shiny

leaflet

dplyr

DT

ggplot2

sf

shinythemes

shinyjs

here

rlang

jsonlite

httr
```

## 3. Lancer l’Application en Local

###  3.1 Cloner le dépôt

```bash
git clone https://github.com/TON-REPO/iut_sd2_rshiny_enedis.git
```

### 3.2 Aller dans le dossier

```
/app_r_shiny
```

### 3.3 Lancer l’application

```
shiny::runApp("app.R")
```

---

## 4. Version en ligne

L’application est disponible ici :

https://rafael-sanz.shinyapps.io/app_savoie/

---

## 5. Structure du dépôt

```
iut_sd2_rshiny_enedis/
│
├── app_r_shiny/                   # Code source de l'application Shiny
│   ├── app.R
│   ├── data/                     # Données utilisées (CSV)
│   └── www/                      # Images, logos et assets
│
├── r_scripts/                    # Scripts R d’analyse (préparation, tests)
│
├── documentation/                # Markdown, Rmd (documentation)
│
└── r_shiny_app_rmd (work_file)/  # Ancienne version de travail
```

---

## 6. Fonctionnalités

### 6.1 Onglet 1 — Données brutes

- Visualisation des DPE existants, neufs et des adresses  
- Filtrage dynamique  
- Téléchargement des données au format CSV  

### 6.2 Onglet 2 — Analyse unidimensionnelle

- Histogrammes des classes énergétiques  
- Répartition des émissions de GES  
- Consommation énergétique  
- Analyse par année de construction  

### 6.3 Onglet 3 — Analyse bivariée & carte

- Corrélations et régressions  
- Carte Leaflet (uniquement adresses du 74 pour la version en ligne)  
- Filtrage multicritère  

### 6.4 Onglet 4 — Synthèse

- Top 10 des communes selon les émissions de GES ou la consommation  
- Statistiques descriptives principales  

---

## 7. Authentification (si activée)

- Nom d’utilisateur : `admin`  
- Mot de passe : `admin`  

> Pour un déploiement public, il est recommandé de modifier ces identifiants ou de désactiver l’authentification basique.

---

## 8. Notes de développement

- Développement progressif en plusieurs scripts (`/r_scripts`).  
- La branche **main** contient la version finale nettoyée.  
- La branche **Application** inclut le script `Deploy.R` pour [shinyapps.io](http://shinyapps.io/).  
- Le déploiement web a nécessité un allègement des données géographiques (adresses du 74 uniquement).

---

## 9. Démonstration

Une démonstration vidéo sera ajoutée prochainement.  
Lien YouTube : *à venir*.

---

## 10. Auteurs

Projet réalisé par :

- Quentin ZAVAGNO  
- Rafael SANZ  

**BUT Science des Données – IUT Lyon 2**
