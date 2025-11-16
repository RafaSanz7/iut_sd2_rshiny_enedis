# DPE – Analyse énergétique des 2 Savoies (73 & 74)
Application R/Shiny pour l’analyse et la visualisation des Diagnostics de Performance Énergétique (DPE)

---

## 🧭 1. Présentation du Projet

Cette application Shiny a été développée dans le cadre du BUT Science des Données (IUT Lyon 2).

### 🎯 Objectif
Explorer, visualiser et analyser les DPE des départements :
- Savoie (73)
- Haute-Savoie (74)

### 📚 Sources des données
- API ADEME (Diagnostics de Performance Énergétique)
- Données géographiques BAN (Base Adresse Nationale)

### ⚠️ Note importante sur le déploiement
Pour respecter les limitations de mémoire du service gratuit **shinyapps.io**, la version en ligne de l’application charge uniquement **les adresses géolocalisées du département 74** pour l'affichage de la carte Leaflet.

Cela permet d’assurer une meilleure stabilité et un temps de chargement optimal.

Toutes les autres analyses statistiques restent basées sur **les deux départements (73 & 74)**.

---

## ⚙️ 2. Prérequis

Packages R nécessaires :

