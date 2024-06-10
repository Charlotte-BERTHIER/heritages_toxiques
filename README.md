# Héritages toxiques, Modéliser les espaces de pollution en Île-de-France

Ce dépôt GitHub rassemble les annexes techniques de notre mémoire de Master 2 en Humanités Numériques à l'Ecole Nationale des Chartes.

## Abstract

La pollution industrielles des sols en Île-deFrance a une histoire et une géographie. Elle est le produit de l'industrialisation, c'est-à-dire de l'organisation socio-spatiale de la production aux XIXe et XXe siècles. Les scandales et mobilisations politiques récents au sujet des PFAS, polluants éternels, ont agité l'actualité en 2023-2024. Diffuse et maintenant éternelle, la pollution généralisée des milieux définit l'Anthropocène, et la relation que les sociétés humaines entretiennent avec leur environnement.  Dès lors, quels sont les impacts territoriaux de la pollution des sols ? Où se manifeste-t-elle ? Quels impacts a la pollution des sols sur l'évolution des territoires ? Nous nous penchons sur le cas particulier des territoires industriels et post-industriels pour comprendre les impacts à long terme de l'industrialisation sur les territoires, leurs héritages toxiques.  

## Cliquez sur les liens ci-dessous pour accéder à nos cartes dynamiques

**Localisation des sites CASIAS en Île-de-France** : https://github.com/charlotte-berthier/heritages_toxiques/cartes_dynamiques/carte_points_ile_de_france.html

**Nombre de sites CASIAS par commune d'Île-de-France** : https://github.com/charlotte-berthier/heritages_toxiques/cartes_dynamiques/carte_nb_sites_par_commune.html

**Localisation des points duplicats dans la base de données CASIAS** : https://github.com/charlotte-berthier/heritages_toxiques/cartes_dynamiques/carte_duplicate_points.html

## Description de l'arborescence \\

**sources** - Contient les sources de données utilisées dans le cadre de notre étude

**results** - Contient les résultats des traitements de données

**redaction** - Contient le fichier LATEX source ayant servi à la rédaction

**r** - Script R utilisé pour l'analyse spatiale

**notebooks** - Scripts Python utilisés pour extraire et traiter les données


## Librairies utilisées 
### En Python

Voir le fichier requirements.txt

### En R 

* library(ggplot2)
* library(mapsf)
* library(sf)
* library(dplyr)
* library(tidyr)
* library(spatstat)

## 
