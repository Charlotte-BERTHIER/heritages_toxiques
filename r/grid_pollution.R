library(mapsf)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ThemePark)
library(bbplot)

####Import Styles de graphiques####
# Package Theme Park
install.packages("remotes")
remotes::install_github("MatthewBJane/ThemePark")

# BBC style package
install.packages('devtools')
devtools::install_github('bbc/bbplot')

#### Import fonction BBPLOT (graphiques) ####

#' Add bbc theme to ggplot chart
#'
#' This function allows you to add the bbc theme to your ggplotgraphics.
#' @keywords bbc_style
#' @export
#' @examples
#' line <- ggplot(line_df, aes(x = year, y = lifeExp)) +
#' geom_line(colour = "#007f7f", size = 1) +
#' geom_hline(yintercept = 0, size = 1, colour="#333333") +
#' bbc_style()

bbc_style <- function() {
  font <- "Helvetica"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=28,
                                       face="bold",
                                       color="#222222"),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=22,
                                          margin=ggplot2::margin(9,0,9,0)),
    plot.caption = ggplot2::element_blank(),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=18,
                                        color="#222222"),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=18,
                                      color="#222222"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
  )
}


#### Découpage des carreau INSEE 200m sur l'Île-de-France ####
carroyage_200m <- (st_read(dsn = "/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/source/Filosofi2017_carreaux_200m_gpkg/Filosofi2017_carreaux_200m_met.gpkg", layer = "Filosofi2017_carreaux_200m_met"))
carroyage_200m$ID <- 1:nrow(carroyage_200m)
carroyage_200m_IDF <- st_intersection(carroyage_200m, communes_idf)
st_write(obj = casias2154, 
         dsn = "data/carr_INSEE_200m_IDF.gpkg", 
         layer = "casias2154", 
         delete_layer = TRUE)

#### IMPORT DES DONNEES ####
communes_idf <- st_read(dsn = "/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/source/GEOFLA/1_DONNEES_LIVRAISON_2021-02-00128/GEOFLA_2-2_SHP_LAMB93_FR-ED161/COMMUNE/COMMUNE.shp") %>% filter(CODE_REG == "11")
casias <- st_read("/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/results/240424_V1/casias_pollution_results_clean.gpkg", layer = "casias_pollution_results_clean")
casias2154 <- st_transform(casias, crs = 2154)

mean_point_casias_2154 <- st_read(dsn ="/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/results/240424_V1/casias_pollution_results_clean.gpkg", layer = "mean_point_casias_2154")
median_point_casias_2154 <- st_read(dsn ="/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/results/240424_V1/casias_pollution_results_clean.gpkg", layer = "median_point_casias_2154")

Banlieue_Nord <- st_read(dsn = "/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/source/GEOFLA/1_DONNEES_LIVRAISON_2021-02-00128/GEOFLA_2-2_SHP_LAMB93_FR-ED161/COMMUNE/COMMUNE.shp") %>% 
  filter(INSEE_COM == c("93066", "93070", "93001", "92024", "92044","92078", "92036", "93055","93061"))

Dept_93 = st_read(dsn = "/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/source/GEOFLA/1_DONNEES_LIVRAISON_2021-02-00128/GEOFLA_2-2_SHP_LAMB93_FR-ED161/COMMUNE/COMMUNE.shp") %>% 
  filter(CODE_DEPT == "93")

PC <- st_read(dsn = "/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/source/GEOFLA/1_DONNEES_LIVRAISON_2021-02-00128/GEOFLA_2-2_SHP_LAMB93_FR-ED161/COMMUNE/COMMUNE.shp") %>% 
  filter(CODE_DEPT %in% c("75","92","93","94"))

#### Vérification des projections ####
mf_map(communes_idf)
mf_map(casias2154, col = blues9, add = TRUE)

#### Groupes et sous-groupes de substances pour les 2 méthodes ####

# Méthode 1 : agrégation par moyenne arithmétique (méthode "simple")
sous_groupes = sous_groupes <- c(paste(names(casias2154)[18:89],"carr", sep = "_")) # OK
groupes = c("chimique","elements_mineraux", "metaux_et_metalloides", "micropolluants_organiques", "pharmaceutiques_et_hormones", "phytosanitaires")

# Méthode 2 : agrégation par indice de pollution potantielle (méthode "article")
sous_groupes_IPP <- c(paste("IPP", names(casias2154)[18:89],"carr", sep = "_"))
groupes_IPP <- c(paste("IPP", groupes, sep = "_"))

# Test - Combine tous les noms pour debug la boucle 
sous_groupes_all <- c(sous_groupes, sous_groupes_IPP)
groupes_all <- c(groupes, groupes_IPP)


#### FONCTIONS FINALES ####

make_pollution_grid_v2 <- function(geography, cellsize, points){
  # geography = objet sf
  # cellsize = integer ; in meters
  # La grille de sortie est dans le crs de la géographie initiale
  # Suppression de la conversion auto en WGS 84
  # points : sf object (gpkg)
  
  grid <- st_make_grid(geography, cellsize = cellsize, square = TRUE)
  grid <- st_sf(ID = 1:length(grid), geometry = grid)
  #grid84 <- st_transform(grid, crs = 4326)
  grid$ID <- 1:nrow(grid)
  
  intersection <- st_intersects(grid, points, sparse = TRUE)
  points_in_grid <- st_join(x = points, y = grid)
  
  # Agrégation de la pollution moyenne au carreau
  variables<- names(points)[18:89]
  for (variable in variables) {
    aggregated_substance <- aggregate(x = list(substance = points_in_grid[[variable]]),
                                      by = list(ID = points_in_grid$ID),
                                      FUN = mean)
    col_name <- paste(variable, "carr", sep = "_")
    names(aggregated_substance)[2] <- col_name
    
    grid <- merge(grid, aggregated_substance, by = "ID", all.x = TRUE)
  }
  
  # Nombre de substances
  grid[is.na(grid)] <- 0
  grid$nb_substances <- (apply(grid, 1, function(row) sum(row != 0, na.rm = TRUE))) -2 # J'exclue les colonnes ID et geometry à la main. à améliorer plus tard
  
  # Nombre de points par carreau
  grid$nb_sites <- sapply(intersection, FUN = length)
  
  # Ajout des moyennes pour les 6 groupes de produits 
  df_temp <- sapply(grid, as.numeric)
  
  if (all(sapply(df_temp, is.numeric))) {
    chimique <- rowMeans(df_temp[, 2:4])
    elements_mineraux <- rowMeans(df_temp[, 5:9])
    metaux_et_metalloides <- rowMeans(df_temp[, 10:24])
    micropolluants_organiques <- rowMeans(df_temp[, 25:56])
    pharmaceutiques_et_hormones <- rowMeans(df_temp[, 57:63])
    phytosanitaires <-rowMeans(df_temp[, 64:73])
    
    df_temp <- cbind(df_temp, 
                     chimique_mean = chimique,
                     elements_mineraux_mean = elements_mineraux,
                     ETM_mean = metaux_et_metalloides,
                     micropolluants_organiques_mean = micropolluants_organiques,
                     pharmaceutiques_et_hormones_mean = pharmaceutiques_et_hormones,
                     phytosanitaires_mean = phytosanitaires)
  } else {
    print("Erreur. Certaines valeurs ne sont pas numériques.")
  }
  
  grid$chimique <- rowMeans(df_temp[, 2:4])
  grid$elements_mineraux <- rowMeans(df_temp[, 5:9])
  grid$metaux_et_metalloides <- rowMeans(df_temp[, 10:24])
  grid$micropolluants_organiques <- rowMeans(df_temp[, 25:56])
  grid$pharmaceutiques_et_hormones <- rowMeans(df_temp[, 57:63])
  grid$phytosanitaires <-rowMeans(df_temp[, 64:73])
  
  return(grid)
} 

normalize <- function(x){
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
} # Normalisation min max

indice_pollution_potentielle <- function(pollution_grid, substance){
  
  # présence de facteurs de pollution
  
  presence_col <- paste0("presence_facteur_", substance) 
  
  pollution_grid <- pollution_grid %>%
    mutate(!!as.name(presence_col) := case_when(
      nb_sites == 0 ~ 1,
      nb_sites >= 1 & nb_sites <= 4 ~ 2,
      nb_sites >= 5 & nb_sites <= 8 ~ 3,
      nb_sites > 8 ~ 4,
      TRUE ~NA_integer_
    ))
  
  # Niveau de pollution (note selon la probabilité de présence de la substance)
  
  # Nom de colonne
  niveau_col <- paste0("niveau_pollution_", substance)
  
  pollution_grid <- pollution_grid %>%
    mutate(!!as.name(niveau_col) := case_when(
      !!as.name(substance) < 1 & !!as.name(substance) < 1.5 ~ 0,
      !!as.name(substance) >= 1 & !!as.name(substance) < 1.5 ~ 1,
      !!as.name(substance) >= 1.5 & !!as.name(substance) < 3.5 ~ 2,
      !!as.name(substance) >= 3.5 & !!as.name(substance) < 5.5 ~ 3,
      !!as.name(substance) >= 5.5 & !!as.name(substance) < 6.5 ~ 4,
      !!as.name(substance) >= 6.5 & !!as.name(substance) < 7.5 ~ 5,
      !!as.name(substance) >= 7.5 & !!as.name(substance) <= 8 ~ 6,
      TRUE ~NA_integer_
    ))
  
  # Calcul de l'indice de pollution potentielle, normalisé entre 0 et 1
  
  temp <- data.frame()
  
  ipp_col <- paste0("IPP_", substance)
  
  pollution_grid[is.na(pollution_grid)] <- 0
  
  temp <- pollution_grid[[presence_col]] * pollution_grid[[niveau_col]]
  
  pollution_grid <- pollution_grid %>%
    mutate(!!ipp_col := normalize(temp))
  
  return(pollution_grid)
  
}

save_grid_to_gpkg <- function(pollution_grid, grid_name){
  st_write(obj = pollution_grid, 
           dsn = "data/pollution_grids.gpkg", 
           layer = grid_name, 
           delete_layer = TRUE)
}

remplir_tableau_synth_grilles <- function(df_pollution_grid, liste_groupes_substances, liste_sous_groupes_substances, grid_name){
  
  df_synth <- df_pollution_grid %>% 
    summarise(grid_name = grid_name,
              nb_tot_carr = n(),
              #nb_carr_casias = sum(df_pollution_grid$nb_sites != 0),
              nb_carr_casias = length(which(df_pollution_grid$nb_sites != 0)))
  
  df_synth <- as_tibble(df_synth)
  
  # Ajouter des colonnes pour chaque substance dans liste_groupes_substances
  for (substance in liste_groupes_substances) {
    substance_sum <- sum(df_pollution_grid[[substance]] != 0)
    df_synth[[substance]] <- substance_sum }
  
  for (substance in liste_sous_groupes_substances) {
    substance_sum <- sum(df_pollution_grid[[substance]] != 0)
    df_synth[[substance]] <- substance_sum }
  
  return(df_synth)
}



#### CREATION DES GRILLES - Done, charger les gpkg pour ne pas re-calculer les couches ####
#200m
pollution_grid_200m_BN <- make_pollution_grid_v2(Banlieue_Nord,200,casias2154)
for (substance in sous_groupes) {
  pollution_grid_200m <- indice_pollution_potentielle(pollution_grid_200m, substance)
}
for (substance in groupes) {
  pollution_grid_200m <- indice_pollution_potentielle(pollution_grid_200m, substance)
}
save_grid_to_gpkg(pollution_grid_200m,"200m_banlieue_nord")
#500m
pollution_grid_500m <- make_pollution_grid_v2(communes_idf,500,casias2154)
for (substance in sous_groupes) {
  pollution_grid_500m <- indice_pollution_potentielle(pollution_grid_500m, substance)}
for (substance in groupes) {
  pollution_grid_500m <- indice_pollution_potentielle(pollution_grid_500m, substance)}
save_grid_to_gpkg(pollution_grid_500m,"500m")
#### 1 KM
pollution_grid_1km <- make_pollution_grid_v2(communes_idf,1000,casias2154)

for (substance in sous_groupes) {
  pollution_grid_1km <- indice_pollution_potentielle(pollution_grid_1km, substance)
}
for (substance in groupes) {
  pollution_grid_1km <- indice_pollution_potentielle(pollution_grid_1km, substance)
}
save_grid_to_gpkg(pollution_grid_1km,"1km")


#### 2 KM
pollution_grid_2km <- make_pollution_grid_v2(communes_idf,2000,casias2154)

for (substance in sous_groupes) {
  pollution_grid_2km <- indice_pollution_potentielle(pollution_grid_2km, substance)
}
for (substance in groupes) {
  pollution_grid_2km <- indice_pollution_potentielle(pollution_grid_2km, substance)
}
save_grid_to_gpkg(pollution_grid_2km,"2km")

#### 3KM
pollution_grid_3km <- make_pollution_grid_v2(communes_idf,3000,casias2154)

for (substance in sous_groupes) {
  pollution_grid_3km <- indice_pollution_potentielle(pollution_grid_3km, substance)
}
for (substance in groupes) {
  pollution_grid_3km <- indice_pollution_potentielle(pollution_grid_3km, substance)
}
save_grid_to_gpkg(pollution_grid_3km,"3km")

#### 4KM
pollution_grid_4km <- make_pollution_grid_v2(communes_idf,4000,casias2154)
for (substance in sous_groupes) {
  pollution_grid_4km <- indice_pollution_potentielle(pollution_grid_4km, substance)
}
for (substance in groupes) {
  pollution_grid_4km <- indice_pollution_potentielle(pollution_grid_4km, substance)
}
save_grid_to_gpkg(pollution_grid_4km,"4km")
#### 5 KM
pollution_grid_5km <- make_pollution_grid_v2(communes_idf,5000,casias2154)
for (substance in sous_groupes) {
  pollution_grid_5km <- indice_pollution_potentielle(pollution_grid_5km, substance)
}
for (substance in groupes) {
  pollution_grid_5km <- indice_pollution_potentielle(pollution_grid_5km, substance)
}
save_grid_to_gpkg(pollution_grid_5km,"5km")
#### 6 KM
pollution_grid_6km <- make_pollution_grid_v2(communes_idf,6000,casias2154)
for (substance in sous_groupes) {
  pollution_grid_6km <- indice_pollution_potentielle(pollution_grid_6km, substance)
}
for (substance in groupes) {
  pollution_grid_6km <- indice_pollution_potentielle(pollution_grid_6km, substance)
}
save_grid_to_gpkg(pollution_grid_6km,"6km")
#### 7 KM
pollution_grid_7km <- make_pollution_grid_v2(communes_idf,7000,casias2154)
for (substance in sous_groupes) {
  pollution_grid_7km <- indice_pollution_potentielle(pollution_grid_7km, substance)
}
for (substance in groupes) {
  pollution_grid_7km <- indice_pollution_potentielle(pollution_grid_7km, substance)
}
save_grid_to_gpkg(pollution_grid_7km,"7km")
#### 8 KM
pollution_grid_8km <- make_pollution_grid_v2(communes_idf,8000,casias2154)
for (substance in sous_groupes) {
  pollution_grid_8km <- indice_pollution_potentielle(pollution_grid_8km, substance)
}
for (substance in groupes) {
  pollution_grid_8km <- indice_pollution_potentielle(pollution_grid_8km, substance)
}
save_grid_to_gpkg(pollution_grid_8km,"8km")
#### 9 KM 
pollution_grid_9km <- make_pollution_grid_v2(communes_idf,9000,casias2154)
for (substance in sous_groupes) {
  pollution_grid_9km <- indice_pollution_potentielle(pollution_grid_9km, substance)
}
for (substance in groupes) {
  pollution_grid_9km <- indice_pollution_potentielle(pollution_grid_9km, substance)
}
save_grid_to_gpkg(pollution_grid_9km,"9km")
#### 10 KM
pollution_grid_10km <- make_pollution_grid_v2(communes_idf,10000,casias2154)
for (substance in sous_groupes) {
  pollution_grid_10km <- indice_pollution_potentielle(pollution_grid_10km, substance)
}
for (substance in groupes) {
  pollution_grid_10km <- indice_pollution_potentielle(pollution_grid_10km, substance)
}
save_grid_to_gpkg(pollution_grid_10km,"10km")
####11 KM
pollution_grid_11km <- make_pollution_grid_v2(communes_idf,11000,casias2154)
for (substance in sous_groupes) {
  pollution_grid_11km <- indice_pollution_potentielle(pollution_grid_11km, substance)
}
for (substance in groupes) {
  pollution_grid_11km <- indice_pollution_potentielle(pollution_grid_11km, substance)
}
save_grid_to_gpkg(pollution_grid_11km,"11km")
#### 12 KM
pollution_grid_12km <- make_pollution_grid_v2(communes_idf,12000,casias2154)
for (substance in sous_groupes) {
  pollution_grid_12km <- indice_pollution_potentielle(pollution_grid_12km, substance)
}
for (substance in groupes) {
  pollution_grid_12km <- indice_pollution_potentielle(pollution_grid_12km, substance)
}
save_grid_to_gpkg(pollution_grid_12km,"12km")
#### 200M CARROYAGE INSEE
pollution_grid_200m <- make_pollution_grid(carroyage_200m_IDF,casias2154)
for (substance in sous_groupes) {
  pollution_grid_200m <- indice_pollution_potentielle(pollution_grid_200m, substance)
}
for (substance in groupes) {
  pollution_grid_200m <- indice_pollution_potentielle(pollution_grid_200m, substance)
}
save_grid_to_gpkg(pollution_grid_200m,"200m_carr_INSEE")





#### IMPORT DES GRILLES - GPKG ####
st_layers("/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/pollution_grids.gpkg")
pollution_grid_12km <- st_read(dsn="/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/pollution_grids.gpkg", layer = "12km")
pollution_grid_11km <- st_read(dsn="/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/pollution_grids.gpkg", layer = "11km")
pollution_grid_10km <- st_read(dsn="/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/pollution_grids.gpkg", layer = "10km")
pollution_grid_9km <- st_read(dsn="/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/pollution_grids.gpkg", layer = "9km")
pollution_grid_8km <- st_read(dsn="/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/pollution_grids.gpkg", layer = "8km")
pollution_grid_7km <- st_read(dsn="/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/pollution_grids.gpkg", layer = "7km")
pollution_grid_6km <- st_read(dsn="/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/pollution_grids.gpkg", layer = "6km")
pollution_grid_5km <- st_read(dsn="/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/pollution_grids.gpkg", layer = "5km")
pollution_grid_4km <- st_read(dsn="/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/pollution_grids.gpkg", layer = "4km")
pollution_grid_3km <- st_read(dsn="/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/pollution_grids.gpkg", layer = "3km")
pollution_grid_2km <- st_read(dsn="/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/pollution_grids.gpkg", layer = "2km")
pollution_grid_1km <- st_read(dsn="/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/pollution_grids.gpkg", layer = "1km")
pollution_grid_500m <- st_read(dsn="/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/pollution_grids.gpkg", layer = "500m")
pollution_grid_200m <- st_read(dsn="/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/pollution_grids.gpkg", layer = "200m")



#### Article Verdier / Chalonge : TABLEAU DE SYNTHÈSE CARROYAGES - Done, charger les données depuis le csv exporté dans le dossier data ####
# Mettre toutes les grilles dans une liste, et préparer le nom des grilles dans le même ordre
pollution_grids <- list(pollution_grid_200m, pollution_grid_500m, 
                        pollution_grid_1km, pollution_grid_2km,
                        pollution_grid_3km, pollution_grid_4km,
                        pollution_grid_5km, pollution_grid_6km,
                        pollution_grid_7km, pollution_grid_8km, 
                        pollution_grid_9km, pollution_grid_10km,
                        pollution_grid_11km, pollution_grid_12km)

names(pollution_grids) = c("pollution_grid_200m", "pollution_grid_500m", 
                           "pollution_grid_1km", "pollution_grid_2km",
                           "pollution_grid_3km", "pollution_grid_4km",
                           "pollution_grid_5km", "pollution_grid_6km",
                           "pollution_grid_7km", "pollution_grid_8km", 
                           "pollution_grid_9km", "pollution_grid_10km",
                           "pollution_grid_11km", "pollution_grid_12km")
print(names((pollution_grids)))

# MÉTHODE 1

# Initialiser une liste pour stocker les résultats
methode1_resultats <- data.frame()

# Boucle pour exécuter la fonction sur plusieurs grilles
for (grid_name in names(pollution_grids)){
  df_pollution_grid <- pollution_grids[[grid_name]]
  #print(names(df_pollution_grid))
  df_synth <- remplir_tableau_synth_grilles(df_pollution_grid, groupes, sous_groupes, grid_name)
  #print(names(df_synth))
  methode1_resultats <- rbind(methode1_resultats, df_synth)}

# Supprime la géométrie qui génère des erreurs dans la table
methode1_resultats <- methode1_resultats %>% select(-geom)
write.csv(methode1_resultats, "/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/methode1_tableau_synthese_grilles_nb_carreaux.csv", row.names = FALSE)

# MÉTHODE 2
# Comptage du nombre absolu de carreaux où chaque substance est détectée
methode2_resultats <- data.frame()
for (grid_name in names(pollution_grids)) {
  df_pollution_grid <- pollution_grids[[grid_name]]
  df_synth <- remplir_tableau_synth_grilles(df_pollution_grid, groupes_IPP, sous_groupes_IPP, grid_name)
  methode2_resultats <- rbind(methode2_resultats, df_synth)}

methode2_resultats <- methode2_resultats %>% select(-geom)
write.csv(methode2_resultats, "/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/methode2_tableau_synthese_grilles_nb_carreaux.csv", row.names = FALSE)

#### Article Verdier / Chalonge : GRAPHIQUE DE SYNTHESE DES CARROYAGES - ANALYSE METHODE 1 ####
methode1_resultats <- read.csv("/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/methode1_tableau_synthese_grilles_nb_carreaux.csv",sep=",")
methode1_synthese_grille_pourc <- methode1_resultats %>% select(-c(10:81))

# Pour chaque groupe de substances, calcule le pourcentage de carreaux
methode1_synthese_grille_pourc$part_carr_casias <- (methode1_synthese_grille_pourc$nb_carr_casias / methode1_synthese_grille_pourc$nb_tot_carr) * 100
methode1_synthese_grille_pourc$part_chimique <- (methode1_synthese_grille_pourc$IPP_chimique / methode1_synthese_grille_pourc$nb_tot_carr) * 100
methode1_synthese_grille_pourc$part_elements_mineraux <- (methode1_synthese_grille_pourc$IPP_elements_mineraux / methode1_synthese_grille_pourc$nb_tot_carr) * 100
methode1_synthese_grille_pourc$part_metaux_metalloides <- (methode1_synthese_grille_pourc$IPP_metaux_et_metalloides / methode1_synthese_grille_pourc$nb_tot_carr) * 100
methode1_synthese_grille_pourc$part_micropolluants_organiques <- (methode1_synthese_grille_pourc$IPP_micropolluants_organiques / methode1_synthese_grille_pourc$nb_tot_carr) * 100
methode1_synthese_grille_pourc$part_pharmaceutiques_hormones <- (methode1_synthese_grille_pourc$IPP_pharmaceutiques_et_hormones / methode1_synthese_grille_pourc$nb_tot_carr) * 100
methode1_synthese_grille_pourc$part_phytosanitaires <- (methode1_synthese_grille_pourc$IPP_phytosanitaires / methode1_synthese_grille_pourc$nb_tot_carr) * 100

# Reshape du df en LONG avec tidyr
methode1_synthese_grille_pourc_long <- methode1_synthese_grille_pourc %>%
  pivot_longer(
    cols = part_carr_casias:part_phytosanitaires,
    names_to = "groupe_de_substances",
    values_to = "pourcentage_carreaux"
  )

write.csv(methode2_synthese_grille_pourc_long, "/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/methode1_tableau_synthese_pourcentages_long.csv", row.names = FALSE)

# Conversion de la variable 'Groupe_de_substances' en facteur pour l'ordonnancement dans le graphique
methode1_synthese_grille_pourc_long$groupe_de_substances <- factor(methode1_synthese_grille_pourc_long$groupe_de_substances)
ordre_grid_name <- c("pollution_grid_200m", "pollution_grid_500m", "pollution_grid_1km", "pollution_grid_2km", "pollution_grid_3km","pollution_grid_4km","pollution_grid_5km","pollution_grid_6km","pollution_grid_7km", "pollution_grid_8km", "pollution_grid_9km", "pollution_grid_10km", "pollution_grid_11km", "pollution_grid_12km")
methode1_synthese_grille_pourc_long$grid_name <- factor(methode1_synthese_grille_pourc_long$grid_name, levels = ordre_grid_name)

# DONE short_labels <- c("200m", "500m", "1km", "2km", "3km", "4km", "5km", "6km", "7km", "8km", "9km", "10km", "11km", "12km")

# /!\ GRAPHIQUE DE BASE SUR R 
ggplot(methode1_synthese_grille_pourc_long, aes(x = grid_name, y = pourcentage_carreaux, color = groupe_de_substances, group = groupe_de_substances)) +
  geom_line(size = 1, alpha = 0.6) +
  geom_point(size = 2, alpha = 0.8) +
  theme_minimal() +
  labs(title = "Evolution de la couverture spatiale de la pollution des sols (méthode 1)",
       subtitle = "par niveau scalaire et familles de polluants",
       x = NULL,
       y = NULL,
       color = "Groupe de substances") +
  scale_x_discrete(labels = short_labels) +
  scale_y_continuous(labels = function(y) paste0(y, "%"), limits = c(0,100), breaks = seq(0, 100, by = 10)) + 
  scale_color_discrete(labels = c("Pollution totale","Substances chimiques", "Eléments minéraux", "Métaux et métalloïdes", "Micropolluants organiques", "Produits pharmaceutiques et hormones", "Produits phytosanitaires")) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)
  theme(bbc_style()) 
#bbc_stylNULL#bbc_style()


#### Article Verdier / Chalonge : SYNTHESE DES CARROYAGES - ANALYSE METHODE 2 ####
# En cas de plantage de R, charger directement depuis les csv enregistrés à l'étape précédente 

methode2_resultats <- read.csv("/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/methode2_tableau_synthese_grilles_nb_carreaux.csv",sep=",")
methode2_synthese_grille_pourc <- methode2_resultats %>% select(-c(10:81))

# Pour chaque groupe de substances, calcule le pourcentage de carreaux
methode2_synthese_grille_pourc$part_carr_casias <- (methode2_resultats$nb_carr_casias / methode2_resultats$nb_tot_carr) * 100
methode2_synthese_grille_pourc$part_chimique <- (methode2_synthese_grille_pourc$IPP_chimique / methode2_synthese_grille_pourc$nb_tot_carr) * 100
methode2_synthese_grille_pourc$part_elements_mineraux <- (methode2_synthese_grille_pourc$IPP_elements_mineraux / methode2_synthese_grille_pourc$nb_tot_carr) * 100
methode2_synthese_grille_pourc$part_metaux_metalloides <- (methode2_synthese_grille_pourc$IPP_metaux_et_metalloides / methode2_synthese_grille_pourc$nb_tot_carr) * 100
methode2_synthese_grille_pourc$part_micropolluants_organiques <- (methode2_synthese_grille_pourc$IPP_micropolluants_organiques / methode2_synthese_grille_pourc$nb_tot_carr) * 100
methode2_synthese_grille_pourc$part_pharmaceutiques_hormones <- (methode2_synthese_grille_pourc$IPP_pharmaceutiques_et_hormones / methode2_synthese_grille_pourc$nb_tot_carr) * 100
methode2_synthese_grille_pourc$part_phytosanitaires <- (methode2_synthese_grille_pourc$IPP_phytosanitaires / methode2_synthese_grille_pourc$nb_tot_carr) * 100

# Reshape du df en LONG avec tidyr
methode2_synthese_grille_pourc_long <- methode2_synthese_grille_pourc %>%
  pivot_longer(
    cols = part_carr_casias:part_phytosanitaires,
    names_to = "groupe_de_substances",
    values_to = "pourcentage_carreaux"
  )

write.csv(methode2_synthese_grille_pourc_long, "/Users/charlotteberthier/Documents/Memoire/9_Git_heritages_toxiques/r/data/methode2_tableau_synthese_pourcentages_long.csv", row.names = FALSE)

# Conversion de la variable 'Groupe_de_substances' en facteur pour l'ordonnancement dans le graphique
methode2_synthese_grille_pourc_long$groupe_de_substances <- factor(methode2_synthese_grille_pourc_long$groupe_de_substances)
ordre_grid_name <- c("pollution_grid_200m", "pollution_grid_500m", "pollution_grid_1km", "pollution_grid_2km", "pollution_grid_3km","pollution_grid_4km","pollution_grid_5km","pollution_grid_6km","pollution_grid_7km", "pollution_grid_8km", "pollution_grid_9km", "pollution_grid_10km", "pollution_grid_11km", "pollution_grid_12km")
methode2_synthese_grille_pourc_long$grid_name <- factor(methode2_synthese_grille_pourc_long$grid_name, levels = ordre_grid_name)

short_labels <- c("200m", "500m", "1km", "2km", "3km", "4km", "5km", "6km", "7km", "8km", "9km", "10km", "11km", "12km")

# /!\ GRAPHIQUE DE BASE SUR R 
ggplot(methode2_synthese_grille_pourc_long, aes(x = grid_name, y = pourcentage_carreaux, color = groupe_de_substances, group = groupe_de_substances)) +
  geom_line(size = 1, alpha = 0.6) +
  geom_point(size = 2, alpha = 0.8) +
  theme_minimal() +
  labs(title = "Evolution de la couverture spatiale de la pollution des sols (Méthode 2 : IPP)",
       subtitle = "par niveau scalaire et familles de polluants",
       caption = "Charlotte BERTHIER, Sources : CASIAS et ActiviPoll v.3",
       x = NULL,
       y = NULL,
       color = "Groupe de substances") +
  scale_x_discrete(labels = short_labels) +
  scale_y_continuous(labels = function(y) paste0(y, "%"), limits = c(0,100), breaks = seq(0, 100, by = 10)) + 
  scale_color_discrete(labels = c("Pollution totale","Substances chimiques", "Eléments minéraux", "Métaux et métalloïdes", "Micropolluants organiques", "Produits pharmaceutiques et hormones", "Produits phytosanitaires")) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)
  theme(bbc_style()) 
  #bbc_stylNULL#bbc_style()


###
# Graphique à facettes pour distinguer les groupes de substances
ggplot(methode2_synthese_grille_pourc_long, aes(x = grid_name, y = pourcentage_carreaux, color = groupe_de_substances, group = groupe_de_substances)) +
  geom_line(size = 1, alpha = 0.6) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~ groupe_de_substances, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Evolution de la couverture spatiale de la pollution des sols (Méthode 2 : IPP)",
    subtitle = "par niveau scalaire et familles de polluants",
    caption = "Charlotte BERTHIER, Sources : CASIAS et ActiviPoll v.3",
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  scale_x_discrete(labels = short_labels) +
  scale_y_continuous(labels = function(y) paste0(y, "%"), limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  scale_color_discrete(labels = c(
    "Pollution totale",
    "Substances chimiques", 
    "Éléments minéraux", 
    "Métaux et métalloïdes", 
    "Micropolluants organiques", 
    "Produits pharmaceutiques et hormones", 
    "Produits phytosanitaires"
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #bbc_style() # Assuming bbc_style() is a custom theme you have defined elsewhere






#### EXPORT OK - CARTE DES POINTS CASIAS (récup de Python les points moyens et médians) ####

mean_point_casias_2154['Nom'] <- "Point Moyen"
median_point_casias_2154['Nom'] <- "Point Médian"

mf_theme("agolalight")
mf_map(communes_idf,
       type='base',
       border = 'white',
       lwd = .3)
mf_map(casias2154,
       col = 'darkcyan',
       cex = 0.8,
       add = TRUE)
mf_map(mean_point_casias_2154,
       symbol = "circle",
       col = 'brown1',
       cex = 1.5,
       add = TRUE)
mf_map(median_point_casias_2154, 
       col = 'darkorchid2',
       cex = 1.5,
       add = TRUE)
mf_legend_t(
  pos = "topright",
  val = c("Point Moyen", "Point Médian"),
  pal = c("brown1", "darkorchid2"),
  title = "Points centraux", 
  title_cex = 1,
  val_cex = 0.8,
  cex = .5
  )
mf_layout(
  title = "Anciens sites industriels en Île-de-France",
  credits = "Charlotte BERTHIER, Source: CASIAS, 2024")

#### EXPORT 10 CARTES DES GRILLES IPP de 1 à 12km ####

par(mfrow = c(1,2))
mf_theme("agolalight")
mf_map(communes_idf,
       type='base',
       border = 'black',
       lwd = .3)
mf_map(
  pollution_grid_1km, 
  var ="IPP_chimique", 
  type="choro", 
  breaks = "jenks",
  nbreaks = 5,
  pal = "YlOrRd",
  border = "white",
  lwd = .1,
  alpha = 0.6,
  leg_pos = "topright",
  leg_title = "Indice de pollution potentielle",
  leg_horiz = TRUE,
  leg_size = 0.5,
  add = TRUE)
mf_layout(
  title = "Polluants chimiques - 1km",
  credits = "Charlotte BERTHIER, Sources: CASIAS & ActiviPol v.3, 2024")
mf_theme("agolalight")
mf_map(communes_idf,
       type='base',
       border = 'black',
       lwd = .3)
mf_map(
  pollution_grid_2km, 
  var ="IPP_chimique", 
  type="choro", 
  breaks = "jenks",
  nbreaks = 5,
  pal = "YlOrRd",
  border = "white",
  lwd = .1,
  alpha = 0.6,
  leg_pos = "topright",
  leg_title = "Indice de Pollution Potentielle",
  leg_horiz = TRUE,
  leg_size = 0.5,
  add = TRUE)
mf_layout(
  title = "Polluants chimiques - 2km",
  credits = "Charlotte BERTHIER, Sources: CASIAS & ActiviPol v.3, 2024")

par(mfrow = c(1,2))
mf_theme("agolalight")
mf_map(communes_idf,
       type='base',
       border = 'black',
       lwd = .3)
mf_map(
  pollution_grid_3km, 
  var ="IPP_chimique", 
  type="choro", 
  breaks = "jenks",
  nbreaks = 5,
  pal = "YlOrRd",
  border = "white",
  lwd = .1,
  alpha = 0.6,
  leg_pos = "topright",
  leg_title = "Indice de Pollution Potentielle",
  leg_horiz = TRUE,
  leg_size = 0.5,
  add = TRUE)
mf_layout(
  title = "Polluants chimiques - 3km",
  credits = "Charlotte BERTHIER, Sources: CASIAS & ActiviPol v.3, 2024")
mf_theme("agolalight")
mf_map(communes_idf,
       type='base',
       border = 'black',
       lwd = .3)
mf_map(
  pollution_grid_4km, 
  var ="IPP_chimique", 
  type="choro", 
  breaks = "jenks",
  nbreaks = 5,
  pal = "YlOrRd",
  border = "white",
  lwd = .1,
  alpha = 0.6,
  leg_pos = "topright",
  leg_title = "Indice de Pollution Potentielle",
  leg_horiz = TRUE,
  leg_size = 0.5,
  add = TRUE)
mf_layout(
  title = "Polluants chimiques - 4km",
  credits = "Charlotte BERTHIER, Sources: CASIAS & ActiviPol v.3, 2024")

par(mfrow = c(1,2))
mf_theme("agolalight")
mf_map(communes_idf,
       type='base',
       border = 'black',
       lwd = .3)
mf_map(
  pollution_grid_5km, 
  var ="IPP_chimique", 
  type="choro", 
  breaks = "jenks",
  nbreaks = 5,
  pal = "YlOrRd",
  border = "white",
  lwd = .1,
  alpha = 0.6,
  leg_pos = "topright",
  leg_title = "Indice de Pollution Potentielle",
  leg_horiz = TRUE,
  leg_size = 0.5,
  add = TRUE)
mf_layout(
  title = "Polluants chimiques - 5km",
  credits = "Charlotte BERTHIER, Sources: CASIAS & ActiviPol v.3, 2024")
mf_theme("agolalight")
mf_map(communes_idf,
       type='base',
       border = 'black',
       lwd = .3)
mf_map(
  pollution_grid_6km, 
  var ="IPP_chimique", 
  type="choro", 
  breaks = "jenks",
  nbreaks = 5,
  pal = "YlOrRd",
  border = "white",
  lwd = .1,
  alpha = 0.6,
  leg_pos = "topright",
  leg_title = "Indice de Pollution Potentielle",
  leg_horiz = TRUE,
  leg_size = 0.5,
  add = TRUE)
mf_layout(
  title = "Polluants chimiques - 6km",
  credits = "Charlotte BERTHIER, Sources: CASIAS & ActiviPol v.3, 2024")

par(mfrow = c(1,2))
mf_theme("agolalight")
mf_map(communes_idf,
       type='base',
       border = 'black',
       lwd = .3)
mf_map(
  pollution_grid_7km, 
  var ="IPP_chimique", 
  type="choro", 
  breaks = "jenks",
  nbreaks = 5,
  pal = "YlOrRd",
  border = "white",
  lwd = .1,
  alpha = 0.6,
  leg_pos = "topright",
  leg_title = "Indice de Pollution Potentielle",
  leg_horiz = TRUE,
  leg_size = 0.5,
  add = TRUE)
mf_layout(
  title = "Polluants chimiques - 7km",
  credits = "Charlotte BERTHIER, Sources: CASIAS & ActiviPol v.3, 2024")
mf_theme("agolalight")
mf_map(communes_idf,
       type='base',
       border = 'black',
       lwd = .3)
mf_map(
  pollution_grid_8km, 
  var ="IPP_chimique", 
  type="choro", 
  breaks = "jenks",
  nbreaks = 5,
  pal = "YlOrRd",
  border = "white",
  lwd = .1,
  alpha = 0.6,
  leg_pos = "topright",
  leg_title = "Indice de Pollution Potentielle",
  leg_horiz = TRUE,
  leg_size = 0.5,
  add = TRUE)
mf_layout(
  title = "Polluants chimiques - 8km",
  credits = "Charlotte BERTHIER, Sources: CASIAS & ActiviPol v.3, 2024")

par(mfrow = c(1,2))
mf_theme("agolalight")
mf_map(communes_idf,
       type='base',
       border = 'black',
       lwd = .3)
mf_map(
  pollution_grid_9km, 
  var ="IPP_chimique", 
  type="choro", 
  breaks = "jenks",
  nbreaks = 5,
  pal = "YlOrRd",
  border = "white",
  lwd = .1,
  alpha = 0.6,
  leg_pos = "topright",
  leg_title = "Indice de Pollution Potentielle",
  leg_horiz = TRUE,
  leg_size = 0.5,
  add = TRUE)
mf_layout(
  title = "Polluants chimiques - 9km",
  credits = "Charlotte BERTHIER, Sources: CASIAS & ActiviPol v.3, 2024")
mf_theme("agolalight")
mf_map(communes_idf,
       type='base',
       border = 'black',
       lwd = .3)
mf_map(
  pollution_grid_10km, 
  var ="IPP_chimique", 
  type="choro", 
  breaks = "jenks",
  nbreaks = 5,
  pal = "YlOrRd",
  border = "white",
  lwd = .1,
  alpha = 0.6,
  leg_pos = "topright",
  leg_title = "Indice de Pollution Potentielle",
  leg_horiz = TRUE,
  leg_size = 0.5,
  add = TRUE)
mf_layout(
  title = "Polluants chimiques - 10km",
  credits = "Charlotte BERTHIER, Sources: CASIAS & ActiviPol v.3, 2024")

par(mfrow = c(1,2))
mf_theme("agolalight")
mf_map(communes_idf,
       type='base',
       border = 'black',
       lwd = .3)
mf_map(
  pollution_grid_11km, 
  var ="IPP_chimique", 
  type="choro", 
  breaks = "jenks",
  nbreaks = 5,
  pal = "YlOrRd",
  border = "white",
  lwd = .1,
  alpha = 0.6,
  leg_pos = "topright",
  leg_title = "Indice de Pollution Potentielle",
  leg_horiz = TRUE,
  leg_size = 0.5,
  add = TRUE)
mf_layout(
  title = "Polluants chimiques - 11km",
  credits = "Charlotte BERTHIER, Sources: CASIAS & ActiviPol v.3, 2024")
mf_theme("agolalight")
mf_map(communes_idf,
       type='base',
       border = 'black',
       lwd = .3)
mf_map(
  pollution_grid_12km, 
  var ="IPP_chimique", 
  type="choro", 
  breaks = "jenks",
  nbreaks = 5,
  pal = "YlOrRd",
  border = "white",
  lwd = .1,
  alpha = 0.6,
  leg_pos = "topright",
  leg_title = "Indice de Pollution Potentielle",
  leg_horiz = TRUE,
  leg_size = 0.5,
  add = TRUE)
mf_layout(
  title = "Polluants chimiques - 12km",
  credits = "Charlotte BERTHIER, Sources: CASIAS & ActiviPol v.3, 2024")




#### NORMAL MAPS 200 ET 500m ####

mf_theme("agolalight")
mf_map(communes_idf,
       type='base',
       border = 'black',
       lwd = .2)
mf_map(
  pollution_grid_500m, 
  var ="IPP_chimique", 
  type="choro", 
  breaks = "jenks",
  nbreaks = 5,
  pal = "YlOrRd",
  border = "white",
  lwd = .1,
  alpha = 0.6,
  leg_pos = "bottomright",
  leg_title = "IPP",
  leg_frame = TRUE,
  leg_horiz = TRUE,
  leg_size = 0.5,
  add = TRUE)
mf_layout(
  title = "Polluants chimiques - 500m",
  credits = "Charlotte BERTHIER, Sources: CASIAS & ActiviPol v.3, 2024")

# "IPP_pfc_pfoa_pfos__carr"
# "pfc_pfoa_pfos__carr"







#### Carte points par communes - Carte faite en python /!\ ####

# DATA PREP

library(dplyr)

com_casias <- casias2154 %>%
  group_by(nom_commune) %>%
  summarise(nb_sites = n()) %>%
  arrange(desc(nb_sites)) %>%
  mutate(
    top10 = row_number() <= 10,
    top20 = row_number() >10 & row_number() <= 20,
    top100 = row_number() > 20 & row_number() <= 100,
    above200 = nb_sites >= 200,
    above100 = nb_sites >=100)


com_casias$classement <- ifelse(com_casias$top10, "Top10", ifelse(com_casias$top20, "Top20", ifelse(com_casias$top100, "Top100", NA)))

st_crs(com_casias)

st_write(obj = com_casias, 
         dsn = "/data/casias_com.gpkg", 
         layer = "com_casias", 
         delete_layer = TRUE)



# COLORS
transparent_color <- rgb(160,32,240, alpha = 0, maxColorValue = 255)
com_casias$color <- ifelse(com_casias$nom_commune %in% top10_communes,
                           rgb(160,32,240, alpha = 0.8, maxColorValue = 255),
                           NA)

com_casias_filtre <- com_casias[com_casias$classement != "Autre", ]

# MAP WITH MAPSF 
mf_theme("agolalight")
mf_map(communes_idf,
       type = "base")
mf_map(
  com_casias[com_casias$above200 == TRUE, ], 
  var = "nb_sites",
  type = "prop",
  symbol = "circle",
  inches = 0.25,
  col = "purple",
  border = "white",
  lwd = 1,
  leg_pos = "bottomleft",
  leg_adj = c(1,2),
  leg_title = "Nb de sites Casias",
  add = TRUE)
mf_layout(
  title = "Nombre de sites CASIAS par communes",
  credits = paste0(
    "Charlotte BERTHIER - 2024\n",
    "Sources: CASIAS & ActiviPol v.3"))


mf_map(
  com_casias_filtre[com_casias_filtre$classement == "Top100", ], 
  var = c("nb_sites", "classement"),
  type = "prop_typo",
  symbol = "circle",
  border = "white",
  lwd = 0.5,
  leg_pos = "bottomleft",
  leg_title = c("Nb de sites Casias", "Ranking"),
  add = TRUE)

mf_map(
  com_casias_filtre[com_casias_filtre$classement == "Top20", ], 
  var = c("nb_sites", "classement"),
  type = "prop_typo",
  symbol = "circle",
  col = "red",
  border = "white",
  lwd = 0.5,
  leg_pos = "bottomleft",
  leg_title = c("Nb de sites Casias", "Ranking"),
  add = TRUE)

# Ajouter les cercles pour Top10
mf_map(
  com_casias_filtre[com_casias_filtre$classement == "Top10", ], 
  var = c("nb_sites", "classement"),
  type = "prop_typo",
  symbol = "circle",
  col = "blue",
  border = "white",
  lwd = 0.5,
  leg_pos = "bottomleft",
  leg_title = c("Nb de sites Casias", "Ranking"),
  add = TRUE)



"""
AUTRE POSSIBLITE POUR RESHAPE EN LONG
# Reshape du dataframe en format LONG pour le graphique
methode2_synthese_grille_pourc_long.1 <- reshape(methode2_synthese_grille_pourc,
                                  direction = "long", 
                                  varying = list(names(methode2_synthese_grille_pourc[9:16])),
                                  v.names = "Pourcentage_carreaux",
                                  idvar = c("grid_name"),
                                  timevar = "Groupe_de_substances",
                                  times = names(methode2_synthese_grille_pourc[9:16]))
"""