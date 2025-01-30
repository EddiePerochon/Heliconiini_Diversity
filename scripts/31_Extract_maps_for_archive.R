##### Script 31: Create only archives for distribution maps #####

##################################
#       Author: Maël Doré        #
#  Contact: mael.dore@gmail.com  #
##################################


### Goals = 
  # Extract distribution maps for ssp, OMU, species, and phenotypic groups to create the only archive
###


### Inputs
  # Range maps of Heliconiini generated for this study
###

### Outputs
  # Organize folder with all distribution maps for ssp, OMU, species, and phenotypic groups
  # Both format: PDF for vizualization and RDS for GIS in R
###

### Reference for the online archive: 10.5281/zenodo.10903661


# Clean environment
rm(list = ls())

##### 1/ Load stuff ####

### 1.1/ Load libraries ####

library(tidyverse)
library(raster)
library(sf)
library(qpdf)

### 1.2/ Load taxa lists ####

load(file = paste0("./input_data/occurences_df/list_occ_after_clipping_rs.RData"))

list_ssp <- list_occ[, c("biomodTag"), drop = F]
list_ssp$subspecies <- str_replace_all(string = list_ssp$biomodTag, pattern = "\\.", "_")
list_ssp$subspecies[list_ssp$subspecies == "Agraulis_spnov"] <- "Agraulis_sp.nov"
list_ssp$subspecies[list_ssp$subspecies == "Heliconius_demeter_subspnov"] <- "Heliconius_demeter_subsp.nov"

list_OMU <- list_occ[, c("OMU_ss"), drop = F] %>% 
  distinct(OMU_ss)
list_OMU$OMU <- str_replace_all(string = list_OMU$OMU_ss, pattern = "\\.", "_")
list_OMU$OMU[list_OMU$OMU == "Agraulis_spnov_VANILLAE"] <- "Agraulis_sp.nov_VANILLAE"

list_species <- list_occ[, c("Sp_ID"), drop = F] %>% 
  distinct(Sp_ID)
list_species$species <- str_replace_all(string = list_species$Sp_ID, pattern = "\\.", "_") 
list_species$species[list_species$species == "Agraulis_spnov"] <- "Agraulis_sp.nov"

list_ring <- list_occ[, c("MR_ss"), drop = F] %>%
  distinct(MR_ss)
list_ring$PHENOTYPIC_GROUP <- list_ring$MR_ss

### 1.3/ Load stacks of maps

All_ssp_proba_stack <- readRDS(file = "./outputs/Indices_stacks/All_ssp_proba_stack_Jaccard.80.rds")
All_OMU_proba_stack <- readRDS(file = "./input_data/Species_data/OMU_proba_stack_ss.RDS")
All_sp_proba_stack <- readRDS(file = "./outputs/Indices_stacks/All_sp_proba_stack_Jaccard.80.rds")
All_ring_proba_stack <- readRDS(file = "./outputs/Indices_stacks/All_ring_proba_stack_Jaccard_ss.80.rds")
All_ring_richness_stack <- readRDS(file = "./outputs/Indices_stacks/All_ring_rich_stack_Jaccard_ss.80.rds")

plot(All_ssp_proba_stack)

nlayers(All_ssp_proba_stack) # 439 ssp
nlayers(All_OMU_proba_stack) # 141 OMU (ss)
nlayers(All_sp_proba_stack) # 74 species
nlayers(All_ring_proba_stack) # 38 phenotypic groups (ss)
nlayers(All_ring_richness_stack) # 38 phenotypic groups (ss)

### 1.4/ Load occurrence dataset ####

# All occurrences
load(file = "./input_data/occurences_df/Occ_MW_df.RData")
# Occurrences used in SDM
Occ_used_for_SDM <- readRDS("./input_data/occurences_df/Occ_used_for_SDM.RDS")

# # Filter occurrences to keep only the ones used in SDM
# matching_vector <- paste0(Occ_df$code_unit, "_", round(Occ_df$Lat, 1), "_", round(Occ_df$Lon, 1))
# matching_vector_for_SDM <- paste0(Occ_used_for_SDM$Tag, "_", round(Occ_used_for_SDM$LatWGS, 1), "_", round(Occ_used_for_SDM$LonWGS, 1))
# 
# matching_vector[1:10]
# matching_vector_for_SDM[1:10]
# 
# table(matching_vector %in% matching_vector_for_SDM)

Occ_df$code_unit[Occ_df$code_unit == "Heliconius_demeter_subsp._nov."] <- "Heliconius_demeter_subsp.nov"
Occ_df$OMU_ss[Occ_df$OMU_ss == "Agraulis.sp_nov.VANILLAE"] <- "Agraulis.spnov.VANILLAE"

table(Occ_df$code_unit %in% list_ssp$subspecies)
table(list_ssp$subspecies %in% Occ_df$code_unit)

table(Occ_df$OMU_ss %in% list_OMU$OMU_ss)
table(list_OMU$OMU_ss %in% Occ_df$OMU_ss)

names(All_ssp_proba_stack)

##### 2/ Deal with ssp maps ####

identical(names(All_ssp_proba_stack), list_ssp$biomodTag)

### 2.1/ Plot ssp maps + save rds ###

for (i in 1:nlayers(All_ssp_proba_stack))
{
  # i <- 1
  # i <- 2
  
  ssp_i <- list_ssp$subspecies[i]
  
  # Extract raster layer
  ssp_raster_i <- raster::subset(x = All_ssp_proba_stack, subset = i)
  names(ssp_raster_i) <- ssp_i
  
  # Export RDS
  saveRDS(ssp_raster_i, file = paste0("./maps/Distribution_maps/Subspecies_maps/",ssp_i,"_map.rds"))
  
  # Extract occurrence data
  Occ_df_i <- Occ_df[Occ_df$code_unit == ssp_i, ]
  
  # Convert to sp object
  unit_occ_points_sp
  unit_occ_points_sp <- SpatialPointsDataFrame(coords = Occ_df_i[, c("longitude_MW", "latitude_MW")], 
                                               data = Occ_df_i,
                                               proj4string = CRS("+proj=moll +lon_0=-79 +lat_0=6 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"))
  
  # Function adding the points to the plot on each layer
  add_occ_points <- function() 
  {
    plot(unit_occ_points_sp, add = TRUE, col = "red", pch = 16, cex = 0.3)
  }
  
  # Plot raster + occurrences
  pdf(file = paste0("./maps/Distribution_maps/Subspecies_maps/",ssp_i,"_map.pdf"), height = 6, width = 7)
  
  plot(ssp_raster_i, main = ssp_i, addfun = add_occ_points)
  
  dev.off()
  
  # Need to convert lat/long...
 
  if(i %% 10 == 0)
  {
    cat(paste0(Sys.time(), " - Map plotted for ssp n°",i, "/",nlayers(All_ssp_proba_stack),"\n"))
  }
}

### 2.2/ Aggregate all ssp maps ####

# List all PDF
all_maps_path <- list.files(path = "./maps/Distribution_maps/Subspecies_maps/", pattern = "_map.pdf", full.names = T)
nb_maps <- length(all_maps_path)

# Combine all maps
qpdf::pdf_combine(input = all_maps_path, output = paste0("./maps/Distribution_maps/Subspecies_maps/0_All_ssp_maps.pdf"))


### 2.3/ Export stack of all maps ####

names(All_ssp_proba_stack) <- list_ssp$subspecies
saveRDS(object = All_ssp_proba_stack, file = "./maps/Distribution_maps/Subspecies_maps/0_All_ssp_maps.rds")


##### 3/ Deal with OMU maps ####

# Check whether matching orders
identical(names(All_OMU_proba_stack), list_OMU$OMU_ss)

### 3.1/ Plot OMU maps + save rds ###

for (i in 1:nlayers(All_OMU_proba_stack))
{
  # i <- 1
  # i <- 7
  
  OMU_i <- list_OMU$OMU[i]
  OMU_ss_i <- list_OMU$OMU_ss[i]
  
  # Extract raster layer
  OMU_raster_i <- raster::subset(x = All_OMU_proba_stack, subset = i)
  names(OMU_raster_i) <- OMU_i
  
  # Export RDS
  saveRDS(OMU_raster_i, file = paste0("./maps/Distribution_maps/OMU_maps/",OMU_i,"_map.rds"))
  
  # Extract occurrence data
  Occ_df_i <- Occ_df[Occ_df$OMU_ss == OMU_ss_i, ]
  
  # Convert to sp object
  unit_occ_points_sp
  unit_occ_points_sp <- SpatialPointsDataFrame(coords = Occ_df_i[, c("longitude_MW", "latitude_MW")], 
                                               data = Occ_df_i,
                                               proj4string = CRS("+proj=moll +lon_0=-79 +lat_0=6 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"))
  
  # Function adding the points to the plot on each layer
  add_occ_points <- function() 
  {
    plot(unit_occ_points_sp, add = TRUE, col = "red", pch = 16, cex = 0.3)
  }
  
  # Plot raster + occurrences
  pdf(file = paste0("./maps/Distribution_maps/OMU_maps/",OMU_i,"_map.pdf"), height = 6, width = 7)
  
  plot(OMU_raster_i, main = OMU_i, addfun = add_occ_points)
  
  dev.off()
  
  # Need to convert lat/long...
  
  if(i %% 10 == 0)
  {
    cat(paste0(Sys.time(), " - Map plotted for OMU n°",i, "/",nlayers(All_OMU_proba_stack),"\n"))
  }
}

### 3.2/ Aggregate all OMU maps ####

# List all PDF
all_maps_path <- list.files(path = "./maps/Distribution_maps/OMU_maps/", pattern = "_map.pdf", full.names = T)
nb_maps <- length(all_maps_path)

# Combine all maps
qpdf::pdf_combine(input = all_maps_path, output = paste0("./maps/Distribution_maps/OMU_maps/0_All_OMU_maps.pdf"))


### 3.3/ Export stack of all maps ####

names(All_OMU_proba_stack) <- list_OMU$OMU
saveRDS(object = All_OMU_proba_stack, file = "./maps/Distribution_maps/OMU_maps/0_All_OMU_maps.rds")



##### 4/ Deal with species maps ####

# Check whether matching orders
identical(names(All_sp_proba_stack), list_species$species)

Occ_df$Genus_species <- paste0(Occ_df$genus, ".", Occ_df$esp)
Occ_df$Genus_species <- str_remove_all(string = Occ_df$Genus_species, pattern = "_")
  
### 4.1/ Plot sp maps + save rds ###

for (i in 1:nlayers(All_sp_proba_stack))
{
  # i <- 1
  # i <- 7

  sp_i <- list_species$species[i]
  Sp_ID_i <- list_species$Sp_ID[i]
  
  # Extract raster layer
  sp_raster_i <- raster::subset(x = All_sp_proba_stack, subset = i)
  names(sp_raster_i) <- sp_i
  
  # Export RDS
  saveRDS(sp_raster_i, file = paste0("./maps/Distribution_maps/Species_maps/",sp_i,"_map.rds"))
  
  # Extract occurrence data
  Occ_df_i <- Occ_df[Occ_df$Genus_species == Sp_ID_i, ]
  
  # Convert to sp object
  unit_occ_points_sp
  unit_occ_points_sp <- SpatialPointsDataFrame(coords = Occ_df_i[, c("longitude_MW", "latitude_MW")], 
                                               data = Occ_df_i,
                                               proj4string = CRS("+proj=moll +lon_0=-79 +lat_0=6 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"))
  
  # Function adding the points to the plot on each layer
  add_occ_points <- function() 
  {
    plot(unit_occ_points_sp, add = TRUE, col = "red", pch = 16, cex = 0.3)
  }
  
  # Plot raster + occurrences
  pdf(file = paste0("./maps/Distribution_maps/Species_maps/",sp_i,"_map.pdf"), height = 6, width = 7)
  
  plot(sp_raster_i, main = sp_i, addfun = add_occ_points)
  
  dev.off()
  
  # Need to convert lat/long...
  
  if(i %% 10 == 0)
  {
    cat(paste0(Sys.time(), " - Map plotted for species n°",i, "/",nlayers(All_sp_proba_stack),"\n"))
  }
}

### 4.2/ Aggregate all species maps ####

# List all PDF
all_maps_path <- list.files(path = "./maps/Distribution_maps/Species_maps/", pattern = "_map.pdf", full.names = T)
nb_maps <- length(all_maps_path)

# Combine all maps
qpdf::pdf_combine(input = all_maps_path, output = paste0("./maps/Distribution_maps/Species_maps/0_All_sp_maps.pdf"))


### 4.3/ Export stack of all maps ####

names(All_sp_proba_stack) <- list_species$species
saveRDS(object = All_sp_proba_stack, file = "./maps/Distribution_maps/Species_maps/0_All_sp_maps.rds")


##### 5/ Deal with phenotypic group maps ####

# Check whether matching orders
identical(names(All_ring_proba_stack), list_ring$PHENOTYPIC_GROUP)
identical(names(All_ring_richness_stack), list_ring$PHENOTYPIC_GROUP)

### 5.1/ Plot ring maps + save rds ###

# Both proba (ranges) and richness

for (i in 1:nlayers(All_ring_proba_stack))
{
  # i <- 1
  # i <- 7
  
  ring_i <- list_ring$PHENOTYPIC_GROUP[i]
  
  # Extract raster layer
  ring_proba_raster_i <- raster::subset(x = All_ring_proba_stack, subset = i)
  names(ring_proba_raster_i) <- ring_i
  ring_richness_raster_i <- raster::subset(x = All_ring_richness_stack, subset = i)
  names(ring_richness_raster_i) <- ring_i
  
  # Export RDS
  saveRDS(ring_proba_raster_i, file = paste0("./maps/Distribution_maps/Phenotypic_group_maps/Range_maps/Phenotypic_group_range_map_",ring_i,".rds"))
  saveRDS(ring_richness_raster_i, file = paste0("./maps/Distribution_maps/Phenotypic_group_maps/Richness_maps/Phenotypic_group_richness_map_",ring_i,".rds"))
  
  # Extract occurrence data
  Occ_df_i <- Occ_df[Occ_df$Ring_ss == ring_i, ]
  
  # Convert to sp object
  unit_occ_points_sp
  unit_occ_points_sp <- SpatialPointsDataFrame(coords = Occ_df_i[, c("longitude_MW", "latitude_MW")], 
                                               data = Occ_df_i,
                                               proj4string = CRS("+proj=moll +lon_0=-79 +lat_0=6 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"))
  
  # Function adding the points to the plot on each layer
  add_occ_points <- function() 
  {
    plot(unit_occ_points_sp, add = TRUE, col = "red", pch = 16, cex = 0.3)
  }
  
  # Plot raster + occurrences for range maps
  pdf(file = paste0("./maps/Distribution_maps/Phenotypic_group_maps/Range_maps/Phenotypic_group_range_map_",ring_i,".pdf"), height = 6, width = 7)
  
  plot(ring_proba_raster_i, main = ring_i, addfun = add_occ_points)
  
  dev.off()
  
  # Plot raster + occurrences for richness maps
  pdf(file = paste0("./maps/Distribution_maps/Phenotypic_group_maps/Richness_maps/Phenotypic_group_richness_map_",ring_i,".pdf"), height = 6, width = 7)
  
  plot(ring_richness_raster_i, main = ring_i, addfun = add_occ_points)
  
  dev.off()
  
  # Need to convert lat/long...
  
  if(i %% 10 == 0)
  {
    cat(paste0(Sys.time(), " - Map plotted for ring n°",i, "/",nlayers(All_ring_proba_stack),"\n"))
  }
}

### 5.2/ Aggregate all ring maps ####

## For range maps

# List all PDF
all_maps_path <- list.files(path = "./maps/Distribution_maps/Phenotypic_group_maps/Range_maps/", pattern = ".pdf", full.names = T)
nb_maps <- length(all_maps_path)

# Combine all maps
qpdf::pdf_combine(input = all_maps_path, output = paste0("./maps/Distribution_maps/Phenotypic_group_maps/Range_maps/0_All_phenotypic_groups_range_maps.pdf"))

## For richness maps

# List all PDF
all_maps_path <- list.files(path = "./maps/Distribution_maps/Phenotypic_group_maps/Richness_maps/", pattern = ".pdf", full.names = T)
nb_maps <- length(all_maps_path)

# Combine all maps
qpdf::pdf_combine(input = all_maps_path, output = paste0("./maps/Distribution_maps/Phenotypic_group_maps/Richness_maps/0_All_phenotypic_groups_richness_maps.pdf"))


### 5.3/ Export stack of all maps ####

## For range maps
names(All_ring_proba_stack) <- list_ring$PHENOTYPIC_GROUP
saveRDS(object = All_ring_proba_stack, file = "./maps/Distribution_maps/Phenotypic_group_maps/Range_maps/0_All_phenotypic_groups_range_maps.rds")

## For richness maps
names(All_ring_richness_stack) <- list_ring$PHENOTYPIC_GROUP
saveRDS(object = All_ring_richness_stack, file = "./maps/Distribution_maps/Phenotypic_group_maps/Richness_maps/0_All_phenotypic_groups_richness_maps.rds")


