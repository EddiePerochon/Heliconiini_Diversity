
###### Script 30: Map bioregions ######

# Map bioregions used in the analyses (Figure S3)

###

# Inputs: 
   # Natural Earth political map
   # WWF Ecoregions (Olson et al., 2001)
   # Neotropical Provinces by Morrone et al., 2022

# Outputs:
   # Map of bioregions (Figure S3)
###


##### 1/ Load stuff ####

# Clean environment
rm(list = ls())

library(raster)
library(sf)
library(rnaturalearth)
library(tidyverse)
library(ggspatial)

### 1.1/ Load reference raster to delineate bbox ####

Helico_sp_richness <- readAll(readRDS(file = paste0("../outputs/Indices_maps/tot.sp.richness_Jaccard.80.rds")))

plot(Helico_sp_richness)
bbox_reference_Mollweide <- extent(Helico_sp_richness)

### 1.2/ Load geopolitical map of countries ####

?ne_countries()

country_boundaries <- ne_countries(scale = "medium", type = "countries", continent = c("North America", "South America"))
country_boundaries <- country_boundaries[, c("admin", "adm0_a3")]
  
plot(country_boundaries[, "admin"])

### 1.3/ Fuse to obtain terrestrial boundaries as a polygon ####

?ne_coastline() # To obtain LINESTRING
?st_union

terrestrial_boundaries <- st_sf(st_union(country_boundaries))

### 1.3/ Load Morrone's provinces 

Morrone_provinces <- st_read(dsn = "../input_data/Map_stuff/Morrone_Neotropical_provinces/", layer = "NeotropicMap_Geo")

plot(Morrone_province[, "Provincias"])

### 1.4/ Load Brazilian Atlantic forest shp

Mata_Atlantica_shp <- readRDS("D:/Mael_D/R_projects/Heliconiini_diversity_Eddie/input_data/Map_stuff/Mata_Atlantica_shp.rds")

##### 2/ Merge Morrone's provinces into bioregions (arbitrarily) ####

### 2.1/ Merge provinces by bioregions ####

## Central America = Mexican Transition Zone (Subregion) + Mesoamerican (Dominio) + Guatuso-Talamanca province + Puntarenas-Chiriqui province

Central_America_sf <- Morrone_provinces[(Morrone_provinces$Subregion == "Mexican Transition Zone") | (Morrone_provinces$Dominio == "Mesoamerican") | (Morrone_provinces$Provincias %in% c("Guatuso-Talamanca province", "Puntarenas-Chiriqui province")), ]
Central_America_sf <- st_sf(st_union(Central_America_sf))

st_geometry(Central_America_sf) <- "geometry"
Central_America_sf$Bioregion <- "Central_America"

Central_America_sf <- nngeo::st_remove_holes(Central_America_sf)

plot(Central_America_sf)

## Carribean Islands = Antillean (Subregion)

Carribean_Islands_sf <- Morrone_provinces[(Morrone_provinces$Subregion == "Antillean"), ]
Carribean_Islands_sf <- st_sf(st_union(Carribean_Islands_sf))

st_geometry(Carribean_Islands_sf) <- "geometry"
Carribean_Islands_sf$Bioregion <- "Carribean_Islands"

plot(Carribean_Islands_sf)

## Western lowlands = Pacific (Dominio) - [Guatuso-Talamanca province and Puntarenas-Chiriqui province] - [Cauca province]

Western_lowlands_sf <- Morrone_provinces[(Morrone_provinces$Dominio == "Pacific") & !(Morrone_provinces$Provincias %in% c("Guatuso-Talamanca province", "Puntarenas-Chiriqui province", "Cauca province")), ]
Western_lowlands_sf <- st_sf(st_union(Western_lowlands_sf))

st_geometry(Western_lowlands_sf) <- "geometry"
Western_lowlands_sf$Bioregion <- "Western_lowlands"

plot(Western_lowlands_sf)

## Northern Andes = Paramo province + Cauca province

Northern_Andes_sf <- Morrone_provinces[(Morrone_provinces$Provincias %in% c("Paramo province", "Cauca province")), ]
Northern_Andes_sf <- st_sf(st_union(Northern_Andes_sf))

st_geometry(Northern_Andes_sf) <- "geometry"
Northern_Andes_sf$Bioregion <- "Northern_Andes"

plot(Northern_Andes_sf)

## Central Andes = Puna province + Cuyan High Andean province

Central_Andes_sf <- Morrone_provinces[(Morrone_provinces$Provincias %in% c("Puna province", "Cuyan High Andean province")), ]
Central_Andes_sf <- st_sf(st_union(Central_Andes_sf))

st_geometry(Central_Andes_sf) <- "geometry"
Central_Andes_sf$Bioregion <- "Central_Andes"

plot(Central_Andes_sf)

## Coastal Deserts = Desert province + Atacama province

Coastal_Deserts_sf <- Morrone_provinces[(Morrone_provinces$Provincias %in% c("Desert province", "Atacama province")), ]
Coastal_Deserts_sf <- st_sf(st_union(Coastal_Deserts_sf))

st_geometry(Coastal_Deserts_sf) <- "geometry"
Coastal_Deserts_sf$Bioregion <- "Coastal_Deserts"

plot(Coastal_Deserts_sf)

## Guyana Shield = Guianan Lowlands province + Guianan province + Roraima province

Guyana_Shield_sf <- Morrone_provinces[(Morrone_provinces$Provincias %in% c("Guianan Lowlands province", "Guianan province", "Roraima province")), ]
Guyana_Shield_sf <- st_sf(st_union(Guyana_Shield_sf))

st_geometry(Guyana_Shield_sf) <- "geometry"
Guyana_Shield_sf$Bioregion <- "Guyana_Shield"

plot(Guyana_Shield_sf)

## Amazon basin = South Brazilian (Dominio) + South-eastern Amazonian (Dominio) + Napo province + Imeri province + Para province

Amazon_Basin_sf <- Morrone_provinces[(Morrone_provinces$Dominio %in% c("South Brazilian", "South-eastern Amazonian")) | (Morrone_provinces$Provincias %in% c("Napo province", "Imeri province")), ]
sf_use_s2(FALSE)
Amazon_Basin_sf <- st_sf(st_union(Amazon_Basin_sf))
sf_use_s2(TRUE)

Para_sf <- Morrone_provinces[(Morrone_provinces$Provincias %in% c("Para province")), ]
sf_use_s2(FALSE)
Para_sf <- st_buffer(x = Para_sf, dist = 0.02)
sf_use_s2(TRUE)

plot(Para_sf[, "Provincias"])
plot(Amazon_Basin_sf)

sf_use_s2(FALSE)
Amazon_Basin_sf <- st_sf(st_union(Amazon_Basin_sf, Para_sf))
Amazon_Basin_sf <- st_sf(st_union(Amazon_Basin_sf))
sf_use_s2(TRUE)

st_geometry(Amazon_Basin_sf) <- "geometry"
Amazon_Basin_sf$Bioregion <- "Amazon_Basin"

plot(Amazon_Basin_sf)

## Caatinga = Caatinga province

Caatinga_sf <- Morrone_provinces[(Morrone_provinces$Provincias %in% c("Caatinga province")), ]
Caatinga_sf <- st_sf(st_union(Caatinga_sf))

st_geometry(Caatinga_sf) <- "geometry"
Caatinga_sf$Bioregion <- "Caatinga"

plot(Caatinga_sf)

## Cerrado = Cerrado province

Cerrado_sf <- Morrone_provinces[(Morrone_provinces$Provincias %in% c("Cerrado province")), ]
Cerrado_sf <- st_sf(st_union(Cerrado_sf))

st_geometry(Cerrado_sf) <- "geometry"
Cerrado_sf$Bioregion <- "Cerrado"

plot(Cerrado_sf)

## Chacos = Chaco province + Monte province + Comechingones province

Chacos_sf <- Morrone_provinces[(Morrone_provinces$Provincias %in% c("Chaco province", "Monte province", "Comechingones province")), ]
sf_use_s2(FALSE)
Chacos_sf <- st_sf(st_union(Chacos_sf))
sf_use_s2(TRUE)

st_geometry(Chacos_sf) <- "geometry"
Chacos_sf$Bioregion <- "Chacos"

plot(Chacos_sf)

## Pampa = Pampean province + Esteros del Ibera province (need buffer on it)

Pampa_sf <- Morrone_provinces[(Morrone_provinces$Provincias %in% c("Pampean province")), ]
Pampa_rivers_sf <- Morrone_provinces[(Morrone_provinces$Provincias %in% c("Esteros del Ibera province")), ]
sf_use_s2(FALSE)
Pampa_rivers_sf <- st_buffer(x = Pampa_rivers_sf, dist = 0.05)
sf_use_s2(TRUE)

plot(Pampa_sf[, "Provincias"])
plot(Pampa_rivers_sf[, "Provincias"])

sf_use_s2(FALSE)
Pampa_sf <- st_sf(st_union(Pampa_sf, Pampa_rivers_sf))
Pampa_sf <- st_sf(st_union(Pampa_sf))
sf_use_s2(TRUE)

st_geometry(Pampa_sf) <- "geometry"
Pampa_sf$Bioregion <- "Pampa"

plot(Mata_Atlantica_shp)
plot(Pampa_sf, add = T)

## Brazilian Atlantic Forest = Parana Dominio - Esteros del Ibera province

BAF_sf <- Morrone_provinces[(Morrone_provinces$Dominio %in% c("Parana")) & !(Morrone_provinces$Provincias %in% c("Esteros del Ibera province")), ]
BAF_sf <- st_sf(st_union(BAF_sf))
st_geometry(BAF_sf) <- "geometry"
BAF_sf$Bioregion <- "BAF"

BAF_sf <- nngeo::st_remove_holes(BAF_sf)

plot(BAF_sf)


## Nearctic = Substract Neotropics from terrestrial boundaries + Remove anything below Latitude 18.8° (use a buffer to make it clean)

Neotropics_sf <- Morrone_provinces[(Morrone_provinces$Region %in% c("Neotropical")), ]
sf_use_s2(FALSE)
Neotropics_sf <- st_sf(st_union(Neotropics_sf))
sf_use_s2(TRUE)

plot(Neotropics_sf)

plot(terrestrial_boundaries)

sf_use_s2(FALSE)
Neotropics_sf_buffered <- st_buffer(x = Neotropics_sf, dist = 0.05)
Not_Neotropics_sf <- st_difference(terrestrial_boundaries, Neotropics_sf_buffered)
Not_Neotropics_sf <- st_sf(st_union(Not_Neotropics_sf))
sf_use_s2(TRUE)

plot(Not_Neotropics_sf)

bbox_reference_Mollweide
crs(Nearctic_sf)

Nearctic_limits <- st_bbox(obj = c(xmin = -180, xmax = 180, ymin = 18.8, ymax = 90), crs = crs(Nearctic_sf))

sf_use_s2(FALSE)
Nearctic_sf <- st_crop(x = Not_Neotropics_sf, y = Nearctic_limits)
sf_use_s2(TRUE)

st_geometry(Nearctic_sf) <- "geometry"
Nearctic_sf$Bioregion <- "Nearctic"

plot(Nearctic_sf)

## Southern America = Substract Neotropics from terrestrial boundaries + Remove anything above Latitude -23.0° (use a buffer to make it clean)

Southern_America_limits <- st_bbox(obj = c(xmin = -180, xmax = 180, ymin = -90, ymax = -23), crs = crs(Nearctic_sf))

sf_use_s2(FALSE)
Southern_America_sf <- st_crop(x = Not_Neotropics_sf, y = Southern_America_limits)
sf_use_s2(TRUE)

st_geometry(Southern_America_sf) <- "geometry"
Southern_America_sf$Bioregion <- "Southern_America"

plot(Southern_America_sf)

### 2.2/ Combine all bioregions ####

Bioregions_sf <- rbind(Central_America_sf, Carribean_Islands_sf, Western_lowlands_sf, Northern_Andes_sf, Central_Andes_sf, Coastal_Deserts_sf, Guyana_Shield_sf, Amazon_Basin_sf, Caatinga_sf, Cerrado_sf, Chacos_sf, Pampa_sf, BAF_sf, Nearctic_sf, Southern_America_sf)

plot(Bioregions_sf)

saveRDS(object = Bioregions_sf, file = "../input_data/Map_stuff/Bioregions_sf.rds")

##### 3/ Adjust CRS, bbox, resolution ####

Bioregions_sf <- readRDS(file = "../input_data/Map_stuff/Bioregions_sf.rds")
Helico_sp_richness <- readAll(readRDS(file = paste0("../outputs/Indices_maps/tot.sp.richness_Jaccard.80.rds")))

crs(Helico_sp_richness)
bbox_reference_Mollweide

Bioregions_sf_Mollweide <- st_transform(Bioregions_sf, crs = st_crs(Helico_sp_richness))
country_boundaries_Mollweide <- st_transform(country_boundaries, crs = st_crs(Helico_sp_richness))
terrestrial_boundaries_Mollweide <- st_transform(terrestrial_boundaries, crs = st_crs(Helico_sp_richness))


##### 4/ Plot using ggplot2 ####

# Prepare fill scale
bioregion_levels <- levels(as.factor(Bioregions_sf$Bioregion))
bioregion_labels <- c("Amazon basin", "Brazilian Atlantic Forest", "Caatinga", "Carribean islands", "Central America", "Central Andes", "Cerrado", "Chacos", "Coastal deserts", "Guyana shield", "Nearctic", "Northern Andes", "Pampa", "Southern America", "Western lowlands")
bioregion_colors <- c("darkgreen", "limegreen", "chocolate1", "darkviolet", "firebrick1", "burlywood4", "darkgoldenrod1", "bisque", "gold1", "seagreen", "steelblue", "orange4", "yellow2", "azure2", "darkolivegreen2")
 
# GGplot
bioregions_map_ggplot <- ggplot(data = Bioregions_sf_Mollweide) +
  geom_sf(aes(fill = Bioregion), col = "grey20") +
  
  # Add country borders
  geom_sf(data = country_boundaries_Mollweide, fill = NA, col = "grey90", linewidth = 0.7) +
  
  # Add terrestrial limits
  geom_sf(data = terrestrial_boundaries_Mollweide, fill = NA, col = "grey20") +
  
  # Set limits
  coord_sf(xlim = c(-5456.522, 4953.478), ylim = c(-4494.85, 5705.15), expand = FALSE) +
  
  scale_fill_manual(breaks = bioregion_levels, values = bioregion_colors, labels = bioregion_labels) +
  
  # Add scale
  annotation_scale(location = "bl",
                   bar_cols = c("black"),
                   pad_x = unit(0.1, "npc"),
                   pad_y = unit(0.05, "npc"),
                   width_hint = 0.2,
                   text_cex = 0) +
  
  # Add scale legend
  annotate(geom = "text", x = -3500, y = -3400, label = "2000 km", size = 7) +
  
  # Remove axis titles
  xlab("") +
  ylab("") +
  
  # # Add north arrow
  # annotation_north_arrow(location = "tr", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  
  theme(panel.grid.major = element_line(color = "grey70", linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "aliceblue", color = "black"),
        legend.title = element_text(size  = 20, margin = margin(b = 8)), 
        legend.text = element_text(size = 15),
        legend.key = element_rect(colour = NA, size = 5),
        legend.key.size = unit(1.8, "line"),
        legend.spacing.y = unit(0.5, "line"),
        axis.text = element_text(size = 18, color = "black"),
        axis.text.x = element_text(margin = margin(t = 10)),
        axis.text.y = element_text(margin = margin(r = 8)))

  
pdf(file = "../Maps/Bioregions_map.pdf", width = 10, height = 10)
print(bioregions_map_ggplot)
dev.off()




{
  map_indices_Mollweide <- function(x,                                    # Raster to map
                                    color_palette = pal_bl_red_Mannion,   # Color palette
                                    main_title,                           # Main title
                                    main_title_cex = 1.5,                 # Main title size
                                    
                                    xlim = c(-4600, 4600),   # Limit of plot on x-axis (Longitude)
                                    ylim = c(-4450, 3400),    # Limit of plot on y-axis (Latitude)
                                    axis_cex = 1.4,             # Axes size
                                    
                                    xlab = "",                # X-axis label
                                    ylab = "",                # Y-axis label
                                    x_axis_breaks = c(-3930, -2170, -420, 1500, 3050),            # X-axis tick breaks
                                    y_axis_breaks = c(-3650, -2450, -1220, 0, 1230, 2445),        # Y-axis tick breaks
                                    x_axis_labels = c("120°E", "100°E", "80°E", "60°E", "40°E"),      # X-axis tick labels
                                    y_axis_labels = c("30°S", "20°S", "10°S", "0°", "10°N", "20°N"),  # Y-axis tick labels
                                    
                                    legend_title,             # Legend title
                                    legend_title_cex = 1.4,   # Legend title size
                                    legend_title_x = -3550,   # Legend title x position
                                    legend_title_y = 430,     # Legend title y position
                                    legend_cex = 1.4,         # Legend size
                                    legend_breaks,            # Legend tick positions
                                    legend_location = c(-4100, -3800, -3950, 0),  # Legend position
                                    
                                    scale_bar_position = c(-2600, -4000),  # Scale bar position
                                    
                                    arrow_scale = 0.45,           # North arrow size
                                    arrow_padin = c(0.15, 0.15),  # North arrow position adjustement
                                    
                                    facet_letter = "",                  # Small case letter for facet
                                    facet_letter_col = "black",         # Color of case letter for facet
                                    facet_letter_cex = 2.2,             # Size of small case letter for facet
                                    facet_letter_inset = c(0, -0.008))  # Position adjustment of small case letter for facet
  
  {
    # Plot raster background without axis
    image(x, col = color_palette,
          xlim = xlim, ylim = ylim, axes = F,
          xlab = xlab, ylab = ylab)
    title(main = main_title, cex.main = main_title_cex, line = 1)
    
    # Generate axes with manual positioning of ticks
    axis(side = 1, at = x_axis_breaks, labels = x_axis_labels, cex.axis = axis_cex, lwd = 0.2, lwd.ticks = 1, gap.axis = 0, padj = 0.5)
    axis(side = 2, at = y_axis_breaks, labels = y_axis_labels, cex.axis = axis_cex, lwd = 0.2, lwd.ticks = 1, gap.axis = 0)
    
    # Add background, borders and graticules
    plot(large_bg_mask_Mollweide, lwd = 1, border = "grey20", col = "aliceblue", add = T)
    plot(grid_Mollweide_out, lty = 92, col = "grey80", add = T)
    plot(bbox_sp_Mollweide, lwd = 2, border = "black", col = NA, add = T)
    plot(country_borders_Mollweide, lwd = 1, border = "#00000030", col = NA, add = T)
    # plot(bioregions_shp_Mollweide, lwd = 1, border = "#00000030", col = NA, add = T)
    
    # Add scale bar in legend
    scalebar(d = 2000, type = "line", lwd = 4, divs = 4, xy = scale_bar_position, label = c("", "2000 km", ""), adj = c(0.5, -0.8), font = 2, cex = 1.2)
    prettymapr::addnortharrow(scale = arrow_scale, padin = arrow_padin, text.col = "#00000000")
    rangeBuilder::addRasterLegend(x, locs = legend_breaks, cex.axis = legend_cex, ramp = color_palette, ncolors = 200, border = T, location = legend_location)
    rangeBuilder::addRasterLegend(x, locs = legend_breaks, cex.axis = legend_cex, ramp = color_palette, ncolors = 200, border = T, location = legend_location)
    graphics::text(x = legend_title_x, y = legend_title_y, font = 2, cex = legend_title_cex, label = legend_title)
    
    # Add facet letter
    legend(legend = facet_letter, x = "bottomright", bty = "n", text.col = facet_letter_col,
           text.font = 2, cex = facet_letter_cex, inset = facet_letter_inset)
    
  }
}


### Put ISO3 names on country

### Use transparency and color on bioregions


# world_map_sp <- readRDS(file = "../input_data/Map_stuff/world_map_sp.rds")
# 
# plot(world_map_sp)
# 
# xmin = -120 ; xmax = -30 ; ymin = -37 ; ymax = 28
# e <- extent(c(xmin,xmax,ymin,ymax))
# 
# America_map_sp <- crop(world_map_sp, e)


load(file = "../input_data/Map_stuff/country_borders.RData") # Load country borders
# rivers <- st_read("../input_data/Map_stuff/Major_rivers/MajorRivers.shp") # Load rivers
# load(file = "../input_data/Map_stuff/bg_mask.RData") # Load bg shp

xmin = -120 ; xmax = -30 ; ymin = -37 ; ymax = 28
e <- extent(c(xmin,xmax,ymin,ymax))

DEM_SRTM_v4_1m <- readRDS(file = "../input_data/SRTM/DEM_SRTM_v4_1m.rds")
DEM_SRTM_v4_1m_cropped <- crop(DEM_SRTM_v4_1m, e)

pdf(file = paste0("../maps/Bioregions/Regions.pdf"), height = 12, width = 15)

plot(DEM_SRTM_v4_1m_cropped, col = gray.colors(100, start = 0.9, end = 0, gamma = 2.2, alpha = NULL))
plot(country_borders, border = "#00000030", add = T)

dev.off()

# Load country borders
load(file = "../input_data/Map_stuff/country_borders.RData") 

# Plot map

pdf(file = paste0("../maps/Bioregions/Regions.pdf"), height = 12, width = 15)

plot(DEM_SRTM_v4_1m_cropped, col = gray.colors(100, start = 0.9, end = 0, gamma = 2.2, alpha = NULL))
plot(country_borders, border = "#00000030", add = T)
plot(Guyana_Shield_shp, col = "bisque", add = T)
plot(Caatinga_shp, col = "chocolate1", add = T)
plot(Cerrado_shp, col = "darkgoldenrod1", add = T)
plot(Mata_Atlantica_shp5, col = "darkgreen", add = T)
plot(full_CA_shp, col = "firebrick1", add = T)
plot(Northern_Andes_shp, col = "azure2", add = T)
plot(Central_Andes_shp, col = "darkgrey", add = T)
plot(Western_Lowlands_shp, col = "limegreen", add = T)
plot(Coastal_desert_shp, col = "yellow", add = T)
plot(Llanos_shp, col = "darkviolet", add = T)
plot(Chacos_shp, col = "deeppink", add = T)
plot(Caribbean_Islands_shp, col = "pink", add = T)
plot(Western_Amazon_shp, col = "cyan", add = T)
plot(Lower_Amazon_shp, col = "blue", add = T)
plot(Pantanal_shp, col = "grey100", add = T)
plot(Pampas_shp, col = "tan4", add = T)

dev.off()


grid_Mollweide_out <- readRDS(file = "../input_data/Map_stuff/grid_Mollweide_out.rds")
large_bg_mask_Mollweide <- readRDS(file = "../input_data/Map_stuff/large_bg_mask_Mollweide.rds")
bbox_sp_Mollweide <- readRDS(file = "../input_data/Map_stuff/bbox_sp_Mollweide.rds")

DEM_SRTM_v4_1m_cropped_Mollweide <- projectRaster(from = DEM_SRTM_v4_1m_cropped, method = "bilinear", # Method for interpolation => "ngb" = nearest neighbor for qualitative (or discrete) variables . "bilinear" = for quantitative variables
                                                  
                                                  crs = "+proj=moll +lon_0=-75 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs", # If you have the CRS arguments
                                                  alignOnly = F)

plot(DEM_SRTM_v4_1m_cropped_Mollweide, col = gray.colors(100, start = 0.9, end = 0, gamma = 2.2, alpha = NULL))

pdf(file = paste0("../maps/Regions_background_Mollweide.pdf"), height = 12, width = 15)

image(DEM_SRTM_v4_1m_cropped_Mollweide, 
      col = gray.colors(100, start = 0.9, end = 0, gamma = 2.2, alpha = NULL),
      xlim = c(-4600, 4600), ylim = c(-4450, 3400), axes = F,
      xlab = "", ylab = "",
      legend = F)

axis(side = 1, at = c(-3930, -2170, -420, 1500, 3050), padj = 0.5, labels = c("120°E", "100°E", "80°E", "60°E", "40°E"), cex.axis = 2, lwd = 0.2, lwd.ticks = 2)
axis(side = 2, at = c(-3650, -2450, -1220, 0, 1230, 2445), labels = c("30°S", "20°S", "10°S", "0°", "10°N", "20°N"), cex.axis = 2, lwd = 0.2, lwd.ticks = 2)

plot(grid_Mollweide_out, lty = 92, col = "grey80", add = T)
plot(bbox_sp_Mollweide, lwd = 2, border = "black", col = NA, add = T)

scalebar(d = 2000, type = "line", lwd = 4, divs = 4, xy = c(-3900, -4000), label = c("", "2000 km", ""), adj = c(0.5, -0.8), font = 2, cex = 2)
prettymapr::addnortharrow(scale = 0.9, padin = c(0.45, 0.45), text.col = "#00000000")

dev.off()


