##### Script 29: Test for spatial correlations across tribes and indices #####

##################################
#       Author: Maël Doré        #
#  Contact: mael.dore@gmail.com  #
##################################


### Goals = 
  # Test for spatial correlations among indices for heliconiines
  # Test for spatial correlations between heliconiines and ithomiines for all biodiversity indices
  # Generate summary tables of tests
###


### Inputs
  # Biodiversity maps of Heliconiini (this study)
  # Biodiversity maps of Ithomiini (Doré et al., 2022: 10.1111/ddi.13455)
###

### Outputs
  # Summary tables of spatial correlation tests
  # Include corrected df, rho stats, Q95%, p-value
###


# Clean environment
rm(list = ls())

##### 1/ Load stuff ####

### 1.1/ Load libraries ####

library(tidyverse)
library(openxlsx)
library(raster)
library(sf)
library(spatial) # To compute spatial correlogram
library(cvequality) # To test for differences in CV

### 1.2/ Load Heliconiini biodiversity maps ####

Helico_sp_richness <- readAll(readRDS(file = paste0("./outputs/Indices_maps/tot.sp.richness_Jaccard.80.rds")))
Helico_sp_mean_geo_rarity <- readAll(readRDS(file = "./outputs/Indices_maps/sp.mean.rarity_Leroy_Jaccard.80_contrasted.rds"))   # Leroy's weighting
Helico_Faith_PD <- readAll(readRDS(file = "./outputs/Indices_Maps/PD.raster_Jaccard.80.rds"))

Helico_phenotypic_richness <- readAll(readRDS(file = "./outputs/Indices_maps/ring.richness_Jaccard_ss.80.rds"))
Helico_mean_phenotypic_geo_rarity <- readAll(readRDS(file = "./outputs/Indices_maps/ring.mean.rarity_Leroy_Jaccard_ss.80.rds"))    # Leroy's weighting
Helico_mean_phenotypic_group_size <- readAll(readRDS(file = "./outputs/Indices_maps/weighted_mean_ring_size.rds"))

Helico_all_indices <- stack(Helico_sp_richness, Helico_sp_mean_geo_rarity, Helico_Faith_PD, Helico_phenotypic_richness, Helico_mean_phenotypic_geo_rarity, Helico_mean_phenotypic_group_size)
names(Helico_all_indices) <- c("Helico_sp_richness", "Helico_sp_mean_geo_rarity", "Helico_Faith_PD", "Helico_phenotypic_richness", "Helico_mean_phenotypic_geo_rarity", "Helico_mean_phenotypic_group_size")

plot(Helico_all_indices)

### 1.3/ Load Ithomiini biodiversity maps ####

Itho_sp_richness <- readRDS(file = paste0("./outputs/Ithomiini_index_maps/tot.sp.richness_Jaccard.80.rds"))
Itho_sp_mean_geo_rarity <- readRDS(file = "./outputs/Ithomiini_index_maps/sp.mean.rarity_Leroy_Jaccard.80.rds")   # Leroy's weighting
Itho_Faith_PD <- readRDS(file = "./outputs/Ithomiini_index_maps/PD.raster_Jaccard.80.rds")

Itho_phenotypic_richness <- readRDS(file = paste0("./outputs/Ithomiini_index_maps/ring.richness_Jaccard.80.rds"))
Itho_mean_phenotypic_geo_rarity <- readRDS(file = "./outputs/Ithomiini_index_maps/ring.mean.rarity_Leroy_Jaccard.80.rds")    # Leroy's weighting
Itho_mean_phenotypic_group_size <- readRDS(file = "./outputs/Ithomiini_index_maps/weighted_mean_ring_size.rds")

Itho_all_indices <- stack(Itho_sp_richness, Itho_sp_mean_geo_rarity, Itho_Faith_PD, Itho_phenotypic_richness, Itho_mean_phenotypic_geo_rarity, Itho_mean_phenotypic_group_size)
names(Itho_all_indices) <- c("Itho_sp_richness", "Itho_sp_mean_geo_rarity", "Itho_Faith_PD", "Itho_phenotypic_richness", "Itho_mean_phenotypic_geo_rarity", "Itho_mean_phenotypic_group_size")

plot(Itho_all_indices)


##### 2/ Adjust raster data to match ####

### 2.1/ Match CRS ####

crs(Helico_all_indices) # Mollweide
crs(Itho_all_indices)   # WGS84

Itho_all_indices <- raster::projectRaster(from = Itho_all_indices, to = Helico_all_indices)

### 2.2/ Match resolution and bbox ####

Itho_all_indices <- raster::resample(x = Itho_all_indices, y = Helico_all_indices)

plot(Itho_all_indices)

### 2.3/ Add null terrestrial values ####

# Fill with NA or null values when appropriate

terrestrial_bg_Mollweide <- Helico_all_indices[[1]] * 0
plot(terrestrial_bg_Mollweide)

Itho_all_indices <- raster::cover(Itho_all_indices, terrestrial_bg_Mollweide)
plot(Itho_all_indices)

### 2.4/ Save raster stacks ####

saveRDS(Helico_all_indices, file = "./outputs/Correlation_tests/Helico_all_indices.rds")
saveRDS(Itho_all_indices, file = "./outputs/Correlation_tests/Itho_all_indices.rds")

### 2.4/ Extract data ####

# Check if I included this in the function...
# Create a function that extract the data, ask to use the intersect or the union, run the test, save the variogram plots and the results, also ask if should export the df

# Use only data within the intersection of the two ranges


# Create data table for tests

##### 3/ Run Spearman's correlation tests with Clifford's correction for df ####

# Because tests are based on ranks, be careful how they manage equal values (for the zeros)

### 3.1/ Function to run Spearman's correlation tests with Clifford's correction for df ####

compute_spatial_correlation_test_with_Clifford_correction <- function (layer_1, layer_2,               # Raster layers to test for spatial correlation
                                                                       mask_1 = NULL, mask_2 = NULL,   # Rraster layers used as masks to delineate the range of the analyses for each data layer
                                                                       bg_value = 0,                   # Which value should be used to detect the limit of the range? (NA are always considered outside of the range)
                                                                       sample_size = NULL, seed = 1,   # To set the number of grid cells to sample randomly
                                                                       test_method = "spearman",       # Correlation test: "spearman" (non-parametric, default) or "pearson" (parametric)
                                                                       test_alternative = "greater",   # Select the type of test: "two.sided", "greater", "less"
                                                                       alpha = 0.05,                   # Significance threshold: type of test = "greater", so the quantile = 1 - alpha
                                                                       range_type = "intersect",       # Select the range of the analysis: "intersect" or "union"
                                                                       default_value = 0,              # Which value to use as default outside of the range if using "union"
                                                                       plot_correlogram = F,           # Should the correlogram used to adjust df based on spatial autocorrelation be plotted?
                                                                       export_uncorrected = F,         # Should the test results for uncorrected df be exported alongside the corrected test?
                                                                       export_data = F,                # Should the data table be provided in the output?
                                                                       export_distance_threshold = F)   # Should the distance threshold used to account for positive spatial autocorrelation be exported?

{
  # # If needed, generate mask_stack for grid cells within the range of the layers in data_stack
  # if (is.null(mask_stack))
  # {
  #   mask_brick <- (data_stack == bg_value)
  #   mask_brick <- raster::subs(x = mask_brick, y = data.frame(id = 1, v = NA), subsWithNA = FALSE)
  #   mask_stack <- stack(mask_brick)
  #   plot(mask_stack)
  # }
  
  # If needed, generate mask_1 for grid cells within the range of layer_1
  if (is.null(mask_1))
  {
    mask_1 <- layer_1 == bg_value
    mask_1[mask_1[] == 1] <-  NA
    # plot(mask_1)
  }
  
  # If needed, generate mask_2 for grid cells within the range of layer_2
  if (is.null(mask_2))
  {
    mask_2 <- layer_2 == bg_value
    mask_2[mask_2[] == 1] <-  NA
    # plot(mask_2)
  }
  
  # Generate mask for analyses according to the range_type
  if (range_type == "intersect") # Use only grid cells within both ranges
  {
    mask_data <- raster::mask(mask_1, mask_2)
    mask_data <- raster::mask(mask_data, mask_1)
    # plot(mask_data)
    
    # No need for correction for default values as all grid cells have data in both initial layers
    layer_1_corrected <- layer_1
    layer_2_corrected <- layer_2
  }
  if (range_type == "union") # Use grid cells within at least one range
  {
    mask_data <- raster::cover(mask_1, mask_2)
    mask_data <- raster::cover(mask_data, mask_1)
    plot(mask_data)
    
    # Correct initial layers to fill grid cells outside of initial range with the default value
    default_mask <- mask_data + default_value # Create raster of grid cells to use as default value if outside of initial range
    
    layer_1_corrected <- raster::mask(layer_1, mask_1) # Remove values outside of initial range
    layer_1_corrected <- raster::cover(layer_1_corrected, default_mask) # Fill with default values
    # plot(layer_1_corrected)
    
    layer_2_corrected <- raster::mask(layer_2, mask_2) # Remove values outside of initial range
    layer_2_corrected <- raster::cover(layer_2_corrected, default_mask) # Fill with default values
    # plot(layer_2_corrected)
  }
  
  # Stack layers
  all_layers <- stack(layer_1_corrected, layer_2_corrected)
  
  # Extract all data within data range
  sample_all_indices <- which(mask_data[] == 0) 
  all_coords <- coordinates(mask_data)
  all_coords_in_range <- all_coords[sample_all_indices, ]
  data_df <- data.frame(raster::extract(x = all_layers, y = all_coords_in_range))
  
  # Subsample data if needed
  if (!is.null(sample_size))
  {
    if (sample_size < nrow(data_df))
    {
      set.seed <- seed
      subsample_indices <- sample(x = 1:nrow(data_df), size = sample_size, replace = F)
      data_df <- data_df[subsample_indices, ]
      all_coords_in_range <- all_coords_in_range[subsample_indices, ]
      cat(paste0("Data sampled from ",sample_size," random grid cells\n"))
    } else {
      cat(paste0("No sampling needed as there are ",nrow(data_df), " grid cells with data available within the analyses range. Less than the requested ", sample_size," sample size.\n"))
    }
  }

  ## Compute autocorrelogram of each layer and store autocorrelation coefficients used to estimate the variance of the sampling distribution of Spearman's rho, and effective sample size afterward
  autocorr_coefs <- list(NULL)
  max_class_indices <- c()
  N_layers <- raster::nlayers(all_layers)
  N_obs <- nrow(data_df)
  
  for (i in 1:N_layers)
  {
    # i <- 1
    
    # Get the layer values
    Y <- data_df[ ,i]
    
    # Compute ranks
    Y_ranks <- rank(Y)
    
    # Transform points into a surface
    Y_surf_sp <- spatial::surf.ls(np = 0, x = all_coords_in_range[,1], y = all_coords_in_range[,2], z = Y_ranks)
    
    # Use Struges' rule to define the number of distance bins (Doubled for better precision)
    nb_bins <- round(1 + log2(N_obs)) * 2
    correl_output <- spatial::correlogram(krig = Y_surf_sp, nint = nb_bins, plotit = plot_correlogram)
    
    # Store autocorrelation coefficients of each class
    autocorr_coefs[[i]] <- correl_output$y
    
    # Detect last spatial class with positive autocorrelation
    max_class_index <- which.min(correl_output$y > 0) - 1
    max_class_indices <- c(max_class_indices, max_class_index)
    
    # Get counts of pairs by distance classes
    pairs_cnt <- correl_output$cnt
  }
  
  # Compute uncorrected correlation test
  uncorrected_test <- stats::cor.test(x = data_df[ ,1], y = data_df[ ,2], method = test_method, alternative = test_alternative, na.rm = T)
  cor_coeff <- round(uncorrected_test$estimate, 3) # Correlation coefficent (rho for Spearman, r for Pearson)
  stat_uncorrected <- uncorrected_test$statistic #  Uncorrected test stat (S for Spearman rank test, t for Pearson t-test)
  t_uncorrected <- cor_coeff * sqrt(N_obs - 2) / sqrt(1 - cor_coeff^2) #  Uncorrected t-stat
  p_uncorrected <- uncorrected_test$p.value #  Uncorrected p-value
  
  # Extract nb of spatial classes with positive autocorrelation in both layers
  max_class_nb <- min(max_class_indices)
  # Extract distance threshold for autocorrelation
  dist_threshold <- correl_output$x[max_class_nb + 1]
  
  # Estimate the variance of the sampling distribution (sampling error) of Spearman's rho from the first N classes where autocorrelation is still present 
  nr1r2_sp <- pairs_cnt * autocorr_coefs[[1]] * autocorr_coefs[[2]]
  SE_hat_sp <- sum(nr1r2_sp[1:max_class_nb]) / N_obs^2
  
  # Compute corrected sample size
  N_obs_corrected <- 1 + (1 / SE_hat_sp)
  
  # Compute the corrected t-test with corrected sample size for the df
  t_corrected <- cor_coeff * sqrt(N_obs_corrected - 2) / sqrt(1 - cor_coeff^2) #  Corrected t-stat
  
  # Compute the corrected p-value according to the type of test
  if (test_alternative %in% c("greater")) { p_corrected <- (1 - pt(q = t_corrected, df = N_obs_corrected - 2)) } # Corrected p-value for a "greater" or "less" test
  if (test_alternative %in% c("less")) { p_corrected <- pt(q = t_corrected, df = N_obs_corrected - 2) } # Corrected p-value for a "greater" or "less" test
  if (test_alternative == "two.sided") { p_corrected <- 2 * (1 - pt(q = abs(t_corrected), df = N_obs_corrected - 2)) } # Corrected p-value for a "two.sided" test

  # Compute the threshold value of the t-stat (for corrected and non-corrected) !
  if (test_alternative %in% c("greater"))
  { 
    Q_alpha_uncorrected <- qt(p = (1 - alpha), df = N_obs - 2) # Threshold value of the t-stats to reach significance
    Q_alpha_corrected <- qt(p = (1 - alpha), df = N_obs_corrected - 2) # Threshold value of the t-stats to reach significance
  }
  if (test_alternative %in% c("less"))
  { 
    Q_alpha_uncorrected <- qt(p = alpha, df = N_obs - 2) # Threshold value of the t-stats to reach significance
    Q_alpha_corrected <- qt(p = alpha, df = N_obs_corrected - 2) # Threshold value of the t-stats to reach significance
  }
  if (test_alternative %in% c("two.sided"))
  { 
    Q_alpha_uncorrected <- qt(p = (1 - alpha/2), df = N_obs - 2) # Threshold value of the t-stats to reach significance
    Q_alpha_corrected <- qt(p = (1 - alpha/2), df = N_obs_corrected - 2) # Threshold value of the t-stats to reach significance
  }

  ## Export results
  
  # Summarize results for the corrected test
  summary_test <- data.frame(cor_coeff = cor_coeff, t_stat = round(t_corrected, 2), Nobs = N_obs, df = round(N_obs_corrected - 2, 1), Q_alpha = Q_alpha_corrected, p_value = round(p_corrected, 3))
  
  # Add uncorrected test if needed
  if (export_uncorrected)
  {
    summary_uncorrected_test <- data.frame(cor_coeff = cor_coeff, t_stat = round(t_uncorrected, 2), Nobs = N_obs, df = N_obs - 2, Q_alpha = Q_alpha_uncorrected, p_value = round(p_uncorrected, 3))
    summary_test <- rbind(summary_test, summary_uncorrected_test)
    row.names(summary_test) <- c("corrected", "uncorrected")
  }
  
  output <- list(summary_df = summary_test)
  
  ## Export data table AND/OR distance threshold if needed
  if (export_data)
  {
    data_df <- cbind(data_df, all_coords_in_range)
    output <- append(x = output, values = list(data_df = data_df))
  }
  if (export_distance_threshold)
  {
    output <- append(x = output, values = list(distance_threshold = dist_threshold))
  }

  ## Unlist if only one output
  if (length(output) == 1) { output <- output[[1]] }
  
  ## Return output
  return(output)
}
  
# Test with species richness between tribes
compute_spatial_correlation_test_with_Clifford_correction(layer_1 = Helico_all_indices[[1]],
                                                          layer_2 = Itho_all_indices[[1]],               # Stack of raster layers to test for spatial correlation
                                                          mask_1 = NULL, mask_2 = NULL,                 # Stack of raster layers used as mask to delineate the range of the analyses for each data layer
                                                          bg_value = 0,                   # Which value should be used to detect the limit of the range? (NA are always considered outside of the range)
                                                          sample_size = NULL, seed = 1,   # To set the number of grid cells to sample randomly
                                                          test_method = "spearman",       # Correlation test: "spearman" (non-parametric, default) or "pearson" (parametric)
                                                          test_alternative = "greater",   # Select the type of test: "two.sided", "greater", "less"
                                                          alpha = 0.05,                   # Significance threshold: type of test = "greater", so the quantile = 1 - alpha
                                                          range_type = "intersect",       # Select the range of the analysis: "intersect" or "union"
                                                          default_value = 0,              # Which value to use as default outside of the range if using "union"
                                                          plot_correlogram = T,           # Should the correlogram used to adjust df based on spatial autocorrelation be plotted?
                                                          export_uncorrected = T,         # Should the test results for uncorrected df be exported alongside the corrected test?
                                                          export_data = F,                # Should the data table be provided in the output?
                                                          export_distance_threshold = T)         # Should the test results for uncorrected df be exported alongside the corrected test?


### 3.2/ Define masks for Itho and Helico ranges ####

# Mask for Heliconiini based on species richness
Helico_range_mask <- Helico_all_indices[[1]] == 0
Helico_range_mask[Helico_range_mask[] == 1] <-  NA
plot(Helico_range_mask)

saveRDS(object = Helico_range_mask, file = "./outputs/Correlation_tests/Helico_range_mask.rds")

# Mask for Ithomiini based on species richness
Itho_range_mask <- Itho_all_indices[[1]] == 0
Itho_range_mask[Itho_range_mask[] == 1] <-  NA
plot(Itho_range_mask)

saveRDS(object = Itho_range_mask, file = "./outputs/Correlation_tests/Itho_range_mask.rds")


### 3.3/ Run tests across indices for Heliconiini ####

# Initiate final list
summary_list_pairwise_Helico_indices <- list()

N_indices <- nlayers(Helico_all_indices)

## Double loop per pair of indices
# for (i in 1:N_indices) # All pairwise correlations
for (i in 1) # Only correlation with species richness
{
  # i <- 1
  for (j in (i+1):N_indices) # Most efficient version with no useless double tests
  {
    # j <- 2
    
    # Test correlation between pairwise indices
    summary_df_ij <- compute_spatial_correlation_test_with_Clifford_correction(layer_1 = Helico_all_indices[[i]],
                                                              layer_2 = Helico_all_indices[[j]],               # Raster layers to test for spatial correlation
                                                              mask_1 = Helico_range_mask,     # Raster layers used as mask to delineate the range of the analyses for each data layer
                                                              mask_2 = Helico_range_mask,     # Raster layers used as mask to delineate the range of the analyses for each data layer
                                                              bg_value = 0,                   # Which value should be used to detect the limit of the range? (NA are always considered outside of the range)
                                                              sample_size = NULL, seed = 1,   # To set the number of grid cells to sample randomly
                                                              test_method = "spearman",       # Correlation test: "spearman" (non-parametric, default) or "pearson" (parametric)
                                                              test_alternative = "greater",   # Select the type of test: "two.sided", "greater", "less"
                                                              alpha = 0.05,                   # Significance threshold: type of test = "greater", so the quantile = 1 - alpha
                                                              range_type = "intersect",       # Select the range of the analysis: "intersect" or "union"
                                                              default_value = 0,              # Which value to use as default outside of the range if using "union"
                                                              plot_correlogram = F,           # Should the correlogram used to adjust df based on spatial autocorrelation be plotted?
                                                              export_uncorrected = T,         # Should the test results for uncorrected df be exported alongside the corrected test?
                                                              export_data = F,                # Should the data table be provided in the output?
                                                              export_distance_threshold = F)  # Should the test results for uncorrected df be exported alongside the corrected test?
    
    # Name of pairwise test
    pairwise_name <- paste0(names(Helico_all_indices[[i]]), "_", names(Helico_all_indices[[j]])) 
    
    # Save summary table
    old_names <- names(summary_list_pairwise_Helico_indices)
    summary_list_pairwise_Helico_indices <- append(x = summary_list_pairwise_Helico_indices, values = list(summary_df_ij))
    names(summary_list_pairwise_Helico_indices) <- c(old_names, pairwise_name)
    
    cat(paste0(Sys.time(), " - Correlation computed for i = ",i, "/", names(Helico_all_indices[[i]]), " and j = ",j, "/", names(Helico_all_indices[[j]]), "\n"))
  }
}

# Reorder output as data.frame
summary_list_pairwise_Helico_indices
summary_df_pairwise_Helico_indices <- lapply(X = summary_list_pairwise_Helico_indices, FUN = function (x) { x$test_type <- row.names(x) ; return(x) } )
summary_df_pairwise_Helico_indices <- bind_rows(summary_df_pairwise_Helico_indices)
summary_df_pairwise_Helico_indices$index <- rep(x = names(Helico_all_indices)[-1], each = 2)
row.names(summary_df_pairwise_Helico_indices) <- NULL

# Save/export result
saveRDS(summary_df_pairwise_Helico_indices, file =  paste0("./outputs/Correlation_tests/summary_df_pairwise_Helico_indices"))
write.xlsx(x = summary_df_pairwise_Helico_indices, file = "./outputs/Correlation_tests/summary_df_pairwise_Helico_indices.xlsx")


### 3.4/ Run tests between tribes for all indices ####

# Initiate final list
summary_list_pairwise_Helico_Itho <- list()

N_indices <- nlayers(Helico_all_indices)

## Simple loop per pair of indices
for (i in 1:N_indices) # All pairwise correlations
{
  # i <- 1
  
  # Test correlation between pairwise indices
  summary_df_i <- compute_spatial_correlation_test_with_Clifford_correction(layer_1 = Helico_all_indices[[i]],
                                                                             layer_2 = Itho_all_indices[[i]],               # Raster layers to test for spatial correlation
                                                                             mask_1 = Helico_range_mask,     # Raster layers used as mask to delineate the range of the analyses for each data layer
                                                                             mask_2 = Itho_range_mask,     # Raster layers used as mask to delineate the range of the analyses for each data layer
                                                                             bg_value = 0,                   # Which value should be used to detect the limit of the range? (NA are always considered outside of the range)
                                                                             sample_size = NULL, seed = 1,   # To set the number of grid cells to sample randomly
                                                                             test_method = "spearman",       # Correlation test: "spearman" (non-parametric, default) or "pearson" (parametric)
                                                                             test_alternative = "greater",   # Select the type of test: "two.sided", "greater", "less"
                                                                             alpha = 0.05,                   # Significance threshold: type of test = "greater", so the quantile = 1 - alpha
                                                                             range_type = "intersect",       # Select the range of the analysis: "intersect" or "union"
                                                                             default_value = 0,              # Which value to use as default outside of the range if using "union"
                                                                             plot_correlogram = F,           # Should the correlogram used to adjust df based on spatial autocorrelation be plotted?
                                                                             export_uncorrected = T,         # Should the test results for uncorrected df be exported alongside the corrected test?
                                                                             export_data = F,                # Should the data table be provided in the output?
                                                                             export_distance_threshold = F)  # Should the test results for uncorrected df be exported alongside the corrected test?
  
  # Name of pairwise test
  index_name <- str_remove(string = names(Helico_all_indices[[i]]), pattern = "Helico_") 
  
  # Save summary table
  old_names <- names(summary_list_pairwise_Helico_Itho)
  summary_list_pairwise_Helico_Itho <- append(x = summary_list_pairwise_Helico_Itho, values = list(summary_df_i))
  names(summary_list_pairwise_Helico_Itho) <- c(old_names, index_name)
  
  cat(paste0(Sys.time(), " - Correlation inter-tribe computed for i = ",i, "/ ", index_name, "\n"))
}

# Reorder output as data.frame
summary_list_pairwise_Helico_Itho
summary_df_pairwise_Helico_Itho <- lapply(X = summary_list_pairwise_Helico_Itho, FUN = function (x) { x$test_type <- row.names(x) ; return(x) } )
summary_df_pairwise_Helico_Itho <- bind_rows(summary_df_pairwise_Helico_Itho)
summary_df_pairwise_Helico_Itho$index <- rep(x = str_remove(string = names(Helico_all_indices), pattern = "Helico_"), each = 2)
row.names(summary_df_pairwise_Helico_Itho) <- NULL
summary_df_pairwise_Helico_Itho

# Save/export result
saveRDS(summary_df_pairwise_Helico_Itho, file =  paste0("./outputs/Correlation_tests/summary_df_pairwise_Helico_Itho"))
write.xlsx(x = summary_df_pairwise_Helico_Itho, file = "./outputs/Correlation_tests/summary_df_pairwise_Helico_Itho.xlsx")


#### 4/ Test for homoscedasticity of species richness between tribes ####

### 4.1/ Function to compute corrected sample size based on spatial autocorrelation ####

compute_corrected_sample_size <- function (df, # df with data and coordinates
                                           data_index = 1, # Index of the data variable
                                           x_index = 2, # Index of the X variable
                                           y_index = 3, # Index of the Y variable
                                           plot_correlogram = F, # Should the correlogram used to adjust df based on spatial autocorrelation be plotted?
                                           export_distance_threshold = F)  # Should the test results for uncorrected df be exported alongside the corrected test?
{
  # Compute uncorrected sample size = nb of observations
  N_obs <- nrow(df)
  
  # Get the data
  Y <- df[ , data_index]
  
  # Compute ranks
  Y_ranks <- rank(Y)
  
  # Transform points into a surface
  Y_surf_sp <- spatial::surf.ls(np = 0, x = df[, x_index], y = df[, y_index], z = Y_ranks)
  
  # Use Struges' rule to define the number of distance bins (Doubled for better precision)
  nb_bins <- round(1 + log2(N_obs)) * 2
  correl_output <- spatial::correlogram(krig = Y_surf_sp, nint = nb_bins, plotit = plot_correlogram)
  
  # Store autocorrelation coefficients of each class
  autocorr_coefs <- correl_output$y
  
  # Detect last spatial class with positive autocorrelation
  max_class_index <- which.min(correl_output$y > 0) - 1
  
  # Get counts of pairs by distance classes
  pairs_cnt <- correl_output$cnt
  
  # Extract distance threshold for autocorrelation
  dist_threshold <- correl_output$x[max_class_index + 1]
  
  # Estimate the variance of the sampling distribution (sampling error) from the first N classes where autocorrelation is still present 
  nr1r2_sp <- pairs_cnt * autocorr_coefs * autocorr_coefs
  SE_hat_sp <- sum(nr1r2_sp[1:max_class_index]) / N_obs^2
  
  # Compute corrected sample size
  N_obs_corrected <- 1 + (1 / SE_hat_sp)
  
  ## Export results
  output <- list(N_obs_corrected = N_obs_corrected)
  
  ## Export distance threshold if needed
  if (export_distance_threshold)
  {
    output <- append(x = output, values = list(distance_threshold = dist_threshold))
  }
  
  ## Unlist if only one output
  if (length(output) == 1) { output <- output[[1]] }
  
  ## Return output
  return(output)
  
}

### 4.2/ For species richness ####

readRDS(file = paste0("./outputs/Correlation_tests/summary_df_pairwise_Helico_Itho"))

# Use adjusted df recorded for the t-test, to correct for F-test

?var.test() # Parametric F-test
?fligner.test() # Non-parametric F-test (but not sure how to adjust for spatial autocorrelation...)

stats:::var.test.default
source(file = "./functions/var.test_adjusted.R")

## 4.2.1/ Obtain data for the test

sp_richness_cor_test <- compute_spatial_correlation_test_with_Clifford_correction(layer_1 = Helico_all_indices[[1]],
                                                          layer_2 = Itho_all_indices[[1]],               # Raster layers to test for spatial correlation
                                                          mask_1 = Helico_range_mask,     # Raster layers used as mask to delineate the range of the analyses for each data layer
                                                          mask_2 = Itho_range_mask,     # Raster layers used as mask to delineate the range of the analyses for each data layer
                                                          bg_value = 0,                   # Which value should be used to detect the limit of the range? (NA are always considered outside of the range)
                                                          sample_size = NULL, seed = 1,   # To set the number of grid cells to sample randomly
                                                          test_method = "spearman",       # Correlation test: "spearman" (non-parametric, default) or "pearson" (parametric)
                                                          test_alternative = "greater",   # Select the type of test: "two.sided", "greater", "less"
                                                          alpha = 0.05,                   # Significance threshold: type of test = "greater", so the quantile = 1 - alpha
                                                          range_type = "intersect",       # Select the range of the analysis: "intersect" or "union"
                                                          default_value = 0,              # Which value to use as default outside of the range if using "union"
                                                          plot_correlogram = F,           # Should the correlogram used to adjust df based on spatial autocorrelation be plotted?
                                                          export_uncorrected = F,         # Should the test results for uncorrected df be exported alongside the corrected test?
                                                          export_data = T,                # Should the data table be provided in the output?
                                                          export_distance_threshold = F)  # Should the test results for uncorrected df be exported alongside the corrected test?

sp_richness_data_df <- sp_richness_cor_test$data_df
sp_richness_adjusted_df <- sp_richness_cor_test$summary_df$df

## 4.2.2/ Compute corrected sample size for Heliconiini

Helico_Nobs_corrected <- compute_corrected_sample_size(df = sp_richness_data_df , # df with data and coordinates
                                                       data_index = 1, # Index of the data variable
                                                       x_index = 3, # Index of the X variable
                                                       y_index = 4, # Index of the Y variable
                                                       plot_correlogram = T, # Should the correlogram used to adjust df based on spatial autocorrelation be plotted?
                                                       export_distance_threshold = T)
Helico_Nobs_corrected
Helico_Nobs_corrected <- Helico_Nobs_corrected$N_obs_corrected

## 4.2.3/ Compute corrected sample size for Ithomiini

Itho_Nobs_corrected <- compute_corrected_sample_size(df = sp_richness_data_df , # df with data and coordinates
                                                       data_index = 2, # Index of the data variable
                                                       x_index = 3, # Index of the X variable
                                                       y_index = 4, # Index of the Y variable
                                                       plot_correlogram = T, # Should the correlogram used to adjust df based on spatial autocorrelation be plotted?
                                                       export_distance_threshold = T)
Itho_Nobs_corrected
Itho_Nobs_corrected <- Itho_Nobs_corrected$N_obs_corrected

## 4.2.4/ Compute corrected var.test with adjusted DF

# Compute variances
Helico_var <- var(sp_richness_data_df[,1]) ; Helico_var 
Itho_var <- var(sp_richness_data_df[,2]) ; Itho_var


sp_richness_var_test_df <- var.test_adjusted(x = sp_richness_data_df[,1], # Helico data
                                             y = sp_richness_data_df[,2], # Itho data
                                             ratio = 1, # Expected ratio of variances for the null hypothesis = 1
                                             df_x = Helico_Nobs_corrected - 1, # Adjusted DF using Clifford's correction
                                             df_y = Itho_Nobs_corrected - 1, # Adjusted DF using Clifford's correction
                                             alternative = "less")
                                             
sp_richness_var_test_df

sp_richness_F_stat <- sp_richness_var_test_df$statistic ; sp_richness_F_stat
var_test_Q5 <- qf(p = 0.05, df1 =  Helico_Nobs_corrected - 1, df2 = Itho_Nobs_corrected - 1, lower.tail = T)
var_test_p_value <- sp_richness_var_test_df$p.value
  

str(sp_richness_var_test_df)

sp_richness_summary_var_test <- data.frame(Helico_var = Helico_var, Itho_var = Itho_var,
                                           F_stat = round(sp_richness_F_stat, 3),
                                           Helico_df = round(Helico_Nobs_corrected - 1, 1), Itho_df = round(Itho_Nobs_corrected - 1, 1),
                                           Q5 = var_test_Q5, p_value = round(var_test_p_value, 3))
sp_richness_summary_var_test

saveRDS(object = sp_richness_summary_var_test, file = "./outputs/Correlation_tests/sp_richness_summary_var_test.rds")


## 4.2.5/ Test for differences in CV (account for differences in means)

source(file = "./functions/CV_test_adjusted.R")

# Compute Coefficent of Variation to account for difference in means
Helico_CV <- sd(sp_richness_data_df[,1]) / mean(sp_richness_data_df[,1]) ; Helico_CV 
Itho_CV <- sd(sp_richness_data_df[,2]) / mean(sp_richness_data_df[,2]) ; Itho_CV

# Build df with grouping factors
sp_richness_data_df_for_CV_test <- data.frame(sp_richness = c(sp_richness_data_df[,1], sp_richness_data_df[,2]),
                                              tribe = rep(x = c("Helico", "Itho"), each = nrow(sp_richness_data_df)))

# Feltz and Miller’s (1996) asymptotic test
sp_richness_asymptotic_CV_test <- cvequality::asymptotic_test(x = sp_richness_data_df_for_CV_test$sp_richness, y = sp_richness_data_df_for_CV_test$tribe, seed = 1)

sp_richness_asymptotic_CV_test <- cv_test_adjusted(x = sp_richness_data_df_for_CV_test$sp_richness,
                                                   y = sp_richness_data_df_for_CV_test$tribe,
                                                   seed = 1,
                                                   adjusted_n1 = Helico_Nobs_corrected,
                                                   adjusted_n2 = Itho_Nobs_corrected)

sp_richness_asymptotic_CV_test

alpha <- 0.05

CV_test_khi_stat <- sp_richness_asymptotic_CV_test$D_AD
CV_test_p_value <- pchisq(q = CV_test_khi_stat, df = 1, lower.tail = F) # Threshold value of the t-stats to reach significance
CV_test_Q95 <- qchisq(p = (1 - alpha), df = 1) # Threshold value of the t-stats to reach significance

sp_richness_summary_CV_test <- data.frame(Helico_CV = Helico_CV, Itho_CV = Itho_CV, khi_stat = round(CV_test_khi_stat, 2), Nobs = nrow(sp_richness_data_df_for_CV_test), df = 1, Q95 = CV_test_Q95, p_value = round(CV_test_p_value, 3))
sp_richness_summary_CV_test <- data.frame(Helico_CV = Helico_CV, Itho_CV = Itho_CV, khi_stat = round(CV_test_khi_stat, 2), Adjusted_Nobs = round(Helico_Nobs_corrected + Itho_Nobs_corrected), df = 1, Q95 = CV_test_Q95, p_value = round(CV_test_p_value, 3))
sp_richness_summary_CV_test

saveRDS(object = sp_richness_summary_CV_test, file = "./outputs/Correlation_tests/sp_richness_summary_CV_test.rds")
sp_richness_summary_CV_test <- readRDS(file = "./outputs/Correlation_tests/sp_richness_summary_CV_test.rds")


### 4.3/ For phylogenetic diversity ####


readRDS(file = paste0("./outputs/Correlation_tests/summary_df_pairwise_Helico_Itho"))

# Use adjusted df recorded for the t-test, to correct for F-test

?var.test() # Parametric F-test
?fligner.test() # Non-parametric F-test (but not sure how to adjust for spatial autocorrelation...)

stats:::var.test.default
source(file = "./functions/var.test_adjusted.R")

## 4.3.1/ Obtain data for the test

names(Itho_all_indices)

Faith_PD_cor_test <- compute_spatial_correlation_test_with_Clifford_correction(layer_1 = Helico_all_indices[[3]],
                                                                                  layer_2 = Itho_all_indices[[3]],               # Raster layers to test for spatial correlation
                                                                                  mask_1 = Helico_range_mask,     # Raster layers used as mask to delineate the range of the analyses for each data layer
                                                                                  mask_2 = Itho_range_mask,     # Raster layers used as mask to delineate the range of the analyses for each data layer
                                                                                  bg_value = 0,                   # Which value should be used to detect the limit of the range? (NA are always considered outside of the range)
                                                                                  sample_size = NULL, seed = 1,   # To set the number of grid cells to sample randomly
                                                                                  test_method = "spearman",       # Correlation test: "spearman" (non-parametric, default) or "pearson" (parametric)
                                                                                  test_alternative = "greater",   # Select the type of test: "two.sided", "greater", "less"
                                                                                  alpha = 0.05,                   # Significance threshold: type of test = "greater", so the quantile = 1 - alpha
                                                                                  range_type = "intersect",       # Select the range of the analysis: "intersect" or "union"
                                                                                  default_value = 0,              # Which value to use as default outside of the range if using "union"
                                                                                  plot_correlogram = F,           # Should the correlogram used to adjust df based on spatial autocorrelation be plotted?
                                                                                  export_uncorrected = F,         # Should the test results for uncorrected df be exported alongside the corrected test?
                                                                                  export_data = T,                # Should the data table be provided in the output?
                                                                                  export_distance_threshold = F)  # Should the test results for uncorrected df be exported alongside the corrected test?

Faith_PD_data_df <- Faith_PD_cor_test$data_df
Faith_PD_adjusted_df <- Faith_PD_cor_test$summary_df$df

## 4.3.2/ Compute corrected sample size for Heliconiini

Helico_Nobs_corrected <- compute_corrected_sample_size(df = Faith_PD_data_df , # df with data and coordinates
                                                       data_index = 1, # Index of the data variable
                                                       x_index = 3, # Index of the X variable
                                                       y_index = 4, # Index of the Y variable
                                                       plot_correlogram = T, # Should the correlogram used to adjust df based on spatial autocorrelation be plotted?
                                                       export_distance_threshold = T)
Helico_Nobs_corrected
Helico_Nobs_corrected <- Helico_Nobs_corrected$N_obs_corrected

## 4.3.3/ Compute corrected sample size for Ithomiini

Itho_Nobs_corrected <- compute_corrected_sample_size(df = Faith_PD_data_df , # df with data and coordinates
                                                     data_index = 2, # Index of the data variable
                                                     x_index = 3, # Index of the X variable
                                                     y_index = 4, # Index of the Y variable
                                                     plot_correlogram = T, # Should the correlogram used to adjust df based on spatial autocorrelation be plotted?
                                                     export_distance_threshold = T)
Itho_Nobs_corrected
Itho_Nobs_corrected <- Itho_Nobs_corrected$N_obs_corrected

## 4.3.4/ Compute corrected var.test with adjusted DF

# Compute variances
Helico_var <- var(Faith_PD_data_df[,1]) ; Helico_var 
Itho_var <- var(Faith_PD_data_df[,2]) ; Itho_var


Faith_PD_var_test_df <- var.test_adjusted(x = Faith_PD_data_df[,1], # Helico data
                                             y = Faith_PD_data_df[,2], # Itho data
                                             ratio = 1, # Expected ratio of variances for the null hypothesis = 1
                                             df_x = Helico_Nobs_corrected - 1, # Adjusted DF using Clifford's correction
                                             df_y = Itho_Nobs_corrected - 1, # Adjusted DF using Clifford's correction
                                             alternative = "less")

Faith_PD_var_test_df

Faith_PD_F_stat <- Faith_PD_var_test_df$statistic ; Faith_PD_F_stat
var_test_Q5 <- qf(p = 0.05, df1 =  Helico_Nobs_corrected - 1, df2 = Itho_Nobs_corrected - 1, lower.tail = T)
var_test_p_value <- Faith_PD_var_test_df$p.value


Faith_PD_summary_var_test <- data.frame(Helico_var = Helico_var, Itho_var = Itho_var,
                                           F_stat = round(Faith_PD_F_stat, 3),
                                           Helico_df = round(Helico_Nobs_corrected - 1, 1), Itho_df = round(Itho_Nobs_corrected - 1, 1),
                                           Q5 = var_test_Q5, p_value = round(var_test_p_value, 3))
Faith_PD_summary_var_test

saveRDS(object = Faith_PD_summary_var_test, file = "./outputs/Correlation_tests/Faith_PD_summary_var_test.rds")


## 4.3.5/ Test for differences in CV (account for differences in means)

source(file = "./functions/CV_test_adjusted.R")

# Compute Coefficent of Variation to account for difference in means
Helico_CV <- sd(Faith_PD_data_df[,1]) / mean(Faith_PD_data_df[,1]) ; Helico_CV 
Itho_CV <- sd(Faith_PD_data_df[,2]) / mean(Faith_PD_data_df[,2]) ; Itho_CV

# Build df with grouping factors
Faith_PD_data_df_for_CV_test <- data.frame(sp_richness = c(Faith_PD_data_df[,1], Faith_PD_data_df[,2]),
                                              tribe = rep(x = c("Helico", "Itho"), each = nrow(Faith_PD_data_df)))

# Feltz and Miller’s (1996) asymptotic test
Faith_PD_asymptotic_CV_test <- cvequality::asymptotic_test(x = Faith_PD_data_df_for_CV_test$sp_richness, y = Faith_PD_data_df_for_CV_test$tribe, seed = 1)

Faith_PD_asymptotic_CV_test <- cv_test_adjusted(x = Faith_PD_data_df_for_CV_test$sp_richness,
                                                   y = Faith_PD_data_df_for_CV_test$tribe,
                                                   seed = 1,
                                                   adjusted_n1 = Helico_Nobs_corrected,
                                                   adjusted_n2 = Itho_Nobs_corrected)

Faith_PD_asymptotic_CV_test

CV_test_khi_stat <- Faith_PD_asymptotic_CV_test$D_AD
CV_test_p_value <- pchisq(q = CV_test_khi_stat, df = 1, lower.tail = F) # Threshold value of the t-stats to reach significance
CV_test_Q95 <- qchisq(p = (1 - alpha), df = 1) # Threshold value of the t-stats to reach significance

Faith_PD_summary_CV_test <- data.frame(Helico_CV = Helico_CV, Itho_CV = Itho_CV, khi_stat = round(CV_test_khi_stat, 2), Nobs = nrow(Faith_PD_data_df_for_CV_test), df = 1, Q95 = CV_test_Q95, p_value = round(CV_test_p_value, 3))
Faith_PD_summary_CV_test <- data.frame(Helico_CV = Helico_CV, Itho_CV = Itho_CV, khi_stat = round(CV_test_khi_stat, 2), Adjusted_Nobs = round(Helico_Nobs_corrected + Itho_Nobs_corrected, 1), df = 1, Q95 = CV_test_Q95, p_value = round(CV_test_p_value, 3))
Faith_PD_summary_CV_test

saveRDS(object = Faith_PD_summary_CV_test, file = "./outputs/Correlation_tests/Faith_PD_summary_CV_test.rds")
Faith_PD_summary_CV_test <- readRDS(file = "./outputs/Correlation_tests/Faith_PD_summary_CV_test.rds")





