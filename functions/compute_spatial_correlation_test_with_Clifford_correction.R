##### Function to run Spearman's correlation tests with Clifford's correction for df ####

# Compute test for spatial correlation between two raster layers
# Adjust df according to Clifford's correction accounting for positive spatial autocorrelation

# Sources:
  # Clifford, P., S. Richardson, and D. Hemon (1989). Assessing the significance of the correlation between two spatial processes. Biometrics 45: 123–134.
  # R script adapted from Chapter 11.2 in Spatial Data Analysis in Ecology and Agriculture Using R by Plant, 2018


### Inputs = two raster layers (with similar CRS/bbox/resolution)

### Outputs =
  # Summary table with correlation coefficient, t-stat, nb of observation, degrees of freedom, significant threshold, p-value
  # Can provide the summary of the non-adjusted version of the test
  # Can provide data used for the test
  # Can provide the distance threshold where spatial autocorrelation disappear in one of the two layers
  # Can plot correlograms


compute_spatial_correlation_test_with_Clifford_correction <- function (layer_1, layer_2,               # Raster layers to test for spatial correlation
                                                                       mask_1 = NULL, mask_2 = NULL,   # Raster layers used as masks to delineate the range of the analyses for each data layer
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
                                                                       export_distance_threshold = F)  # Should the distance threshold used to account for positive spatial autocorrelation be exported?

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
  if (!is.null(subsample))
  {
    if (subsample < nrow(data_df))
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
