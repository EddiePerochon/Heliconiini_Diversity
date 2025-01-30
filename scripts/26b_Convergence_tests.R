##### Script 25b: Plot results of niche convergence tests #####


##################################
#       Author: Maël Doré        #
#  Contact: mael.dore@gmail.com  #
##################################


### Goals = 
  # Plot histograms of global tests for Helico, Itho, Inter-tribe
  # Extract summary tables for global tests for Helico, Itho, Inter-tribe
  # Extract summary tables for mimetic group tests for Helico, Itho, Inter-tribe
###


### Inputs
  # Heliconiini and Inter-tribe MCD test outputs
  # Ithomiini MCD test outputs (Doré et al., 2023: 10.1111/ddi.13455)
###

### Outputs
  # Histograms of global tests
  # Summary tables of niche convergence tests
  # Include Nunits, Npairs, MCDobs, Q50%, Q95%, p-value
###


# Clean environment
rm(list = ls())

library(tidyverse)
library(readxl)
library(openxlsx)


### Loop per type of phenotypic groups
for (m in c("ss", "sl"))
# for (m in c("sl"))
{
  # m <- "ss"
  # m <- "sl"
  
  ##### 1/ Load stuff ####
  
  # ## Load mimetic groups update table
  # Mimetic_classification_update <- read_excel("Mimetic_classification_update.xlsx")
  
  ### 1.1/ Load Heliconiini outputs ####
  
  # ## Load Heliconiini outputs from Script 25 (lambda = 0.346)
  # 
  # # Load mean MCD obs values for all pairs and comimics
  # load(file = paste0("./outputs/Niche_evolution/MCD_obs_stats_",m,".RData")) 
  # # Global_MCD_obs, Comimic_MCD_obs, Comimic_MCD_obs_std
  # Helico_global_MCD_obs_old <- Comimic_MCD_obs_std
  # rm(Comimic_MCD_obs, Comimic_MCD_obs_std, Global_MCD_obs)
  # 
  # # Load mean MCD obs per mimetic groups
  # load(file = paste0("./outputs/Niche_evolution/MCD_per_ring_obs_",m,".RData")) # Load mean MCD obs value for each mimetic group
  # # MCD_per_ring_obs, MCD_std_per_ring_obs
  # Helico_group_MCD_obs_old <- MCD_std_per_ring_obs
  # rm(MCD_per_ring_obs, MCD_std_per_ring_obs)
  # 
  # # Load null data from permutation: all comimics
  # load(file = paste0("./outputs/Niche_evolution/MCD_null_stats_",m,".RData"))
  # # Global_MCD_null, Comimic_MCD_null, Comimic_MCD_std_null
  # Helico_global_MCD_null_old <- Comimic_MCD_std_null
  # rm(Global_MCD_null, Comimic_MCD_null, Comimic_MCD_std_null)
  # 
  # # Load null data from permutation: per mimetic groups 
  # load(file = paste0("./outputs/Niche_evolution/MCD_per_ring_null_stats_",m,".RData"))
  # # MCD_per_ring_null, MCD_std_per_ring_null
  # Helico_group_MCD_null_old <- MCD_std_per_ring_null
  # rm(MCD_per_ring_null, MCD_std_per_ring_null)
  
  ## Load Heliconiini outputs from Script 26 (lambda = 0.798)
  
  # Load mean MCD obs values for all pairs and comimics
  load(file = paste0("./outputs/Niche_evolution/MCD_obs_stats_HI_HELICO_",m,".RData"))
  # Global_MCD_obs, Comimic_MCD_obs, Comimic_MCD_obs_std
  Helico_global_MCD_obs <- Comimic_MCD_obs_std
  rm(Comimic_MCD_obs, Comimic_MCD_obs_std, Global_MCD_obs)
  
  # Load mean MCD obs per mimetic groups
  load(file = paste0("./outputs/Niche_evolution/MCD_per_ring_obs_HI_HELICO_",m,".RData"))
  # MCD_per_ring_obs, MCD_std_per_ring_obs
  Helico_group_MCD_obs <- MCD_std_per_ring_obs
  rm(MCD_per_ring_obs, MCD_std_per_ring_obs)
  
  # Load null data from permutation: all comimics
  load(file = paste0("./outputs/Niche_evolution/MCD_null_stats_HI_HELICO_",m,".RData"))
  # Global_MCD_null, Comimic_MCD_null, Comimic_MCD_std_null
  Helico_global_MCD_null <- Comimic_MCD_std_null
  rm(Global_MCD_null, Comimic_MCD_null, Comimic_MCD_std_null)
  
  # Load null data from permutation: per mimetic groups 
  load(file = paste0("./outputs/Niche_evolution/MCD_per_ring_null_stats_HI_HELICO_",m,".RData"))
  # MCD_per_ring_null, MCD_std_per_ring_null
  Helico_group_MCD_null <- MCD_std_per_ring_null
  rm(MCD_per_ring_null, MCD_std_per_ring_null)
  
  # ## Compare results
  # Helico_global_MCD_obs_old ; Helico_global_MCD_obs
  # Helico_group_MCD_obs_old ; Helico_group_MCD_obs
  # summary(Helico_global_MCD_null_old) ; summary(Helico_global_MCD_null)
  # summary(Helico_group_MCD_null_old) ; summary(Helico_group_MCD_null)
  # # Fairly similar. Keep the results from Script 26
  
  ### 1.2 / Load Ithomiini outputs ####
  
  # Only for "ss", because Ithomiini groups have not been merged besides shared rings
  if (m == "ss")
  {
    # ## Load outputs from Doré et al., 2023 (lambda = 0.408)
    # 
    # # Load mean MCD obs values 
    # load(file = paste0("./Ithomiini/MCD_obs_stats_719.RData"))
    # Itho_global_MCD_obs_old <- Comimic_MCD_obs_std
    # rm(Global_MCD_obs, Comimic_MCD_obs, Comimic_MCD_obs_std)
    # 
    # # Load mean MCD obs per mimetic groups
    # load(file = paste0("./Ithomiini/MCD_per_ring_obs_719.RData"))
    # Itho_group_MCD_obs_old <- MCD_std_per_ring_obs[sort(names(MCD_std_per_ring_obs))]
    # rm(MCD_per_ring_obs, MCD_std_per_ring_obs)
    # 
    # # Load null data from permutation: all comimics
    # load(file = paste0("./Ithomiini/MCD_null_stats_719_from_obs.RData"))
    # Itho_global_MCD_null_old <- Comimic_MCD_std_null
    # rm(Global_MCD_null, Comimic_MCD_null, Comimic_MCD_std_null)
    # 
    # # Load null data from permutation: per mimetic groups 
    # load(file = paste0("./Ithomiini/MCD_per_ring_null_stats_719_from_obs.RData")) 
    # Itho_group_MCD_null_old <- MCD_std_per_ring_null[, sort(colnames(MCD_std_per_ring_null))]
    # rm(MCD_per_ring_null, MCD_std_per_ring_null)
    
    ## Load outputs from Script 26 (lambda = 0.798)
    
    # Load mean MCD obs values for all pairs and comimics
    load(file = paste0("./outputs/Niche_evolution/MCD_obs_stats_HI_ITHO_",m,".RData"))
    # Global_MCD_obs, Comimic_MCD_obs, Comimic_MCD_obs_std
    Itho_global_MCD_obs <- Comimic_MCD_obs_std
    rm(Comimic_MCD_obs, Comimic_MCD_obs_std, Global_MCD_obs)
    
    # Load mean MCD obs per mimetic groups
    load(file = paste0("./outputs/Niche_evolution/MCD_per_ring_obs_HI_ITHO_",m,".RData"))
    # MCD_per_ring_obs, MCD_std_per_ring_obs
    Itho_group_MCD_obs <- MCD_std_per_ring_obs
    rm(MCD_per_ring_obs, MCD_std_per_ring_obs)
    
    # Load null data from permutation: all comimics
    load(file = paste0("./outputs/Niche_evolution/MCD_null_stats_HI_ITHO_",m,".RData"))
    # Global_MCD_null, Comimic_MCD_null, Comimic_MCD_std_null
    Itho_global_MCD_null <- Comimic_MCD_std_null
    rm(Global_MCD_null, Comimic_MCD_null, Comimic_MCD_std_null)
    
    # Load null data from permutation: per mimetic groups 
    load(file = paste0("./outputs/Niche_evolution/MCD_per_ring_null_stats_HI_ITHO_",m,".RData"))
    # MCD_per_ring_null, MCD_std_per_ring_null
    Itho_group_MCD_null <- MCD_std_per_ring_null
    rm(MCD_per_ring_null, MCD_std_per_ring_null)
    
    # ## Compare results
    # Itho_global_MCD_obs_old ; Itho_global_MCD_obs
    # Itho_group_MCD_obs_old ; Itho_group_MCD_obs
    # summary(Itho_global_MCD_null_old) ; summary(Itho_global_MCD_null)
    # summary(Itho_group_MCD_null_old) ; summary(Itho_group_MCD_null)
    # # Fairly similar. Keep the results from Script 26 for consistency
  }
  
  ### 1.3/ Load Inter-tribe outputs ####
  
  # Load mean MCD obs values for all pairs and comimics 
  load(file = paste0("./outputs/Niche_evolution/MCD_obs_stats_HI_INTER_",m,".RData"))
  Inter_global_MCD_obs <- Comimic_MCD_obs_std
  rm(Comimic_MCD_obs, Comimic_MCD_obs_std, Global_MCD_obs)
  # Load mean MCD obs per mimetic groups
  load(file = paste0("./outputs/Niche_evolution/MCD_per_ring_obs_HI_INTER_",m,".RData")) # Load mean MCD obs value for each mimetic group
  Inter_group_MCD_obs <- MCD_std_per_ring_obs
  rm(MCD_per_ring_obs, MCD_std_per_ring_obs)
  
  # Load null data from permutation: all comimics
  load(file = paste0("./outputs/Niche_evolution/MCD_null_stats_HI_INTER_",m,".RData"))
  Inter_global_MCD_null <- Comimic_MCD_std_null
  rm(Global_MCD_null, Comimic_MCD_null, Comimic_MCD_std_null)
  # Load null data from permutation: per mimetic groups 
  load(file = paste0("./outputs/Niche_evolution/MCD_per_ring_null_stats_HI_INTER_",m,".RData"))
  Inter_group_MCD_null <- MCD_std_per_ring_null
  rm(MCD_per_ring_null, MCD_std_per_ring_null)

  
  ##### 2/ Extract summary tables for global tests #####
  
  # Load previous summary table for groups/rings
  load(file = paste0( "./outputs/Niche_evolution/MCD_ring_summary_table_HeIt_",m,".RData"))
  MCD_ring_summary_table
  
  ### 2.1/ For global tests ####
  
  ## Heliconiini

  # Load summary table to extract total nb of pairs
  Helico_group_MCD_summary_table <- MCD_ring_summary_table[MCD_ring_summary_table$tribe == "Heliconiini", ]
  
  # Extract nb of units and pairs
  Helico_global_N_units <- sum(Helico_group_MCD_summary_table$N_units) ; Helico_global_N_units
  Helico_global_N_pairs <- sum(Helico_group_MCD_summary_table$N_pairs) ; Helico_global_N_pairs
  
  # Get data
  Helico_global_MCD_obs
  Helico_global_MCD_null
  Helico_global_MCD_dist <- c(Helico_global_MCD_obs, Helico_global_MCD_null)
  
  # Compute global summary stats
  Helico_global_MCD_Q50 <- median(Helico_global_MCD_dist) ; Helico_global_MCD_Q50
  Helico_global_MCD_Q5 <- quantile(Helico_global_MCD_dist, 0.05) ; Helico_global_MCD_Q5
  Helico_global_MCD_p_value <- round(ecdf(x = Helico_global_MCD_dist)(Helico_global_MCD_obs), 3) ; Helico_global_MCD_p_value
  
  # Create summary df
  Helico_global_MCD_summary_df <- data.frame(Tribe = "Heliconiini",
                                             N_units = Helico_global_N_units,
                                             N_pairs = Helico_global_N_pairs,
                                             MCD_obs = round(Helico_global_MCD_obs, 3),
                                             MCD_Q50 = round(Helico_global_MCD_Q50, 3),
                                             MCD_Q5 = round(Helico_global_MCD_Q5, 3),
                                             p_value = round(Helico_global_MCD_p_value, 3)
  )
  Helico_global_MCD_summary_df
  
  ## Ithomiini
  
  if (m == "ss")
  {
    
    # Load summary table to extract total nb of pairs
    Itho_group_MCD_summary_table <- MCD_ring_summary_table[MCD_ring_summary_table$tribe == "Ithomiini", ]
    
    # Extract nb of units and pairs
    Itho_global_N_units <- sum(Itho_group_MCD_summary_table$N_units) ; Itho_global_N_units
    Itho_global_N_pairs <- sum(Itho_group_MCD_summary_table$N_pairs, na.rm = T) ; Itho_global_N_pairs
    
    # Get data
    Itho_global_MCD_obs
    Itho_global_MCD_null
    Itho_global_MCD_dist <- c(Itho_global_MCD_obs, Itho_global_MCD_null) 
    
    # Compute global summary stats
    Itho_global_MCD_Q50 <- median(Itho_global_MCD_dist) ; Itho_global_MCD_Q50
    Itho_global_MCD_Q5 <- quantile(Itho_global_MCD_dist, 0.05) ; Itho_global_MCD_Q5
    Itho_global_MCD_p_value <- round(ecdf(x = Itho_global_MCD_dist)(Itho_global_MCD_obs), 3) ; Itho_global_MCD_p_value
    
    # Create summary df
    Itho_global_MCD_summary_df <- data.frame(Tribe = "Ithomiini",
                                             N_units = Itho_global_N_units,
                                             N_pairs = Itho_global_N_pairs,
                                             MCD_obs = round(Itho_global_MCD_obs, 3),
                                             MCD_Q50 = round(Itho_global_MCD_Q50, 3),
                                             MCD_Q5 = round(Itho_global_MCD_Q5, 3),
                                             p_value = round(Itho_global_MCD_p_value, 3)
    )
    Itho_global_MCD_summary_df
  
  }
  
  ## Inter
  
  # Load summary table to extract total nb of pairs
  Inter_group_MCD_summary_table <- MCD_ring_summary_table[MCD_ring_summary_table$tribe == "Inter-tribe", ]
  
  # Extract nb of units and pairs
  Inter_global_N_units <- sum(Inter_group_MCD_summary_table$N_units) ; Inter_global_N_units
  Inter_global_N_pairs <- sum(Inter_group_MCD_summary_table$N_pairs, na.rm = T) ; Inter_global_N_pairs
  
  # Get data
  Inter_global_MCD_obs
  Inter_global_MCD_null
  Inter_global_MCD_dist <- c(Inter_global_MCD_obs, Inter_global_MCD_null) ; 
  
  # Compute global summary stats
  Inter_global_MCD_Q50 <- median(Inter_global_MCD_dist) ; Inter_global_MCD_Q50
  Inter_global_MCD_Q5 <- quantile(Inter_global_MCD_dist, 0.05) ; Inter_global_MCD_Q5
  Inter_global_MCD_p_value <- round(ecdf(x = Inter_global_MCD_dist)(Inter_global_MCD_obs), 3) ; Inter_global_MCD_p_value
  
  # Create summary df
  Inter_global_MCD_summary_df <- data.frame(Tribe = "Inter-tribe",
                                            N_units = Inter_global_N_units,
                                            N_pairs = Inter_global_N_pairs,
                                            MCD_obs = round(Inter_global_MCD_obs, 3),
                                            MCD_Q50 = round(Inter_global_MCD_Q50, 3),
                                            MCD_Q5 = round(Inter_global_MCD_Q5, 3),
                                            p_value = round(Inter_global_MCD_p_value, 3)
  )
  Inter_global_MCD_summary_df
  
  
  ## Merge all groups
  
  if (m == "ss")
  {
    global_tests_MCD_summary_df <- rbind(Helico_global_MCD_summary_df, Inter_global_MCD_summary_df, Itho_global_MCD_summary_df)
  } else {
    global_tests_MCD_summary_df <- rbind(Helico_global_MCD_summary_df, Inter_global_MCD_summary_df)
  }
  
  row.names(global_tests_MCD_summary_df) <- NULL
  global_tests_MCD_summary_df
  
  saveRDS(global_tests_MCD_summary_df, file = paste0("./outputs/Niche_evolution/global_tests_MCD_summary_df_",m,".rds"))
  openxlsx::write.xlsx(x = global_tests_MCD_summary_df, file = paste0("./outputs/Niche_evolution/global_tests_MCD_summary_df_",m,".xlsx"))
  
  
  ##### 3/ Plot global test histograms ####
  
  ### 3.1/ Function to plot histograms ####
  
  plot_histogram_MCD_perm_test <- function(MCD_obs, MCD_null,
                                           N_pairs,
                                           title, cex_title = 1.3,
                                           cex_axis = 1.5, cex_lab = 1.6, cex_legend = 1.4,
                                           breaks = 30,
                                           arrow_btm = 5, arrow_top = 80, arrow_adjust = 0,
                                           inset_Q = c(0.02, 0.10),
                                           inset_obs = c(0.02, 0.30),
                                           panel_letter = "", cex_panel_letter = 2.0)
  {
    
    full_dist <- c(MCD_obs, MCD_null)
    
    # Extract Q stats and p-value
    MCD_Q50 <- median(full_dist)
    MCD_Q5 <- quantile(full_dist, 0.05)
    MCD_p_value <- round(ecdf(x = full_dist)(MCD_obs), 3)
    
    hist(full_dist,
         breaks = breaks, freq = TRUE, col = "gray", 
         main = title, 
         xlab = expression(paste("Standardized Mean pairwise Climatic Distance", sep = "")),
         cex.axis = cex_axis, cex.lab = cex_lab, cex.main = cex_title, lwd = 2)
    arrows(x0 = MCD_obs + arrow_adjust, y0 = arrow_top, x1 = MCD_obs + arrow_adjust, y1 = arrow_btm, length = 0.1, lwd = 3)
    abline(v = MCD_Q50, lty = 2, lwd = 2)
    abline(v = MCD_Q5, lty = 2, lwd = 2, col = "red")
    
    # Insert quantiles legend
    legend(legend = c(paste0("Median = ", format(round(MCD_Q50, 3), nsmall = 3)), 
                      paste0("Q5% = ", format(round(MCD_Q5, 3), nsmall = 3))), 
           x = "topleft", inset = inset_Q, y.intersp = 1.2, lty = 2 , lwd = 2, col = c("black", "red"), cex = cex_legend, bty ="n")
    
    # Insert legend
    legend(legend = c(paste0("MCD obs = ", format(round(MCD_obs, 3), nsmall = 3)),
                      paste0("N pairs = ", N_pairs),
                      paste0("p ≤ ", format(round(MCD_p_value, 3), nsmall = 3))), 
           x = "topleft", inset = inset_obs, y.intersp = 1.1, cex = cex_legend, bty ="n")
    
    
    # Add panel legend
    legend(legend = panel_letter, text.font = 2,
           x = "topright", inset = c(0.00, 0.00), xjust = 0.5,
           cex = cex_panel_letter, bty ="n")
    
  }
  
  
  ### 3.2/ Heliconiini ####
  
  Helico_global_MCD_obs
  Helico_global_MCD_null
  
  pdf(file = paste0("./outputs/Niche_evolution/Helico_global_MCD_histo_",m,".pdf"), width = 8, height = 6)
  initial_margins <- par()$mar 
  par(mar = c(5, 5, 4, 2)) # bltr
  plot_histogram_MCD_perm_test(MCD_obs = Helico_global_MCD_obs,
                               MCD_null = Helico_global_MCD_null,
                               N_pairs = Helico_global_N_pairs,
                               title = paste0("Heliconiini - ",m),
                               cex_title = 2.0,
                               inset_Q = c(0.02, 0.00),
                               inset_obs = c(-0.02, 0.25),
                               arrow_top = 65,
                               arrow_adjust = 0.00)
  par(mar = initial_margins)
  dev.off()
  
  
  ### 3.3/ Ithomiini ####
  
  if (m == "ss")
  {
    Itho_global_MCD_obs
    Itho_global_MCD_null
    
    pdf(file = paste0("./outputs/Niche_evolution/Itho_global_MCD_histo_",m,".pdf"), width = 8, height = 6)
    initial_margins <- par()$mar 
    par(mar = c(5, 5, 4, 2)) # bltr
    plot_histogram_MCD_perm_test(MCD_obs = Itho_global_MCD_obs,
                                 MCD_null = Itho_global_MCD_null,
                                 N_pairs = Itho_global_N_pairs,
                                 title = paste0("Ithomiini - ",m),
                                 cex_title = 2.0,
                                 inset_Q = c(0.02, 0.00),
                                 inset_obs = c(-0.02, 0.25),
                                 arrow_btm = 10,
                                 arrow_top = 125,
                                 arrow_adjust = 0.0025)
    par(mar = initial_margins)
    dev.off()
  }
  
  ### 3.4/ Inter-tribe ####
  
  Inter_global_MCD_obs
  Inter_global_MCD_null
  
  pdf(file = paste0("./outputs/Niche_evolution/Inter_global_MCD_histo_",m,".pdf"), width = 8, height = 6)
  initial_margins <- par()$mar 
  par(mar = c(5, 5, 4, 2)) # bltr
  plot_histogram_MCD_perm_test(MCD_obs = Inter_global_MCD_obs,
                               MCD_null = Inter_global_MCD_null,
                               N_pairs = Inter_global_N_pairs,
                               title = paste0("Inter-tribe - ",m),
                               cex_title = 2.0,
                               arrow_btm = 5,
                               arrow_top = 40,
                               inset_Q = c(0.02, 0.00),
                               inset_obs = c(-0.02, 0.25),
                               arrow_adjust = 0.00)
  par(mar = initial_margins)
  dev.off()
  
  
  ##### 4/ Extract summary tables per mimetic rings ####
  
  # Load previous summary table for groups/rings
  load(file = paste0("./outputs/Niche_evolution/MCD_ring_summary_table_HeIt_",m,".RData"))
  MCD_ring_summary_table
  
  # Function to compute p_values as quantile from a null distribution
  compute_p_value <- function (x) 
  { 
    test <- any(is.na(x))
    if(test)
    { 
      y <- NA 
    } else { 
      y <- round(ecdf(x = x)(x[1]), 3) 
    } 
    return(y)
  }
  
  # Function to assign significativity level based on p_values
  assign_signif_lvl <- function (x) 
  { 
    signifs <- rep("", length(x))
    
    signifs[x <= 0.05] <- "*"
    signifs[x <= 0.01] <- "**"
    signifs[x <= 0.001] <- "***"
    
    signifs[is.na(x)] <- NA
    
    return(signifs)
  }
  
  ### 4.1/ Heliconiini ####
  
  Helico_group_MCD_obs
  Helico_group_MCD_null
  
  Helico_group_MCD_dist <- rbind(Helico_group_MCD_obs, Helico_group_MCD_null) ; 
  
  Helico_group_MCD_Q50 <- apply(X = Helico_group_MCD_dist, MARGIN = 2, FUN = median) ; Helico_group_MCD_Q50
  Helico_group_MCD_Q5 <- apply(X = Helico_group_MCD_dist, MARGIN = 2, FUN = quantile, probs =  0.05, na.rm = T) ; Helico_group_MCD_Q5
  Helico_group_MCD_p_value <- apply(X = Helico_group_MCD_dist, MARGIN = 2, FUN = compute_p_value) ; Helico_group_MCD_p_value
  Helico_group_MCD_signif <- assign_signif_lvl(Helico_group_MCD_p_value) ; Helico_group_MCD_signif
  
  # Get nb of units and pairs from previous summary table
  Previous_summary_table <- MCD_ring_summary_table[MCD_ring_summary_table$tribe == "Heliconiini", ]
  Previous_summary_table$PHENOTYPIC_GROUP <- str_remove(string = Previous_summary_table$ring, pattern = ".HELICO")
  
  Helico_group_N_units <- Previous_summary_table$N_units
  Helico_group_N_units <- Helico_group_N_units[match(x = Previous_summary_table$PHENOTYPIC_GROUP, table = names(Helico_group_MCD_obs))]
  Helico_group_N_pairs <- Previous_summary_table$N_pairs
  Helico_group_N_pairs <- Helico_group_N_pairs[match(x = Previous_summary_table$PHENOTYPIC_GROUP, table = names(Helico_group_MCD_obs))]
  
  # Bind everything
  Helico_group_MCD_summary_df <- cbind(as.numeric(Helico_group_N_units), as.numeric(Helico_group_N_pairs), as.numeric(round(Helico_group_MCD_obs, 3)), as.numeric(round(Helico_group_MCD_Q50, 3)), as.numeric(round(Helico_group_MCD_Q5, 3)), as.numeric(round(Helico_group_MCD_p_value, 3)), Helico_group_MCD_signif)
  PHENOTYPIC_GROUP <- names(Helico_group_MCD_obs)
  Helico_group_MCD_summary_df <- as.data.frame(cbind(PHENOTYPIC_GROUP, Helico_group_MCD_summary_df))
  names(Helico_group_MCD_summary_df) <- c("PHENOTYPIC_GROUP", "N_units", "N_pairs", "MCD_obs", "MCD_Q50", "MCD_Q5", "p_value", "signif_lvl")
  
  # Add tribe
  Helico_group_MCD_summary_df$Tribe <- "Heliconiini"
  Helico_group_MCD_summary_df <- Helico_group_MCD_summary_df %>% 
    dplyr::select(Tribe, PHENOTYPIC_GROUP, N_units, N_pairs, MCD_obs, MCD_Q50, MCD_Q5, p_value, signif_lvl)
  Helico_group_MCD_summary_df
  
  if (m == "ss")
  {
    # Add JUDITH which disappear from the summary table because it is not included in the phylogeny
    JUDITH_MCD_summary_df <- data.frame(Tribe = "Heliconiini", PHENOTYPIC_GROUP = "JUDITH", N_units = 1, N_pairs = 0, MCD_obs = NA, MCD_Q50 = NA, MCD_Q5 = NA, p_value = NA, signif_lvl = NA)
    Helico_group_MCD_summary_df <- rbind(Helico_group_MCD_summary_df, JUDITH_MCD_summary_df)
  }

  Helico_group_MCD_summary_df <- Helico_group_MCD_summary_df %>% 
    arrange(PHENOTYPIC_GROUP)
  Helico_group_MCD_summary_df 
  
  # # Update mimetic group names
  # 
  # Old_names <- str_replace(string = Mimetic_classification_update$`Initial name`, pattern = " ", replacement = "\\.")
  # Old_names <- str_replace(string = Old_names, pattern = "-", replacement = "\\.")
  # Mimetic_classification_update$Old_names <- Old_names
  # 
  # Helico_group_MCD_summary_df <- left_join(x = Helico_group_MCD_summary_df,
  #                                          y = Mimetic_classification_update[, c("Old_names", "New name")],
  #                                          by = c("MIMETIC_GROUP" = "Old_names"))
  # 
  # Helico_group_MCD_summary_df <- Helico_group_MCD_summary_df %>% 
  #   mutate(MIMETIC_GROUP = `New name`) %>%
  #   select(Tribe, MIMETIC_GROUP, N_units, N_pairs, MCD_obs, MCD_Q50, MCD_Q5, p_value, signif_lvl) %>% 
  #   arrange(MIMETIC_GROUP)
  
  Helico_group_MCD_summary_df
  
  saveRDS(Helico_group_MCD_summary_df, file = paste0("./outputs/Niche_evolution/Helico_group_MCD_summary_df_",m,".rds"))
  
  
  ### 4.2/ Ithomiini ####
  
  if (m == "ss")
  {
    Itho_group_MCD_obs
    Itho_group_MCD_null
    
    Itho_group_MCD_dist <- rbind(Itho_group_MCD_obs, Itho_group_MCD_null)
    
    Itho_group_MCD_Q50 <- apply(X = Itho_group_MCD_dist, MARGIN = 2, FUN = median) ; Itho_group_MCD_Q50
    Itho_group_MCD_Q5 <- apply(X = Itho_group_MCD_dist, MARGIN = 2, FUN = quantile, probs =  0.05, na.rm = T) ; Itho_group_MCD_Q5
    Itho_group_MCD_p_value <- apply(X = Itho_group_MCD_dist, MARGIN = 2, FUN = compute_p_value) ; Itho_group_MCD_p_value
    Itho_group_MCD_signif <- assign_signif_lvl(Itho_group_MCD_p_value) ; Itho_group_MCD_signif
    
    # Get nb of units and pairs from previous summary table
    Previous_summary_table <- MCD_ring_summary_table[MCD_ring_summary_table$tribe == "Ithomiini", ]
    Previous_summary_table$PHENOTYPIC_GROUP <- str_remove(string = Previous_summary_table$ring, pattern = ".ITHO")
    
    Itho_group_N_units <- Previous_summary_table$N_units
    Itho_group_N_units <- Itho_group_N_units[match(x = names(Itho_group_MCD_obs), table = Previous_summary_table$PHENOTYPIC_GROUP)]
    Itho_group_N_pairs <- Previous_summary_table$N_pairs
    Itho_group_N_pairs <- Itho_group_N_pairs[match(x = names(Itho_group_MCD_obs), table = Previous_summary_table$PHENOTYPIC_GROUP)]
    
    # Bind everything
    Itho_group_MCD_summary_df <- cbind(as.numeric(Itho_group_N_units), as.numeric(Itho_group_N_pairs), as.numeric(round(Itho_group_MCD_obs, 3)), as.numeric(round(Itho_group_MCD_Q50, 3)), as.numeric(round(Itho_group_MCD_Q5, 3)), as.numeric(round(Itho_group_MCD_p_value, 3)), Itho_group_MCD_signif)
    PHENOTYPIC_GROUP <- names(Itho_group_MCD_obs)
    Itho_group_MCD_summary_df <- as.data.frame(cbind(PHENOTYPIC_GROUP, Itho_group_MCD_summary_df))
    names(Itho_group_MCD_summary_df) <- c("PHENOTYPIC_GROUP", "N_units", "N_pairs", "MCD_obs", "MCD_Q50", "MCD_Q5", "p_value", "signif_lvl")
    
    # Add tribe
    Itho_group_MCD_summary_df$Tribe <- "Ithomiini"
    Itho_group_MCD_summary_df <- Itho_group_MCD_summary_df %>% 
      select(Tribe, PHENOTYPIC_GROUP, N_units, N_pairs, MCD_obs, MCD_Q50, MCD_Q5, p_value, signif_lvl) %>% 
      arrange(PHENOTYPIC_GROUP)
    
    Itho_group_MCD_summary_df
    
    saveRDS(Itho_group_MCD_summary_df, file = paste0("./outputs/Niche_evolution/Itho_group_MCD_summary_df_",m,".rds"))
  }
  
  ### 4.3/ Inter-tribe ####
  
  Inter_group_MCD_obs
  Inter_group_MCD_null
  
  Inter_group_MCD_dist <- rbind(Inter_group_MCD_obs, Inter_group_MCD_null) ; 
  
  Inter_group_MCD_Q50 <- apply(X = Inter_group_MCD_dist, MARGIN = 2, FUN = median) ; Inter_group_MCD_Q50
  Inter_group_MCD_Q5 <- apply(X = Inter_group_MCD_dist, MARGIN = 2, FUN = quantile, probs =  0.05, na.rm = T) ; Inter_group_MCD_Q5
  Inter_group_MCD_p_value <- apply(X = Inter_group_MCD_dist, MARGIN = 2, FUN = compute_p_value) ; Inter_group_MCD_p_value
  Inter_group_MCD_signif <- assign_signif_lvl(Inter_group_MCD_p_value) ; Inter_group_MCD_signif
  
  # Get nb of units and pairs from previous summary table
  Previous_summary_table <- MCD_ring_summary_table[MCD_ring_summary_table$tribe == "Inter-tribe", ]
  Previous_summary_table$PHENOTYPIC_GROUP <- str_remove(string = Previous_summary_table$ring, pattern = ".INTER")
  
  Inter_group_N_units <- Previous_summary_table$N_units
  Inter_group_N_units <- Inter_group_N_units[match(x = Previous_summary_table$PHENOTYPIC_GROUP, table = names(Inter_group_MCD_obs))]
  Inter_group_N_pairs <- Previous_summary_table$N_pairs
  Inter_group_N_pairs <- Inter_group_N_pairs[match(x = Previous_summary_table$PHENOTYPIC_GROUP, table = names(Inter_group_MCD_obs))]
  
  # Bind everything
  Inter_group_MCD_summary_df <- cbind(as.numeric(Inter_group_N_units), as.numeric(Inter_group_N_pairs), as.numeric(round(Inter_group_MCD_obs, 3)), as.numeric(round(Inter_group_MCD_Q50, 3)), as.numeric(round(Inter_group_MCD_Q5, 3)), as.numeric(round(Inter_group_MCD_p_value, 3)), Inter_group_MCD_signif)
  PHENOTYPIC_GROUP <- str_remove(string = names(Inter_group_MCD_obs), pattern = ".BOTH")
  Inter_group_MCD_summary_df <- as.data.frame(cbind(PHENOTYPIC_GROUP, Inter_group_MCD_summary_df))
  names(Inter_group_MCD_summary_df) <- c("PHENOTYPIC_GROUP", "N_units", "N_pairs", "MCD_obs", "MCD_Q50", "MCD_Q5", "p_value", "signif_lvl")
  
  # Add tribe
  Inter_group_MCD_summary_df$Tribe <- "Inter-tribe"
  Inter_group_MCD_summary_df <- Inter_group_MCD_summary_df %>% 
    select(Tribe, PHENOTYPIC_GROUP, N_units, N_pairs, MCD_obs, MCD_Q50, MCD_Q5, p_value, signif_lvl) %>% 
    arrange(PHENOTYPIC_GROUP)
  
  Inter_group_MCD_summary_df
  
  saveRDS(Inter_group_MCD_summary_df, file = paste0("./outputs/Niche_evolution/Inter_group_MCD_summary_df_",m,".rds"))
  
  
  ### 4.4/ Bind all tribes ####
  
  Helico_group_MCD_summary_df <- readRDS(file = paste0("./outputs/Niche_evolution/Helico_group_MCD_summary_df_",m,".rds"))
  Inter_group_MCD_summary_df <- readRDS(file = paste0("./outputs/Niche_evolution/Inter_group_MCD_summary_df_",m,".rds"))
  
  if (m == "ss")
  {
    Itho_group_MCD_summary_df <- readRDS(file = paste0("./outputs/Niche_evolution/Itho_group_MCD_summary_df_",m,".rds"))
    all_groups_MCD_summary_df <- rbind(Helico_group_MCD_summary_df, Inter_group_MCD_summary_df, Itho_group_MCD_summary_df)
  } else {
    all_groups_MCD_summary_df <- rbind(Helico_group_MCD_summary_df, Inter_group_MCD_summary_df)
  }
  all_groups_MCD_summary_df
  
  saveRDS(all_groups_MCD_summary_df, file = paste0("./outputs/Niche_evolution/all_groups_MCD_summary_df_",m,".rds"))
  write.xlsx(x = all_groups_MCD_summary_df, paste0(file = "./outputs/Niche_evolution/all_groups_MCD_summary_df_",m,".xlsx"))

}
