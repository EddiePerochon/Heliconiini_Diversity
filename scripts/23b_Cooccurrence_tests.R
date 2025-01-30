##### Script 23b: Plot results of spatial cooccurrence tests #####


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
# Heliconiini and Inter-tribe Bray-Curtis test outputs
# Ithomiini Bray-Curtis test outputs (Doré et al., 2023: 10.1111/ele.14198)
###

### Outputs
# Histograms of global tests
# Summary tables of spatial cooccurrence tests
# Include Nunits, Npairs, BCobs, Q50%, Q95%, p-value
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
  
  ## Load Heliconiini outputs from Script 22
  
  # Load mean BC obs values for all pairs, comimics, and non comimics (for Helico?)
  load(file = paste0("./outputs/Community_Structure/All_Global_BC_OMU_",m,".RData")) 
  
  Helico_global_BC_obs <- Global_mimic_mean_BC
  rm(Global_mimic_mean_BC, Global_no.mimic_mean_BC, Global_mean_BC)
  
  # Load mean BC obs per mimetic groups
  load(file = paste0("./outputs/Community_Structure/mean_BC_OMU_",m,".RData")) # Load mean BC obs value for each mimetic group
  Helico_group_BC_obs <- mean_BC
  rm(mean_BC)
  
  # # Load null data from permutation from Script 22: all comimics, and per mimetic groups 
  # load(file = paste0("./outputs/Community_Structure/Permutations/All_simul_mean_BC_OMU_",m,".RData"))
  # Helico_group_BC_null_old <- mean_BC_null
  # Helico_global_BC_null_old <- BC_mimic_null
  # rm(mean_BC_null, BC_mimic_null, BC_no_mimic_null)
  
  ## Load Heliconiini outputs from Script 23
  
  # Should be similar to:
  load(file = paste0("./outputs/Community_Structure/Permutations/All_simul_mean_BC_OMU_HE_",m,".RData"))
  Helico_group_BC_null <- mean_BC_null
  Helico_global_BC_null <- BC_mimic_null
  rm(mean_BC_null, BC_mimic_null, BC_no_mimic_null)

  # summary(Helico_global_BC_null_old) # Does not seem compatible with mean overall value of 0.8926!
  # summary(Helico_global_BC_null) # Seems compatible with mean overall value of 0.8926
  
  
  ## Load Ithomiini outputs (from Doré et al., 2023) 
  
  # Only for "ss", because Ithomiini groups have not been merged besides shared rings
  if (m == "ss")
  {
    # Load mean BC obs values 
    load(file = "./Ithomiini/All_Global_BC.RData") 
    Itho_global_BC_obs <- Global_mimic_mean_BC
    rm(Global_mimic_mean_BC, Global_no.mimic_mean_BC, Global_mean_BC)
    
    # Load mean BC obs per mimetic groups
    load(file = paste0("./Ithomiini/mean_BC.RData"))
    Itho_group_BC_obs <- mean_BC
    rm(mean_BC)
    
    # Load null data from permutation: all comimics, and per mimetic groups 
    load(file = "./Ithomiini/All_simul_mean_BC.RData") 
    Itho_group_BC_null <- mean_BC_null
    Itho_global_BC_null <- BC_mimic_null
    rm(mean_BC_null, BC_mimic_null, BC_no_mimic_null)
    
    # ## Load Ithomiini outputs from Script 23
    # 
    # # Should be similar to:
    # load(file = paste0("./outputs/Community_Structure/Permutations/All_simul_mean_BC_OMU_IT_",m,".RData"))
    # Itho_group_BC_null_2 <- mean_BC_null
    # Itho_global_BC_null_2 <- BC_mimic_null
    # rm(mean_BC_null, BC_mimic_null, BC_no_mimic_null)
    # 
    # summary(Itho_global_BC_null) # Seems compatible with mean overall value of 0.9494!
    # summary(Itho_global_BC_null_2) # Seems not so much compatible with mean overall value of 0.9494 (but run is not finished yet)
  }
  
  ## Inter-tribe outputs
  
  # Load mean BC obs values for all pairs, comimics, and non comimics for shared rings
  load(file = paste0("./outputs/Community_Structure/BC_mimicry_ring_mat/IthoVSHelico/All_shared_rings_Global_BC_HeIt_OMU_",m,".RData")) 
  # Global_mean_BC, Global_mimic_BOTH_mean_BC, Global_mimic_INTER_mean_BC, Global_no.mimic_mean_BC
  
  Inter_global_BC_obs <- Global_mimic_INTER_mean_BC
  Both_global_BC_obs <- Global_mimic_BOTH_mean_BC
  rm(Global_no.mimic_mean_BC, Global_mean_BC, Global_mimic_INTER_mean_BC, Global_mimic_BOTH_mean_BC)
  
  # # Load mean BC for shared rings, across all pairs (BOTH)
  # load(file = paste0("./outputs/Community_Structure/BC_mimicry_ring_mat/IthoVSHelico/mean_BC_HI.BOTH_HeIt_OMU_",m,".RData")) # Save final vector with mean BC values per ring
  # Both_group_BC_obs <- mean_BC_HI_BOTH
  # rm(mean_BC_HI_BOTH)
  
  # Load mean BC for shared rings, across inter-tribes pairs only (INTER)
  load(file = paste0("./outputs/Community_Structure/BC_mimicry_ring_mat/IthoVSHelico/mean_BC_HI.INTER_HeIt_OMU_",m,".RData")) # Save final vector with mean BC values per ring
  Inter_group_BC_obs <- mean_BC_HI_INTER
  rm(mean_BC_HI_INTER)
  
  # Load mean BC obs per mimetic groups (groups x tribes)
  load(file = paste0("./outputs/Community_Structure/BC_mimicry_ring_mat/IthoVSHelico/mean_BC_HeIt_OMU_",m,".RData")) # Save final vector with mean BC values per ring
  All_ring.tribes_group_BC_obs <- mean_BC
  rm(mean_BC)
  Shared_groups <- str_remove(string = names(Inter_group_BC_obs), pattern = "\\..*")
  Shared_groups_match <- str_detect(string = names(All_ring.tribes_group_BC_obs), pattern = paste0(Shared_groups, collapse = "|"))
  Per_tribes_shared_group_BC_obs <- All_ring.tribes_group_BC_obs[Shared_groups_match]
  
  # # Load null data from permutation: all comimics, and per mimetic groups for BOTH pairs
  # load(file = paste0("./outputs/Community_Structure/Permutations/All_simul_mean_BC_OMU_HEIT.BOTH_",m,".RData"))
  # # mean_BC_BOTH_null, BC_mimic_null.BOTH
  # Both_group_BC_null <- mean_BC_BOTH_null
  # Both_global_BC_null <- BC_mimic_null.BOTH
  # rm(mean_BC_BOTH_null, BC_mimic_null.BOTH)
  
  # Load null data from permutation: all comimics, and per mimetic groups for INTER pairs
  load(file = paste0("./outputs/Community_Structure/Permutations/All_simul_mean_BC_OMU_HEIT.INTER_",m,".RData"))
  # mean_BC_INTER_null, BC_mimic_null.INTER
  Inter_group_BC_null <- mean_BC_INTER_null
  Inter_global_BC_null <- BC_mimic_null.INTER
  rm(mean_BC_INTER_null, BC_mimic_null.INTER)
  
  # ### All = both tribes considered (with shared rings as BOTH pairs)
  # 
  # load(file = paste0("./outputs/Community_Structure/BC_mimicry_ring_mat/IthoVSHelico/All_ring.tribes_Global_BC_HeIt_OMU_",m,".RData"))
  # All_global_BC_obs <- Global_mimic_mean_BC
  # rm(Global_mean_BC, Global_mimic_mean_BC, Global_no.mimic_mean_BC)
  
  # No permutations computed for All data.
  
  
  ##### 2/ Extract summary tables for global tests #####
  
  ### 2.1/ For global tests ####
  
  ## Heliconiini
  
  # Load summary table to extract total nb of pairs
  # Helico_group_BC_summary_table <- read.csv2(file = paste0("./tables/BC_ring_summary_table_OMU_",m,".csv"))
  Helico_group_BC_summary_table <- read.xlsx(xlsxFile = paste0("./tables/BC_ring_summary_table_OMU_",m,".xlsx"))
  
  Helico_global_N_pairs <- sum(as.numeric(Helico_group_BC_summary_table$N_pairs)) ; Helico_global_N_pairs
  
  Helico_global_BC_obs
  Helico_global_BC_null
  Helico_global_BC_dist <- c(Helico_global_BC_obs, Helico_global_BC_null)
  
  Helico_global_BC_Q50 <- median(Helico_global_BC_dist) ; Helico_global_BC_Q50
  Helico_global_BC_Q5 <- quantile(Helico_global_BC_dist, 0.05) ; Helico_global_BC_Q5
  Helico_global_BC_p_value <- round(ecdf(x = Helico_global_BC_dist)(Helico_global_BC_obs), 3) ; Helico_global_BC_p_value
  
  Helico_global_BC_summary_df <- data.frame(Tribe = "Heliconiini",
                                            N_pairs = Helico_global_N_pairs,
                                            BC_obs = round(Helico_global_BC_obs, 3),
                                            BC_Q50 = round(Helico_global_BC_Q50, 3),
                                            BC_Q5 = round(Helico_global_BC_Q5, 3),
                                            p_value = round(Helico_global_BC_p_value, 3)
  )
  Helico_global_BC_summary_df
  
  ## Ithomiini
  
  if (m == "ss")
  {
    # Load summary table to extract total nb of pairs
    load(file = paste0("./Ithomiini/BC_ring_summary_table.Rdata"))
    Itho_group_BC_summary_table <- BC_ring_summary_table
    Itho_global_N_pairs <- sum(Itho_group_BC_summary_table$N_pairs, na.rm = T) ; Itho_global_N_pairs
    
    Itho_global_BC_obs
    Itho_global_BC_null
    Itho_global_BC_dist <- Itho_global_BC_null
    Itho_global_BC_dist[1] <- Itho_global_BC_obs
    
    Itho_global_BC_Q50 <- median(Itho_global_BC_dist) ; Itho_global_BC_Q50
    Itho_global_BC_Q5 <- quantile(Itho_global_BC_dist, 0.05) ; Itho_global_BC_Q5
    Itho_global_BC_p_value <- round(ecdf(x = Itho_global_BC_dist)(Itho_global_BC_obs), 3) ; Itho_global_BC_p_value
    
    Itho_global_BC_summary_df <- data.frame(Tribe = "Ithomiini",
                                            N_pairs = Itho_global_N_pairs,
                                            BC_obs = round(Itho_global_BC_obs, 3),
                                            BC_Q50 = round(Itho_global_BC_Q50, 3),
                                            BC_Q5 = round(Itho_global_BC_Q5, 3),
                                            p_value = round(Itho_global_BC_p_value, 3)
    )
    Itho_global_BC_summary_df
  } else {
    Itho_global_BC_summary_df <- data.frame(Tribe = "Ithomiini",
                                            N_pairs = NA,
                                            BC_obs = NA,
                                            BC_Q50 = NA,
                                            BC_Q5 = NA,
                                            p_value = NA)
  }
  
  ## Inter
  
  # # Load previous summary table
  # load(file = paste0( "./outputs/Community_Structure/BC_ring_summary_table_HeIt.Rdata"))
  # BC_ring_summary_table
  # 
  # # Load summary table to extract total nb of pairs
  # Inter_global_BC_summary_df <- BC_ring_summary_table[BC_ring_summary_table$tribe == "INTER", ]
  # Inter_global_N_pairs <- sum(Inter_global_BC_summary_df$N_pairs, na.rm = T) ; Inter_global_N_pairs
  
  # Extract number of pairs from pair matrices
  Inter_group_N_pairs_list <- NA
  Both_group_N_pairs_list <- NA
  # Loop per shared rings
  for (i in seq_along(Shared_groups))
  {
    shared_group_i <- Shared_groups[i]
    
    load(file = paste0("./outputs/Community_Structure/BC_mimicry_ring_mat/IthoVSHelico/mat.index.HI.",shared_group_i,".INTER_HeIt_OMU_",m,".RData"))
    Inter_group_N_pairs_list[i] <- ncol(mat.index.INTER)
    
    load(file = paste0("./outputs/Community_Structure/BC_mimicry_ring_mat/IthoVSHelico/mat.index.HI.",shared_group_i,".BOTH_HeIt_OMU_",m,".RData"))
    Both_group_N_pairs_list[i] <- ncol(mat.index)
  }
  # Assign names of shared rings
  names(Inter_group_N_pairs_list) <- paste0(Shared_groups,".INTER") ; Inter_group_N_pairs_list
  names(Both_group_N_pairs_list) <- paste0(Shared_groups,".BOTH") ; Both_group_N_pairs_list
  
  # Compute sum across shared rings
  Inter_global_N_pairs <- sum(Inter_group_N_pairs_list, na.rm = T) ; Inter_global_N_pairs
  Both_global_N_pairs <- sum(Both_group_N_pairs_list, na.rm = T) ; Both_global_N_pairs
    
  # Merge observed and null data to compute summary stats
  Inter_global_BC_obs
  Inter_global_BC_null
  Inter_global_BC_dist <- c(Inter_global_BC_obs, Inter_global_BC_null)
  
  Inter_global_BC_Q50 <- median(Inter_global_BC_dist) ; Inter_global_BC_Q50
  Inter_global_BC_Q5 <- quantile(Inter_global_BC_dist, 0.05) ; Inter_global_BC_Q5
  Inter_global_BC_p_value <- round(ecdf(x = Inter_global_BC_dist)(Inter_global_BC_obs), 3) ; Inter_global_BC_p_value
  
  Inter_global_BC_summary_df <- data.frame(Tribe = "Inter-tribe",
                                           N_pairs = Inter_global_N_pairs,
                                           BC_obs = round(Inter_global_BC_obs, 3),
                                           BC_Q50 = round(Inter_global_BC_Q50, 3),
                                           BC_Q5 = round(Inter_global_BC_Q5, 3),
                                           p_value = round(Inter_global_BC_p_value, 3)
  )
  Inter_global_BC_summary_df
  
  
  # ## Both
  # 
  # # Merge observed and null data to compute summary stats
  # Both_global_BC_obs
  # Both_global_BC_null
  # Both_global_BC_dist <- c(Both_global_BC_obs, Both_global_BC_null)
  # 
  # Both_global_BC_Q50 <- median(Both_global_BC_dist) ; Both_global_BC_Q50
  # Both_global_BC_Q5 <- quantile(Both_global_BC_dist, 0.05) ; Both_global_BC_Q5
  # Both_global_BC_p_value <- round(ecdf(x = Both_global_BC_dist)(Both_global_BC_obs), 3) ; Both_global_BC_p_value
  # 
  # Both_global_BC_summary_df <- data.frame(Tribe = "Inter-tribe",
  #                                          N_pairs = Both_global_N_pairs,
  #                                          BC_obs = round(Both_global_BC_obs, 3),
  #                                          BC_Q50 = round(Both_global_BC_Q50, 3),
  #                                          BC_Q5 = round(Both_global_BC_Q5, 3),
  #                                          p_value = round(Both_global_BC_p_value, 3)
  # )
  # Both_global_BC_summary_df
  
  
  ## Merge all groups
  
  global_tests_BC_summary_df <- rbind(Helico_global_BC_summary_df, Itho_global_BC_summary_df, Inter_global_BC_summary_df)
  row.names(global_tests_BC_summary_df) <- NULL
  global_tests_BC_summary_df
  
  saveRDS(global_tests_BC_summary_df, file = paste0("./outputs/Community_Structure/global_tests_BC_summary_df_",m,".rds"))
  
  
  ##### 3/ Plot global test histograms ####
  
  ### 3.1/ Function to plot histograms ####
  
  plot_histogram_perm_test <- function(BC_obs, BC_null,
                                       N_pairs,
                                       title, cex_title = 1.3,
                                       cex_axis = 1.5, cex_lab = 1.6, cex_legend = 1.4,
                                       breaks = 30,
                                       arrow_btm = 5, arrow_top = 80, arrow_adjust = 0,
                                       inset_Q = c(0.02, 0.10),
                                       inset_obs = c(0.02, 0.30),
                                       panel_letter = "", cex_panel_letter = 2.0)
  {
    
    full_dist <- c(BC_obs, BC_null)
    
    # Extract Q stats and p-value
    BC_Q50 <- median(full_dist)
    BC_Q5 <- quantile(full_dist, 0.05)
    BC_p_value <- round(ecdf(x = full_dist)(BC_obs), 3)
    
    hist(full_dist,
         breaks = breaks, freq = TRUE, col = "gray", 
         main = title, 
         xlab = expression(paste("Mean Bray-Curtis dissimilarities", sep = "")),
         cex.axis = cex_axis, cex.lab = cex_lab, cex.main = cex_title, lwd = 2)
    arrows(x0 = BC_obs + arrow_adjust, y0 = arrow_top, x1 = BC_obs + arrow_adjust, y1 = arrow_btm, length = 0.1, lwd = 3)
    abline(v = BC_Q50, lty = 2, lwd = 2)
    abline(v = BC_Q5, lty = 2, lwd = 2, col = "red")
    
    # Insert quantiles legend
    legend(legend = c(paste0("Median = ", format(round(BC_Q50, 3), nsmall = 3)), 
                      paste0("Q5% = ", format(round(BC_Q5, 3), nsmall = 3))), 
           x = "topleft", inset = inset_Q, y.intersp = 1.2, lty = 2 , lwd = 2, col = c("black", "red"), cex = cex_legend, bty ="n")
    
    # Insert legend
    legend(legend = c(paste0("BC obs = ", format(round(BC_obs, 3), nsmall = 3)),
                      paste0("N pairs = ", N_pairs),
                      paste0("p ≤ ", format(round(BC_p_value, 3), nsmall = 3))), 
           x = "topleft", inset = inset_obs, y.intersp = 1.1, cex = cex_legend, bty ="n")
    
    
    # Add panel legend
    legend(legend = panel_letter, text.font = 2,
           x = "topright", inset = c(0.00, 0.00), xjust = 0.5,
           cex = cex_panel_letter, bty ="n")
    
  }
  
  
  ### 3.2/ Heliconiini ####
  
  Helico_global_BC_obs
  Helico_global_BC_null
  
  pdf(file = paste0("./outputs/Community_Structure/Helico_global_BC_histo_",m,".pdf"), width = 8, height = 6)
  initial_margins <- par()$mar 
  par(mar = c(5, 5, 4, 2)) # bltr
  plot_histogram_perm_test(BC_obs = Helico_global_BC_obs,
                           BC_null = Helico_global_BC_null,
                           N_pairs = Helico_global_N_pairs,
                           title = "Heliconiini",
                           cex_title = 2.0,
                           inset_Q = c(0.02, 0.00),
                           inset_obs = c(-0.02, 0.25),
                           arrow_top = 80,
                           arrow_adjust = 0.0015)
  par(mar = initial_margins)
  dev.off()
  
  
  ### 3.3/ Ithomiini ####
  
  if (m == "ss")
  {
    Itho_global_BC_obs
    Itho_global_BC_null_999 <- Itho_global_BC_null[1:999]
    
    pdf(file = paste0("./outputs/Community_Structure/Itho_global_BC_histo_",m,".pdf"), width = 8, height = 6)
    initial_margins <- par()$mar 
    par(mar = c(5, 5, 4, 2)) # bltr
    plot_histogram_perm_test(BC_obs = Itho_global_BC_obs,
                             BC_null = Itho_global_BC_null_999,
                             N_pairs = Itho_global_N_pairs,
                             title = "Ithomiini",
                             cex_title = 2.0,
                             inset_Q = c(0.02, 0.00),
                             inset_obs = c(-0.02, 0.25),
                             arrow_btm = 10,
                             arrow_top = 165,
                             arrow_adjust = 0.0007)
    par(mar = initial_margins)
    dev.off()
  }
  
  ### 3.4/ Inter-tribe ####
  
  Inter_global_BC_obs
  Inter_global_BC_null
  
  pdf(file = paste0("./outputs/Community_Structure/Inter_global_BC_histo_",m,".pdf"), width = 8, height = 6)
  initial_margins <- par()$mar 
  par(mar = c(5, 5, 4, 2)) # bltr
  plot_histogram_perm_test(BC_obs = Inter_global_BC_obs,
                           BC_null = Inter_global_BC_null,
                           N_pairs = Inter_global_N_pairs,
                           title = "Inter-tribe",
                           cex_title = 2.0,
                           arrow_top = 110,
                           inset_Q = c(0.02, 0.00),
                           inset_obs = c(-0.02, 0.25),
                           arrow_adjust = 0.001)
  par(mar = initial_margins)
  dev.off()
  
  
  ##### 4/ Extract summary tables per mimetic rings ####
  
  # # Load previous summary table
  # load(file = paste0( "./outputs/Community_Structure/BC_ring_summary_table_HeIt.Rdata"))
  # BC_ring_summary_table
  
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
    
    signifs[x < 0.05] <- "*"
    signifs[x < 0.01] <- "**"
    signifs[x <= 0.001] <- "***"
    
    signifs[is.na(x)] <- NA
    
    return(signifs)
  }
  
  ### 4.1/ Heliconiini ####
  
  # Load summary table
  Previous_summary_table <- read.xlsx(xlsxFile = paste0("./tables/BC_ring_summary_table_OMU_",m,".xlsx"))
  names(Previous_summary_table)[2] <- "Phenotypic_group"
  
  Helico_group_BC_obs
  Helico_group_BC_null
  
  Helico_group_BC_dist <- rbind(Helico_group_BC_obs, Helico_group_BC_null) ; 
  
  Helico_group_BC_Q50 <- apply(X = Helico_group_BC_dist, MARGIN = 2, FUN = median, na.rm = T) ; Helico_group_BC_Q50
  Helico_group_BC_Q5 <- apply(X = Helico_group_BC_dist, MARGIN = 2, FUN = quantile, probs =  0.05, na.rm = T) ; Helico_group_BC_Q5
  Helico_group_BC_p_value <- apply(X = Helico_group_BC_dist, MARGIN = 2, FUN = compute_p_value) ; Helico_group_BC_p_value
  Helico_group_BC_signif <- assign_signif_lvl(Helico_group_BC_p_value) ; Helico_group_BC_signif
  
  # # Get nb of units and pairs from previous summary table
  # Previous_summary_table <- Helico_group_BC_summary_table[BC_ring_summary_table$tribe == "HELICONIINI", ]
  # Previous_summary_table$MIMETIC_GROUP <- str_remove(string = Previous_summary_table$ring, pattern = ".HELICO")
  
  Helico_group_N_units <- as.numeric(Previous_summary_table$N_units)
  Helico_group_N_units <- Helico_group_N_units[match(x = names(Helico_group_BC_obs), table = Previous_summary_table$Phenotypic_group)]
  Helico_group_N_pairs <- as.numeric(Previous_summary_table$N_pairs)
  Helico_group_N_pairs <- Helico_group_N_pairs[match(x = names(Helico_group_BC_obs), table = Previous_summary_table$Phenotypic_group)]
  
  # Bind everything
  Helico_group_BC_summary_df <- cbind(as.numeric(Helico_group_N_units), as.numeric(Helico_group_N_pairs), as.numeric(round(Helico_group_BC_obs, 3)), as.numeric(round(Helico_group_BC_Q50, 3)), as.numeric(round(Helico_group_BC_Q5, 3)), as.numeric(round(Helico_group_BC_p_value, 3)), Helico_group_BC_signif)
  Phenotypic_group <- names(Helico_group_BC_obs)
  Helico_group_BC_summary_df <- as.data.frame(cbind(Phenotypic_group, Helico_group_BC_summary_df))
  names(Helico_group_BC_summary_df) <- c("Phenotypic_group", "N_units", "N_pairs", "BC_obs", "BC_Q50", "BC_Q5", "p_value", "signif_lvl")
  
  # Add tribe
  Helico_group_BC_summary_df$Tribe <- "Heliconiini"
  Helico_group_BC_summary_df <- Helico_group_BC_summary_df %>% 
    dplyr::select(Tribe, Phenotypic_group, N_units, N_pairs, BC_obs, BC_Q50, BC_Q5, p_value, signif_lvl)
  Helico_group_BC_summary_df
  
  # # Update mimetic group names
  # Old_names <- str_replace(string = Mimetic_classification_update$`Initial name`, pattern = " ", replacement = "\\.")
  # Old_names <- str_replace(string = Old_names, pattern = "-", replacement = "\\.")
  # Mimetic_classification_update$Old_names <- Old_names
  # 
  # Helico_group_BC_summary_df <- left_join(x = Helico_group_BC_summary_df,
  #                                         y = Mimetic_classification_update[, c("Old_names", "New name")],
  #                                         by = c("MIMETIC_GROUP" = "Old_names"))
  # 
  # Helico_group_BC_summary_df <- Helico_group_BC_summary_df %>% 
  #   mutate(MIMETIC_GROUP = `New name`) %>%
  #   dplyr::select(Tribe, MIMETIC_GROUP, N_units, N_pairs, BC_obs, BC_Q50, BC_Q5, p_value, signif_lvl) %>% 
  #   arrange(MIMETIC_GROUP)
  
  Helico_group_BC_summary_df <- Helico_group_BC_summary_df %>% 
    arrange(Phenotypic_group)
  
  saveRDS(Helico_group_BC_summary_df, file = paste0("./outputs/Community_Structure/Helico_group_BC_summary_df_",m,".rds"))
  
  
  ### 4.2/ Ithomiini ####
  
  if (m == "ss")
  {
    Itho_group_BC_obs
    Itho_group_BC_null
    
    Itho_group_BC_dist <- rbind(Itho_group_BC_obs, Itho_group_BC_null)
    Itho_group_BC_dist <- Itho_group_BC_dist[-1001, ]
    
    Itho_group_BC_Q50 <- apply(X = Itho_group_BC_dist, MARGIN = 2, FUN = median) ; Itho_group_BC_Q50
    Itho_group_BC_Q5 <- apply(X = Itho_group_BC_dist, MARGIN = 2, FUN = quantile, probs =  0.05, na.rm = T) ; Itho_group_BC_Q5
    Itho_group_BC_p_value <- apply(X = Itho_group_BC_dist, MARGIN = 2, FUN = compute_p_value) ; Itho_group_BC_p_value
    Itho_group_BC_signif <- assign_signif_lvl(Itho_group_BC_p_value) ; Itho_group_BC_signif
    
    # Get nb of units and pairs from previous summary table
    load(file = paste0("./Ithomiini/BC_ring_summary_table.Rdata"))
    Previous_summary_table <- BC_ring_summary_table
    
    Itho_group_N_units <- Previous_summary_table$N_units
    Itho_group_N_units <- Itho_group_N_units[match(x = Previous_summary_table$ring, table = names(Itho_group_BC_obs))]
    Itho_group_N_pairs <- Previous_summary_table$N_pairs
    Itho_group_N_pairs <- Itho_group_N_pairs[match(x = Previous_summary_table$ring, table = names(Itho_group_BC_obs))]
    
    # Bind everything
    Itho_group_BC_summary_df <- cbind(as.numeric(Itho_group_N_units), as.numeric(Itho_group_N_pairs), as.numeric(round(Itho_group_BC_obs, 3)), as.numeric(round(Itho_group_BC_Q50, 3)), as.numeric(round(Itho_group_BC_Q5, 3)), as.numeric(round(Itho_group_BC_p_value, 3)), Itho_group_BC_signif)
    Phenotypic_group <- names(Itho_group_BC_obs)
    Itho_group_BC_summary_df <- as.data.frame(cbind(Phenotypic_group, Itho_group_BC_summary_df))
    names(Itho_group_BC_summary_df) <- c("Phenotypic_group", "N_units", "N_pairs", "BC_obs", "BC_Q50", "BC_Q5", "p_value", "signif_lvl")
    
    # Add tribe
    Itho_group_BC_summary_df$Tribe <- "Ithomiini"
    Itho_group_BC_summary_df <- Itho_group_BC_summary_df %>% 
      dplyr::select(Tribe, Phenotypic_group, N_units, N_pairs, BC_obs, BC_Q50, BC_Q5, p_value, signif_lvl) %>% 
      arrange(Phenotypic_group)
    
    Itho_group_BC_summary_df
    
    saveRDS(Itho_group_BC_summary_df, file = paste0("./outputs/Community_Structure/Itho_group_BC_summary_df_",m,".rds"))
  }
  
  ### 4.3/ Inter-tribe ####
  
  Inter_group_BC_obs
  Inter_group_BC_null
  
  Inter_group_BC_dist <- rbind(Inter_group_BC_obs, Inter_group_BC_null) ; 
  
  Inter_group_BC_Q50 <- apply(X = Inter_group_BC_dist, MARGIN = 2, FUN = median) ; Inter_group_BC_Q50
  Inter_group_BC_Q5 <- apply(X = Inter_group_BC_dist, MARGIN = 2, FUN = quantile, probs =  0.05, na.rm = T) ; Inter_group_BC_Q5
  Inter_group_BC_p_value <- apply(X = Inter_group_BC_dist, MARGIN = 2, FUN = compute_p_value) ; Inter_group_BC_p_value
  Inter_group_BC_signif <- assign_signif_lvl(Inter_group_BC_p_value) ; Inter_group_BC_signif
  
  # Get nb of pairs from above computations
  Inter_group_N_pairs <- Inter_group_N_pairs_list
  Inter_group_N_pairs <- Inter_group_N_pairs[match(x = names(Inter_group_BC_obs), table = names(Inter_group_N_pairs_list))]
  
  # Get number of units from list of OMUs
  COMIM_rings <- readRDS(file = paste0("./Ithomiini/COMIM_rings_",m,".rds"))
  list_OMU_HeIt <- readRDS(file = paste0("./input_data/Species_data/list_OMU_HeIt_",m,".RDS"))
  
  Inter_group_N_units <- NA
  for (i in seq_along(COMIM_rings$Ring))
  {
    Ring_HI <- COMIM_rings$Ring[i]
    Inter_group_N_units[i] <- sum(list_OMU_HeIt$Ring == Ring_HI)
  }
  names(Inter_group_N_units) <- COMIM_rings$Ring
    
  Inter_group_N_units <- Inter_group_N_units[match(x = str_remove(names(Inter_group_BC_obs), pattern = "\\.INTER"), table = names(Inter_group_N_units))]
   
  # Bind everything
  Inter_group_BC_summary_df <- cbind(as.numeric(Inter_group_N_units), as.numeric(Inter_group_N_pairs), as.numeric(round(Inter_group_BC_obs, 3)), as.numeric(round(Inter_group_BC_Q50, 3)), as.numeric(round(Inter_group_BC_Q5, 3)), as.numeric(round(Inter_group_BC_p_value, 3)), Inter_group_BC_signif)
  Phenotypic_group <- str_remove(names(Inter_group_BC_obs), pattern = "\\.INTER")
  Inter_group_BC_summary_df <- as.data.frame(cbind(Phenotypic_group, Inter_group_BC_summary_df))
  names(Inter_group_BC_summary_df) <- c("Phenotypic_group", "N_units", "N_pairs", "BC_obs", "BC_Q50", "BC_Q5", "p_value", "signif_lvl")
  
  # Add tribe
  Inter_group_BC_summary_df$Tribe <- "Inter-tribe"
  Inter_group_BC_summary_df <- Inter_group_BC_summary_df %>% 
    dplyr::select(Tribe, Phenotypic_group, N_units, N_pairs, BC_obs, BC_Q50, BC_Q5, p_value, signif_lvl) %>% 
    arrange(Phenotypic_group)
  
  Inter_group_BC_summary_df
  
  saveRDS(Inter_group_BC_summary_df, file = paste0("./outputs/Community_Structure/Inter_group_BC_summary_df_",m,".rds"))
  
  
  ### 4.4/ Bind all tribes ####
  
  Helico_group_BC_summary_df <- readRDS(file = paste0("./outputs/Community_Structure/Helico_group_BC_summary_df_",m,".rds"))
  Inter_group_BC_summary_df <- readRDS(file = paste0("./outputs/Community_Structure/Inter_group_BC_summary_df_",m,".rds"))
  
  if (m == "ss")
  {
    Itho_group_BC_summary_df <- readRDS(file = paste0("./outputs/Community_Structure/Itho_group_BC_summary_df_",m,".rds"))
    all_groups_BC_summary_df <- rbind(Helico_group_BC_summary_df, Inter_group_BC_summary_df, Itho_group_BC_summary_df)
  } else {
    all_groups_BC_summary_df <- rbind(Helico_group_BC_summary_df, Inter_group_BC_summary_df)
  }
  
  saveRDS(all_groups_BC_summary_df, file = paste0("./outputs/Community_Structure/all_groups_BC_summary_df_",m,".rds"))
  write.xlsx(x = all_groups_BC_summary_df, file = paste0("./outputs/Community_Structure/all_groups_BC_summary_df_",m,".xlsx"))
  
}


