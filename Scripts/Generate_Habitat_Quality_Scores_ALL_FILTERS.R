
# ------------------------------------------------------------------------------------------------------------------
#
#         Add Filters to the Habitat Quality
# 
# ------------------------------------------------------------------------------------------------------------------

test_x = TRUE
if(test_x){
  species="Spring Chinook"
  basins = c("Methow"  ,  "Entiat"  ,  "Wenatchee", "Okanogan" )
  
}

Generate_Habitat_Quality_Output_Table_WITH_FILTERS = function(species, basins, habitat_quality_scores_colnames_output , write_to_xls_x){
  
  # ------------------------------------------------------------------------------
  #       Establish species-specific variable names
  # ------------------------------------------------------------------------------
  print(paste("---------------------------------- START HQ Pathway FOR: ",species, sep=""))
  if(species == "Spring Chinook"){
    # ---------------- species reach ---------------
    species_reach = 'Spring.Chinook.Reach'
    # ---------------- species AU Rank RESTORATION ----------
    AU_rank_name_restoration = 'SPCHNTier_Restoration'
    # ---------------- species AU Rank PROTECTION ----------
    AU_rank_name_protection = 'SPCHNTier_Protection'
    # --------------- Life stage Sum column name ----------
    life_stage_sum_column = 'SPCH_Life_Stage_Sum'
    # ------------------- output names ----------------------
    restoration_output_name = paste(paste("Spring_Chinook_Habitat_Quality_RESTORATION" , 
                                          paste(basins_to_include, collapse = "_"), sep="_"),
                                    ".xlsx", sep="")
    protection_output_name = paste(paste("Spring_Chinook_Habitat_Quality_PROTECTION" , 
                                         paste(basins_to_include, collapse = "_"), sep="_"),
                                   ".xlsx", sep="")
    
    
  }else if(species == "Steelhead"){
    # ---------------- species reach ---------------
    species_reach = 'Steelhead.Reach'
    # ---------------- species AU Rank ----------
    AU_rank_name_restoration = 'STLTier_Restoration'
    # ---------------- species AU Rank PROTECTION ----------
    AU_rank_name_protection = 'STLTier_Protection'
    # --------------- Life stage Sum column name ----------
    life_stage_sum_column = 'SH_Life_Stage_Sum'
    # ------------------- output names ----------------------
    restoration_output_name = paste(paste("Steelhead_Habitat_Quality_RESTORATION" , 
                                          paste(basins_to_include, collapse = "_"), sep="_"),
                                    ".xlsx", sep="")
    protection_output_name = paste(paste("Steelhead_Habitat_Quality_PROTECTION" , 
                                         paste(basins_to_include, collapse = "_"), sep="_"),
                                   ".xlsx", sep="")
    
  }else if(species == "Bull Trout"){
    # ---------------- species reach ---------------
    species_reach = 'Bull.Trout.Reach'
    # ---------------- species AU Rank ----------
    AU_rank_name_restoration = 'BTTier_Restoration'
    # ---------------- species AU Rank PROTECTION ----------
    AU_rank_name_protection = 'BTTier_Protection'
    # --------------- Life stage Sum column name ----------
    life_stage_sum_column = 'BT_Life_Stage_Sum'
    # ------------------- output names ----------------------
    restoration_output_name = paste(paste("Bull_Trout_Habitat_Quality_RESTORATION" , 
                                          paste(basins_to_include, collapse = "_"), sep="_"),
                                    ".xlsx", sep="")
    protection_output_name = paste(paste("Bull_Trout_Habitat_Quality_PROTECTION" , 
                                         paste(basins_to_include, collapse = "_"), sep="_"),
                                   ".xlsx", sep="")
    
    
  }else{
    print('Incorrectly entered species name - re-type species name')
    
  }
  
  #  ---------------------------------------------------------------------------------
  #           Establish Reach Information Data Frame Just for this Output
  #  ---------------------------------------------------------------------------------
  # -------- Reach Information data frame for this species -----------
  Species_Reach_Information_data = Reach_Information_data
  # --------------- combined all data -------------
  Output_All_Combined  = Species_Reach_Information_data[,c("ReachName","Basin","Assessment.Unit")]
  
  # ---------- AU Ranks data frame for this species ---------
  Species_AU_Ranks_data = AU_Ranks_data
  
  print(paste("Total Initial Reaches (HQ Pathway): ", nrow(Species_Reach_Information_data), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to only have Species-specific reaches   
  #  ---------------------------------------------------------------------------------
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_Reach_Information_data$Species_Reaches = Species_Reach_Information_data[species_reach]
  Species_Reach_Information_data_all = Species_Reach_Information_data
  # ----------------------- filter out for only reaches with this species --------------
  Species_Reach_Information_data = Species_Reach_Information_data %>%  
    filter(Species_Reaches   == 'yes')
  print(paste("Total reaches after species-reach filter: ", nrow(Species_Reach_Information_data), sep=""))
  
  # ----------- add to combined ----------
  Species_Reach_Information_data_merge = Species_Reach_Information_data_all[,c("ReachName", species_reach)]
  Output_All_Combined = merge(Output_All_Combined, Species_Reach_Information_data_merge, by="ReachName", all.x=TRUE)
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to only have Basins desired for this analysis
  #  ---------------------------------------------------------------------------------
  
  if(length(basins)==1){
    Species_Reach_Information_data = Species_Reach_Information_data %>% 
      filter(Basin==basins[1])
  }else if(length(basins)==2){
    Species_Reach_Information_data = Species_Reach_Information_data %>% 
      filter((Basin==basins[1]) | (Basin==basins[2]) )
  }else if(length(basins)==3){
    Species_Reach_Information_data = Species_Reach_Information_data %>% 
      filter((Basin==basins[1]) | (Basin==basins[2]) | (Basin==basins[3]) )
  }else if(length(basins)==4){
    Species_Reach_Information_data = Species_Reach_Information_data %>% 
      filter((Basin==basins[1]) | (Basin==basins[2]) | (Basin==basins[3])  | (Basin==basins[4]) )
  }
  
  print(paste("Total reaches after only having basins of interest: ", nrow(Species_Reach_Information_data), sep=""))
  
  
  #  ---------------------------------------------------------------------------------
  #            Add Confinement to Unfiltered Output (NOTE - only used for Restoration, not for Protection)
  #  ---------------------------------------------------------------------------------
  
  # ----------- add to combined ----------
  Confinement_Scores_for_combo = Confinement_Scores[,c("ReachName","Score")]
  colnames(Confinement_Scores_for_combo) = c("ReachName","Confinement_Score")
  Output_All_Combined = merge(Output_All_Combined, Confinement_Scores_for_combo, by="ReachName", all.x=TRUE)
  
  #  ---------------------------------------------------------------------------------
  #            Add Life Stage Sum to Unfiltered Output
  #  ---------------------------------------------------------------------------------
  
  # ----------- add to combined ----------
  Life_Stage_for_combo = Life_Stage_Priorities_AU_and_Reach_data[,c("ReachName",life_stage_sum_column)]
  Output_All_Combined = merge(Output_All_Combined, Life_Stage_for_combo, by="ReachName", all.x=TRUE)
  
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to select for AU rank - RESTORATION
  #  ---------------------------------------------------------------------------------
  
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_AU_Ranks_data["Species_AU_Ranks"] = Species_AU_Ranks_data[AU_rank_name_restoration]
  # ----------------------- filter out for only reaches with this species --------------
  Species_AU_Ranks_data_restoration = Species_AU_Ranks_data %>%  
    filter(Species_AU_Ranks    %in%   AU_Rank)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Species_Reach_Information_data_restoration = Species_Reach_Information_data %>%  
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data_restoration$`Assessment Unit`)
  
  print(paste("Restoration - total AU rank filter: ", nrow(Species_Reach_Information_data_restoration), sep=""))
  
  # ----------- add to combined ----------
  
  if(species == "Steelhead"){
    
    tiers_AUs = AU_Ranks_data[ , c("Assessment Unit","Subbasin", "SPCHNTier_Restoration", "STLTier_Restoration","BTTier_Restoration")]
    AU_Ranks_Okanogan_to_merge = as.data.frame(AU_Ranks_Okanogan$`EDT AU`)
    colnames(AU_Ranks_Okanogan_to_merge) = "Assessment Unit"
    AU_Ranks_Okanogan_to_merge$Subbasin = "Okanogan"
    AU_Ranks_Okanogan_to_merge$SPCHNTier_Restoration = NA
    AU_Ranks_Okanogan_to_merge$STLTier_Restoration = AU_Ranks_Okanogan$`AU Restoration Rank`
    AU_Ranks_Okanogan_to_merge$BTTier_Restoration = NA
    tiers_AUs = rbind(tiers_AUs, AU_Ranks_Okanogan_to_merge)
    
  # ------------- for Spring Chinook of Bull Trout ----------
  }else{
    
    tiers_AUs = AU_Ranks_data[ , c("Assessment Unit","Subbasin", "SPCHNTier_Restoration", "STLTier_Restoration","BTTier_Restoration")]
    
  }

  
  tiers_AUs = tiers_AUs[ ,c("Assessment Unit", AU_rank_name_restoration)]
  colnames(tiers_AUs) = c("Assessment.Unit",AU_rank_name_restoration)
  Output_All_Combined = merge(Output_All_Combined, tiers_AUs, by="Assessment.Unit", all.x=TRUE)
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to select for AU rank - PROTECTION
  #  ---------------------------------------------------------------------------------
  
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_AU_Ranks_data["Species_AU_Ranks"] = Species_AU_Ranks_data[AU_rank_name_protection]
  # ----------------------- filter out for only reaches with this species --------------
  Species_AU_Ranks_data_protection = Species_AU_Ranks_data %>%  
    filter(Species_AU_Ranks    %in%   AU_Rank)
  # ------------------------ identify after AUs that pass this filter in reach-based table ----------
  Species_Reach_Information_data_protection = Species_Reach_Information_data %>%  
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data_protection$`Assessment Unit`)
  
  print(paste("Protection - total after AU rank filter: ", nrow(Species_Reach_Information_data_protection), sep=""))
  
  # ----------- add to combined ----------
  if(species == "Steelhead"){
    tiers_AUs = AU_Ranks_data[, c("Assessment Unit","Subbasin","SPCHNTier_Protection","STLTier_Protection", "BTTier_Protection")]
    AU_Ranks_Okanogan_to_merge = as.data.frame(AU_Ranks_Okanogan$`EDT AU`)
    colnames(AU_Ranks_Okanogan_to_merge) = "Assessment Unit"
    AU_Ranks_Okanogan_to_merge$Subbasin = NA
    AU_Ranks_Okanogan_to_merge$SPCHNTier_Protection = NA
    AU_Ranks_Okanogan_to_merge$STLTier_Protection = AU_Ranks_Okanogan$`AU Protection Rank`
    AU_Ranks_Okanogan_to_merge$BTTier_Protection = NA
    tiers_AUs = rbind(tiers_AUs, AU_Ranks_Okanogan_to_merge)
    
  # ----------------- IF Spring Chinook or Bull Trout ----------  
  }else{
    tiers_AUs = AU_Ranks_data[, c("Assessment Unit","Subbasin","SPCHNTier_Protection","STLTier_Protection", "BTTier_Protection")]
  }
  tiers_AUs = tiers_AUs[ ,c("Assessment Unit", AU_rank_name_protection)]
  colnames(tiers_AUs) = c("Assessment.Unit",AU_rank_name_protection)
  Output_All_Combined = merge(Output_All_Combined, tiers_AUs, by="Assessment.Unit", all.x=TRUE)
  
  
  #  ---------------------------------------------------------------------------------
  #           Add % Habitat Quality
  #  ---------------------------------------------------------------------------------
  # NOTE: for Okanogan - HQ_Score is based on % of Template in EDT
  
  # ------------- pull from Habitat_Quality_Scores ------
  Habitat_Quality_Restoration_Protection_Scores = Habitat_Quality_Scores[,c("ReachName","HQ_Pct", "HQ_Score_Restoration" , "HQ_Score_Protection" )]
  
  # ----------- add to combined ----------
  Output_All_Combined = merge(Output_All_Combined, Habitat_Quality_Restoration_Protection_Scores, by="ReachName", all.x=TRUE)
  
  
  #  ---------------------------------------------------------------------------------
  #           Combine Filters with the Habitat_Quality_Scores
  #  ---------------------------------------------------------------------------------
  # ----------- add to combined ----------
  Habitat_Quality_Scores_indiv_attributes = Habitat_Quality_Scores[,c("ReachName",habitat_quality_scores_colnames_output)]
  Output_All_Combined = merge(Output_All_Combined, Habitat_Quality_Scores_indiv_attributes, by="ReachName", all.x=TRUE)
  
  
  return(Output_All_Combined)
}

#  ---------------------------------------------------------------------------------
#
#           Combine Spring Chinook and Steelhead HQ output for all reaches
#
#  ---------------------------------------------------------------------------------


FUNCTION_combine_HQ_ALL_Filters_no_Bull_Trout = function(HQ_Spring_Chinook, HQ_Steelhead, write_to_xls ){
  
  # ------------------ general info --------------
  Habitat_Quality_Scores_ALL_Species = HQ_Spring_Chinook[,c(1:3)]
  
  # ---------------- confinement ------------
  HQ_Spring_Chinook_confinement = HQ_Spring_Chinook[,c(1,5)]
  Habitat_Quality_Scores_ALL_Species = merge(Habitat_Quality_Scores_ALL_Species, HQ_Spring_Chinook_confinement, by="ReachName", all.x=TRUE)
  
  # ---------------- species ------------
  HQ_Spring_Chinook_species_reach = HQ_Spring_Chinook[,c(1,4)]
  Habitat_Quality_Scores_ALL_Species = merge(Habitat_Quality_Scores_ALL_Species, HQ_Spring_Chinook_species_reach, by="ReachName", all.x=TRUE)
  HQ_Steelhead_species_reach = HQ_Steelhead[,c(1,4)]
  Habitat_Quality_Scores_ALL_Species = merge(Habitat_Quality_Scores_ALL_Species, HQ_Steelhead_species_reach, by="ReachName", all.x=TRUE)
  
  # ---------------- Life Stage Sum ------------
  HQ_Spring_Chinook_life_stage = HQ_Spring_Chinook[,c(1,6)]
  Habitat_Quality_Scores_ALL_Species = merge(Habitat_Quality_Scores_ALL_Species, HQ_Spring_Chinook_life_stage, by="ReachName", all.x=TRUE)
  HQ_Steelhead_life_stage = HQ_Steelhead[,c(1,6)]
  Habitat_Quality_Scores_ALL_Species = merge(Habitat_Quality_Scores_ALL_Species, HQ_Steelhead_life_stage, by="ReachName", all.x=TRUE)
  
  # ---------------- Restoration and Protection Tiers (AU-level) ------------
  HQ_Spring_Chinook_AU_tiers = HQ_Spring_Chinook[,c(1,7:8)]
  Habitat_Quality_Scores_ALL_Species = merge(Habitat_Quality_Scores_ALL_Species, HQ_Spring_Chinook_AU_tiers, by="ReachName", all.x=TRUE)
  HQ_Steelhead_AU_tiers = HQ_Steelhead[,c(1,7:8)]
  Habitat_Quality_Scores_ALL_Species = merge(Habitat_Quality_Scores_ALL_Species, HQ_Steelhead_AU_tiers, by="ReachName", all.x=TRUE)
  
  # ---------------- HQ Pct, Restoration and Protection scores AND individual habitat quality scores ------------
  HQ_Scores_x = HQ_Spring_Chinook[,c(1,9:ncol(HQ_Spring_Chinook))]
  Habitat_Quality_Scores_ALL_Species = merge(Habitat_Quality_Scores_ALL_Species, HQ_Scores_x, by="ReachName", all.x=TRUE)
  
  
  # ------------------- export data ------------
  if(write_to_xls_x){
    output_path_x =  paste(output_path,'Habitat_Quality_Scores_ALL_Species_and_Filters.xlsx', sep="")
    write_xlsx(Habitat_Quality_Scores_ALL_Species,output_path_x )
    
  }
  
  
  return(Habitat_Quality_Scores_ALL_Species)
  
  
}




