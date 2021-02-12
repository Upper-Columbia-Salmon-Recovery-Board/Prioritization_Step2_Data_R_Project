

# ---------------------------------------------------------------------------
#
#      SCRIPT:  Habitat Quality Pathway FILTER
#
#      R Project to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
#   Function to generate Habitat Quality Pathway output
#
# ---------------------------------------------------------------------------

species = "Spring Chinook"
basins = c("Methow",  "Entiat","Wenatchee")

Generate_Habitat_Quality_Output_Table = function(species, basins){
  
  # ------------------------------------------------------------------------------
  #       Establish species-specific variable names
  # ------------------------------------------------------------------------------
  
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
    restoration_output_name = paste(paste("Spring_Chinook_Habitat_Quality_RESTORATOIN" , 
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
    restoration_output_name = paste(paste("Steelhead_Habitat_Quality_RESTORATOIN" , 
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
  restoration_output_name = paste(paste("Bull_Trout_Habitat_Quality_RESTORATOIN" , 
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
  # ---------- AU Ranks data frame for this species ---------
  Species_AU_Ranks_data = AU_Ranks_data
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to only have Species-specific reaches   
  #  ---------------------------------------------------------------------------------
  
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_Reach_Information_data$Species_Reaches = Species_Reach_Information_data[species_reach]
  # ----------------------- filter out for only reaches with this species --------------
  Species_Reach_Information_data = Species_Reach_Information_data %>%  
    filter(Species_Reaches   == 'yes')
  
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
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to select for AU rank - PROTECTION
  #  ---------------------------------------------------------------------------------
  
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_AU_Ranks_data["Species_AU_Ranks"] = Species_AU_Ranks_data[AU_rank_name_protection]
  # ----------------------- filter out for only reaches with this species --------------
  Species_AU_Ranks_data_protection = Species_AU_Ranks_data %>%  
    filter(Species_AU_Ranks    %in%   AU_Rank)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Species_Reach_Information_data_protection = Species_Reach_Information_data %>%  
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data_protection$`Assessment Unit`)
  
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to select for Habitat Quality RESTORATION score
  #  ---------------------------------------------------------------------------------
  
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  Habitat_Quality_Scores_Restoration = Habitat_Quality_Scores %>%  
    filter(HQ_Score_Restoration   >=   SCORE_Criteria_Habitat_Quality_Pathway)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Habitat_Quality_Pathway_Restoration = Habitat_Quality_Scores_Restoration %>%  
    filter(ReachName   %in%   Species_Reach_Information_data_restoration$`ReachName`)
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to select for Habitat Quality PROTECTION score
  #  ---------------------------------------------------------------------------------
  
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score critera --------------
  Habitat_Quality_Scores_Protection = Habitat_Quality_Scores %>%  
    filter(HQ_Score_Protection   >=   SCORE_Criteria_Habitat_Quality_Pathway)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Habitat_Quality_Pathway_Protection = Habitat_Quality_Scores_Protection %>%  
    filter(ReachName   %in%   Species_Reach_Information_data_protection$`ReachName`)
  
  #  ---------------------------------------------------------------------------------
  #           Reach Confinement (NOT - only used for Restoration, not for Protection)
  #  ---------------------------------------------------------------------------------
  
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score critera --------------
  Confinement_Scores_Restoration = Confinement_Scores %>%  
    filter(Score   >=   Reach_Confinement_SCORE_Criteria)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Habitat_Quality_Pathway_Restoration = Habitat_Quality_Pathway_Restoration %>%  
    filter(ReachName   %in%   Confinement_Scores_Restoration$`ReachName`)
  
  #  ---------------------------------------------------------------------------------
  #           Number of Life Stages Filter 
  #  ---------------------------------------------------------------------------------
  
  # -------------------- add additional column for this particular species reach presence ---------------
  Life_Stage_Priorities_AU_and_Reach_data["Life_Stage_Sum_Column"] = Life_Stage_Priorities_AU_and_Reach_data[life_stage_sum_column]
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  Life_Stage_Priorities_AU_and_Reach_data_FILTERED = Life_Stage_Priorities_AU_and_Reach_data %>%  
    filter(Life_Stage_Sum_Column   >=   Sum_Life_Stage_Criteria)
  
  
  # ------------------------ identify reaches that pass through the RESTORATION filter ----------
  Habitat_Quality_Pathway_Restoration = Habitat_Quality_Pathway_Restoration %>%  
    filter(ReachName   %in%   Life_Stage_Priorities_AU_and_Reach_data_FILTERED$`ReachName`)
  # ------------------------ identify reaches that pass through the PROTECTION filter ----------
  Habitat_Quality_Pathway_Protection = Habitat_Quality_Pathway_Protection %>%  
    filter(ReachName   %in%   Life_Stage_Priorities_AU_and_Reach_data_FILTERED$`ReachName`)
  
  # -------------------------------------------------------
  #     Output Protection Scores (if reaches generated)
  # ------------------------------------------------------- 
  
  if(nrow(Habitat_Quality_Pathway_Restoration) > 0){
    
    # -------------------------------------------------------
    #         identify all individual habitat attribute scores at 3 (or lower) and 1
    # -------------------------------------------------------
    # NOTE: these two columns are added at the very end 
    
    # -------------------- Restoration -----------
    colnames_x = habitat_quality_scores_colnames_for_sum
    indiv_habitat_attributes_impaired = apply(as.matrix(Habitat_Quality_Pathway_Restoration[,habitat_quality_scores_colnames_for_sum]), 
                                              MARGIN = 1, list_indiv_habitat_attributes_low_FUNCTION)
    indiv_habitat_attributes_impaired_restoration = t(as.data.frame(indiv_habitat_attributes_impaired))
    colnames(indiv_habitat_attributes_impaired_restoration) = c("unacceptable_1_indiv_habitat_attributes", "unacceptable_and_at_risk_1_3_indiv_habitat_attributes")
    Habitat_Quality_Pathway_Restoration = cbind(Habitat_Quality_Pathway_Restoration,indiv_habitat_attributes_impaired_restoration )
    #---------------- remove "_score" from habitat attribute names ------
    Habitat_Quality_Pathway_Restoration[,'unacceptable_1_indiv_habitat_attributes'] = gsub('_score','',Habitat_Quality_Pathway_Restoration[,'unacceptable_1_indiv_habitat_attributes'])
    Habitat_Quality_Pathway_Restoration[,'unacceptable_and_at_risk_1_3_indiv_habitat_attributes'] = gsub('_score','',Habitat_Quality_Pathway_Restoration[,'unacceptable_and_at_risk_1_3_indiv_habitat_attributes'])
    #---------------- remove "_Mean" from habitat attribute names ------
    Habitat_Quality_Pathway_Restoration[,'unacceptable_1_indiv_habitat_attributes'] = gsub('_Mean','',Habitat_Quality_Pathway_Restoration[,'unacceptable_1_indiv_habitat_attributes'])
    Habitat_Quality_Pathway_Restoration[,'unacceptable_and_at_risk_1_3_indiv_habitat_attributes'] = gsub('_Mean','',Habitat_Quality_Pathway_Restoration[,'unacceptable_and_at_risk_1_3_indiv_habitat_attributes'])
    #---------------- remove ",NA" from habitat attribute names ------
    Habitat_Quality_Pathway_Restoration[,'unacceptable_1_indiv_habitat_attributes'] = gsub(',NA','',Habitat_Quality_Pathway_Restoration[,'unacceptable_1_indiv_habitat_attributes'])
    Habitat_Quality_Pathway_Restoration[,'unacceptable_and_at_risk_1_3_indiv_habitat_attributes'] = gsub(',NA','',Habitat_Quality_Pathway_Restoration[,'unacceptable_and_at_risk_1_3_indiv_habitat_attributes'])
    
    #  ---------------------------------------------------------------------------------
    #           Write output data to output file
    #  ---------------------------------------------------------------------------------
    
    # ----------------------- Restoration --------------------
    output_path_x =  paste(output_path,restoration_output_name, sep="")
    write_xlsx(Habitat_Quality_Pathway_Restoration,output_path_x )
    
    
    
  }else{
    
    print(paste("No Restoration Reaches generated for species: ", species, sep=""))
    
  }
  
  
  # -------------------------------------------------------
  #     Output Protection Scores (if reaches generated)
  # ------------------------------------------------------- 
  
  if(nrow(Habitat_Quality_Pathway_Protection) > 0){
    
    # -------------------------------------------------------
    #         identify all individual habitat attribute scores at 3 (or lower) and 1
    # -------------------------------------------------------
    # NOTE: these two columns are added at the very end 

    # -------------------- Protection -----------
    colnames_x = habitat_quality_scores_colnames_for_sum
    indiv_habitat_attributes_impaired = apply(as.matrix(Habitat_Quality_Pathway_Protection[,habitat_quality_scores_colnames_for_sum]), MARGIN = 1, list_indiv_habitat_attributes_low_FUNCTION)
    indiv_habitat_attributes_impaired_protection = t(as.data.frame(indiv_habitat_attributes_impaired))
    colnames(indiv_habitat_attributes_impaired_protection) = c("unacceptable_1_indiv_habitat_attributes", "unacceptable_and_at_risk_1_3_indiv_habitat_attributes")
    Habitat_Quality_Pathway_Protection = cbind(Habitat_Quality_Pathway_Protection,indiv_habitat_attributes_impaired_protection )
    #---------------- remove "_score" from habitat attribute names ------
    Habitat_Quality_Pathway_Protection[,'unacceptable_1_indiv_habitat_attributes'] = gsub('_score','',Habitat_Quality_Pathway_Protection[,'unacceptable_1_indiv_habitat_attributes'])
    Habitat_Quality_Pathway_Protection[,'unacceptable_and_at_risk_1_3_indiv_habitat_attributes'] = gsub('_score','',Habitat_Quality_Pathway_Protection[,'unacceptable_and_at_risk_1_3_indiv_habitat_attributes'])
    
    
    #  ---------------------------------------------------------------------------------
    #           Write output data to output file
    #  ---------------------------------------------------------------------------------

    # ----------------------- Protection --------------------
    output_path_x =  paste(output_path,protection_output_name, sep="")
    write_xlsx(Habitat_Quality_Pathway_Protection,output_path_x )
    
    
    
    
  }else{
    
    print(paste("No Protection Reaches generated for species: ", species, sep=""))
    
  }



  
  # --------------------- Put Restoration and Protection into a list --------------
  Habitat_Quality_Pathway_Output = list( 
    "Habitat_Quality_Pathway_Restoration" = Habitat_Quality_Pathway_Restoration,
    "Habitat_Quality_Pathway_Protection" = Habitat_Quality_Pathway_Protection
  )
  
  return(Habitat_Quality_Pathway_Output)
  
}


# ------------------ Function to list all the rows below invididual habitat criteria -------------------------

list_indiv_habitat_attributes_low_FUNCTION <- function(row){
  
  # ---------------- Individual Habitat Attribute Score: 1  --------
  columns_impaired_x = which(row <= 1)
  columns_impaired_x = colnames_x[columns_impaired_x]
  habitat_attributes_impaired_1 = paste(columns_impaired_x, collapse=',' )
  
  # ---------------- Individual Habitat Attribute Score: 3 or lower  --------
  columns_impaired_x = which(row <= 3)
  columns_impaired_x = colnames_x[columns_impaired_x]
  habitat_attributes_impaired_3_or_lower = paste(columns_impaired_x, collapse=',' )
  
  
  # ------- output -----------
  return(c(habitat_attributes_impaired_1, habitat_attributes_impaired_3_or_lower  ))
  
}


# ------------------ Function to list all the rows below individual habitat criteria -------------------------

list_indiv_habitat_attributes_low_FUNCTION <- function(row){
  
  # ---------------- Individual Habitat Attribute Score: 1  --------
  columns_impaired_x = which(row <= 1)
  columns_impaired_x = colnames_x[columns_impaired_x]
  habitat_attributes_impaired_1 = paste(columns_impaired_x, collapse=',' )
  
  # ---------------- Individual Habitat Attribute Score: 3 or lower  --------
  columns_impaired_x = which(row <= 3)
  columns_impaired_x = colnames_x[columns_impaired_x]
  habitat_attributes_impaired_3_or_lower = paste(columns_impaired_x, collapse=',' )
  
  
  # ------- output -----------
  return(c(habitat_attributes_impaired_1, habitat_attributes_impaired_3_or_lower  ))
  
}
  