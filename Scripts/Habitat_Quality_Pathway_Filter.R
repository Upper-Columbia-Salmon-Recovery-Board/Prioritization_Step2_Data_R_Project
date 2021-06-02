

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

#  to test
species = "Bull Trout"
basins = c("Methow",  "Entiat","Wenatchee", "Okanogan")

Generate_Habitat_Quality_Output_Table = function(species, basins, habitat_quality_scores_colnames_for_sum ){
  
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
  # ---------- AU Ranks data frame for this species ---------
  Species_AU_Ranks_data = AU_Ranks_data
  
  print(paste("Total Initial Reaches (HQ Pathway): ", nrow(Species_Reach_Information_data), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to only have Species-specific reaches   
  #  ---------------------------------------------------------------------------------
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_Reach_Information_data$Species_Reaches = Species_Reach_Information_data[species_reach]
  # ----------------------- filter out for only reaches with this species --------------
  Species_Reach_Information_data = Species_Reach_Information_data %>%  
    filter(Species_Reaches   == 'yes')
  print(paste("Total reaches after species-reach filter: ", nrow(Species_Reach_Information_data), sep=""))
  
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

  #  ---------------------------------------------------------------------------------
  #            Filter out to select for Habitat Quality RESTORATION score
  #  ---------------------------------------------------------------------------------
  
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  Habitat_Quality_Scores_Restoration = Habitat_Quality_Scores %>%  
    filter(HQ_Score_Restoration   >=   SCORE_Criteria_Habitat_Quality_Pathway_Restoration)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Habitat_Quality_Pathway_Restoration = Habitat_Quality_Scores_Restoration %>%  
    filter(ReachName   %in%   Species_Reach_Information_data_restoration$`ReachName`)
  
  print(paste("Restoration - total after HQ score filter: ", nrow(Habitat_Quality_Pathway_Restoration), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to select for Habitat Quality PROTECTION score
  #  ---------------------------------------------------------------------------------
  
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  Habitat_Quality_Scores_Protection = Habitat_Quality_Scores %>%  
    filter(HQ_Score_Protection   >=   SCORE_Criteria_Habitat_Quality_Pathway_Protection)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Habitat_Quality_Pathway_Protection = Habitat_Quality_Scores_Protection %>%  
    filter(ReachName   %in%   Species_Reach_Information_data_protection$`ReachName`)
  
  print(paste("Protection - total after HQ score filter: ", nrow(Habitat_Quality_Pathway_Protection), sep=""))
  
  
  #  ---------------------------------------------------------------------------------
  #           Reach Confinement (NOTE - only used for Restoration, not for Protection)
  #  ---------------------------------------------------------------------------------
  
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  Confinement_Scores_Restoration = Confinement_Scores %>%  
    filter(Score   >=   Reach_Confinement_SCORE_Criteria)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Habitat_Quality_Pathway_Restoration = Habitat_Quality_Pathway_Restoration %>%  
    filter(ReachName   %in%   Confinement_Scores_Restoration$`ReachName`)
  
  print(paste("HQ Pathway-RESTORATION - total reaches after reach confinement filter: ", nrow(Habitat_Quality_Pathway_Restoration), sep=""))
  
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
  
  print(paste("HQ Pathway-RESTORATION - total reaches after life stages number filter: ", nrow(Habitat_Quality_Pathway_Restoration), sep=""))
  
  print(paste("HQ Pathway-PROTECTION - total reaches after life stages number filter: ", nrow(Habitat_Quality_Pathway_Protection), sep=""))
  
  # -------------------------------------------------------
  #     Output Scores (if reaches generated)
  # ------------------------------------------------------- 
  
  if( nrow(Habitat_Quality_Pathway_Restoration) > 0 ){
    
    # -------------------------------------------------------
    #         identify all individual habitat attribute scores at 3 (or lower) and 1
    # -------------------------------------------------------
    # NOTE: these two columns are added at the very end 
    
    # -------------------- Restoration -----------
    # --------------- identify habitat attributes at 1 (Unacceptable) OR 3 (At Risk) -----------------------
    #indiv_habitat_attributes_impaired = apply( as.matrix(Habitat_Quality_Pathway_Restoration[,habitat_quality_scores_colnames_for_sum]), 
    #                                          MARGIN = 1, )
    #indiv_habitat_attributes_impaired_restoration = t(as.data.frame(indiv_habitat_attributes_impaired))
    indiv_habitat_attributes_impaired_restoration = c()
    for(rowx in 1:nrow(Habitat_Quality_Pathway_Restoration)){
      three_scores_output = list_indiv_habitat_attributes_low_FUNCTION(Habitat_Quality_Pathway_Restoration[rowx,habitat_quality_scores_colnames_for_sum], habitat_quality_scores_colnames_for_sum)
      indiv_habitat_attributes_impaired_restoration = rbind(indiv_habitat_attributes_impaired_restoration,three_scores_output )
    }
    
    colnames(indiv_habitat_attributes_impaired_restoration) = c("unacceptable_1_indiv_habitat_attributes", "at_risk_2_or_3_indiv_habitat_attributes", "unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes")
    indiv_habitat_attributes_impaired_restoration = as.data.frame(indiv_habitat_attributes_impaired_restoration)
    indiv_habitat_attributes_impaired_restoration$ReachName = Habitat_Quality_Pathway_Restoration$ReachName  # add reach name
    # ----------------- convert blank rows (where criteria not met) to NA -----------------
    indiv_habitat_attributes_impaired_restoration[indiv_habitat_attributes_impaired_restoration==""] <- NA
    # ------------------------- remove any rows that are both NA (no habitat attributes at Unacceptable or At Risk ) --------
    if( any(rowSums(is.na(indiv_habitat_attributes_impaired_restoration))==ncol(indiv_habitat_attributes_impaired_restoration) )  ){
      indiv_habitat_attributes_impaired_restoration = indiv_habitat_attributes_impaired_restoration[-which(rowSums(is.na(indiv_habitat_attributes_impaired_restoration))==ncol(indiv_habitat_attributes_impaired_restoration)), ] }
    # -------------------------------- add habitat attributes at Unacceptable or At Risk (merge so only Reaches still present persist) ----------------
    Habitat_Quality_Pathway_Restoration = merge(Habitat_Quality_Pathway_Restoration, indiv_habitat_attributes_impaired_restoration, by="ReachName")
    #---------------- remove "_score" from habitat attribute names ------
    Habitat_Quality_Pathway_Restoration[,'unacceptable_1_indiv_habitat_attributes'] = gsub('_score','',Habitat_Quality_Pathway_Restoration[,'unacceptable_1_indiv_habitat_attributes'])
    Habitat_Quality_Pathway_Restoration[,'at_risk_2_or_3_indiv_habitat_attributes'] = gsub('_score','',Habitat_Quality_Pathway_Restoration[,'at_risk_2_or_3_indiv_habitat_attributes'])
    Habitat_Quality_Pathway_Restoration[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'] = gsub('_score','',Habitat_Quality_Pathway_Restoration[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'])
    #---------------- remove "_Mean" from habitat attribute names ------
    Habitat_Quality_Pathway_Restoration[,'unacceptable_1_indiv_habitat_attributes'] = gsub('_Mean','',Habitat_Quality_Pathway_Restoration[,'unacceptable_1_indiv_habitat_attributes'])
    Habitat_Quality_Pathway_Restoration[,'at_risk_2_or_3_indiv_habitat_attributes'] = gsub('_Mean','',Habitat_Quality_Pathway_Restoration[,'at_risk_2_or_3_indiv_habitat_attributes'])
    Habitat_Quality_Pathway_Restoration[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'] = gsub('_Mean','',Habitat_Quality_Pathway_Restoration[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'])
    #---------------- remove ",NA" from habitat attribute names ------
    Habitat_Quality_Pathway_Restoration[,'unacceptable_1_indiv_habitat_attributes'] = gsub(',NA','',Habitat_Quality_Pathway_Restoration[,'unacceptable_1_indiv_habitat_attributes'])
    Habitat_Quality_Pathway_Restoration[,'at_risk_2_or_3_indiv_habitat_attributes'] = gsub(',NA','',Habitat_Quality_Pathway_Restoration[,'at_risk_2_or_3_indiv_habitat_attributes'])
    Habitat_Quality_Pathway_Restoration[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'] = gsub(',NA','',Habitat_Quality_Pathway_Restoration[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'])
    
    print(paste("--- Restoration - TOTAL reaches after habitat attributes filter: ", nrow(Habitat_Quality_Pathway_Restoration), sep=""))
    
    #  ---------------------------------------------------------------------------------
    #           Write output data to output file
    #  ---------------------------------------------------------------------------------
    
    # ----------------------- Restoration --------------------
    output_path_x =  paste(output_path,restoration_output_name, sep="")
    write_xlsx(Habitat_Quality_Pathway_Restoration,output_path_x )
    
  }else{
    
    print(paste("--- No Restoration Reaches generated for species: ", species, sep=""))
    
  }
  
  
  # -------------------------------------------------------
  #     Output Protection Scores (if reaches generated)
  # ------------------------------------------------------- 
  
  if( nrow(Habitat_Quality_Pathway_Protection) > 0 ){

    # -------------------------------------------------------
    # NOTE: these two columns added in Restoration NOTE added here since Protection
    #       does not factor in individual habitat attributes for restoration
    
    print(paste("--- Protection - TOTAL reaches after habitat attributes filter: ", nrow(Habitat_Quality_Pathway_Protection), sep=""))
    
    #  ---------------------------------------------------------------------------------
    #           Write output data to output file
    #  ---------------------------------------------------------------------------------

    # ----------------------- Protection --------------------
    output_path_x =  paste(output_path,protection_output_name, sep="")
    write_xlsx(Habitat_Quality_Pathway_Protection,output_path_x )
    
  }else{
    
    print(paste("--- No Protection Reaches generated for species: ", species, sep=""))
    
  }
  
  # --------------------- Put Restoration and Protection into a list --------------
  Habitat_Quality_Pathway_Output = list( 
    "Habitat_Quality_Pathway_Restoration" = Habitat_Quality_Pathway_Restoration,
    "Habitat_Quality_Pathway_Protection" = Habitat_Quality_Pathway_Protection
  )
  
  # ------------- empty data frame IF A) is Bull Trout and B) is to be excluded -----------
  if(exclude_bull_trout == "yes" & species == "Bull Trout"){
    
    # --------------------- Restoration ------------------
    # ------ remove all but one row -------
    Habitat_Quality_Pathway_Output[["Habitat_Quality_Pathway_Restoration"]] = Habitat_Quality_Pathway_Output[["Habitat_Quality_Pathway_Restoration"]][1,]
    # ------ make all NA -------
    for(col_i in 1:ncol(Habitat_Quality_Pathway_Output[["Habitat_Quality_Pathway_Restoration"]])){
      Habitat_Quality_Pathway_Output[["Habitat_Quality_Pathway_Restoration"]][1,col_i] = NA
    }
    
    # --------------------- Protection ------------------
    # ------ remove all but one row -------
    Habitat_Quality_Pathway_Output[["Habitat_Quality_Pathway_Protection"]] = Habitat_Quality_Pathway_Output[["Habitat_Quality_Pathway_Protection"]][1,]
    # ------ make all NA -------
    for(col_i in 1:ncol(Habitat_Quality_Pathway_Output[["Habitat_Quality_Pathway_Protection"]])){
      Habitat_Quality_Pathway_Output[["Habitat_Quality_Pathway_Protection"]][1,col_i] = NA
    }
  }
  
  return(Habitat_Quality_Pathway_Output)
  
}


# ------------------ Function to list all the rows below individual habitat criteria -------------------------
list_indiv_habitat_attributes_low_FUNCTION <- function(habitat_row, colnames_x){
  
  # ---------------- Individual Habitat Attribute Score: 1  --------
  columns_impaired_x = which(habitat_row <= 1)
  columns_impaired_x = colnames_x[columns_impaired_x]
  habitat_attributes_impaired_1 = paste(columns_impaired_x, collapse=',' )
  
  # ---------------- Individual Habitat Attribute Score: 3 or lower  --------
  columns_impaired_x = which(habitat_row <= 3 & habitat_row > 1)
  columns_impaired_x = colnames_x[columns_impaired_x]
  habitat_attributes_impaired_2_or_3 = paste(columns_impaired_x, collapse=',' )
  
  # ---------------- Individual Habitat Attribute Score: 3 or lower  --------
  columns_impaired_x = which(habitat_row <= 3)
  columns_impaired_x = colnames_x[columns_impaired_x]
  habitat_attributes_impaired_3_or_lower = paste(columns_impaired_x, collapse=',' )
  
  # ------- output -----------
  return(c(habitat_attributes_impaired_1,habitat_attributes_impaired_2_or_3, habitat_attributes_impaired_3_or_lower  ))
  
}


# -------------------- just to print out which reaches are in Unacceptable (1) or At Risk (2 or 3)

#for(rowx in 1:nrow(Habitat_Quality_Pathway_Restoration)){
#  print(  paste(paste("-------REACH ",rowx), paste(" -------: ",Habitat_Quality_Pathway_Restoration[rowx,1]))    )
#  if(any(Habitat_Quality_Pathway_Restoration[rowx,7:19]==1)){
#    print( "SCORES of 1")}
#  if(any(Habitat_Quality_Pathway_Restoration[rowx,7:19] <= 3 & Habitat_Quality_Pathway_Restoration[rowx,7:19] > 1)){
#    print("SCORES of > 1 and <= 3")}}


