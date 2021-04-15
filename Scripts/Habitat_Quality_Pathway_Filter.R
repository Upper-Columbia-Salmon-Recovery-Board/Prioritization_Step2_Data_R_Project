

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
species = "Steelhead"
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



# ------------------------------------------------------------------------------
#
#        Okanogan Habitat Quality FIlter
#
# ------------------------------------------------------------------------------


#  to test
species = "Steelhead"

Generate_Habitat_Quality_Output_Table_Okanogan = function( species ){
  
  # ------------------------------------------------------------------------------
  #       Establish species-specific variable names
  # ------------------------------------------------------------------------------
  print(paste("---------------------------------- START HQ Pathway FOR: ",species, sep=""))
  if(species == "Steelhead"){
    # ---------------- species reach ---------------
    species_reach = 'Steelhead.Reach'
    # ---------------- species AU Rank ----------
    AU_rank_name_restoration = 'AU Restoration Rank'
    # ---------------- species AU Rank PROTECTION ----------
    AU_rank_name_protection = 'AU Protection Rank'
    # --------------- Life stage Sum column name ----------
    life_stage_sum_column = 'SH_Life_Stage_Sum'
    # ------------------- output names ----------------------
    restoration_output_name = paste(paste("Steelhead_Habitat_Quality_RESTORATION_Okanogan" , ".xlsx", sep="") )
    protection_output_name = paste(paste("Steelhead_Habitat_Quality_PROTECTION_Okanogan" ,".xlsx", sep=""))
  }else{
    print('Incorrectly entered species name - re-type species name')
    
  }
  
  
  #  ---------------------------------------------------------------------------------
  #           Create AU Ranks data frame
  #  ---------------------------------------------------------------------------------
  Species_AU_Ranks_data = AU_Ranks_Okanogan
  
  #  ---------------------------------------------------------------------------------
  #           Establish Reach Information Data Frame Just for this Output
  #  ---------------------------------------------------------------------------------
  # -------- Reach Information data frame for this species -----------
  Species_Reach_Information_data = Reach_Information_data
  # --------------------- Filter by Okanogan -------------
  Species_Reach_Information_data = Species_Reach_Information_data[which(Species_Reach_Information_data$Basin == "Okanogan"),]
  
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
  #            Filter out to select for AU rank - RESTORATION
  #  ---------------------------------------------------------------------------------
  
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_AU_Ranks_data["Species_AU_Ranks"] = Species_AU_Ranks_data[AU_rank_name_restoration]
  # ----------------------- filter out for only reaches with this species --------------
  Species_AU_Ranks_data_restoration = Species_AU_Ranks_data %>%  
    filter(Species_AU_Ranks    %in%   AU_Rank)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Species_Reach_Information_data_restoration = Species_Reach_Information_data %>%  
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data_restoration$`RTT AU`)
  
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
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data_protection$`RTT AU`)
  
  print(paste("Protection - total after AU rank filter: ", nrow(Species_Reach_Information_data_protection), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #           Reach Confinement (NOTE - only used for Restoration, not for Protection)
  #  ---------------------------------------------------------------------------------
  
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  Confinement_Scores_Restoration = Confinement_Scores %>%  
    filter(Score   >=   Reach_Confinement_SCORE_Criteria)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Species_Reach_Information_data_restoration = Species_Reach_Information_data_restoration %>%  
    filter(ReachName   %in%   Confinement_Scores_Restoration$`ReachName`)
  
  print(paste("HQ Pathway-RESTORATION - total reaches after reach confinement filter: ", nrow(Species_Reach_Information_data_restoration), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #           Number of Life Stages Filter 
  #  ---------------------------------------------------------------------------------
  
  # -------------------- add additional column for this particular species reach presence ---------------
  Life_Stage_Priorities_AU_and_Reach_data["Life_Stage_Sum_Column"] = Life_Stage_Priorities_AU_and_Reach_data[life_stage_sum_column]
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  Life_Stage_Priorities_AU_and_Reach_data_FILTERED = Life_Stage_Priorities_AU_and_Reach_data %>%  
    filter(Life_Stage_Sum_Column   >=   Sum_Life_Stage_Criteria)
  
  # ------------------------ identify reaches that pass through the RESTORATION filter ----------
  Habitat_Quality_Pathway_Restoration = Species_Reach_Information_data_restoration %>%  
    filter(ReachName   %in%   Life_Stage_Priorities_AU_and_Reach_data_FILTERED$`ReachName`)
  # ------------------------ identify reaches that pass through the PROTECTION filter ----------
  Habitat_Quality_Pathway_Protection = Species_Reach_Information_data_protection %>%  
    filter(ReachName   %in%   Life_Stage_Priorities_AU_and_Reach_data_FILTERED$`ReachName`)
  
  print(paste("HQ Pathway-RESTORATION - total reaches after life stages number filter: ", nrow(Habitat_Quality_Pathway_Restoration), sep=""))
  
  print(paste("HQ Pathway-PROTECTION - total reaches after life stages number filter: ", nrow(Habitat_Quality_Pathway_Protection), sep=""))
  
  
  #  ---------------------------------------------------------------------------------
  #            RESTORATION = Filter out for AU-level % Habitat Quality (EDT = "% Template")
  #  ---------------------------------------------------------------------------------
  # Filter for <80% habitat function using %HabitatQuality Tab 
  # ------------------- filter out % of Template that is below certain score --------------------
  PRCNT_Habitat_Quality_Okanogan_EDT_FILTER_RESTORATION = PRCNT_Habitat_Quality_Okanogan_EDT[which(PRCNT_Habitat_Quality_Okanogan_EDT$HQ_Score <= PRCNT_of_Template_Restoration_Score), ]
  
  # ------------------------ identify reaches that pass through the PROTECTION filter ----------
  Habitat_Quality_Pathway_Restoration = Habitat_Quality_Pathway_Restoration %>%  
    filter(ReachName   %in%   PRCNT_Habitat_Quality_Okanogan_EDT_FILTER_RESTORATION$`ReachName`)
  
  print(paste("HQ Pathway-RESTORATION - total reaches after life stages number filter: ", nrow(Habitat_Quality_Pathway_Restoration), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #            PROTECTION = Filter out for AU-level % Habitat Quality (EDT = "% Template")
  #  ---------------------------------------------------------------------------------
  # Filter for <80% habitat function using %HabitatQuality Tab 
  # ------------------- filter out % of Template that is below certain score --------------------
  PRCNT_Habitat_Quality_Okanogan_EDT_FILTER_PROTECTION = PRCNT_Habitat_Quality_Okanogan_EDT[which(PRCNT_Habitat_Quality_Okanogan_EDT$HQ_Score >= PRCNT_of_Template_Protection_Score), ]
  
  # ------------------------ identify reaches that pass through the PROTECTION filter ----------
  Habitat_Quality_Pathway_Protection = Habitat_Quality_Pathway_Protection %>%  
    filter(ReachName   %in%   PRCNT_Habitat_Quality_Okanogan_EDT_FILTER_PROTECTION$`ReachName`)
  
  print(paste("HQ Pathway-PROTECTION - total reaches after life stages number filter: ", nrow(Habitat_Quality_Pathway_Protection), sep=""))
  
  # -------------------------------------------------------
  #     Output Scores (if reaches generated)
  # ------------------------------------------------------- 
  
  # ------- IF there are still reaches that got past filters --------
  if( nrow(Habitat_Quality_Pathway_Restoration) > 0 ){
    
    # -------------------------------------------------------
    #         identify all individual habitat attribute scores at 3 (or lower) and 1
    # -------------------------------------------------------
    # NOTE: these two columns are added at the very end 
    
    # -------------------- Restoration -----------
    # --------------- identify habitat attributes at 1 (Unacceptable) OR 3 (At Risk) -----------------------
    # HabitatAttribute_Ratings_level_2
    indiv_habitat_attributes_impaired_restoration = c() 
    
    # ------ list column names/all habitat attributes -----------------
    colnames_restoration_x = unique(HabitatAttribute_Ratings_level_2$`RTT Habitat Attribute`)
    colnames_restoration_x = colnames_restoration_x[order(colnames_restoration_x)]
    
    for(rowx in 1:nrow(Habitat_Quality_Pathway_Restoration)){
      # --------------------- pull habitat attributes for this reach -----------------
      habitat_attributes_output_x = HabitatAttribute_Ratings_level_2[which(HabitatAttribute_Ratings_level_2$Reach ==  Habitat_Quality_Pathway_Restoration$ReachName[rowx] ), ]
      # ------------ function to identify habitat attributes at 1 and 3 (and list for each reach) ---------
      output_row_x = list_indiv_habitat_attributes_low_FUNCTION_OKANOGAN(habitat_attributes_output_x, colnames_restoration_x)
      # ------------------ combine row -------------------
      indiv_habitat_attributes_impaired_restoration = rbind(indiv_habitat_attributes_impaired_restoration,output_row_x )
    }
    
    # ----------------------- Remove any rows that had no unacceptable or at risk habitat attributes -----------
    NA_df = is.na( indiv_habitat_attributes_impaired_restoration[ ,colnames(indiv_habitat_attributes_impaired_restoration)[ (length(colnames(indiv_habitat_attributes_impaired_restoration))-2):length(colnames(indiv_habitat_attributes_impaired_restoration))  ]  ] ) 
    NA_rows_x = which(NA_df[,1] & NA_df[,2] & NA_df[,3])
    indiv_habitat_attributes_impaired_restoration = indiv_habitat_attributes_impaired_restoration[-NA_rows_x, ]
    
    print(paste("--- Restoration - TOTAL reaches after habitat attributes filter: ", nrow(indiv_habitat_attributes_impaired_restoration), sep=""))
    
    #  ---------------------------------------------------------------------------------
    #           Write output data to output file
    #  ---------------------------------------------------------------------------------
    
    # ----------------------- Restoration --------------------
    output_path_x =  paste(output_path,restoration_output_name, sep="")
    write_xlsx(indiv_habitat_attributes_impaired_restoration, output_path_x)
    
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
    "Habitat_Quality_Pathway_Restoration" = indiv_habitat_attributes_impaired_restoration,
    "Habitat_Quality_Pathway_Protection" = Habitat_Quality_Pathway_Protection
  )
  
  return(Habitat_Quality_Pathway_Output)
  
}


#  ---------------------------------------------------------------------------------
#          FUNCTION: Pull Unacceptable and At Risk Habitat Attributes
#  ---------------------------------------------------------------------------------

# ------------------ Function to list all the rows below individual habitat criteria -------------------------
# ----- to test -----
test_x = FALSE
if(test_x){
  reach_okanogan_data_frame = habitat_attributes_output_x
  colnames_x = unique(HabitatAttribute_Ratings_level_2$`RTT Habitat Attribute`)
  colnames_x = colnames_x[order(colnames_x)]
}


list_indiv_habitat_attributes_low_FUNCTION_OKANOGAN <- function(reach_okanogan_data_frame, colnames_x){
  
  # ------------------ create the row with all the attributes ---------------
  #  rows have column names that include that habitat attributes AND the scores 
  row_x = as.data.frame(c(  Reach_Information_data[which(Reach_Information_data$ReachName == reach_okanogan_data_frame$Reach[1]),1:6], 
                            rep(NA, length(colnames_x) + 3)  ) )
  colnames(row_x)[7:(length(colnames_x) + 6)] = colnames_x 
  colnames(row_x)[(length(colnames_x) + 7):ncol(row_x) ] = c("unacceptable_1_indiv_habitat_attributes" , "at_risk_2_or_3_indiv_habitat_attributes", 
                         "unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes"  )
  
  for(attribute_x in colnames_x){
    
    # ---------- pull habitat attribute ----------------
    habitat_attribute_x_i = which(reach_okanogan_data_frame$`RTT Habitat Attribute` == attribute_x)
    
    # -------- if multiple RTT habitat attributes for a single EDT habitat attribute -----------
    if( length(habitat_attribute_x_i) > 1){
      # ----------------- get lowest Function Conditoin (habitat attribute) score ------------
      score_x = min(reach_okanogan_data_frame$Function_Condition[habitat_attribute_x_i])
      # row_x_i = which( reach_okanogan_data_frame$Function_Condition[habitat_attribute_x_i] == score_x )

    # ------------- if RTT and EDT attributes are 1:1 (one RTT attribute for one EDT attribute) -----
    }else{
      score_x = min(reach_okanogan_data_frame$Function_Condition[habitat_attribute_x_i])
    }
    
    # -------------- insert score in correct column --------------
    row_x[,attribute_x] = score_x
    
    # ------------------- insert name in correct column -------------------
    if(score_x == 1){
      # --------------- insert habitat name --------
      row_x$unacceptable_1_indiv_habitat_attributes = paste(row_x$unacceptable_1_indiv_habitat_attributes, attribute_x, sep=",")
      row_x$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes = paste(row_x$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes, attribute_x, sep=",")
      # -------- strip white space from names -------------
      row_x$unacceptable_1_indiv_habitat_attributes = gsub("\\s", "", row_x$unacceptable_1_indiv_habitat_attributes)  
      row_x$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes = gsub("\\s", "", row_x$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes)  
    }
    
    if(score_x > 1 & score_x <= 3){
      # --------------- insert habitat name --------
      row_x$at_risk_2_or_3_indiv_habitat_attributes = paste(row_x$at_risk_2_or_3_indiv_habitat_attributes, attribute_x, sep=",")
      row_x$at_risk_2_or_3_indiv_habitat_attributes = gsub("\\s", "", row_x$at_risk_2_or_3_indiv_habitat_attributes)  
    }
    
  }
  
  # ----------------- remove leading NAs  -------------------
  row_x[,'unacceptable_1_indiv_habitat_attributes'] = gsub('NA,','',row_x[,'unacceptable_1_indiv_habitat_attributes'])
  row_x[,'at_risk_2_or_3_indiv_habitat_attributes'] = gsub('NA,','',row_x[,'at_risk_2_or_3_indiv_habitat_attributes'])
  row_x[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'] = gsub('NA,','',row_x[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'])
  
  # ------------------ change habitat attribute score names so they match MetEntWen output -----------
  # ------- remove blank space ------
  colnames(row_x)[7:(ncol(row_x)-3)] = gsub("\\s", "", colnames(row_x)[7:(ncol(row_x)-3)]) 
  # ---------- add "score" to end ------
  colnames(row_x)[7:(ncol(row_x)-3)] = paste(colnames(row_x)[7:(ncol(row_x)-3)], "_score", sep="") 
  
  # ------- output -----------
  return( row_x )
  
}

#  ---------------------------------------------------------------------------------
#
#           FUNCTION to combine main (Methow, Entiat, and Wenatchee) and Okanogan HQ Output
#
#  ---------------------------------------------------------------------------------
test_x = FALSE
if(test_x){
  MetEntWen_data_frame = Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']]
  Okanogan_data_frame = Habitat_Quality_Pathway_Steelhead_OKANOGAN[['Habitat_Quality_Pathway_Restoration']]
}

Combine_MetEntWen_and_Okanogan_Output = function(MetEntWen_data_frame,   Okanogan_data_frame,  habitat_quality_scores_colnames_for_combo){
  
  # -------------- update "Riparian_Mean" name ---------------
  x = which(habitat_quality_scores_colnames_for_combo == "Riparian_Mean")
  habitat_quality_scores_colnames_for_combo[x] = "Riparian_Mean_score"
  
  # -------------- start with basic info --------------
  df_add_to_MetEntWen = Okanogan_data_frame[,1:6]
  
  # ---------------- loop through all the habitat attributes ----------
  for(colx in habitat_quality_scores_colnames_for_combo){
    
    # ------- if column is in Okanogan data -----
    if( any(colnames(Okanogan_data_frame) == colx) ){
      df_add_to_MetEntWen[,colx] = Okanogan_data_frame[,colx]
    # ------- if column not in Okanogan data ------
    }else{
      df_add_to_MetEntWen[,colx] = NA
    }
  }
  
  # --------------- HQ_Sum is NA (for now) -----------
  df_add_to_MetEntWen$HQ_Sum = NA
  
  # ----------------- add HQ_Pct, HQ Restoration and Protection, and unacceptable/at risk attributes ----
  # ---------- truncate the HQ Score data frame -----------
  PRCNT_Habitat_Quality_Okanogan_EDT_Truncated = PRCNT_Habitat_Quality_Okanogan_EDT[,c("ReachName","HQ_Score")]
  # -------- merge HQ Score -----
  df_add_to_MetEntWen = merge(df_add_to_MetEntWen, PRCNT_Habitat_Quality_Okanogan_EDT_Truncated, by="ReachName")
  colnames(df_add_to_MetEntWen)[length(colnames(df_add_to_MetEntWen))] = "HQ_Pct"
  
  # ------------------------------ HQ Restoration Score ------------------------
  df_add_to_MetEntWen = df_add_to_MetEntWen  %>%
    mutate(HQ_Score_Restoration = ifelse(HQ_Pct  > Restoration_Scoring$Category_Lower[1] & 
                                           HQ_Pct  < Restoration_Scoring$Category_Upper[1] , Restoration_Scoring$Score[1],
                                         ifelse(HQ_Pct  >= Restoration_Scoring$Category_Lower[2] & 
                                                  HQ_Pct  <= Restoration_Scoring$Category_Upper[2] , Restoration_Scoring$Score[2],
                                                ifelse(HQ_Pct  > Restoration_Scoring$Category_Lower[3] & 
                                                         HQ_Pct  <= Restoration_Scoring$Category_Upper[3] , Restoration_Scoring$Score[3],
                                                       NA))))
  # ------------------------------ HQ Protection Score ------------------------
  df_add_to_MetEntWen = df_add_to_MetEntWen  %>%
    mutate(HQ_Score_Protection = ifelse(HQ_Pct  > Protection_Scoring$Category_Lower [1] & 
                                          HQ_Pct  < Protection_Scoring$Category_Upper[1] , Protection_Scoring$Score[1],
                                        ifelse(HQ_Pct  >= Protection_Scoring$Category_Lower[2] & 
                                                 HQ_Pct  <= Protection_Scoring$Category_Upper[2] , Protection_Scoring$Score[2],
                                               ifelse(HQ_Pct  > Protection_Scoring$Category_Lower[3] & 
                                                        HQ_Pct  <= Protection_Scoring$Category_Upper[3] , Protection_Scoring$Score[3],
                                                      NA))))
  
  # --------------------- add At Risk and Unnacceptable habitat attributes -----------------
  df_add_to_MetEntWen[,colnames(Okanogan_data_frame)[(length(colnames(Okanogan_data_frame))-2):length(colnames(Okanogan_data_frame))]] =
    Okanogan_data_frame[,colnames(Okanogan_data_frame)[(length(colnames(Okanogan_data_frame))-2):length(colnames(Okanogan_data_frame))]]
  
  # ---------------- update Riparian column name -----------
  x = which(colnames(df_add_to_MetEntWen) == "Riparian_Mean_score")
  colnames(df_add_to_MetEntWen)[x] = "Riparian_Mean"
  
  # ----------------- combine prepared Okanogan data frame with MetEntWen data frame -----
  MetEntWen_data_frame_updated = rbind(MetEntWen_data_frame,  df_add_to_MetEntWen)
  
  # ------------------ Return ---------------
  return(MetEntWen_data_frame_updated)
}




