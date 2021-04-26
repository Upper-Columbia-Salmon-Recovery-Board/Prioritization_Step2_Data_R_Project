
# ---------------------------------------------------------------------------
#
#      SCRIPT:  Limiting Factor Pathway FILTER
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
#   Function to generate Limiting Factor Pathway output
#
# ---------------------------------------------------------------------------

test_x = TRUE
if(test_x){
  species = "Steelhead"
}



Generate_Limiting_Factor_Output_Table_Okanogan = function(species, basins){
  
  # ------------------------------------------------------------------------------
  #       Establish species-specific variable names
  # ------------------------------------------------------------------------------
  
  if(species == "Steelhead"){
    # ---------------- species reach ---------------
    species_reach = 'Steelhead.Reach'
    # ---------------- species AU Rank ----------
    AU_rank_name_restoration = 'AU Restoration Rank'
    # ---------------- species AU Rank PROTECTION ----------
    AU_rank_name_protection = 'AU Protection Rank'
    # --------------- Life stage Sum column name ----------
    life_stage_sum_column = 'SH_Life_Stage_Sum'
    # ---------------- life stage priority names ---------
    life_stages_priorities_species_specific =  life_stages_priorities[['steelhead_life_stages']]
    # ------------------- output names ----------------------
    restoration_output_name = paste(paste("Steelhead_Limiting_Factors_RESTORATOIN_Okanogan" , 
                                          paste(basins_to_include, collapse = "_"), sep="_"),
                                    ".xlsx", sep="")
    protection_output_name = paste(paste("Steelhead_Limiting_Factors_PROTECTION_Okanogan" , 
                                         paste(basins_to_include, collapse = "_"), sep="_"),
                                   ".xlsx", sep="")
    
  
    
  }else{
    print('Incorrectly entered species name - re-type species name')
    
  }
  
  #  ---------------------------------------------------------------------------------
  #           Establish Reach Information Data Frame Just for this Output
  #  ---------------------------------------------------------------------------------
  # -------- Reach Information data frame for this species -----------
  Species_Reach_Information_data = Reach_Information_data[which(Reach_Information_data$Basin == "Okanogan"), ]
  
  #  ---------------------------------------------------------------------------------
  #           Create AU Ranks data frame
  #  ---------------------------------------------------------------------------------
  Species_AU_Ranks_data = AU_Ranks_Okanogan
  
  print(paste("Total Initial Reaches (LF Pathway): ", nrow(Species_Reach_Information_data), sep=""))
  
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
  #           Number of Life Stages Filter 
  #  ---------------------------------------------------------------------------------
  
  # -------------------- add additional column for this particular species reach presence ---------------
  Life_Stage_Priorities_AU_and_Reach_data["Life_Stage_Sum_Column"] = Life_Stage_Priorities_AU_and_Reach_data[life_stage_sum_column]
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  Life_Stage_Priorities_AU_and_Reach_data_FILTERED = Life_Stage_Priorities_AU_and_Reach_data %>%  
    filter(Life_Stage_Sum_Column   >=   Sum_Life_Stage_Criteria)
  # ------------------------ identify reaches that pass through the RESTORATION filter ----------
  Species_Reach_Information_data = Species_Reach_Information_data %>%  
    filter(ReachName   %in%   Life_Stage_Priorities_AU_and_Reach_data_FILTERED$`ReachName`)
  
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
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data_restoration$`RTT AU`)
  
  print(paste("Restoration - AU rank filter: ", nrow(Species_Reach_Information_data_restoration), sep=""))
  
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
  #          Generate Life Stage Tables and Score for Restoration and Protection (not considering filters above)
  #  ---------------------------------------------------------------------------------
  
  Reaches_Limiting_Factor_Pathway_FILTERED = Generate_EDT_Output_for_Limiting_Factor(species)
  # NOTE: Reaches_Limiting_Factor_Pathway_FILTERED will potentially have duplicated reaches 
  #        since a reach could be priority for multiple life stages
  # NOTE: above function filters based on life stage priority at AU level as well
  # --------------- remove all the NAs and trailing commas ----------------------
  Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_1_indiv_habitat_attributes'] = str_remove(Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_1_indiv_habitat_attributes'], ",NA")
  Reaches_Limiting_Factor_Pathway_FILTERED[,'at_risk_2_or_3_indiv_habitat_attributes'] = str_remove(Reaches_Limiting_Factor_Pathway_FILTERED[,'at_risk_2_or_3_indiv_habitat_attributes'], ",NA")
  Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'] = str_remove(Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'], ",NA")
  # ---------------- remove NA ------------
  Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_1_indiv_habitat_attributes'] = str_remove(Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_1_indiv_habitat_attributes'], "NA")
  Reaches_Limiting_Factor_Pathway_FILTERED[,'at_risk_2_or_3_indiv_habitat_attributes'] = str_remove(Reaches_Limiting_Factor_Pathway_FILTERED[,'at_risk_2_or_3_indiv_habitat_attributes'], "NA")
  Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'] = str_remove(Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'], "NA")
  # ----------------- remove commas -------------
  Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_1_indiv_habitat_attributes'] = (gsub("^\\,|\\,$", "", Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_1_indiv_habitat_attributes']))
  Reaches_Limiting_Factor_Pathway_FILTERED[,'at_risk_2_or_3_indiv_habitat_attributes'] = (gsub("^\\,|\\,$", "", Reaches_Limiting_Factor_Pathway_FILTERED[,'at_risk_2_or_3_indiv_habitat_attributes']))
  Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'] = (gsub("^\\,|\\,$", "", Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes']))
  # ------------------ remove _score ---------------
  Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_1_indiv_habitat_attributes'] =  (  gsub('_score','',Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_1_indiv_habitat_attributes']) )
  Reaches_Limiting_Factor_Pathway_FILTERED[,'at_risk_2_or_3_indiv_habitat_attributes'] = (  gsub('_score','',Reaches_Limiting_Factor_Pathway_FILTERED[,'at_risk_2_or_3_indiv_habitat_attributes']) )
  Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'] = (  gsub('_score','',Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes']) )
  #  ----------------------- remove _Mean -----------------------
  Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_1_indiv_habitat_attributes'] =  (  gsub('_Mean','',Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_1_indiv_habitat_attributes']) )
  Reaches_Limiting_Factor_Pathway_FILTERED[,'at_risk_2_or_3_indiv_habitat_attributes'] = (  gsub('_Mean','',Reaches_Limiting_Factor_Pathway_FILTERED[,'at_risk_2_or_3_indiv_habitat_attributes']) )
  Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'] = (  gsub('_Mean','',Reaches_Limiting_Factor_Pathway_FILTERED[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes']) )
  
  print(paste("TOTAL reaches passing through life stage (without applying above filters): ", length(unique(Reaches_Limiting_Factor_Pathway_FILTERED$ReachName)), sep=""))
  
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to select for Habitat Quality RESTORATION score
  #  ---------------------------------------------------------------------------------
  
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  Limiting_Factor_Scores_Restoration = Reaches_Limiting_Factor_Pathway_FILTERED %>%  
    filter(LF_Score_Restoration   >=   SCORE_Criteria_Habitat_Quality_Pathway_Restoration)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Limiting_Factor_Pathway_Restoration = Limiting_Factor_Scores_Restoration %>%  
    filter(ReachName   %in%   Species_Reach_Information_data_restoration$`ReachName`)
  
  print(paste("Restoration - total after HQ score filter: ", length(unique(Limiting_Factor_Pathway_Restoration$ReachName)), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to select for Habitat Quality PROTECTION score
  #  ---------------------------------------------------------------------------------
  
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score critera --------------
  Limiting_Factor_Scores_Protection = Reaches_Limiting_Factor_Pathway_FILTERED %>%  
    filter(LF_Score_Protection   >=   SCORE_Criteria_Habitat_Quality_Pathway_Protection)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Limiting_Factor_Pathway_Protection = Limiting_Factor_Scores_Protection %>%  
    filter(ReachName   %in%   Species_Reach_Information_data_protection$`ReachName`)
  
  print(paste("Protection - total after HQ score filter: ", length(unique(Limiting_Factor_Pathway_Protection$ReachName)), sep=""))
  
  
  
  #  ---------------------------------------------------------------------------------
  #           Only pull 1 (unacceptable) and 3 (at risk) from the 
  #  ---------------------------------------------------------------------------------
  Limiting_Factors_Okanogan_EDT_filtered = Limiting_Factors_Okanogan_EDT[Limiting_Factors_Okanogan_EDT$RTT_Limiting_Factor_Score <= Individual_Habitat_Attribute_Score,]
  
  
  
  
  
  # --------------------- but Restoration and Protectoin into a list --------------
  Limiting_Factor_Pathway_Output = list( 
    "Limiting_Factor_Pathway_Restoration" = Limiting_Factor_Pathway_Restoration,
    "Limiting_Factor_Pathway_Protection" = Limiting_Factor_Pathway_Protection
  )
  
  return(Limiting_Factor_Pathway_Output)
  
  
}




# ---------------------------------------------------------------------------
#
#   Function to generate Life Stage Tables (all the habitat attributes and scores)
#
# ---------------------------------------------------------------------------

Generate_EDT_Output_for_Limiting_Factor = function(species){
  
  # -------------------------------------------------------
  #     Generate list of life stages for this species
  #--------------------------------------------------------
  
  # -------------------- pull habitat attributes/life stages JUST for this species ---------
  Attribute_LifeStage_Crosswalk_Life_Stage = Attribute_LifeStage_Crosswalk %>%
    filter(Species  %in% species  )
  

  
  return(Reaches_Limiting_Factor_Pathway_FILTERED)
  
}



# -----------------------------------------------------------------------------------------
#     Function to pull ALL habitat attributes for each life stage, calculate Restoration
#         and Protection scores, list all 1 or 3 habitat attributes        
# -----------------------------------------------------------------------------------------
# attributes not in data: Entrainment - Fry OR Entrainment- Summer Rearing (Entrainment/Stranding) is present AND Predators- Adults are misspelled

Generate_individual_life_stage_score = function(species, life_stage){
  
  # -------------------- pull habitat attributes/life stages JUST for this species ---------
  Attribute_LifeStage_Crosswalk_Life_Stage = Attribute_LifeStage_Crosswalk %>%
    filter(Species  %in% species  )
  
  # ------------------ pull habitat attributes JUST for this life stage -------------------
  habitat_attributes_life_stage_list = Attribute_LifeStage_Crosswalk_Life_Stage %>%
    filter(Attribute_LifeStage_Crosswalk_Life_Stage$'Life Stage'  %in%  life_stage  )
  
  # --------------------------------------------------------------------------------
  #         generate all the scores for all the habitat attributes for EVERY reach in basin(s)
  # --------------------------------------------------------------------------------
  
  Habitat_Attribute_Scores_for_individual_Life_Stage = c()
  
  for(habitat_attribute_x in habitat_attributes_life_stage_list$'Habitat Attribute'){
    
    # --------------------- generate Habitat Attribute scores for this life stage -------
    Habitat_Attribute_Scores_Life_Stage = Habitat_Attribute_Scores %>%
      filter(Habitat_Attribute %in% habitat_attribute_x)
    # -----------------------pull only the habitat attribute score ---------
    Habitat_Attribute_Score_x = Habitat_Attribute_Scores_Life_Stage$Habitat_Attribute_Score
    # --------------- convert to a data frame and add column name-----------
    Habitat_Attribute_Score_x = as.data.frame(Habitat_Attribute_Score_x)
    colnames(Habitat_Attribute_Score_x) = habitat_attribute_x
    
    # -------------- combine with other habitat attributes for this life stage ------------
    # -------- if it's the first habitat attribute -----
    if( is.null(nrow(Habitat_Attribute_Scores_for_individual_Life_Stage)) ){
      Habitat_Attribute_Scores_for_Life_Stages_REACHES_BASINS = Habitat_Attribute_Scores_Life_Stage[,c("ReachName","Basin")]
      Habitat_Attribute_Scores_for_individual_Life_Stage = Habitat_Attribute_Score_x
      #------- if it's not the first habitat attribute -----
    }else if( nrow(Habitat_Attribute_Score_x) > 0 ){
      Habitat_Attribute_Scores_for_individual_Life_Stage = cbind(Habitat_Attribute_Scores_for_individual_Life_Stage, Habitat_Attribute_Score_x)
    }
  }
  
  # --------------- convert to data frame ----------------
  Habitat_Attribute_Scores_for_individual_Life_Stage = as.data.frame(Habitat_Attribute_Scores_for_individual_Life_Stage)
  #print( " -----------------------------------------------------------------------------------------------------------------------")
  
  #print(Habitat_Attribute_Scores_for_individual_Life_Stage[which(Habitat_Attribute_Scores_for_Life_Stages_REACHES_BASINS$ReachName == 'Nason Creek Lower 07'), ])
  #print(" --- --- ---")
  #print(which(Habitat_Attribute_Scores_for_Life_Stages_REACHES_BASINS$ReachName == 'Nason Creek Lower 07'))
  # -------------------------------------------------------
  #         identify all individual habitat attribute scores at 3 (or lower) and 1 in EACH Reach
  # -------------------------------------------------------
  # NOTE: these two columns are added at the very end 
  
  colnames_x = colnames(Habitat_Attribute_Scores_for_individual_Life_Stage)
  # NOTE - this way might work - I changed the Function list_indiv_habitat_attributes_low_FUNCTION
  #indiv_habitat_attributes_impaired = apply(as.matrix(Habitat_Attribute_Scores_for_individual_Life_Stage), 
  #                                          MARGIN = 1, list_indiv_habitat_attributes_low_FUNCTION)
  #indiv_habitat_attributes_impaired = t(as.data.frame(indiv_habitat_attributes_impaired))
  
  indiv_habitat_attributes_impaired = c()
  for(rowx in 1:nrow(Habitat_Attribute_Scores_for_individual_Life_Stage)){
    three_scores_output = list_indiv_habitat_attributes_low_FUNCTION(Habitat_Attribute_Scores_for_individual_Life_Stage[rowx,], colnames_x)
    indiv_habitat_attributes_impaired = rbind(indiv_habitat_attributes_impaired,three_scores_output )
  }
  
  colnames(indiv_habitat_attributes_impaired) = c("unacceptable_1_indiv_habitat_attributes","at_risk_2_or_3_indiv_habitat_attributes",  "unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes")
  
  #print(" ----- -  ------------------- - - -------")
  #print(indiv_habitat_attributes_impaired[439,])
  #print(" ----- -  ------------------- - - -------")
  
  # -------------------------------------------------------
  #         generate a single score SUM and PERCENT value for ALL reaches across all basin(s)
  # -------------------------------------------------------
  
  # ----------- total number of habitat attributes ------
  number_habitat_attributes = ncol(Habitat_Attribute_Scores_for_individual_Life_Stage)
  # ---------- Summation of Habitat Attributes ---------
  Habitat_Attribute_Scores_for_individual_Life_Stage$LF_Sum = rowSums(Habitat_Attribute_Scores_for_individual_Life_Stage)
  # -------- add Reach Names and Basin to beginning of data frame -----
  Habitat_Attribute_Scores_for_individual_Life_Stage = cbind(Habitat_Attribute_Scores_for_Life_Stages_REACHES_BASINS, Habitat_Attribute_Scores_for_individual_Life_Stage)
  # ----------------- Percent of Habitat Attributes ------------
  Habitat_Attribute_Scores_for_individual_Life_Stage$LF_Pct = Habitat_Attribute_Scores_for_individual_Life_Stage$LF_Sum/(number_habitat_attributes*5)
  
  
  # ------------------------------------------------------------------------------------- 
  #                 calculate HQ Restoration and Protection Score
  # ------------------------------------------------------------------------------------- 
  
  # ------------------------------------- Restoration ---------------------------------
  Habitat_Attribute_Scores_for_individual_Life_Stage = Habitat_Attribute_Scores_for_individual_Life_Stage  %>%
    mutate(LF_Score_Restoration = ifelse(LF_Pct  > Restoration_Scoring$Category_Lower[1] & 
                                           LF_Pct  <= Restoration_Scoring$Category_Upper[1] , Restoration_Scoring$Score[1],
                                         ifelse(LF_Pct  > Restoration_Scoring$Category_Lower[2] & 
                                                  LF_Pct  <= Restoration_Scoring$Category_Upper[2] , Restoration_Scoring$Score[2],
                                                ifelse(LF_Pct  > Restoration_Scoring$Category_Lower[3] & 
                                                         LF_Pct  <= Restoration_Scoring$Category_Upper[3] , Restoration_Scoring$Score[3],
                                                       NA))))
  
  # ------------------------------------- Protection ---------------------------------
  Habitat_Attribute_Scores_for_individual_Life_Stage = Habitat_Attribute_Scores_for_individual_Life_Stage  %>%
    mutate(LF_Score_Protection = ifelse(LF_Pct  > Protection_Scoring$Category_Lower [1] & 
                                          LF_Pct  <= Protection_Scoring$Category_Upper[1] , Protection_Scoring$Score[1],
                                        ifelse(LF_Pct  > Protection_Scoring$Category_Lower[2] & 
                                                 LF_Pct  <= Protection_Scoring$Category_Upper[2] , Protection_Scoring$Score[2],
                                               ifelse(LF_Pct  > Protection_Scoring$Category_Lower[3] & 
                                                        LF_Pct  <= Protection_Scoring$Category_Upper[3] , Protection_Scoring$Score[3],
                                                      NA))))
  # -------------------- add columns with which habitat attributes are impaired -------
  Habitat_Attribute_Scores_for_individual_Life_Stage = cbind(Habitat_Attribute_Scores_for_individual_Life_Stage, indiv_habitat_attributes_impaired)
  
  # --------------- transform into a tibble -------
  Habitat_Attribute_Scores_for_individual_Life_Stage = as_data_frame(Habitat_Attribute_Scores_for_individual_Life_Stage)
  
  #print(Habitat_Attribute_Scores_for_individual_Life_Stage[which(Habitat_Attribute_Scores_for_individual_Life_Stage$ReachName == "Nason Creek Lower 07"),])
  #print( " -----------------------------------------------------------------------------------------------------------------------")
  return(Habitat_Attribute_Scores_for_individual_Life_Stage) 
}


# -----------------------------------------------------------------------------------------
#     Function to list all the rows (rows/reaches of habitat attributes) below individual habitat criteria
#                 
# -----------------------------------------------------------------------------------------

# habitat_row = Habitat_Attribute_Scores_for_individual_Life_Stage[439,]
# colnames_x = colnames(Habitat_Attribute_Scores_for_individual_Life_Stage)
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

# ------------------ output data -------------------------
#Habitat_Quality_Scores = as.data.frame(Habitat_Quality_Scores)
#output_path_x =  paste(output_path,'Habitat_Quality_Scores.xlsx', sep="")
#write.xlsx(  Habitat_Quality_Scores,
#  output_path_x, col.names = TRUE,  row.names = FALSE, append = FALSE, showNA = TRUE, password = NULL)

# -----------------------------------------------------------------------------------------
#     Function to apply the AU life stage priority as a filter
# -----------------------------------------------------------------------------------------

# life_stage = "Fry"
Life_Stage_Priority_Filter_Function = function(life_stage, Habitat_Attribute_Scores_for_individual_Life_Stage, Life_Stage_Priority){
  
  # -------------------------------------------------------
  #     Get reaches with this life stage as the specified life stage priority
  # -------------------------------------------------------
  
  # -------- ONE Life_Stage_Priority -----------
  if(length(Life_Stage_Priority) == 1){
    life_stages_priorities_species_specific_list = Life_Stage_Priorities_AU_and_Reach_data %>%
      filter(Life_Stage_Priorities_AU_and_Reach_data[life_stages_priorities_species_specific[[life_stage]]] ==  Life_Stage_Priority)   # 
    
    # -------- TWO Life_Stage_Priority -----------
  }else if(length(Life_Stage_Priority) == 2){
    
    life_stages_priorities_species_specific_list = Life_Stage_Priorities_AU_and_Reach_data %>%
      filter(Life_Stage_Priorities_AU_and_Reach_data[life_stages_priorities_species_specific[[life_stage]]] ==  Life_Stage_Priority[1]  |
               Life_Stage_Priorities_AU_and_Reach_data[life_stages_priorities_species_specific[[life_stage]]] ==  Life_Stage_Priority[2] )   # 
    
    # -------- THREE Life_Stage_Priority -----------
  }else if(length(Life_Stage_Priority) == 3){
    
    life_stages_priorities_species_specific_list = Life_Stage_Priorities_AU_and_Reach_data %>%
      filter(Life_Stage_Priorities_AU_and_Reach_data[life_stages_priorities_species_specific[[life_stage]]] ==  Life_Stage_Priority[1]  |
               Life_Stage_Priorities_AU_and_Reach_data[life_stages_priorities_species_specific[[life_stage]]] ==  Life_Stage_Priority[2]  | 
               Life_Stage_Priorities_AU_and_Reach_data[life_stages_priorities_species_specific[[life_stage]]] ==  Life_Stage_Priority[3] )   # 
  }
  
  # -------------------------------------------------------
  #     Filter Reaches from Life stage analysis
  # -------------------------------------------------------
  Habitat_Attribute_Scores_for_individual_Life_Stage_Filtered = Habitat_Attribute_Scores_for_individual_Life_Stage %>%  
    filter(Habitat_Attribute_Scores_for_individual_Life_Stage$ReachName   %in%   life_stages_priorities_species_specific_list$ReachName)
  
  
  return(Habitat_Attribute_Scores_for_individual_Life_Stage_Filtered)
  
}



