
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
  basins = "Okanogan"
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
    # ----------------------- life stage presence in reach -------------
    reach_life_stage_presence = steelhead_life_stages_presence
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
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data_restoration$`EDT AU`)
  
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
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data_protection$`EDT AU`)
  
  print(paste("Protection - total after AU rank filter: ", nrow(Species_Reach_Information_data_protection), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #
  #          Finalize Restoration 
  #
  #  ---------------------------------------------------------------------------------
  
  #  ---------------------------------------------------------------------------------
  #           Only pull 1 (unacceptable) and 3 (at risk) from the RTT Limiting Factor Score 
  #  ---------------------------------------------------------------------------------
  Limiting_Factors_Okanogan_EDT_filtered = Limiting_Factors_Okanogan_EDT[Limiting_Factors_Okanogan_EDT$RTT_Limiting_Factor_Score <= Individual_Habitat_Attribute_Score,]
  
  
  #  ---------------------------------------------------------------------------------
  #          Generate Life Stage Tables and Score for Restoration and Protection (not considering filters above)
  #  ---------------------------------------------------------------------------------
  
  Limiting_Factor_Pathway_Restoration = Generate_Species_Output_Table_Okanogan(species, Limiting_Factors_Okanogan_EDT_filtered, reach_life_stage_presence )
  

  #  ---------------------------------------------------------------------------------
  #          Only include reaches that have passed through the previous filters (Species-reaches, # of life stages, AU Restoration priority)
  #  ---------------------------------------------------------------------------------
  
  Limiting_Factor_Pathway_Restoration = Limiting_Factor_Pathway_Restoration %>%  
    filter(ReachName    %in%   Species_Reach_Information_data_restoration$ReachName)
  
  #  ---------------------------------------------------------------------------------
  #          Generate Life Stage Tables and Score for Restoration and Protection (not considering filters above)
  #  ---------------------------------------------------------------------------------
  
  print(paste("TOTAL reaches passing through life stage: ", length(unique(Limiting_Factor_Pathway_Restoration$ReachName)), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #
  #          Finalize Protection 
  #
  #  ---------------------------------------------------------------------------------
  
  # !!!!!! NOTE !!!!!!!!!!!! As of now - not doing protection for the Okanogan with the Limiting Factor Pathway (just for the HQ pathway)
  
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score critera --------------

  
  # print(paste("Protection - total after HQ score filter: ", length(unique(Limiting_Factor_Pathway_Protection$ReachName)), sep=""))
  
  

  
  
  
  # --------------------- but Restoration and Protection into a list --------------
  Limiting_Factor_Pathway_Output = list( 
    "Limiting_Factor_Pathway_Restoration" = Limiting_Factor_Pathway_Restoration
  )
  
  return(Limiting_Factor_Pathway_Output)
  
  
}




# What output should look like View(Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']])


# ---------------------------------------------------------------------------
#
#   Function to generate Life Stage Tables (all the habitat attributes and scores)
#
# ---------------------------------------------------------------------------

Generate_Species_Output_Table_Okanogan = function(species, Limiting_Factors_Okanogan_EDT_filtered, reach_life_stage_presence){
  
  
  # -------------------------------------------------------
  #
  #     Loop Through Life Stages 
  #
  #--------------------------------------------------------

  # -------------------------------------------------------
  #     Loop through each life stage and get habitat attribute scores
  #--------------------------------------------------------
  
  # -------------------------- get list of life stages --------------------
  life_stages = unique(Limiting_Factors_Okanogan_EDT_filtered$`RTT Life Stage`)
  
  #Life_Stages_Habitat_Priorities_ALL= list()
  #Life_Stages_Habitat_Priorities_FILTERED = list()
  Reaches_Limiting_Factor_Pathway_FILTERED_Okanogan = c()
  # ---------------------------- generate the scores for all the life stages -----------------
  print("...processing the following life stages")
  for(life_stage_x in life_stages){
    print(paste("     ....",life_stage_x, sep=""))
    
    # --------------------------------------------------------------------
    #     Get reaches/habitat attributes with this life stage
    #  ------------------------------------------------------------------
    Limiting_Factors_Okanogan_EDT_filtered_life_stage_x = Limiting_Factors_Okanogan_EDT_filtered[which(Limiting_Factors_Okanogan_EDT_filtered$`RTT Life Stage` == life_stage_x), ]
    
    # --------------------------------------------------------------------
    #     Filter out to only include AU with life stage priority AND reach life stage presence 
    #  ------------------------------------------------------------------

    Limiting_Factors_Okanogan_EDT_filtered_life_stage_updated = Life_Stage_Priority_Filter_Function_Okanogan(life_stage_x, Limiting_Factors_Okanogan_EDT_filtered_life_stage_x, Life_Stage_Priority, reach_life_stage_presence)
    
    
    # --------------------------------------------------------------------
    #     Filter for only Level 3s with Level 2 Crosswalks (and therefore RTT habitat)
    # ------------------------------------------------------------------
    Limiting_Factors_Okanogan_EDT_filtered_life_stage_updated = Limiting_Factors_Okanogan_EDT_filtered_life_stage_updated[ which( !is.na( Limiting_Factors_Okanogan_EDT_filtered_life_stage_updated$EDT_Attribute_Level_2 ) ),  ]
    
    
    # --------------------------------------------------------------------
    #    Pull only 1s (Unacceptable) and 3s (At Risk) and prep to combine with Wenatchee, Entiat, and Methow output
    # ------------------------------------------------------------------
    
    Limiting_Factor_Okanogan_At_Risk_and_Unacceptable = pull_Level2_1s_and_3s_from_Level3(Limiting_Factors_Okanogan_EDT_filtered_life_stage_updated, life_stage_x)
      
      
      
    Reaches_Limiting_Factor_Pathway_FILTERED_Okanogan = rbind(Reaches_Limiting_Factor_Pathway_FILTERED_Okanogan,
                                                              Limiting_Factor_Okanogan_At_Risk_and_Unacceptable )
      
  }
  
  return(Reaches_Limiting_Factor_Pathway_FILTERED_Okanogan)
  
}


# -----------------------------------------------------------------------------------------
#
#     Function to A) generate list of Level 2 habitat attribute 1s and 3s
#                 B) prepare output to merge with other Limiting Factor Pathway 
#                 
# -----------------------------------------------------------------------------------------

#Limiting_Factors_Okanogan_EDT_filtered_life_stage_updated = Okanogan_Habitat_Attribute_Scores_for_individual_Life_Stage_ONE_REACH_1 

pull_Level2_1s_and_3s_from_Level3 <- function(Limiting_Factors_Okanogan_EDT_filtered_life_stage_updated,  life_stage_x){
  
  
  # -----------------------------------------------------------------------------------
  #         Start Output Data Frame
  # -----------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------
  #         Loop through each reach, Pull 1s and 3s (from Level 2 Habitat Attribute) for each Level 2 Habitat Attribute
  # -----------------------------------------------------------------------------------
  
  # ------------------- data frame to combine output for all reaches ---------
  output_for_LF_pathway_combined = c()
  unique_reaches_x = unique(Limiting_Factors_Okanogan_EDT_filtered_life_stage_updated$Reach)
  
  for(reach_x in unique_reaches_x){
    
    # ----------------- pull all the habitat attributes for that reach ------------
    reach_data_x = Limiting_Factors_Okanogan_EDT_filtered_life_stage_updated[ which(Limiting_Factors_Okanogan_EDT_filtered_life_stage_updated$Reach == reach_x), ]
    
    # ---------------- combine all the Level 2 habitat attributes (with RTT Crosswalk) -----
    level_2_attributes_x = paste(reach_data_x$EDT_Attribute_Level_2_with_RTT_attribute_crosswalk, collapse=",")
    
    # ----------------- get the unique Level 2 habitat attributes --------
    level_2_attributes_x = unlist(strsplit(level_2_attributes_x, ","))
    level_2_attributes_x = unique(level_2_attributes_x)
    level_2_attributes_x = level_2_attributes_x[order(level_2_attributes_x)]
    
    # ----------------- pull level 2 scores for these attributes ------------
    level_2_attributes_x_df = tibble(level_2_attributes_x)
    colnames(level_2_attributes_x_df) = c("EDT Attribute")
    
    # ------------- data frame for Level 2 Habitat Ratings with just the ReachName -------
    HabitatAttribute_Ratings_Level2_updated_SINGLE_REACH = HabitatAttribute_Ratings_Level2_updated[ 
      which(HabitatAttribute_Ratings_Level2_updated$Reach == reach_x), c("EDT Attribute","Level 2 Functional Condition")]
    
    # ------------------------ Pull Level 2 Habitat Attribute scores  --------------------
    level_2_attributes_x_df_merged =  merge(level_2_attributes_x_df, HabitatAttribute_Ratings_Level2_updated_SINGLE_REACH, by = "EDT Attribute") 
  
    # ------------------- just pull the 1s and 3s ----------
    level_2_attributes_x_df_merged = level_2_attributes_x_df_merged[which(level_2_attributes_x_df_merged$`Level 2 Functional Condition` <= Individual_Habitat_Attribute_Score),  ]
    
    # -------------- crosswalk to RTT Habitat Attribute ----------------
    level_2_attributes_x_df_merged = merge(  level_2_attributes_x_df_merged , AttributeCrosswalk_simple, by = "EDT Attribute")
    
    
    # ---------------------------------------
    #    Create Data Frame to merge
    # ---------------------------------------
    if(nrow(level_2_attributes_x_df_merged) > 0){
      
      # --------- Reach Names ---------
      output_for_LF_pathway_x = as.data.frame(reach_x)
      colnames(output_for_LF_pathway_x) = "ReachName"
      # -------------- Basin, species, life stage -------
      output_for_LF_pathway_x$Basin = "Okanogan"
      output_for_LF_pathway_x$species = species
      output_for_LF_pathway_x$life_stage = life_stage_x
      # ---------------- LF scores (these are blank since this analysis is not done in the Okanogan) --------
      output_for_LF_pathway_x$LF_Sum = NA
      output_for_LF_pathway_x$LF_Pct = NA
      output_for_LF_pathway_x$LF_Score_Restoration = NA
      output_for_LF_pathway_x$LF_Score_Protection = NA
      # ----------- list all the habitat attributes that are 1) Unacceptable, 2) At-Risk, 3) Both ------------------
      # ----------- Unacceptable ------------
      unacceptable_attributes_x = level_2_attributes_x_df_merged$RTT_Habitat_Attribute[level_2_attributes_x_df_merged$`Level 2 Functional Condition` == 1]
      unacceptable_attributes_x = paste(unacceptable_attributes_x, collapse = ",")
      output_for_LF_pathway_x$unacceptable_1_indiv_habitat_attributes = unacceptable_attributes_x
      # ------------ At Risk -----------------
      at_risk_attributes_x = level_2_attributes_x_df_merged$RTT_Habitat_Attribute[level_2_attributes_x_df_merged$`Level 2 Functional Condition` > 1 &
                                                                              level_2_attributes_x_df_merged$`Level 2 Functional Condition` <= 3 ]
      at_risk_attributes_x = paste(at_risk_attributes_x, collapse = ",")
      output_for_LF_pathway_x$at_risk_2_or_3_indiv_habitat_attributes = at_risk_attributes_x
      # -------------- Both - Unacceptable and At Risk ----------------
      both_attributes_x = level_2_attributes_x_df_merged$RTT_Habitat_Attribute
      both_attributes_x = paste(both_attributes_x, collapse = ",")
      output_for_LF_pathway_x$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes = both_attributes_x
      
      # ---------------- combine with other reaches-life stages --------
      output_for_LF_pathway_combined = rbind(output_for_LF_pathway_combined,output_for_LF_pathway_x )
      
    }
    
  }
  
  # ------- output -----------
  return(output_for_LF_pathway_combined)
  
}

# ------------------ output data -------------------------
#Habitat_Quality_Scores = as.data.frame(Habitat_Quality_Scores)
#output_path_x =  paste(output_path,'Habitat_Quality_Scores.xlsx', sep="")
#write.xlsx(  Habitat_Quality_Scores,
#  output_path_x, col.names = TRUE,  row.names = FALSE, append = FALSE, showNA = TRUE, password = NULL)

# -----------------------------------------------------------------------------------------
#     Function to apply the AU life stage priority as a filter
# -----------------------------------------------------------------------------------------

 
Life_Stage_Priority_Filter_Function_Okanogan = function(life_stage_x, Limiting_Factors_Okanogan_EDT_filtered_life_stage_x, Life_Stage_Priority, reach_life_stage_presence){
  
  # -------------------------------------------------------
  #     Get reaches with this life stage as the specified life stage priority
  # -------------------------------------------------------
  
  # -------- ONE Life_Stage_Priority -----------
  if(length(Life_Stage_Priority) == 1){
    life_stages_priorities_species_specific_list = Life_Stage_Priorities_AU_and_Reach_data %>%
      filter(Life_Stage_Priorities_AU_and_Reach_data[life_stages_priorities_species_specific[[life_stage_x]]] ==  Life_Stage_Priority)   # 
    
    # -------- TWO Life_Stage_Priority -----------
  }else if(length(Life_Stage_Priority) == 2){
    
    life_stages_priorities_species_specific_list = Life_Stage_Priorities_AU_and_Reach_data %>%
      filter(Life_Stage_Priorities_AU_and_Reach_data[life_stages_priorities_species_specific[[life_stage_x]]] ==  Life_Stage_Priority[1]  |
               Life_Stage_Priorities_AU_and_Reach_data[life_stages_priorities_species_specific[[life_stage_x]]] ==  Life_Stage_Priority[2] )   # 
    
    # -------- THREE Life_Stage_Priority -----------
  }else if(length(Life_Stage_Priority) == 3){
    
    life_stages_priorities_species_specific_list = Life_Stage_Priorities_AU_and_Reach_data %>%
      filter(Life_Stage_Priorities_AU_and_Reach_data[life_stages_priorities_species_specific[[life_stage_x]]] ==  Life_Stage_Priority[1]  |
               Life_Stage_Priorities_AU_and_Reach_data[life_stages_priorities_species_specific[[life_stage_x]]] ==  Life_Stage_Priority[2]  | 
               Life_Stage_Priorities_AU_and_Reach_data[life_stages_priorities_species_specific[[life_stage_x]]] ==  Life_Stage_Priority[3] )   # 
  }
  
  # -------------------------------------------------------
  #     Pull just Okanogan 
  # -------------------------------------------------------
  
  life_stages_priorities_species_specific_list = life_stages_priorities_species_specific_list[which(life_stages_priorities_species_specific_list$Basin == "Okanogan"),]
  
  # -------------------------------------------------------
  #     Pull Reaches with this life stage present
  # -------------------------------------------------------
  life_stages_priorities_species_specific_list = life_stages_priorities_species_specific_list %>%
    filter(life_stages_priorities_species_specific_list[reach_life_stage_presence[[life_stage_x]]] ==  1)   # 
  
  # -------------------------------------------------------
  #    Filter out Limiting Factor data frame to include reaches that made through priority and life stage presence filter
  # -------------------------------------------------------
  Limiting_Factors_Okanogan_EDT_filtered_life_stage_x_2 = as.data.frame(Limiting_Factors_Okanogan_EDT_filtered_life_stage_x) # convert to data frame
  # ------ filter out reaches -----------
  Limiting_Factors_Okanogan_EDT_filtered_life_stage_updated_x = Limiting_Factors_Okanogan_EDT_filtered_life_stage_x_2 %>%  
    filter(Limiting_Factors_Okanogan_EDT_filtered_life_stage_x_2$Reach   %in%   life_stages_priorities_species_specific_list$ReachName)

  
  return(Limiting_Factors_Okanogan_EDT_filtered_life_stage_updated_x)
  
}




# ----------------------------- compare Limiting Factor AND HabitatAttribute scores for same reach and habitat attribute -------
test_x = FALSE
if(test_x){
  
  unique_reaches_x = unique(Limiting_Factors_Okanogan_EDT_filtered_life_stage_x$Reach)
  
  habitat_attribute_x = "Temperature"
  df_compare_LF_HQ = c()
  for(reach_x in unique_reaches_x){
    
    #print(paste("--------------------------- REACH: ", reach_x))
    
    xLF = which(Limiting_Factors_Okanogan_EDT$Reach == reach_x &
                  Limiting_Factors_Okanogan_EDT$Attribute == habitat_attribute_x)
    #print("LF minimum score: ", min(Limiting_Factors_Okanogan_EDT$RTT_Limiting_Factor_Score[xLF]))
    
    xHQ = which(HabitatAttribute_Ratings_Level3$Reach == reach_x &
                  HabitatAttribute_Ratings_Level3$`EDT Attribute` == habitat_attribute_x)
    #print("HQ score: ", HabitatAttribute_Ratings_Level3$`Level 3 Functional Condition`[xHQ])
    
    df_x = t(as.data.frame(c(reach_x,
                             min(Limiting_Factors_Okanogan_EDT$RTT_Limiting_Factor_Score[xLF], na.rm=T),
                             min(HabitatAttribute_Ratings_Level3$`Level 3 Functional Condition`[xHQ], na.rm=T)    )   )  )
    colnames(df_x) = c('Reach','Limiting_Factors_Okanogan_EDT_min_score','Habitat_Attribute_Rating_Level_3_score')
    rownames(df_x) = reach_x
    df_compare_LF_HQ = rbind(df_compare_LF_HQ,  df_x)
    
  } 
  
  
  
}

#  ---------------------------------------------------------------------------------
#
#           FUNCTION to combine main (Methow, Entiat, and Wenatchee) and Okanogan LF Output
#
#  ---------------------------------------------------------------------------------

test_x = FALSE
if(test_x){
  MetEntWen_data_frame = Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']]
  Okanogan_data_frame = Limiting_Factor_Pathway_Steelhead_OKANOGAN[['Limiting_Factor_Pathway_Restoration']]
}

Combine_MetEntWen_and_Okanogan_Limiting_Factor_Output = function(MetEntWen_data_frame, Okanogan_data_frame){
  
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



















#  ---------------------------------------------------------------------------------
#
#          Function to generate life stages for all Okanogan Reaches
#
#  ---------------------------------------------------------------------------------

Generate_Limiting_Factor_Output_Table_Okanogan_ALL = function(species, basins){
  
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
    # ----------------------- life stage presence in reach -------------
    reach_life_stage_presence = steelhead_life_stages_presence
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
  #
  #          Finalize Restoration 
  #
  #  ---------------------------------------------------------------------------------
  
  
  adult_migration_priority = Limiting_Factors_Okanogan_EDT[which(Limiting_Factors_Okanogan_EDT$`RTT Life Stage` == "Adult Migration" & Limiting_Factors_Okanogan_EDT$RTT_Limiting_Factor_Score <= Individual_Habitat_Attribute_Score), ]
  adult_migration_all = Limiting_Factors_Okanogan_EDT[ which(Limiting_Factors_Okanogan_EDT$`RTT Life Stage` == "Adult Migration" ), ]
  
  #  ---------------------------------------------------------------------------------
  #           Only pull 1 (unacceptable) and 3 (at risk) from the RTT Limiting Factor Score 
  #  ---------------------------------------------------------------------------------
  Limiting_Factors_Okanogan_EDT_filtered = Limiting_Factors_Okanogan_EDT[Limiting_Factors_Okanogan_EDT$RTT_Limiting_Factor_Score <= Individual_Habitat_Attribute_Score,]
  
  
  #  ---------------------------------------------------------------------------------
  #          Generate Life Stage Tables and Score for Restoration and Protection (not considering filters above)
  #  ---------------------------------------------------------------------------------
  
  Limiting_Factor_Pathway_Restoration = Generate_Species_Output_Table_Okanogan(species, Limiting_Factors_Okanogan_EDT_filtered, reach_life_stage_presence )
  
  
  #  ---------------------------------------------------------------------------------
  #          Only include reaches that have passed through the previous filters (Species-reaches, # of life stages, AU Restoration priority)
  #  ---------------------------------------------------------------------------------
  
  Limiting_Factor_Pathway_Restoration = Limiting_Factor_Pathway_Restoration %>%  
    filter(ReachName    %in%   Species_Reach_Information_data_restoration$ReachName)
  
  #  ---------------------------------------------------------------------------------
  #          Generate Life Stage Tables and Score for Restoration and Protection (not considering filters above)
  #  ---------------------------------------------------------------------------------
  
  print(paste("TOTAL reaches passing through life stage: ", length(unique(Limiting_Factor_Pathway_Restoration$ReachName)), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #
  #          Finalize Protection 
  #
  #  ---------------------------------------------------------------------------------
  
  # !!!!!! NOTE !!!!!!!!!!!! As of now - not doing protection for the Okanogan with the Limiting Factor Pathway (just for the HQ pathway)
  
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score critera --------------
  
  
  # print(paste("Protection - total after HQ score filter: ", length(unique(Limiting_Factor_Pathway_Protection$ReachName)), sep=""))
  
  
  
  
  
  
  # --------------------- but Restoration and Protection into a list --------------
  Limiting_Factor_Pathway_Output = list( 
    "Limiting_Factor_Pathway_Restoration" = Limiting_Factor_Pathway_Restoration
  )
  
  return(Limiting_Factor_Pathway_Output)
  
  
}





# -----------------------------------------------------------------------------------
#
#             Function to generate habitat attributes for a specific life stage for each reach
#
# -----------------------------------------------------------------------------------

colnames_data_frame_to_merge = colnames(Habitat_Attribute_Scores_for_individual_Life_Stage)


EDT_generate_habitat_attributes_for_a_life_stage <- function(Okanogan_Habitat_Attribute_Scores_for_individual_Life_Stage,  life_stage_x, colnames_data_frame_to_merge){
  
  
  # -----------------------------------------------------------------------------------
  #         Start Output Data Frame
  # -----------------------------------------------------------------------------------
  
  # -----------------------------------------------------------------------------------
  #         Loop through each reach, Pull 1s and 3s (from Level 2 Habitat Attribute) for each Level 2 Habitat Attribute
  # -----------------------------------------------------------------------------------
  
  # ------------------- data frame to combine output for all reaches ---------
  output_for_LF_pathway_combined = c()
  unique_reaches_x = unique(Okanogan_Habitat_Attribute_Scores_for_individual_Life_Stage$Reach)
  
  for(reach_x in unique_reaches_x){
    
    # ---------------------------------------------------
    #     Generate unnacceptable and at risk (1s and 3) for life stage attributes
    #------------------------------------------------
    
    # ----------------- pull all the habitat attributes for that reach ------------
    Okanogan_Habitat_Attribute_Scores_for_individual_Life_Stage_ONE_REACH = Okanogan_Habitat_Attribute_Scores_for_individual_Life_Stage[which(Okanogan_Habitat_Attribute_Scores_for_individual_Life_Stage$Reach == reach_x),]
    
    # ------------------------- run function that generates Level 2 1s and 3s (Unacceptable and At Risk) Attributes -----------
    scores_1s_3s_output_x = pull_Level2_1s_and_3s_from_Level3(Okanogan_Habitat_Attribute_Scores_for_individual_Life_Stage_ONE_REACH, life_stage_x)
    # --------------------- IF no scores_1s_3s_output_x generated, generate an output with NA values -------------------
    if( is.null(scores_1s_3s_output_x) ){
      scores_1s_3s_output_x_i = Okanogan_Habitat_Attribute_Scores_for_individual_Life_Stage_ONE_REACH[1 , c("Reach")]
      colnames(scores_1s_3s_output_x_i) = "ReachName"
      scores_1s_3s_output_x_i$Basin = "Okanogan"
      scores_1s_3s_output_x_i$species = "Steelhead"
      scores_1s_3s_output_x_i$life_stage = life_stage_x
      scores_1s_3s_output_x_i$LF_Sum = NA
      scores_1s_3s_output_x_i$LF_Pct = NA
      scores_1s_3s_output_x_i$LF_Score_Restoration = NA
      scores_1s_3s_output_x_i$LF_Score_Protection  = NA
      scores_1s_3s_output_x_i$unacceptable_1_indiv_habitat_attributes = NA
      scores_1s_3s_output_x_i$at_risk_2_or_3_indiv_habitat_attributes = NA
      scores_1s_3s_output_x_i$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes = NA
      
      scores_1s_3s_output_x = scores_1s_3s_output_x_i
    }
    
    # ---------------------------------------------------
    #     Generate scores for attributes that overlap with RTT life stage attributes
    #------------------------------------------------
    # ---------------- combine all the Level 2 habitat attributes (with RTT Crosswalk) -----
    level_2_attributes_x = paste(Okanogan_Habitat_Attribute_Scores_for_individual_Life_Stage_ONE_REACH$EDT_Attribute_Level_2_with_RTT_attribute_crosswalk, collapse=",")
    
    # ----------------- get the unique Level 2 habitat attributes --------
    level_2_attributes_x = unlist(strsplit(level_2_attributes_x, ","))
    level_2_attributes_x = unique(level_2_attributes_x)
    level_2_attributes_x = level_2_attributes_x[order(level_2_attributes_x)]
    
    # ----------------- pull level 2 scores for these attributes ------------
    level_2_attributes_x_df = tibble(level_2_attributes_x)
    colnames(level_2_attributes_x_df) = c("EDT Attribute")
    
    # ------------- data frame for Level 2 Habitat Ratings with just the ReachName -------
    HabitatAttribute_Ratings_Level2_updated_SINGLE_REACH = HabitatAttribute_Ratings_Level2_updated[ 
      which(HabitatAttribute_Ratings_Level2_updated$Reach == reach_x), c("EDT Attribute","Level 2 Functional Condition")]
    
    # ------------------------ Pull Level 2 Habitat Attribute scores  --------------------
    level_2_attributes_x_df_merged =  merge(level_2_attributes_x_df, HabitatAttribute_Ratings_Level2_updated_SINGLE_REACH, by = "EDT Attribute") 
    
    # -------------- crosswalk to RTT Habitat Attribute ----------------
    level_2_attributes_x_df_merged = merge(  level_2_attributes_x_df_merged , AttributeCrosswalk_simple, by = "EDT Attribute")
    
    # ---------------------------------------
    #    Prepare data to output and merge with data frame (Habitat_Attribute_Scores_for_individual_Life_Stage)
    # ---------------------------------------
    
    # --------- Reach Names ---------
    output_for_LF_pathway_x = as.data.frame(reach_x)
    colnames(output_for_LF_pathway_x) = "ReachName"
    
    # ----------------------- identify which rows line up with columns in Habitat_Attribute_Scores_for_individual_Life_Stage ----------
    for(column_x in colnames_data_frame_to_merge[2:length(colnames_data_frame_to_merge)]){
      
 
      # ----------- if RTT habitat attribute from Okanogan data matches up with columns ------------
      if( any(level_2_attributes_x_df_merged$RTT_Habitat_Attribute == column_x)  ){
        
        # ------------------- add level 2 functional conditoin (habitat attribute score)
        row_x = which(level_2_attributes_x_df_merged$RTT_Habitat_Attribute == column_x)
        new_col_x = as.data.frame(min(as.data.frame(level_2_attributes_x_df_merged$`Level 2 Functional Condition`[row_x]), na.rm=T))
        
        colnames(new_col_x) = column_x
        output_for_LF_pathway_x = cbind(output_for_LF_pathway_x, new_col_x)
      
      # --------------- if one of 1s and 3s output (will be NA if all level 2 functional condition were 5) -----
      }else if( any( colnames(scores_1s_3s_output_x) == column_x)  ){
        
        # ------------------- add level 2 functional conditoin (habitat attribute score)
        col_x = which( colnames(scores_1s_3s_output_x) == column_x)
        new_col_x = as.data.frame(scores_1s_3s_output_x[,col_x])
        colnames(new_col_x) = column_x
        output_for_LF_pathway_x = cbind(output_for_LF_pathway_x, new_col_x)
        
        
        # -------- IF no column ------------
      }else{
        
        new_col_x = as.data.frame(NA)
        colnames(new_col_x) = column_x
        output_for_LF_pathway_x = cbind(output_for_LF_pathway_x, new_col_x)
        
      }

    }
    # ----------------- add column names ------------
    colnames(output_for_LF_pathway_x) = colnames_data_frame_to_merge
    # -------------- add all the EDT Level 2 functional conditions into one cell
    if(nrow(level_2_attributes_x_df_merged)>0){
      combine_attributes_and_scores = mapply(paste, sep = ": ", level_2_attributes_x_df_merged[,c("RTT_Habitat_Attribute")], level_2_attributes_x_df_merged[,c("Level 2 Functional Condition")])
      combine_attributes_and_scores = paste(combine_attributes_and_scores, sep=" ", collapse=", ")
      
    # ------------ if no functional condition (level 2) scores for this reach -----
    }else{
      combine_attributes_and_scores = "no level 2 functional condition scores for this life stage in this reach"
    }
    output_for_LF_pathway_x$all_functional_condition_for_life_stage = combine_attributes_and_scores
    output_for_LF_pathway_combined = rbind(output_for_LF_pathway_combined,output_for_LF_pathway_x )
  }
  
  #colnames(output_for_LF_pathway_combined)[1:length(colnames_data_frame_to_merge)] = colnames_data_frame_to_merge
  # ------- output -----------
  #output_for_LF_pathway_combined = as.tibble(output_for_LF_pathway_combined)
  #output_for_LF_pathway_combined <- data.frame(lapply(output_for_LF_pathway_combined, as.character), stringsAsFactors=FALSE)
  
  return(output_for_LF_pathway_combined)
  
}


