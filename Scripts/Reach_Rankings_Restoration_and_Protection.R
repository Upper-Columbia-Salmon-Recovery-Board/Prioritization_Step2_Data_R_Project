

# ---------------------------------------------------------------------------
#
#      SCRIPT: Generate Restoration Scores
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
#   Create A Function to output habitat attribute data FROM Habitat Data Raw
#
# ---------------------------------------------------------------------------


#  to test
test = TRUE
if(test){
  species = "Spring Chinook"
  basins = c("Methow",  "Entiat","Wenatchee")
}



Generate_Restoration_or_Protection_Reach_Rankings_Table = function( basins ){
  
  # ------------------------------------------------------------------------------
  #       Establish species-specific variable names
  # ------------------------------------------------------------------------------

  # -------------------- SPRING CHINOOK ---------------------------
  # ---------------- species reach ---------------
  species_reach_Spring_Chinook = 'Spring.Chinook.Reach'
  # ---------------- species AU Rank RESTORATION ----------
  AU_rank_name_restoration_Spring_Chinook = 'SPCHNTier_Restoration'
  # ---------------- species AU Rank PROTECTION ----------
  AU_rank_name_protection_Spring_Chinook = 'SPCHNTier_Protection'
  # --------------- Life stage Sum column name ----------
  life_stage_sum_column_Spring_Chinook = 'SPCH_Life_Stage_Sum'
  # ---------------- Limiting Factor Data frame ---------------
  Limiting_Factor_Restoration_data_frame = Limiting_Factor_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']]
  Limiting_Factor_Protection_data_frame = Limiting_Factor_Spring_Chinook[['Limiting_Factor_Pathway_Protection']]
  # ------------------- output names ----------------------
  restoration_output_name = paste(paste("Spring_Chinook_Reach_Scoring_RESTORATION" , 
                                        paste(basins_to_include, collapse = "_"), sep="_"),
                                  ".xlsx", sep="")
  protection_output_name = paste(paste("Spring_Chinook_Reach_Scoring_PROTECTION" , 
                                       paste(basins_to_include, collapse = "_"), sep="_"),
                                 ".xlsx", sep="")
    
  # -------------------- STEELHEAD -------------------------------------------
  # ---------------- species reach ---------------
  species_reach_Steelhead = 'Steelhead.Reach'
  # ---------------- species AU Rank ----------
  AU_rank_name_restoration_Steelhead = 'STLTier_Restoration'
  # ---------------- species AU Rank PROTECTION ----------
  AU_rank_name_protection_Steelhead = 'STLTier_Protection'
  # --------------- Life stage Sum column name ----------
  life_stage_sum_column_Steelhead = 'SH_Life_Stage_Sum'
  # ---------------- Limiting Factor Data frame ---------------
  Limiting_Factor_Restoration_data_frame = Limiting_Factor_Steelhead[['Limiting_Factor_Pathway_Restoration']]
  Limiting_Factor_Protection_data_frame = Limiting_Factor_Steelhead[['Limiting_Factor_Pathway_Protection']]
  # ------------------- output names ----------------------
  restoration_output_name = paste(paste("Steelhead_Reach_Scoring_RESTORATION" , 
                                        paste(basins_to_include, collapse = "_"), sep="_"),
                                  ".xlsx", sep="")
  protection_output_name = paste(paste("Steelhead_Reach_Scoring_PROTECTION" , 
                                       paste(basins_to_include, collapse = "_"), sep="_"),
                                 ".xlsx", sep="")
    
  if(exclude_bull_trout == "no"){
    # ---------------- species reach ---------------
    species_reach_Bull_Trout = 'Bull.Trout.Reach'
    # ---------------- species AU Rank ----------
    AU_rank_name_restoration_Bull_Trout = 'BTTier_Restoration'
    # ---------------- species AU Rank PROTECTION ----------
    AU_rank_name_protection_Bull_Trout = 'BTTier_Protection'
    # --------------- Life stage Sum column name ----------
    life_stage_sum_column_Bull_Trout = 'BT_Life_Stage_Sum'
    # ---------------- Limiting Factor Data frame ---------------
    Limiting_Factor_Restoration_data_frame = Limiting_Factor_Bull_Trout[['Limiting_Factor_Pathway_Restoration']]
    Limiting_Factor_Protection_data_frame = Limiting_Factor_Bull_Trout[['Limiting_Factor_Pathway_Protection']]
    # ------------------- output names ----------------------
    restoration_output_name = paste(paste("Bull_Trout_Reach_Scoring_RESTORATION" , 
                                          paste(basins_to_include, collapse = "_"), sep="_"),
                                    ".xlsx", sep="")
    protection_output_name = paste(paste("Bull_Trout_Reach_Scoring_PROTECTION" , 
                                         paste(basins_to_include, collapse = "_"), sep="_"),
                                   ".xlsx", sep="")
    
    
  }
  
  #  ---------------------------------------------------------------------------------
  #           Establish Reach Information Data Frame Just for this Output
  #  ---------------------------------------------------------------------------------
  # -------- Reach Information data frame for this species -----------
  Species_Reach_Information_data = Reach_Information_data
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
  
  # ---------- AU Ranks data frame for this species ---------
  Species_AU_Ranks_data_Spring_Chinook = AU_Ranks_data
  Species_AU_Ranks_data_Steelhead = AU_Ranks_data
  Species_AU_Ranks_data_Bull_Trout = AU_Ranks_data
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to only have Species-specific reaches   
  #  ---------------------------------------------------------------------------------
  # -------------------- SPRING CHINOOK: add additional column for this particular species reach presence ---------------
  Spring_Chinook_Reach_Information_data = Species_Reach_Information_data
  Spring_Chinook_Reach_Information_data$Species_Reaches = Spring_Chinook_Reach_Information_data[species_reach_Spring_Chinook]
  # ----------------------- filter out for only reaches with this species --------------
  Spring_Chinook_Reach_Information_data = Spring_Chinook_Reach_Information_data %>%  
    filter(Species_Reaches   == 'yes')
  print(paste("Total reaches after Spring Chinook species-reach filter: ", nrow(Spring_Chinook_Reach_Information_data), sep=""))
  
  # -------------------- STEELHEAD: add additional column for this particular species reach presence ---------------
  Steelhead_Reach_Information_data =  Species_Reach_Information_data
  Steelhead_Reach_Information_data$Species_Reaches = Steelhead_Reach_Information_data[species_reach_Steelhead]
  # ----------------------- filter out for only reaches with this species --------------
  Steelhead_Reach_Information_data = Steelhead_Reach_Information_data %>%  
    filter(Species_Reaches   == 'yes')
  print(paste("Total reaches after Steelhead species-reach filter: ", nrow(Steelhead_Reach_Information_data), sep=""))
  
  if(exclude_bull_trout == "no"){
    # -------------------- BULL TROUT: add additional column for this particular species reach presence ---------------
    Bull_Trout_Reach_Information_data$Species_Reaches = Species_Reach_Information_data[species_reach_Bull_Trout]
    # ----------------------- filter out for only reaches with this species --------------
    Bull_Trout_Reach_Information_data = Bull_Trout_Reach_Information_data %>%  
      filter(Species_Reaches   == 'yes')
    print(paste("Total reaches after Bull Trout species-reach filter: ", nrow(Bull_Trout_Reach_Information_data), sep=""))
    
  }
  
  # ---------------------------------------------------------------------------------------------------------------
  #
  #     Apply Filters 
  #
  # ---------------------------------------------------------------------------------------------------------------

  #  ---------------------------------------------------------------------------------
  #            Filter out to select for AU rank - RESTORATION
  #  ---------------------------------------------------------------------------------
  
  # ------------------- establish AU Rank ------------------
  AU_Rank_Restoration = Restoration_Reach_Scoring[which(Restoration_Reach_Scoring$Indicator == "AU_Ranking" &
                                                          Restoration_Reach_Scoring$Category_Stage == "filter"),"Category"]
  
  # --------------------------- SPRING CHINOOK -----------------
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_AU_Ranks_data_Spring_Chinook ["Species_AU_Ranks"] = Species_AU_Ranks_data_Spring_Chinook [AU_rank_name_restoration_Spring_Chinook]
  # ----------------------- filter out for only reaches with this species --------------
  Species_AU_Ranks_data_Spring_Chinook_restoration = Species_AU_Ranks_data_Spring_Chinook  %>%  
    filter(Species_AU_Ranks    %in%   AU_Rank_Restoration)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Spring_Chinook_Reach_Information_data_restoration = Spring_Chinook_Reach_Information_data %>%  
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data_Spring_Chinook_restoration$`Assessment Unit`)
  
  print(paste("Spring Chinook Restoration - total AU rank filter: ", nrow(Spring_Chinook_Reach_Information_data_restoration), sep=""))
  
  # --------------------------- STEELHEAD -----------------
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_AU_Ranks_data_Steelhead ["Species_AU_Ranks"] = Species_AU_Ranks_data_Steelhead [AU_rank_name_restoration_Steelhead]
  # ----------------------- filter out for only reaches with this species --------------
  Species_AU_Ranks_data_Steelhead_restoration = Species_AU_Ranks_data_Steelhead  %>%  
    filter(Species_AU_Ranks    %in%   AU_Rank_Restoration)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Steelhead_Reach_Information_data_restoration = Steelhead_Reach_Information_data %>%  
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data_Steelhead_restoration$`Assessment Unit`)
  
  print(paste("Steelhead Restoration - total AU rank filter: ", nrow(Steelhead_Reach_Information_data_restoration), sep=""))
  
  # --------------------------- BULL TROUT -----------------
  if(exclude_bull_trout == "no"){
    
    # -------------------- add additional column for this particular species reach presence ---------------
    Species_AU_Ranks_data_Bull_Trout ["Species_AU_Ranks"] = Species_AU_Ranks_data_Bull_Trout [AU_rank_name_restoration_Bull_Trout]
    # ----------------------- filter out for only reaches with this species --------------
    Species_AU_Ranks_data_Bull_Trout_restoration = Species_AU_Ranks_data_Bull_Trout  %>%  
      filter(Species_AU_Ranks    %in%   AU_Rank_Restoration)
    # ------------------------ identify AUs that pass this filter in reach-based table ----------
    Bull_Trout_Reach_Information_data_restoration = Bull_Trout_Reach_Information_data %>%  
      filter(Assessment.Unit    %in%   Species_AU_Ranks_data_Bull_Trout_restoration$`Assessment Unit`)
    
    print(paste("Bull Trout Restoration - total AU rank filter: ", nrow(Bull_Trout_Reach_Information_data_restoration), sep=""))
  }

  #  ---------------------------------------------------------------------------------
  #            Filter out to select for AU rank - PROTECTION
  #  ---------------------------------------------------------------------------------
  
  # !!!!!!!!! Need to divie by species 
  
  # ------------------- establish AU Rank ------------------
  AU_Rank_Protection = Protection_Reach_Scoring[which(Protection_Reach_Scoring$Indicator == "AU_Ranking"  &
                                                       Protection_Reach_Scoring$Category_Stage == "filter"),"Category"]
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_AU_Ranks_data["Species_AU_Ranks"] = Species_AU_Ranks_data[AU_rank_name_protection]
  # ----------------------- filter out for only reaches with this species --------------
  Species_AU_Ranks_data_protection = Species_AU_Ranks_data %>%  
    filter(Species_AU_Ranks    %in%   AU_Rank_Protection)
  # ------------------------ identify after AUs that pass this filter in reach-based table ----------
  Species_Reach_Information_data_protection = Species_Reach_Information_data %>%  
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data_protection$`Assessment Unit`)
  
  print(paste("Protection - total after AU rank filter: ", nrow(Species_Reach_Information_data_protection), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to select for Habitat Quality RESTORATION score
  #  ---------------------------------------------------------------------------------
  
  # ------------------- establish Habitat Quality Score cutoff ------------------
  HQ_Score_Restoration_Reach_Scores = Restoration_Reach_Scoring[which(Restoration_Reach_Scoring$Indicator == "Habitat_Quality_Score" &
                                                          Restoration_Reach_Scoring$Category_Stage == "filter"),c("Category_lower_limit","Category_upper_limit")]
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  Habitat_Quality_Scores_Restoration = Habitat_Quality_Scores %>%  
    filter(HQ_Score_Restoration   >=   HQ_Score_Restoration_Reach_Scores$Category_lower_limit) %>%
    filter(HQ_Score_Restoration   <  HQ_Score_Restoration_Reach_Scores$Category_upper_limit)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  # ----------------------- SPRING CHINOOK ---------------------
  Habitat_Quality_Pathway_Restoration_Spring_Chinook = Habitat_Quality_Scores_Restoration %>%  
    filter(ReachName   %in%   Spring_Chinook_Reach_Information_data_restoration$`ReachName`)
  # ----------------------- STEELHEAD ---------------------
  Habitat_Quality_Pathway_Restoration_Steelhead = Habitat_Quality_Scores_Restoration %>%  
    filter(ReachName   %in%   Steelhead_Reach_Information_data_restoration$`ReachName`)
  # ----------------------- BULL TROUT---------------------
  if(exclude_bull_trout == "no"){
    Habitat_Quality_Pathway_Restoration_Bull_Trout = Habitat_Quality_Scores_Restoration %>%  
      filter(ReachName   %in%   Bull_Trout_Reach_Information_data_restoration$`ReachName`)
  }

  print(paste("Spring Chinook Restoration - total after HQ score filter: ", nrow(Habitat_Quality_Pathway_Restoration_Spring_Chinook), sep=""))
  print(paste("Steelhead Restoration - total after HQ score filter: ", nrow(Habitat_Quality_Pathway_Restoration_Steelhead), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to select for Habitat Quality PROTECTION score
  #  ---------------------------------------------------------------------------------
  
  # !!!!!!!!!!!!!!!!!! need to divide by species -------
  
  # ------------------- establish Habitat Quality Score cutoff ------------------
  HQ_Score_Protection_Reach_Scores = Protection_Reach_Scoring[which(Protection_Reach_Scoring$Indicator == "Habitat_Quality_Score" &
                                                                      Protection_Reach_Scoring$Category_Stage == "filter"),c("Category_lower_limit","Category_upper_limit")]
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  Habitat_Quality_Scores_Protection = Habitat_Quality_Scores %>%  
    filter(HQ_Score_Protection   >=   HQ_Score_Protection_Reach_Scores$Category_lower_limit) %>%
    filter(HQ_Score_Protection   <  HQ_Score_Protection_Reach_Scores$Category_upper_limit)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Habitat_Quality_Pathway_Protection = Habitat_Quality_Scores_Protection %>%  
    filter(ReachName   %in%   Species_Reach_Information_data_protection$`ReachName`)
  
  print(paste("Protection - total after HQ score filter: ", nrow(Habitat_Quality_Pathway_Protection), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #         NEED to add an "OR" filter within the HQ_Protection 
  #       Tracy: ">70% quality for high priority life stages"
  #  ---------------------------------------------------------------------------------
  
  #  ---------------------------------------------------------------------------------
  #           Reach Confinement  - RESTORATION
  #  ---------------------------------------------------------------------------------
  
  # ------------------------- Confinement criteria --------------------
  Reach_Confinement_Criteria_Restoration_Reach_Rankings = Restoration_Reach_Scoring[which(Restoration_Reach_Scoring$Indicator == "Confinement" &
                                                                                                  Restoration_Reach_Scoring$Category_Stage == "filter"),c("Category_lower_limit","Category_upper_limit")]
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  Confinement_Scores_Restoration = Confinement_Scores %>%  
    filter(Confined_Pct    >=   Reach_Confinement_Criteria_Restoration_Reach_Rankings$Category_lower_limit  ) %>%
    filter(Confined_Pct    <   Reach_Confinement_Criteria_Restoration_Reach_Rankings$Category_upper_limit)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Habitat_Quality_Pathway_Restoration_Spring_Chinook = Habitat_Quality_Pathway_Restoration_Spring_Chinook %>%  
    filter(ReachName   %in%   Confinement_Scores_Restoration$`ReachName`)
  Habitat_Quality_Pathway_Restoration_Steelhead = Habitat_Quality_Pathway_Restoration_Steelhead %>%  
    filter(ReachName   %in%   Confinement_Scores_Restoration$`ReachName`)
  
  if(exclude_bull_trout == "no"){
    Habitat_Quality_Pathway_Restoration_Bull_Trout = Habitat_Quality_Pathway_Restoration_Bull_Trout %>%  
      filter(ReachName   %in%   Confinement_Scores_Restoration$`ReachName`)
  }
  print(paste("HQ Pathway-RESTORATION Spring Chinook - total reaches after reach confinement filter: ", nrow(Habitat_Quality_Pathway_Restoration_Spring_Chinook), sep=""))
  print(paste("HQ Pathway-RESTORATION Steelhead - total reaches after reach confinement filter: ", nrow(Habitat_Quality_Pathway_Restoration_Steelhead), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #           Reach Confinement  - PROTECTION
  #  ---------------------------------------------------------------------------------
  
  # !!!!!!!!!!!!!!!!!!! need to divide by species 
  
  # ------------------------- Confinement criteria --------------------
  Reach_Confinement_Criteria_Protection_Reach_Rankings = Protection_Reach_Scoring[which(Protection_Reach_Scoring$Indicator == "Confinement" &
                                                                                          Protection_Reach_Scoring$Category_Stage == "filter"),c("Category_lower_limit","Category_upper_limit")]
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  Confinement_Scores_Protection = Confinement_Scores %>%  
    filter(Confined_Pct    >=   Reach_Confinement_Criteria_Protection_Reach_Rankings$Category_lower_limit  ) %>%
    filter(Confined_Pct    <   Reach_Confinement_Criteria_Protection_Reach_Rankings$Category_upper_limit)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Habitat_Quality_Pathway_Protection = Habitat_Quality_Pathway_Protection %>%  
    filter(ReachName   %in%   Confinement_Scores_Protection$`ReachName`)
  
  print(paste("HQ Pathway-PROTECTION- total reaches after reach confinement filter: ", nrow(Habitat_Quality_Pathway_Protection), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #           Number of Life Stages Filter (Restoration and Protection)
  #  ---------------------------------------------------------------------------------
  
  # ----------- generate life stage filter -------------
  Life_Stage_Priorities_AU_and_Reach_data_FILTER_Restoration = Restoration_Reach_Scoring[which(Restoration_Reach_Scoring$Indicator == "Number_Life_Stage_Presence" &
                                                                                                 Restoration_Reach_Scoring$Category_Stage == "filter"),c("Category_lower_limit","Category_upper_limit")]
  Life_Stage_Priorities_AU_and_Reach_data_FILTER_Protection = Protection_Reach_Scoring[which(Protection_Reach_Scoring$Indicator == "Number_Life_Stage_Presence" &
                                                                                               Protection_Reach_Scoring$Category_Stage == "filter"),c("Category_lower_limit","Category_upper_limit")]
  
  # -------------------- add additional column for this particular species reach presence ---------------
  Life_Stage_Priorities_AU_and_Reach_data["Life_Stage_Sum_Column_Spring_Chinook"] = Life_Stage_Priorities_AU_and_Reach_data[life_stage_sum_column_Spring_Chinook]
  Life_Stage_Priorities_AU_and_Reach_data["Life_Stage_Sum_Column_Steelhead"] = Life_Stage_Priorities_AU_and_Reach_data[life_stage_sum_column_Steelhead]
  if(exclude_bull_trout == "no"){
    Life_Stage_Priorities_AU_and_Reach_data["Life_Stage_Sum_Column_Bull_Trout"] = Life_Stage_Priorities_AU_and_Reach_data[life_stage_sum_column_Bull_Trout]
  }
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  x = which(  Life_Stage_Priorities_AU_and_Reach_data["Life_Stage_Sum_Column_Spring_Chinook"]   >=   Life_Stage_Priorities_AU_and_Reach_data_FILTER_Restoration$Category_lower_limit  |
                Life_Stage_Priorities_AU_and_Reach_data["Life_Stage_Sum_Column_Steelhead"]   >=   Life_Stage_Priorities_AU_and_Reach_data_FILTER_Restoration$Category_lower_limit   )
  Life_Stage_Priorities_AU_and_Reach_data_FILTERED_Restoration = Life_Stage_Priorities_AU_and_Reach_data[x,]
  if(exclude_bull_trout == "no"){
    x = which(  Life_Stage_Priorities_AU_and_Reach_data["Life_Stage_Sum_Column_Bull_Trout"]   >=   Life_Stage_Priorities_AU_and_Reach_data_FILTER_Restoration$Category_lower_limit  )
    Life_Stage_Priorities_AU_and_Reach_data_FILTERED_Restoration = Life_Stage_Priorities_AU_and_Reach_data[x,]
  }
  
  # -- !!!!!!!!!!!!!! need to get all Species in protection
  Life_Stage_Priorities_AU_and_Reach_data_FILTERED_Protection = Life_Stage_Priorities_AU_and_Reach_data %>%  
    filter(Life_Stage_Sum_Column   >=   Life_Stage_Priorities_AU_and_Reach_data_FILTER_Protection$Category_lower_limit)
  
  # ------------------------ identify reaches that pass through the RESTORATION filter ----------
  Habitat_Quality_Pathway_Restoration_Spring_Chinook = Habitat_Quality_Pathway_Restoration_Spring_Chinook %>%  
    filter(ReachName   %in%   Life_Stage_Priorities_AU_and_Reach_data_FILTERED_Restoration$`ReachName`)
  Habitat_Quality_Pathway_Restoration_Steelhead = Habitat_Quality_Pathway_Restoration_Steelhead %>%  
    filter(ReachName   %in%   Life_Stage_Priorities_AU_and_Reach_data_FILTERED_Restoration$`ReachName`)
  if(exclude_bull_trout == "no"){
    Habitat_Quality_Pathway_Restoration_Bull_Trout = Habitat_Quality_Pathway_Restoration_Bull_Trout %>%  
      filter(ReachName   %in%   Life_Stage_Priorities_AU_and_Reach_data_FILTERED_Restoration$`ReachName`)
  }
  
  
  # ------------------------ identify reaches that pass through the PROTECTION filter ----------
  Habitat_Quality_Pathway_Protection = Habitat_Quality_Pathway_Protection %>%  
    filter(ReachName   %in%   Life_Stage_Priorities_AU_and_Reach_data_FILTERED_Protection$`ReachName`)
  
  print(paste("HQ Pathway-RESTORATION Spring Chinook - total reaches after life stages number filter: ", nrow(Habitat_Quality_Pathway_Restoration_Spring_Chinook), sep=""))
  print(paste("HQ Pathway-RESTORATION Steelhead - total reaches after life stages number filter: ", nrow(Habitat_Quality_Pathway_Restoration_Steelhead), sep=""))
  
  print(paste("HQ Pathway-PROTECTION - total reaches after life stages number filter: ", nrow(Habitat_Quality_Pathway_Protection), sep=""))
  
  # -------------------------- COMBINE across the species -----------------------------
  Habitat_Quality_Pathway_Restoration = Habitat_Quality_Pathway_Restoration_Spring_Chinook
  
  # -------- reaches in Steelhead (that are not in Spring Chinook) -------
  reaches_additional_Steelhead = setdiff(Habitat_Quality_Pathway_Restoration_Steelhead$ReachName,
            Habitat_Quality_Pathway_Restoration_Spring_Chinook$ReachName)
  x = Habitat_Quality_Pathway_Restoration_Steelhead$ReachName %in% reaches_additional_Steelhead 
  Habitat_Quality_Pathway_Restoration_Steelhead_X = Habitat_Quality_Pathway_Restoration_Steelhead[x,]

  # ----------- combine ---------------
  Habitat_Quality_Pathway_Restoration = rbind(Habitat_Quality_Pathway_Restoration,Habitat_Quality_Pathway_Restoration_Steelhead_X )
  
  # --------------- add Bull Trout -----------------
  if(exclude_bull_trout == "no"){
    # -------- reaches in Bull Trout (that are not in Spring Chinook) -------
    reaches_additional_Bull_Trout = setdiff(Habitat_Quality_Pathway_Restoration_Bull_Trout$ReachName,
                                            Habitat_Quality_Pathway_Restoration$ReachName)
    x = Habitat_Quality_Pathway_Restoration_Steelhead$ReachName %in% reaches_additional_Bull_Trout 
    Habitat_Quality_Pathway_Restoration_Bull_Trout_X = Habitat_Quality_Pathway_Restoration_Bull_Trout[x,]
    
    # ----------- combine ---------------
    Habitat_Quality_Pathway_Restoration = rbind(Habitat_Quality_Pathway_Restoration,Habitat_Quality_Pathway_Restoration_Bull_Trout_X )
    
  }
  
  # -------------------------------------------------------
  #
  #
  #     Generate Scores
  #
  #
  # ------------------------------------------------------- 
  
  
  # ----------------------------------------------------------------------------------- 
  #
  #         RESTORATION
  #
  # ----------------------------------------------------------------------------------- 
  
  
  if( nrow(Habitat_Quality_Pathway_Restoration) > 0 ){
    
    # ----------------------------------------------------------------------------------- 
    #        Initiate Data Frame
    # ----------------------------------------------------------------------------------- 
    Restoration_Scores_Output = Habitat_Quality_Pathway_Restoration[,c("ReachName","Basin","Assessment.Unit")]
    
    # ----------------------------------------------------------------------------------- 
    #
    #         Confinement Scores
    #
    # ----------------------------------------------------------------------------------- 

    # ------------------ add Confinement Percent ----------
    Restoration_Scores_Output = left_join(Restoration_Scores_Output, 
                                          Confinement_Scores_Restoration[,c("ReachName","Unconfined_Pct")], by = c("ReachName" = "ReachName"))
    # ------------------ indicator ratings  -----------------------
    confinement_metric_data = Restoration_Reach_Scoring[which(Restoration_Reach_Scoring$Indicator == "Confinement" &
                                                                Restoration_Reach_Scoring$Category_Stage == "indicator"),c("Category_lower_limit","Category_upper_limit", "Score")]
    # ----------------- generate score --------------------
    Restoration_Scores_Output = Restoration_Scores_Output  %>%
      mutate(Confinement_SCORES = ifelse(Unconfined_Pct  >= confinement_metric_data$Category_lower_limit[1] & 
                                           Unconfined_Pct  <= confinement_metric_data$Category_upper_limit[1] , confinement_metric_data$Score[1],
                            ifelse(Unconfined_Pct  > confinement_metric_data$Category_lower_limit[2] & 
                                     Unconfined_Pct  <= confinement_metric_data$Category_upper_limit[2] , confinement_metric_data$Score[2],
                                   ifelse(Unconfined_Pct  >= confinement_metric_data$Category_lower_limit[3] & 
                                            Unconfined_Pct  <= confinement_metric_data$Category_upper_limit[3] , confinement_metric_data$Score[3],
                                   NA))))
    
    # ----------------------------------------------------------------------------------- 
    #
    #         Habitat Quality Scores
    #
    # ----------------------------------------------------------------------------------- 
    
    # ------------------ add Habitat Quality Scores ----------
    Habitat_Quality_Pathway_Restoration$Habitat_Quality_Percent = Habitat_Quality_Pathway_Restoration$HQ_Pct*100  # need to establish as percent
    Restoration_Scores_Output = left_join(Restoration_Scores_Output, 
                                          Habitat_Quality_Pathway_Restoration[,c("ReachName","Habitat_Quality_Percent")], by = c("ReachName" = "ReachName"))
    # ------------------ indicator ratings  -----------------------
    habitat_quality_metric_data = Restoration_Reach_Scoring[which(Restoration_Reach_Scoring$Indicator == "Habitat_Quality" &
                                                                Restoration_Reach_Scoring$Category_Stage == "indicator"),c("Category_lower_limit","Category_upper_limit", "Score")]
    # ----------------- generate score --------------------
    Restoration_Scores_Output = Restoration_Scores_Output  %>%
      mutate(Habitat_Quality_SCORES = ifelse(Habitat_Quality_Percent  >= habitat_quality_metric_data$Category_lower_limit[1] & 
                                              Habitat_Quality_Percent  <= habitat_quality_metric_data$Category_upper_limit[1] , habitat_quality_metric_data$Score[1],
                            ifelse(Habitat_Quality_Percent  > habitat_quality_metric_data$Category_lower_limit[2] & 
                                     Habitat_Quality_Percent  <= habitat_quality_metric_data$Category_upper_limit[2] , habitat_quality_metric_data$Score[2],
                                   ifelse(Habitat_Quality_Percent  >= habitat_quality_metric_data$Category_lower_limit[3] & 
                                            Habitat_Quality_Percent  <= habitat_quality_metric_data$Category_upper_limit[3] , habitat_quality_metric_data$Score[3],
                                          NA))))
    
    # ----------------------------------------------------------------------------------- 
    #
    #         Limiting Factors for High Priority Life Stages 
    #
    # ----------------------------------------------------------------------------------- 
    
    # ------------------ add Habitat Quality Scores ----------
    Limiting_Factor_Output = FUNCTION_calc_Limiting_Factor_Score(Habitat_Quality_Pathway_Restoration)
    
    Restoration_Scores_Output = left_join(Restoration_Scores_Output, 
                                          Limiting_Factor_Output, by = c("ReachName" = "ReachName"))
    # ------------------ indicator ratings  -----------------------
    limiting_factor_metric_data = Restoration_Reach_Scoring[which(Restoration_Reach_Scoring$Indicator == "Limiting_Factor_for_High_Priority_Life_Stages" &
                                                                    Restoration_Reach_Scoring$Category_Stage == "indicator"),c("Category_lower_limit","Category_upper_limit", "Score")]
    # ----------------- generate score --------------------
    Restoration_Scores_Output = Restoration_Scores_Output  %>%
      mutate(Limiting_Factor_SCORES = ifelse(Limiting_Factor_Score_Percent  >= limiting_factor_metric_data$Category_lower_limit[1] & 
                                              Limiting_Factor_Score_Percent  <= limiting_factor_metric_data$Category_upper_limit[1] , limiting_factor_metric_data$Score[1],
                                            ifelse(Limiting_Factor_Score_Percent  > limiting_factor_metric_data$Category_lower_limit[2] & 
                                                     Limiting_Factor_Score_Percent  <= limiting_factor_metric_data$Category_upper_limit[2] , limiting_factor_metric_data$Score[2],
                                                   ifelse(Limiting_Factor_Score_Percent  >= limiting_factor_metric_data$Category_lower_limit[3] & 
                                                            Limiting_Factor_Score_Percent  <= limiting_factor_metric_data$Category_upper_limit[3] , limiting_factor_metric_data$Score[3],
                                                          NA))))
    # ----------- set NAs to 1 -----------
    x = which(is.na(Restoration_Scores_Output$Limiting_Factor_SCORES ))
    Restoration_Scores_Output$Limiting_Factor_SCORES[x] = 1
    
    # ----------------------------------------------------------------------------------- 
    #
    #         Number of Life Stages Present 
    #
    # ----------------------------------------------------------------------------------- 
    
    # ------------------------ get the maximum life stage sum number across species -------------------
    if(exclude_bull_trout == "no"){
      Life_Stage_Priorities_AU_and_Reach_data$Life_Stage_Sum_Max <-apply(X=Life_Stage_Priorities_AU_and_Reach_data[,c("Life_Stage_Sum_Column_Spring_Chinook","Life_Stage_Sum_Column_Steelhead", "Life_Stage_Sum_Column_Bull_Trout")], MARGIN=1, FUN=max)
    }else{
      Life_Stage_Priorities_AU_and_Reach_data$Life_Stage_Sum_Max <-apply(X=Life_Stage_Priorities_AU_and_Reach_data[,c("Life_Stage_Sum_Column_Spring_Chinook","Life_Stage_Sum_Column_Steelhead")], MARGIN=1, FUN=max)
    }
    
    # ------------------ add Life Stage Sum ----------
    Restoration_Scores_Output = left_join(Restoration_Scores_Output, 
                                          Life_Stage_Priorities_AU_and_Reach_data[,c("ReachName", "Life_Stage_Sum_Max")], by = c("ReachName" = "ReachName"))
    # ------- update Life Stage Column Names --------
    colnames(Restoration_Scores_Output)[length(colnames(Restoration_Scores_Output))] = "Life_Stage_Sum"
    # ------------------ indicator ratings  -----------------------
    life_stage_metric_data = Restoration_Reach_Scoring[which(Restoration_Reach_Scoring$Indicator == "Number_Life_Stage_Presence" &
                                                                Restoration_Reach_Scoring$Category_Stage == "indicator"),c("Category_lower_limit","Category_upper_limit", "Score")]
    # ----------------- generate score --------------------
    Restoration_Scores_Output = Restoration_Scores_Output  %>%
      mutate(Life_Stage_Sum_SCORES = ifelse(Life_Stage_Sum  >= life_stage_metric_data$Category_lower_limit[1] & 
                                          Life_Stage_Sum  <= life_stage_metric_data$Category_upper_limit[1] , life_stage_metric_data$Score[1],
                                        ifelse(Life_Stage_Sum  > life_stage_metric_data$Category_lower_limit[2] & 
                                                 Life_Stage_Sum  <= life_stage_metric_data$Category_upper_limit[2] , life_stage_metric_data$Score[2],
                                                      NA)))
    
    
    
  }else{
    
    print(paste("--- No Restoration Reaches generated for species: ", species, sep=""))
    
  }
  
  # -------------------------------------------------------
  #     Add Fish Barrier Columns (will fill out below)
  # ------------------------------------------------------- 
  Restoration_Scores_Output$Fish_Barrier_Filter = 0
  
  # -------------------------------------------------------
  #
  #     Calculate Scores and Sort into Tiers
  #
  # ------------------------------------------------------- 
  
  # --------------------- Put Restoration and Protection into a list --------------
  Restoration_Scores_Output$Score_Total = rowSums(Restoration_Scores_Output[, c("Confinement_SCORES", "Habitat_Quality_SCORES", 
                                                                                "Limiting_Factor_SCORES", "Life_Stage_Sum_SCORES")])
  
  # ----------------------------------------------
  #    Rank them (across all reaches)
  # ----------------------------------------------
  Restoration_Scores_Output$Rank_Total = rank( -Restoration_Scores_Output$Score_Total, ties.method= "min")
  
  # ----------------------------------------------
  #    Rank them (within AUs)
  # ----------------------------------------------
  # ------------ unique AUs -------
  unique_AUs = unique(Restoration_Scores_Output$Assessment.Unit)
  # ------------------ start column ----------------
  Restoration_Scores_Output$Rank_AUs = NA
  for(AUx in unique_AUs){
    
    # --------- get the rows for all reaches in this AU ---------------
    x = which(Restoration_Scores_Output$Assessment.Unit == AUx)
    
    # ----------- Rank the Reaches within the AU -----------
    ranks_initial = rank( -Restoration_Scores_Output$Score_Total[x], ties.method= "min")
    Restoration_Scores_Output$Rank_AUs[x] = as.numeric(factor(rank(sort(ranks_initial))))[ranks_initial]
  }
  
  
  # -------------------------------------------------------
  #
  #     Add Fish Barriers
  #
  # ------------------------------------------------------- 
  
  # ----------------- identify if reaches in fish barrier overlap ---------
  reaches_barrier_intersection = intersect(Restoration_Scores_Output$ReachName, Barriers_Pathways_Data$ReachName)
  reaches_barrier_different = setdiff( Barriers_Pathways_Data$ReachName, Restoration_Scores_Output$ReachName)
  
  if( length(reaches_barrier_intersection)>0 ){
    for(reach_x in reaches_barrier_intersection){
      # ------------ identify reach ------------
      x = which(Restoration_Scores_Output$ReachName == reach_x)
      # -------------- mark filter as 1 ------------
      Restoration_Scores_Output$Fish_Barrier_Filter[x] = 1
      # -------------- make AU Rank as 1 ------------
      Restoration_Scores_Output$Rank_AUs[x] = 1
    }
  }
  
  for(reach_x in reaches_barrier_different){
    
    # ---------------- create new row ----------------
    Restoration_Scores_Output[nrow(Restoration_Scores_Output)+1,] <- NA
    # ------------------- add reach name ---------------
    Restoration_Scores_Output$ReachName[nrow(Restoration_Scores_Output)] = reach_x
    # ------- add Basin and Assessment Unit ------------
    reach_info_x = Reach_Information_data[ which(Reach_Information_data$ReachName == reach_x),  ]
    Restoration_Scores_Output$Basin[nrow(Restoration_Scores_Output)] = reach_info_x$Basin
    Restoration_Scores_Output$Assessment.Unit[nrow(Restoration_Scores_Output)] = reach_info_x$Assessment.Unit
    # -------------- mark filter as 1 ------------
    Restoration_Scores_Output$Fish_Barrier_Filter[nrow(Restoration_Scores_Output)] = 1
    # -------------- make AU Rank as 1 ------------
    Restoration_Scores_Output$Rank_AUs[nrow(Restoration_Scores_Output)] = 1
    
  }
  
  
  return(Restoration_Scores_Output)
  
  
  
  # -------------------------------------------------------
  #
  #
  #     Output Protection Scores (if reaches generated)
  #
  #
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
  
  

  
}


# get percent that have 100%
x = which(Restoration_Scores_Output$Habitat_Attribute_Percent_Data_Presence == 1)
length(x) / nrow(Restoration_Scores_Output)

x = which(Restoration_Scores_Output$Habitat_Attribute_Percent_Data_Presence == 0)
length(x) / nrow(Restoration_Scores_Output)



# --------------------------------------------------------------------------------------------------------------------------
#
#
#         Function to calculate Limiting Factors for Reach Rankings
#
#
# --------------------------------------------------------------------------------------------------------------------------

# Habitat_Quality_Pathway_Restoration_MASTER = Habitat_Quality_Pathway_Restoration

FUNCTION_calc_Limiting_Factor_Score = function(Habitat_Quality_Pathway_Restoration){
  
  # ----------- use core metrics? --------
  core_metric_use = "yes"
  
  # -------------------------------------------------------------------------------------------
  #
  #      Identify Species and Life stages Present
  #
  # -------------------------------------------------------------------------------------------

  # --------------------------------------------------------------------------
  #         Generate Limiting Factor Data Table (to fill out and add to)
  # --------------------------------------------------------------------------
  # -------- create unique data table to add habitat attributes ----------
  Limiting_Factor_Reach_Ranking_Scores = Habitat_Quality_Pathway_Restoration[,c('ReachName')]
  # --------------- add columns ----------------
  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_List = NA
  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Missing_List = NA
  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_Total = 0
  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_All_Total = 0
  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Percent_Data_Presence = 0
  Limiting_Factor_Reach_Ranking_Scores$Total_Attribute_Score = 0
  Limiting_Factor_Reach_Ranking_Scores$Species = NA
  
  # --------------------------------------------------------------------------
  #         Get Species
  # --------------------------------------------------------------------------
  # -------- get species list ----------
  if(exclude_bull_trout == "yes"){
    Species_List = c("Spring Chinook", "Steelhead")
  }else{
    Species_List = c("Spring Chinook", "Steelhead", "Bull Trout")
  }
  
  # --------------------------------------------------------------------------
  #         Loop through Each Species
  # --------------------------------------------------------------------------
  for(species_x in Species_List){
    print(paste('------------------',species_x,"------------------"))
    # --------------------------------------------------------------------------
    #         Generate Life Stage Names
    # --------------------------------------------------------------------------
    # ----------------- life stage reach presence name ------------
    if(species_x == "Spring Chinook"){
      life_stages_priorities_species_specific =  life_stages_priorities[['spring_chinook_life_stages']]
      life_stages_prescence_species_specific =  life_stages_prescence[['spring_chinook_life_stages']]
    }
    
    # -------------- pull for Steelhead ------------
    if(species_x == "Steelhead" ){
      # ---------------- life stage priority names ---------
      life_stages_priorities_species_specific =  life_stages_priorities[['steelhead_life_stages']]
      life_stages_prescence_species_specific =  life_stages_prescence[['steelhead_life_stages']]
    }
    
    # -------------- pull for Bull Trout ------------
    if(species_x == "Bull Trout" & exclude_bull_trout == "yes"){
      # ---------------- life stage priority names ---------
      life_stages_priorities_species_specific =  life_stages_priorities[['bull_trout_life_stages']]
      life_stages_prescence_species_specific =  life_stages_prescence[['bull_trout_life_stages']]
    }
    
    # --------------------------------------------------------------------------
    #         Generate Habitat Attribute Scores and Add to Limiting_Factor_Reach_Ranking_Scores
    # --------------------------------------------------------------------------
    # -------------- Get unique life stages ---------------------
    life_stages_unique = unique(Attribute_LifeStage_Crosswalk$`Life Stage`[which(Attribute_LifeStage_Crosswalk$Species == species_x)])

    for(life_stage_x in life_stages_unique){
      print(life_stage_x)
      # ------------------------------------------
      #    Filter out if life stage is high priority in reaches
      # ---------------------------------------------
      # --------------------- high priority reaches -----------------
      life_stages_priorities_species_specific_list = Life_Stage_Priorities_AU_and_Reach_data %>%
        filter(Life_Stage_Priorities_AU_and_Reach_data[life_stages_priorities_species_specific[[life_stage_x]]] ==  Life_Stage_Priority)
      # ----------------- life stage present --------------
      life_stages_priorities_species_specific_list = life_stages_priorities_species_specific_list %>%
        filter(life_stages_priorities_species_specific_list[life_stages_prescence_species_specific[[life_stage_x]]] ==  1)
      # ------------------- just pull the reaches ----------------
      Reaches_life_stage_high_priority = as.data.frame(life_stages_priorities_species_specific_list[,c("ReachName")])[['ReachName']]
      # ------------------ identify overlapping reaches with reaches already filtered ------------
      Reaches_life_stage_high_priority_Overlap = intersect( Reaches_life_stage_high_priority,
                                                           Habitat_Quality_Pathway_Restoration$ReachName )
      
      # ---------------------------------------------
      #    Pull Habitat Attributes 
      # ---------------------------------------------
      habitat_attributes_reaches_x = Generate_individual_life_stage_habitat_attributes(Reaches_life_stage_high_priority_Overlap, species_x, 
                                                        life_stage_x, core_metric_use)
      
      # ---------------- if habitat attribute data present for these reaches and life stages ----------------
      if( !is.na(habitat_attributes_reaches_x[1]) ){
        # ---------------------------------------------
        #   Combine with Data
        # ---------------------------------------------
        # ------------ get the unique reaches -------------
        reaches_Limiting_Factor_x = which(Limiting_Factor_Reach_Ranking_Scores$ReachName %in% habitat_attributes_reaches_x$ReachName)
        # ----------------------- get unique habitat attributes --------------
        colnames_x = colnames(habitat_attributes_reaches_x)[2:length(colnames(habitat_attributes_reaches_x))]
        # ------------ add the life stage and species to the name -----------
        habitat_attributes_life_stage_species_x = paste(colnames_x,species_x,life_stage_x,sep="_")
        # ----------------- loop through each habitat attribute --------
        i = 0
        for(colx in colnames_x){
          i = i + 1
          # --------------- get reaches with data present and absent ----------------
          presence_x = which(habitat_attributes_reaches_x[,colx] > 0 )
          absence_x = which(is.na(habitat_attributes_reaches_x[,colx]) )
          # ----------------------- fill Habitat Attribute Presence List ----------------------
          Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_List[reaches_Limiting_Factor_x[presence_x] ] = paste(Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_List[reaches_Limiting_Factor_x[presence_x] ],  habitat_attributes_life_stage_species_x[i], sep=",")
          # ----------------------- fill Habitat Attribute Missing List ----------------------
          if(length(absence_x) > 0){
            Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Missing_List[reaches_Limiting_Factor_x[absence_x] ] = paste(Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Missing_List[reaches_Limiting_Factor_x[absence_x] ], habitat_attributes_life_stage_species_x[i], sep=",")
          }
          # ----------------------- add to Data Presence Total ----------------------
          Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_Total[reaches_Limiting_Factor_x[presence_x] ] =  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_Total[reaches_Limiting_Factor_x[presence_x] ] + 1
          # ----------------------- add to Data Total  ----------------------
          Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_All_Total[reaches_Limiting_Factor_x] =  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_All_Total[reaches_Limiting_Factor_x]  + 1
          # ----------------------- add to Present Presence ----------------------
          Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Percent_Data_Presence[reaches_Limiting_Factor_x] = Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_Total[reaches_Limiting_Factor_x ] /  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_All_Total[reaches_Limiting_Factor_x]
          # ----------------------- add to Total score----------------------
          Limiting_Factor_Reach_Ranking_Scores$Total_Attribute_Score[reaches_Limiting_Factor_x] = rowSums( cbind(Limiting_Factor_Reach_Ranking_Scores$Total_Attribute_Score[reaches_Limiting_Factor_x], habitat_attributes_reaches_x[,colx]), na.rm=T ) 
          # -----------------------include species----------------------
          Limiting_Factor_Reach_Ranking_Scores$Species[reaches_Limiting_Factor_x] = paste(Limiting_Factor_Reach_Ranking_Scores$Species[reaches_Limiting_Factor_x], species_x) 
        }
        
      }

        
    }

  } # end species loop
  
  # -------------------------------------------------------------------------------------------
  #
  #     Calculate Totals
  #
  # -------------------------------------------------------------------------------------------
  
  Limiting_Factor_Reach_Ranking_Scores$Limiting_Factor_Score_Percent = (Limiting_Factor_Reach_Ranking_Scores$Total_Attribute_Score / (Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_All_Total*5) ) * 100
  
  # -------------------------------------------------------------------------------------------
  #
  #     Return Data
  #
  # -------------------------------------------------------------------------------------------
  Limiting_Factor_Reach_Ranking_Scores_Output = Limiting_Factor_Reach_Ranking_Scores[,c("ReachName","Habitat_Attribute_Percent_Data_Presence","Habitat_Attribute_Missing_List", "Limiting_Factor_Score_Percent")]
  
  return(Limiting_Factor_Reach_Ranking_Scores_Output)
  
}




# -------------------------------------------------------------------
#
#       Function to generate habitat attributes for individual species/life stage combo
#
# -------------------------------------------------------------------

species = "Spring Chinook"
life_stage_x = "Fry"
core_metric_use  = "yes"
reaches_x = Reaches_life_stage_high_priority_Overlap

Generate_individual_life_stage_habitat_attributes = function(reaches_x, species, life_stage_x, core_metric_use){
  
  # -------------------------------------------------------------------
  #  Pull life stage attributes for this species
  # ------------------------------------------------------------------
  
  # -------------------- pull habitat attributes/life stages JUST for this species ---------
  Attribute_LifeStage_Crosswalk_Life_Stage = Attribute_LifeStage_Crosswalk %>%
    filter(Species  %in% species  )
  
  # ------------------ pull habitat attributes JUST for this life stage -------------------
  habitat_attributes_life_stage_list = Attribute_LifeStage_Crosswalk_Life_Stage %>%
    filter(Attribute_LifeStage_Crosswalk_Life_Stage$'Life Stage'  %in%  life_stage_x  )
  # -------------- IF pulling core metrics, filter based on that --------------
  if(core_metric == "yes"){
    habitat_attributes_life_stage_list = habitat_attributes_life_stage_list %>%
      filter(habitat_attributes_life_stage_list$'Life Stage Core Metric?' %in%  "x" )
  }

  
  # -------------------------------------------------------------------
  #      generate all the scores for all the habitat attributes for EVERY reach in basin(s)
  # ------------------------------------------------------------------
  
  # ------------ if habitat attributes present in this life stage/species combo -------
  if( nrow(habitat_attributes_life_stage_list)>0 ){
    
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
        Habitat_Attribute_Scores_for_Life_Stages_REACHES_BASINS = Habitat_Attribute_Scores_Life_Stage[,c("ReachName")]
        Habitat_Attribute_Scores_for_individual_Life_Stage = cbind(Habitat_Attribute_Scores_for_Life_Stages_REACHES_BASINS, Habitat_Attribute_Score_x)
        colnames(Habitat_Attribute_Scores_for_individual_Life_Stage)[1] = "ReachName"
        #------- if it's not the first habitat attribute -----
      }else if( nrow(Habitat_Attribute_Score_x) > 0 ){
        Habitat_Attribute_Scores_for_individual_Life_Stage = cbind(Habitat_Attribute_Scores_for_individual_Life_Stage, Habitat_Attribute_Score_x)
      }
    }
    
    
    # --------------------------------------------------------------------------------
    #         Only Pull for these specific Reaches
    # --------------------------------------------------------------------------------
    Habitat_Attribute_Scores_for_individual_Life_Stage = Habitat_Attribute_Scores_for_individual_Life_Stage %>%
      filter(Habitat_Attribute_Scores_for_individual_Life_Stage$ReachName  %in%  reaches_x  )
    
    if(nrow(Habitat_Attribute_Scores_for_individual_Life_Stage) == 0 ){
      Habitat_Attribute_Scores_for_individual_Life_Stage = NA
    }
    
    return(Habitat_Attribute_Scores_for_individual_Life_Stage)
  }else{
    
    # -------------- if no habitat attributes for this life stage/species combo ------------
    return(NA)
    
  }
 
}



# ------------------------------------------------------------
#
#   Function to add Barrier Prioritization info to Output
#
# ------------------------------------------------------------

# HQ_LF_Combined = Restoration_Prioritization_Output

FUNCTION_Add_Barrier_Data_Rankings = function(Barriers_Pathways_Data,  exclude_bull_trout){
  
  # ------------------- get the reaches ------------------
  for(reach_x in unique(Barriers_Pathways_Data$ReachName) ){
    
    HQ_LF_index = which(HQ_LF_Combined$ReachName == reach_x)
    barrier_index = which(Barriers_Pathways_Data$ReachName == reach_x)
    
    # ------------------------------------------------------------
    #   IF the barriers reach is in the existing list of prioritized reaches
    # ------------------------------------------------------------
    
    if( length(HQ_LF_index) > 0){
      
      # ---------------------------------------------------------------------------
      #    Update Pathways, Action Categories, and Habitat Attributes
      # ---------------------------------------------------------------------------
      
      # ---------------- make barrier pathway a yes --------------
      HQ_LF_Combined$Barrier_Prioritization_Pathway[HQ_LF_index] = "yes"
      
      # ------------- add pathway info -----------
      HQ_LF_Combined$Pathways[HQ_LF_index] = paste(HQ_LF_Combined$Pathways[HQ_LF_index] ,"Barriers_pathway",sep=",")
      HQ_LF_Combined$Number_of_Pathways[HQ_LF_index] = HQ_LF_Combined$Number_of_Pathways[HQ_LF_index] + 1
      
      # ----------------- add Action Categories  ------------
      HQ_LF_Combined$Action_Categories_All_Species[HQ_LF_index] = paste( HQ_LF_Combined$Action_Categories_All_Species[HQ_LF_index], Barriers_Pathways_Data$`Action Category`[barrier_index], sep=",")
      HQ_LF_Combined$Number_Action_Categories_All_Species[HQ_LF_index] = HQ_LF_Combined$Number_Action_Categories_All_Species[HQ_LF_index] + 1
      
      # -------------- add Habitat Attributes ------------
      HQ_LF_Combined$Impaired_Habitat_Attributes_All_Species[HQ_LF_index] = paste( HQ_LF_Combined$Impaired_Habitat_Attributes_All_Species[HQ_LF_index], Barriers_Pathways_Data$Habitat_Attributes[barrier_index], sep=",")    
      HQ_LF_Combined$Number_Impaired_Habitat_Attributes_All_Species[HQ_LF_index] = HQ_LF_Combined$Number_Action_Categories_All_Species[HQ_LF_index] + 1
      
      # -------------- add General Action ------------
      HQ_LF_Combined$Actions[HQ_LF_index] = paste( HQ_LF_Combined$Actions[HQ_LF_index], "Restore Fish Passage", sep=", ")    
      
      
      # ------------------------------------------------------------
      #   IF the barriers reach DOES NOT exist in prioritized reach list
      # ------------------------------------------------------------
      
    }else{
      
      barrier_index = which(Barriers_Pathways_Data$ReachName == reach_x)
      # ------------- add reach information --------------------
      HQ_and_LF_combo_x = as.data.frame( Barriers_Pathways_Data[barrier_index, columns_info]  )
      
      # ------------- add pathway info -----------
      HQ_and_LF_combo_x$Pathways = "Barriers_pathway"
      HQ_and_LF_combo_x$Number_of_Pathways = 1
      # --------- add General Actions column --------
      HQ_and_LF_combo_x$Actions = "Restore Fish Passage"
      
      # ------------ yes or no pathways --------
      HQ_and_LF_combo_x$HabitatQuality_Spring_Chinook_Pathway = "no"
      HQ_and_LF_combo_x$HabitatQuality_Steelhead_Pathway= "no"
      HQ_and_LF_combo_x$HabitatQuality_BullTrout_Pathway= "no"
      HQ_and_LF_combo_x$LimitingFactor_Spring_Chinook_Pathway= "no"	
      HQ_and_LF_combo_x$LimitingFactor_Steelhead_Pathway= "no"	
      HQ_and_LF_combo_x$LimitingFactor_BullTrout_Pathway= "no"
      
      # ----------- add "no" to barrier -----
      HQ_and_LF_combo_x$Barrier_Prioritization_Pathway = "yes"
      
      # ------------- misc other ----------
      species_and_life_stages = FUNCTION_output_life_stages_based_on_species_and_life_stage_presence_FISH_BARRIERs(reach_x, exclude_bull_trout)
      HQ_and_LF_combo_x$Species = species_and_life_stages[1]
      HQ_and_LF_combo_x$SprCh_STLD_BullTr_All_Benefit = NA
      HQ_and_LF_combo_x$Spring_Chinook_Benefit = NA
      HQ_and_LF_combo_x$Spring_Chinook_Habitat_Attributes = NA                    
      HQ_and_LF_combo_x$Spring_Chinook_Actions = NA                                       
      HQ_and_LF_combo_x$Steelhead_Habitat_Attributes = NA                               
      HQ_and_LF_combo_x$Steelhead_Actions = NA 
      HQ_and_LF_combo_x$Bull_Trout_Habitat_Attributes = NA                             
      HQ_and_LF_combo_x$Bull_Trout_Actions = NA
      HQ_and_LF_combo_x$Life_Stages = species_and_life_stages[2]
      
      # ----------------- add Action Categories  ------------
      HQ_and_LF_combo_x$Action_Categories_All_Species = Barriers_Pathways_Data$`Action Category`[barrier_index]
      HQ_and_LF_combo_x$Number_Action_Categories_All_Species = 1
      
      # -------------- add Habitat Attributes ------------
      HQ_and_LF_combo_x$Impaired_Habitat_Attributes_All_Species = Barriers_Pathways_Data$Habitat_Attributes[barrier_index]
      HQ_and_LF_combo_x$Number_Impaired_Habitat_Attributes_All_Species = 1
      
      # ------ more misc -------
      HQ_and_LF_combo_x$Unacceptable_Habitat_Attributes_Presence = NA
      HQ_and_LF_combo_x$Unacceptable_Impaired_Habitat_Attributes_All_Species = NA
      HQ_and_LF_combo_x$Unacceptable_Number_Impaired_Habitat_Attributes_All_Species = NA
      
      HQ_and_LF_combo_x$At_Risk_Habitat_Attributes_Presence = NA
      HQ_and_LF_combo_x$At_Risk_Impaired_Habitat_Attributes_All_Species = NA
      HQ_and_LF_combo_x$At_Risk_Number_Impaired_Habitat_Attributes_All_Species = NA
      
      # ------ Add another row to output ------
      colnames(HQ_and_LF_combo_x) = colnames(HQ_LF_Combined)
      HQ_LF_Combined = rbind(HQ_LF_Combined, HQ_and_LF_combo_x)
      
      
    }
    
  }
  
  # ----------------------------------------- convert lists that are blank to NA --------------
  HQ_LF_Combined$Species[ which( is.na(HQ_LF_Combined$Species) ) ] = "NA"
  HQ_LF_Combined$SprCh_STLD_BullTr_All_Benefit[ which( is.na(HQ_LF_Combined$SprCh_STLD_BullTr_All_Benefit) ) ] = "NA"
  HQ_LF_Combined$Spring_Chinook_Benefit[ which( is.na(HQ_LF_Combined$Spring_Chinook_Benefit) ) ] = "NA"
  HQ_LF_Combined$Life_Stages[ which( is.na(HQ_LF_Combined$Life_Stages) ) ] = "NA"
  
  HQ_LF_Combined$Unacceptable_Habitat_Attributes_Presence[ which( is.na(HQ_LF_Combined$Unacceptable_Habitat_Attributes_Presence) ) ] = "NA"
  HQ_LF_Combined$Unacceptable_Impaired_Habitat_Attributes_All_Species[ which( is.na(HQ_LF_Combined$Unacceptable_Impaired_Habitat_Attributes_All_Species) ) ] = "NA"
  
  HQ_LF_Combined$At_Risk_Habitat_Attributes_Presence[ which( is.na(HQ_LF_Combined$At_Risk_Habitat_Attributes_Presence) ) ] = "NA"
  HQ_LF_Combined$At_Risk_Impaired_Habitat_Attributes_All_Species[ which( is.na(HQ_LF_Combined$At_Risk_Impaired_Habitat_Attributes_All_Species) ) ] = "NA"
  
  
  
  # ------------------- convert NAs in numeric to 0 -------------
  HQ_LF_Combined$Unacceptable_Number_Impaired_Habitat_Attributes_All_Species[ which( is.na(HQ_LF_Combined$Unacceptable_Number_Impaired_Habitat_Attributes_All_Species) ) ] = 0
  
  HQ_LF_Combined$At_Risk_Number_Impaired_Habitat_Attributes_All_Species[ which( is.na(HQ_LF_Combined$At_Risk_Number_Impaired_Habitat_Attributes_All_Species) ) ] = 0
  
  
  return(HQ_LF_Combined)
  
}



