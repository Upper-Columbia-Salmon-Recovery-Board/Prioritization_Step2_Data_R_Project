

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
test_x = FALSE
if(test_x){
  basins = c( "Wenatchee", "Methow", "Entiat", "Okanogan")
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
  Output_ALL_Spring_Chinook_file = paste(paste("Spring_Chinook_Ranks_ALL_OUTPUT" , 
                                paste(basins_to_include, collapse = "_"), sep="_"),
                          ".xlsx", sep="")
    
  # -------------------- STEELHEAD -------------------------------------------
  # ---------------- species reach ---------------
  species_reach_Steelhead = 'Steelhead.Reach'
  # ---------------- species AU Rank ----------
  AU_rank_name_restoration_Steelhead = 'STLTier_Restoration'
  AU_rank_name_restoration_Steelhead_Okanogan  = 'AU Restoration Rank' # FOR Okanogan
  # ---------------- species AU Rank PROTECTION ----------
  AU_rank_name_protection_Steelhead = 'STLTier_Protection'
  AU_rank_name_protection_Steelhead_Okanogan = 'AU Protection Rank'
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
  Output_ALL_Steelhead_file = paste(paste("Steelhead_Ranks_ALL_OUTPUT" , 
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
    Output_ALL_Bull_Trout_file = paste(paste("Bull_Trout_Ranks_ALL_OUTPUT" , 
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
  Species_AU_Ranks_data_Steelhead_Okanogan = AU_Ranks_Okanogan
  if(exclude_bull_trout == "no"){
    Species_AU_Ranks_data_Bull_Trout = AU_Ranks_data
  }
  
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
    Bull_Trout_Reach_Information_data = Species_Reach_Information_data
    Bull_Trout_Reach_Information_data$Species_Reaches = Bull_Trout_Reach_Information_data[species_reach_Bull_Trout]
    # ----------------------- filter out for only reaches with this species --------------
    Bull_Trout_Reach_Information_data = Bull_Trout_Reach_Information_data %>%  
      filter(Species_Reaches   == 'yes')
    print(paste("Total reaches after Bull Trout species-reach filter: ", nrow(Bull_Trout_Reach_Information_data), sep=""))
    
  }
  
  # ---------------------- start data frame that outputs results for all reaches -------------
  Output_Spring_Chinook_All = Reach_Information_data[,c(1:4)]
  Output_Steelhead_All = Reach_Information_data[,c(1:3,5)]
  
  # ---------------------------------------------------------------------------------------------------------------
  #
  #     Priority Assessment Unit Filter
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
  
  # ---------------- add Okanogan -------------
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_AU_Ranks_data_Steelhead_Okanogan["Species_AU_Ranks"] = Species_AU_Ranks_data_Steelhead_Okanogan[AU_rank_name_restoration_Steelhead_Okanogan]
  # ----------------------- filter out for only reaches with this species --------------
  Species_AU_Ranks_data_Steelhead_restoration_Okanogan = Species_AU_Ranks_data_Steelhead_Okanogan %>%  
    filter(Species_AU_Ranks    %in%   AU_Rank_Restoration)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Steelhead_Reach_Information_data_restoration_Okanogan = Steelhead_Reach_Information_data %>%  
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data_Steelhead_restoration_Okanogan$`EDT AU`)

  # ------------------- combine Wen-Ent-Methow and Okanogan ------------
  Steelhead_Reach_Information_data_restoration = rbind(Steelhead_Reach_Information_data_restoration, Steelhead_Reach_Information_data_restoration_Okanogan)
  
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
  
  # -------- add to data frame that includes all reaches -----
  # ------------- Spring Chinook --------

  AU_Rank_Data = Species_AU_Ranks_data_Spring_Chinook [,c("Assessment Unit", "Species_AU_Ranks")]
  colnames(AU_Rank_Data) = c("Assessment.Unit", "AU Restoration Rank")
  Output_Spring_Chinook_All = merge(Output_Spring_Chinook_All  ,AU_Rank_Data, by = "Assessment.Unit", all.x= TRUE )
  # ----------- Steelhead ----------
  AU_Rank_Data = Species_AU_Ranks_data_Steelhead_Okanogan[,c("EDT AU","AU Restoration Rank")]
  colnames(AU_Rank_Data)[1] = "Assessment.Unit"
  AU_Rank_Data2 = Species_AU_Ranks_data_Steelhead [,c("Assessment Unit", "Species_AU_Ranks")]
  colnames(AU_Rank_Data2) = c("Assessment.Unit", colnames(AU_Rank_Data)[2])
  AU_Rank_Data = rbind(AU_Rank_Data, AU_Rank_Data2)
  Output_Steelhead_All = merge(Output_Steelhead_All  ,AU_Rank_Data, by = "Assessment.Unit", all.x= TRUE )
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to select for AU rank - PROTECTION
  #  ---------------------------------------------------------------------------------

  # ------------------- establish AU Rank ------------------
  AU_Rank_Protection = Protection_Reach_Scoring[which(Protection_Reach_Scoring$Indicator == "AU_Ranking"  &
                                                       Protection_Reach_Scoring$Category_Stage == "filter"),"Category"]
  
  # --------------------------- SPRING CHINOOK -----------------
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_AU_Ranks_data_Spring_Chinook ["Species_AU_Ranks"] = Species_AU_Ranks_data_Spring_Chinook [AU_rank_name_protection_Spring_Chinook]
  # ----------------------- filter out for only reaches with this species --------------
  Species_AU_Ranks_data_Spring_Chinook_protection = Species_AU_Ranks_data_Spring_Chinook  %>%  
    filter(Species_AU_Ranks    %in%   AU_Rank_Protection)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Spring_Chinook_Reach_Information_data_protection = Spring_Chinook_Reach_Information_data %>%  
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data_Spring_Chinook_protection$`Assessment Unit`)
  
  print(paste("Spring Chinook Protection - total AU rank filter: ", nrow(Spring_Chinook_Reach_Information_data_protection), sep=""))
  
  # --------------------------- STEELHEAD -----------------
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_AU_Ranks_data_Steelhead ["Species_AU_Ranks"] = Species_AU_Ranks_data_Steelhead [AU_rank_name_protection_Steelhead]
  # ----------------------- filter out for only reaches with this species --------------
  Species_AU_Ranks_data_Steelhead_protection = Species_AU_Ranks_data_Steelhead  %>%  
    filter(Species_AU_Ranks    %in%   AU_Rank_Protection)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Steelhead_Reach_Information_data_protection = Steelhead_Reach_Information_data %>%  
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data_Steelhead_protection$`Assessment Unit`)
  
  # ---------------- add Okanogan -------------
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_AU_Ranks_data_Steelhead_Okanogan["Species_AU_Ranks"] = Species_AU_Ranks_data_Steelhead_Okanogan[AU_rank_name_protection_Steelhead_Okanogan]
  # ----------------------- filter out for only reaches with this species --------------
  Species_AU_Ranks_data_Steelhead_protection_Okanogan = Species_AU_Ranks_data_Steelhead_Okanogan %>%  
    filter(Species_AU_Ranks    %in%   AU_Rank_Protection)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Steelhead_Reach_Information_data_protection_Okanogan = Steelhead_Reach_Information_data %>%  
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data_Steelhead_protection_Okanogan$`EDT AU`)
  
  # ------------------- combine Wen-Ent-Methow and Okanogan ------------
  Steelhead_Reach_Information_data_protection = rbind(Steelhead_Reach_Information_data_protection, Steelhead_Reach_Information_data_protection_Okanogan)

  print(paste("Steelhead Protection - total AU rank filter: ", nrow(Steelhead_Reach_Information_data_protection), sep=""))
  
  # --------------------------- BULL TROUT -----------------
  if(exclude_bull_trout == "no"){
    
    # -------------------- add additional column for this particular species reach presence ---------------
    Species_AU_Ranks_data_Bull_Trout ["Species_AU_Ranks"] = Species_AU_Ranks_data_Bull_Trout [AU_rank_name_protection_Bull_Trout]
    # ----------------------- filter out for only reaches with this species --------------
    Species_AU_Ranks_data_Bull_Trout_protection = Species_AU_Ranks_data_Bull_Trout  %>%  
      filter(Species_AU_Ranks    %in%   AU_Rank_Protection)
    # ------------------------ identify AUs that pass this filter in reach-based table ----------
    Bull_Trout_Reach_Information_data_protection = Bull_Trout_Reach_Information_data %>%  
      filter(Assessment.Unit    %in%   Species_AU_Ranks_data_Bull_Trout_protection$`Assessment Unit`)
    
    print(paste("Bull Trout Protection - total AU rank filter: ", nrow(Bull_Trout_Reach_Information_data_protection), sep=""))
  }
  
  # -------- add to output that includes all reaches -----
  # ------ Spring Chinook --------
  AU_Rank_Data = Species_AU_Ranks_data_Spring_Chinook[,c("Assessment Unit", "Species_AU_Ranks")]
  colnames(AU_Rank_Data) = c("Assessment.Unit", "AU Protection Rank")
  Output_Spring_Chinook_All = merge(Output_Spring_Chinook_All  ,AU_Rank_Data, by = "Assessment.Unit", all.x= TRUE )
  # --------- Steelhead ----------
  AU_Rank_Data = Species_AU_Ranks_data_Steelhead_Okanogan[,c("EDT AU","AU Protection Rank")]
  colnames(AU_Rank_Data)[1] = "Assessment.Unit"
  AU_Rank_Data2 = Species_AU_Ranks_data_Steelhead [,c("Assessment Unit", "Species_AU_Ranks")]
  colnames(AU_Rank_Data2) = c("Assessment.Unit", colnames(AU_Rank_Data)[2])
  AU_Rank_Data = rbind(AU_Rank_Data, AU_Rank_Data2)
  Output_Steelhead_All = merge(Output_Steelhead_All  ,AU_Rank_Data, by = "Assessment.Unit", all.x= TRUE )
  
  #  ---------------------------------------------------------------------------------
  #
  #         Habitat Quality Pathway Filter
  #
  #  ---------------------------------------------------------------------------------
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to select for Habitat Quality RESTORATION score
  #  ---------------------------------------------------------------------------------
  
  # ------------------- establish Habitat Quality Score cutoff ------------------
  HQ_Score_Restoration_Reach_Scores = Restoration_Reach_Scoring[which(Restoration_Reach_Scoring$Indicator == "Habitat_Quality_Score" &
                                                          Restoration_Reach_Scoring$Category_Stage == "filter"),c("Category_lower_limit","Category_upper_limit")]
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  Habitat_Quality_Scores_Restoration = Habitat_Quality_Scores %>%  
    filter(HQ_Pct   >=   HQ_Score_Restoration_Reach_Scores$Category_lower_limit) %>%
    filter(HQ_Pct   <  HQ_Score_Restoration_Reach_Scores$Category_upper_limit)
  # -------------- prepare Okanogan - Steelhead --------------
  Habitat_Quality_Scores_Restoration_Okanogan = Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']]
  Habitat_Quality_Scores_Restoration_Okanogan = Habitat_Quality_Scores_Restoration_Okanogan[which(Habitat_Quality_Scores_Restoration_Okanogan$Basin == "Okanogan"),]
  Habitat_Quality_Scores_Restoration_Okanogan_all = Habitat_Quality_Scores_Restoration_Okanogan
  Habitat_Quality_Scores_Restoration_Okanogan = Habitat_Quality_Scores_Restoration_Okanogan %>%  
    filter(HQ_Pct   >=   HQ_Score_Restoration_Reach_Scores$Category_lower_limit) %>%
    filter(HQ_Pct   <  HQ_Score_Restoration_Reach_Scores$Category_upper_limit)
  
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  # ----------------------- SPRING CHINOOK ---------------------
  Habitat_Quality_Pathway_Restoration_Spring_Chinook = Spring_Chinook_Reach_Information_data_restoration  %>%  
    filter(ReachName   %in%   Habitat_Quality_Scores_Restoration$`ReachName`)
  print(paste("Spring Chinook Restoration - total after HQ score filter: ", nrow(Habitat_Quality_Pathway_Restoration_Spring_Chinook), sep=""))
  # ----------------------- STEELHEAD ---------------------
  Habitat_Quality_Pathway_Restoration_Steelhead = Steelhead_Reach_Information_data_restoration %>%  
    filter(ReachName   %in%   Habitat_Quality_Scores_Restoration$`ReachName`)
  
  # --------- add Okanogan values -------
  Habitat_Quality_Pathway_Restoration_Steelhead_Okanogan = Steelhead_Reach_Information_data_restoration %>%  
    filter(ReachName   %in%   Habitat_Quality_Scores_Restoration_Okanogan$`ReachName`)
  # -------- add to output for all reaches -----
  # -------- add to total output -----
  HQ_data_x = PRCNT_Habitat_Quality_Okanogan_EDT[,c("ReachName","HQ_Score")]
  colnames(HQ_data_x) = c("ReachName", "HQ_Pct")
  HQ_data_x2 = Habitat_Quality_Scores[which(Habitat_Quality_Scores$Basin != "Okanogan"),c("ReachName", "HQ_Pct")]
  HQ_data_x = rbind(HQ_data_x, HQ_data_x2)
  Output_Spring_Chinook_All = merge(Output_Spring_Chinook_All  ,HQ_data_x, by = "ReachName" , all.x=TRUE)  # NOTE - some Okanogan reaches are not present in the EDT results HQ output (PRCNT_Habitat_Quality_Okanogan_EDT)
  Output_Steelhead_All = merge(Output_Steelhead_All  ,HQ_data_x, by = "ReachName", all.x=TRUE )  # NOTE - some Okanogan reaches are not present in the EDT results HQ output (PRCNT_Habitat_Quality_Okanogan_EDT)
  
  # --------- combine Wen-Ent-Wen and Okanogan -----------
  Habitat_Quality_Pathway_Restoration_Steelhead = rbind(Habitat_Quality_Pathway_Restoration_Steelhead, Habitat_Quality_Pathway_Restoration_Steelhead_Okanogan)
  
  print(paste("Steelhead Restoration - total after HQ score filter: ", nrow(Habitat_Quality_Pathway_Restoration_Steelhead), sep=""))
  
  # ----------------------- BULL TROUT---------------------
  if(exclude_bull_trout == "no"){
    Habitat_Quality_Pathway_Restoration_Bull_Trout = Bull_Trout_Reach_Information_data_restoration %>%  
      filter(ReachName   %in%   Habitat_Quality_Scores_Restoration$`ReachName`)
    print(paste("Steelhead Restoration - total after HQ score filter: ", nrow(Habitat_Quality_Pathway_Restoration_Bull_Trout), sep=""))
  }
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to select for Habitat Quality PROTECTION score
  #  ---------------------------------------------------------------------------------
  
  # ------------------- establish Habitat Quality Score cutoff ------------------
  HQ_Score_Protection_Reach_Scores = Protection_Reach_Scoring[which(Protection_Reach_Scoring$Indicator == "Habitat_Quality_Score" &
                                                                      Protection_Reach_Scoring$Category_Stage == "filter"),c("Category_lower_limit","Category_upper_limit")]
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  Habitat_Quality_Scores_Protection = Habitat_Quality_Scores %>%  
    filter(HQ_Pct   >=   HQ_Score_Protection_Reach_Scores$Category_lower_limit) %>%
    filter(HQ_Pct   <  HQ_Score_Protection_Reach_Scores$Category_upper_limit)
  # -------------- prepare Okanogan - Steelhead --------------
  Habitat_Quality_Scores_Protection_Okanogan = Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Protection']]
  Habitat_Quality_Scores_Protection_Okanogan = Habitat_Quality_Scores_Protection_Okanogan[which(Habitat_Quality_Scores_Protection_Okanogan$Basin == "Okanogan"),]
  Habitat_Quality_Scores_Protection_Okanogan = Habitat_Quality_Scores_Protection_Okanogan %>%  
    filter(HQ_Pct   >=   HQ_Score_Protection_Reach_Scores$Category_lower_limit) %>%
    filter(HQ_Pct   <  HQ_Score_Protection_Reach_Scores$Category_upper_limit)
  
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  # ----------------------- SPRING CHINOOK ---------------------
  Habitat_Quality_Pathway_Protection_Spring_Chinook = Spring_Chinook_Reach_Information_data_protection %>%  
    filter(ReachName   %in%   Habitat_Quality_Scores_Protection$`ReachName`)
  print(paste("Spring Chinook Protection - total after HQ score filter: ", nrow(Habitat_Quality_Pathway_Protection_Spring_Chinook), sep=""))
  # ----------------------- STEELHEAD ---------------------
  Habitat_Quality_Pathway_Protection_Steelhead = Steelhead_Reach_Information_data_protection %>%  
    filter(ReachName   %in%   Habitat_Quality_Scores_Protection$`ReachName`)
  # --------- add Okanogan values -------
  Habitat_Quality_Pathway_Protection_Steelhead_Okanogan = Steelhead_Reach_Information_data_protection %>%  
    filter(ReachName   %in%   Habitat_Quality_Scores_Protection_Okanogan$`ReachName`)
  # --------- combine Wen-Ent-Wen and Okanogan -----------
  Habitat_Quality_Pathway_Protection_Steelhead = rbind(Habitat_Quality_Pathway_Protection_Steelhead, Habitat_Quality_Pathway_Protection_Steelhead_Okanogan)
  
  print(paste("Steelhead Protection - total after HQ score filter: ", nrow(Habitat_Quality_Pathway_Protection_Steelhead), sep=""))
  
  # -------- add to output for all reaches -----
  # NOTE: no need to include here - since Restoration and Protection are driven by HQ_Pct
  
  
  # ----------------------- BULL TROUT---------------------
  if(exclude_bull_trout == "no"){
    Habitat_Quality_Pathway_Protection_Bull_Trout = Bull_Trout_Reach_Information_data_protection %>%  
      filter(ReachName   %in%   Habitat_Quality_Scores_Protection$`ReachName`)
    print(paste("Steelhead Protection - total after HQ score filter: ", nrow(Habitat_Quality_Pathway_Protection_Bull_Trout), sep=""))
  }
  
  #  ---------------------------------------------------------------------------------
  #
  #         Limiting Factor for a High Priority Life Stage
  #
  #  ---------------------------------------------------------------------------------
  
  #  ---------------------------------------------------------------------------------
  #       Restoration - pull reaches with Limiting Factor for a High Priority Life Stage
  #  ---------------------------------------------------------------------------------
  
  # ----------------------- SPRING CHINOOK ---------------------
  # ------------- only pull reaches with unacceptable attributes -----------
  Limiting_Factor_Pathway_Spring_Chinook_Reach_Ranking = Limiting_Factor_Pathway_Spring_Chinook[["Limiting_Factor_Pathway_Restoration"]][nchar(Limiting_Factor_Pathway_Spring_Chinook[["Limiting_Factor_Pathway_Restoration"]]$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes)>0,]
  # --------------- filter  the reaches ---------------
  Limiting_Factor_Pathway_Restoration_Spring_Chinook = Spring_Chinook_Reach_Information_data_restoration %>%  
    filter(ReachName   %in%   Limiting_Factor_Pathway_Spring_Chinook_Reach_Ranking$`ReachName`)
  print(paste("Spring Chinook Restoration - total after LF score filter: ", nrow(Limiting_Factor_Pathway_Restoration_Spring_Chinook), sep=""))
  # -------------- combine with HQ Pathway ------------
  
  # ----------------------- STEELHEAD ---------------------
  # ------------- only pull reaches with unacceptable attributes -----------
  Limiting_Factor_Pathway_Steelhead_Reach_Ranking = Limiting_Factor_Pathway_Steelhead[["Limiting_Factor_Pathway_Restoration"]][nchar(Limiting_Factor_Pathway_Steelhead[["Limiting_Factor_Pathway_Restoration"]]$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes)>0,]
  # --------------- filter  the reaches ---------------
  Limiting_Factor_Pathway_Restoration_Steelhead = Steelhead_Reach_Information_data_restoration %>%  
    filter(ReachName   %in%   Limiting_Factor_Pathway_Steelhead_Reach_Ranking$`ReachName`)
  print(paste("Steelhead Restoration - total after LF score filter: ", nrow(Limiting_Factor_Pathway_Restoration_Steelhead), sep=""))
 
  # ------------------------------------------------------------------------ 
  #                     add to output of all reaches 
  # ------------------------------------------------------------------------ 
  
  
  # ------------------ function that outputs the entire row (so you can check) ------
  Limiting_Factor_Output_ALL = FUNCTION_calc_Limiting_Factor_Score(Output_Spring_Chinook_All)
  
  
  # ------------------------- Spring Chinook ---------------------------------- 
  # list "yes" or "no" if it has a limiting factor in a high priority life stage (maybe list the limiting factor OR life stage?)
  # ----------- loop through each reach and identify if the reach has a limiting factor in a priority life stage
  Output_LF_all = c()
  for(reach_x in Output_Spring_Chinook_All$ReachName[which(Output_Spring_Chinook_All$Basin != "Okanogan")]){
    
    # ------------------ pull the priority life stages in this reach -------------------
    output_life_stages_x = FUNCTION_pull_High_Priority_Life_Stages_for_a_reach(reach_x, "Spring Chinook")
    
    reach_in_LF_output = any(Limiting_Factor_Pathway_Steelhead[["Limiting_Factor_Pathway_Restoration"]]$ReachName == reach_x)
    # --------------- IF there is a reach in the LF Pathway output -------------
    if(reach_in_LF_output){
      rows_x = which(Limiting_Factor_Pathway_Spring_Chinook[["Limiting_Factor_Pathway_Restoration"]]$ReachName == reach_x)
      #output_life_stages_x = c()
      output_limiting_factor_x = c()
      for(x in rows_x){
        #output_life_stages_x = c(output_life_stages_x, Limiting_Factor_Pathway_Steelhead[["Limiting_Factor_Pathway_Restoration"]][x,c("life_stage")])
        output_limiting_factor_x = c(output_limiting_factor_x, Limiting_Factor_Pathway_Spring_Chinook[["Limiting_Factor_Pathway_Restoration"]][x,c("unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes")])
      }
      
      # ------------- IF reach is not in the LF pathway -----
    }else{
      #output_life_stages_x = "No Priority Life Stages with Limiting Factors"
      output_limiting_factor_x = "No Priority Life Stages with Limiting Factors"
    }
    output_life_stages_x = paste(output_life_stages_x, collapse = ",")
    output_limiting_factor_x = paste(output_limiting_factor_x, collapse = ",")
    Output_LF_x = t(as.data.frame(c(reach_x,output_life_stages_x, output_limiting_factor_x )))
    colnames(Output_LF_x) = c("ReachName", "Priority_Life_Stages_with_Limiting_Factors","Limiting_Factors_for_Priority_Life_Stages")
    Output_LF_all = rbind(Output_LF_all, Output_LF_x )
  }
  # ------------- combine -------------
  Output_Spring_Chinook_All = merge(  Output_Spring_Chinook_All  ,  Output_LF_all, by = "ReachName", all.x= TRUE )
  
  # ------------- verify which reach passes HQ pathway or LF Pathway for RESTORATION -------------
  HQ_or_LF_RESTORATION_filter_pass = c()
  for(reach_x in Output_Spring_Chinook_All$ReachName[which(Output_Spring_Chinook_All$Basin != "Okanogan")]){
    # -------------- pull row with reach --------
    x = which(Output_Spring_Chinook_All$ReachName == reach_x)
    
    # --------------- if HQ score is NA, set to 100 (so it won't pass through the filter), otherwise, pull the HQ_Pct)
    if(is.na(Output_Spring_Chinook_All$HQ_Pct[x])){
      HQ_Pct_x = 100
    }else{
      HQ_Pct_x = Output_Spring_Chinook_All$HQ_Pct[x]
    }
    if(HQ_Pct_x< HQ_Score_Restoration_Reach_Scores$Category_upper_limit | 
       Output_Spring_Chinook_All$Limiting_Factors_for_Priority_Life_Stages[x] != "No Priority Life Stages with Limiting Factors"){
      output_x = t(as.data.frame(c(reach_x, "yes")))
    }else{
      output_x = t(as.data.frame(c(reach_x, "no")))
    }
    HQ_or_LF_RESTORATION_filter_pass = rbind(HQ_or_LF_RESTORATION_filter_pass, output_x)
  }
  colnames(HQ_or_LF_RESTORATION_filter_pass) = c("ReachName","HQ_or_LF_restoration_filter_pass_yes_no")
  # ------------- combine -------------
  Output_Spring_Chinook_All = merge(Output_Spring_Chinook_All  ,HQ_or_LF_RESTORATION_filter_pass, by = "ReachName", all.x= TRUE )
  
  
  # ------------------------- Steelhead ---------------------------------- 
  # list "yes" or "no" if it has a limiting factor in a high priority life stage (maybe list the limiting factor OR life stage?)
  # ----------- loop through each reach and identify if the reach has a limiting factor in a priority life stage
  Output_LF_all = c()
  for(reach_x in Output_Steelhead_All$ReachName){
    
    # ------------------ pull the priority life stages in this reach -------------------
    output_life_stages_x = FUNCTION_pull_High_Priority_Life_Stages_for_a_reach(reach_x, "Steelhead")
    
    reach_in_LF_output = any(Limiting_Factor_Pathway_Steelhead[["Limiting_Factor_Pathway_Restoration"]]$ReachName == reach_x)
    # --------------- IF there is a reach in the LF Pathway output -------------
    if(reach_in_LF_output){
      rows_x = which(Limiting_Factor_Pathway_Steelhead[["Limiting_Factor_Pathway_Restoration"]]$ReachName == reach_x)
      #output_life_stages_x = c()
      output_limiting_factor_x = c()
      for(x in rows_x){
        #output_life_stages_x = c(output_life_stages_x, Limiting_Factor_Pathway_Steelhead[["Limiting_Factor_Pathway_Restoration"]][x,c("life_stage")])
        output_limiting_factor_x = c(output_limiting_factor_x, Limiting_Factor_Pathway_Steelhead[["Limiting_Factor_Pathway_Restoration"]][x,c("unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes")])
      }
      
    # ------------- IF reach is not in the LF pathway -----
    }else{
      #output_life_stages_x = "No Priority Life Stages with Limiting Factors"
      output_limiting_factor_x = "No Priority Life Stages with Limiting Factors"
    }
    output_life_stages_x = paste(output_life_stages_x, collapse = ",")
    output_limiting_factor_x = paste(output_limiting_factor_x, collapse = ",")
    Output_LF_x = t(as.data.frame(c(reach_x,output_life_stages_x, output_limiting_factor_x )))
    colnames(Output_LF_x) = c("ReachName", "Priority_Life_Stages_with_Limiting_Factors","Limiting_Factors_for_Priority_Life_Stages")
    Output_LF_all = rbind(Output_LF_all, Output_LF_x )
  }
  # ------------- combine -------------
  Output_Steelhead_All = merge(Output_Steelhead_All  ,Output_LF_all, by = "ReachName", all.x= TRUE )
  
  # ------------- verify which reach passes HQ pathway or LF Pathway for RESTORATION -------------
  HQ_or_LF_RESTORATION_filter_pass = c()
  for(reach_x in Output_Steelhead_All$ReachName){
    # -------------- pull row with reach --------
    x = which(Output_Steelhead_All$ReachName == reach_x)
    
    # --------------- if HQ score is NA, set to 100 (so it won't pass through the filter), otherwise, pull the HQ_Pct)
    if(is.na(Output_Steelhead_All$HQ_Pct[x])){
      HQ_Pct_x = 100
    }else{
      HQ_Pct_x = Output_Steelhead_All$HQ_Pct[x]
    }
    if(HQ_Pct_x< HQ_Score_Restoration_Reach_Scores$Category_upper_limit | 
       Output_Steelhead_All$Limiting_Factors_for_Priority_Life_Stages[x] != "No Priority Life Stages with Limiting Factors"){
      output_x = t(as.data.frame(c(reach_x, "yes")))
    }else{
      output_x = t(as.data.frame(c(reach_x, "no")))
    }
    HQ_or_LF_RESTORATION_filter_pass = rbind(HQ_or_LF_RESTORATION_filter_pass, output_x)
  }
  colnames(HQ_or_LF_RESTORATION_filter_pass) = c("ReachName","HQ_or_LF_restoration_filter_pass_yes_no")
  # ------------- combine -------------
  Output_Steelhead_All = merge(Output_Steelhead_All  ,HQ_or_LF_RESTORATION_filter_pass, by = "ReachName", all.x= TRUE )
  
 
  # ----------------------- BULL TROUT---------------------
  if(exclude_bull_trout == "no"){
    # ------------- only pull reaches with unacceptable attributes -----------
    Limiting_Factor_Pathway_Bull_Trout_Reach_Ranking = Limiting_Factor_Pathway_Bull_Trout[["Limiting_Factor_Pathway_Restoration"]][nchar(Limiting_Factor_Pathway_Bull_Trout[["Limiting_Factor_Pathway_Restoration"]]$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes)>0,]
    # --------------- filter  the reaches ---------------
    Limiting_Factor_Pathway_Restoration_Bull_Trout = Bull_Trout_Reach_Information_data_restoration %>%  
      filter(ReachName   %in%  Limiting_Factor_Pathway_Bull_Trout_Reach_Ranking$`ReachName`)
    print(paste("Bull Trout - total after LF score filter: ", nrow(Limiting_Factor_Pathway_Restoration_Bull_Trout), sep=""))
  }
  
  #  ---------------------------------------------------------------------------------
  #       Protection - pull reaches with Limiting Factor for a High Priority Life Stage
  #  ---------------------------------------------------------------------------------
  
  # ----------------------- SPRING CHINOOK ---------------------
  Limiting_Factor_Pathway_Protection_Spring_Chinook_LF_Protection_Pass = Spring_Chinook_Limiting_Factor_Scores_ALL_REACHES[which(Spring_Chinook_Limiting_Factor_Scores_ALL_REACHES$LF_Pct > 0.7), ]
  Limiting_Factor_Pathway_Protection_Spring_Chinook = Spring_Chinook_Reach_Information_data_protection %>%  
    filter(ReachName   %in%   Limiting_Factor_Pathway_Protection_Spring_Chinook_LF_Protection_Pass$ReachName)
  print(paste("Spring Chinook Restoration - total after LF score filter: ", nrow(Limiting_Factor_Pathway_Protection_Spring_Chinook), sep=""))
  # -------------- combine with HQ Pathway ------------
  
  # ----------------------- STEELHEAD ---------------------
  Limiting_Factor_Pathway_Protection_Steelhead_LF_Protection_Pass = Steelhead_Limiting_Factor_Scores_ALL_REACHES[which(Steelhead_Limiting_Factor_Scores_ALL_REACHES$LF_Pct > 0.7), ]
  Limiting_Factor_Pathway_Protection_Steelhead = Steelhead_Reach_Information_data_protection %>%  
    filter(ReachName   %in%   Limiting_Factor_Pathway_Protection_Steelhead_LF_Protection_Pass$ReachName)
  print(paste("Steelhead Restoration - total after LF score filter: ", nrow(Limiting_Factor_Pathway_Protection_Steelhead), sep=""))
  
  # ----------------------- BULL TROUT---------------------
  if(exclude_bull_trout == "no"){
    Limiting_Factor_Pathway_Protection_Bull_Trout = Bull_Trout_Reach_Information_data_protection %>%  
      filter(ReachName   %in%   Limiting_Factor_Pathway_Bull_Trout[["Limiting_Factor_Pathway_Protection"]]$`ReachName`)
    print(paste("Bull Trout - total after LF score filter: ", nrow(Limiting_Factor_Pathway_Protection_Bull_Trout), sep=""))
  }
  
  #  ---------------------------------------------------------------------------------
  #       Protection - all outputs - verify which reach passes HQ pathway or LF Pathway for PROTECTION
  #  ---------------------------------------------------------------------------------
  
  # ----------------------------- Spring Chinook ----------------------------
  # NOTE: put in LF score first
  HQ_or_LF_PROTECTION_filter_pass = c()
  for(reach_x in Output_Spring_Chinook_All$ReachName[which(Output_Spring_Chinook_All$Basin != "Okanogan")]){
    # -------------- pull row with reach --------
    x = which(Output_Spring_Chinook_All$ReachName == reach_x)
    
    # --------------- if HQ score is NA, set to 0 (so won't pass through filter), otherwise, pull the HQ_Pct)
    if(is.na(Output_Spring_Chinook_All$HQ_Pct[x])){
      HQ_Pct_x = 0
    }else{
      HQ_Pct_x = Output_Spring_Chinook_All$HQ_Pct[x]
    }
    if(HQ_Pct_x > HQ_Score_Protection_Reach_Scores$Category_lower_limit | 
       Output_Spring_Chinook_All$Limiting_Factors_for_Priority_Life_Stages[x] != "No Priority Life Stages with Limiting Factors"){
      output_x = t(as.data.frame(c(reach_x, "yes")))
    }else{
      output_x = t(as.data.frame(c(reach_x, "no")))
    }
    HQ_or_LF_PROTECTION_filter_pass = rbind(HQ_or_LF_PROTECTION_filter_pass, output_x)
  }
  colnames(HQ_or_LF_PROTECTION_filter_pass) = c("ReachName","HQ_or_LF_protection_filter_pass_yes_no")
  # ------------- combine -------------
  Output_Spring_Chinook_All = merge(Output_Spring_Chinook_All  ,HQ_or_LF_PROTECTION_filter_pass, by = "ReachName", all.x= TRUE )
  
  # ----------------------------- Steelhead ------------------------------
  # NOTE: put in LF score first
  HQ_or_LF_PROTECTION_filter_pass = c()
  for(reach_x in Output_Steelhead_All$ReachName){
    # -------------- pull row with reach --------
    x = which(Output_Steelhead_All$ReachName == reach_x)
    
    # --------------- if HQ score is NA, set to 0 (so won't pass through filter), otherwise, pull the HQ_Pct)
    if(is.na(Output_Steelhead_All$HQ_Pct[x])){
      HQ_Pct_x = 0
    }else{
      HQ_Pct_x = Output_Steelhead_All$HQ_Pct[x]
    }
    if(HQ_Pct_x > HQ_Score_Protection_Reach_Scores$Category_lower_limit | 
       Output_Steelhead_All$Limiting_Factors_for_Priority_Life_Stages[x] != "No Priority Life Stages with Limiting Factors"){
      output_x = t(as.data.frame(c(reach_x, "yes")))
    }else{
      output_x = t(as.data.frame(c(reach_x, "no")))
    }
    HQ_or_LF_PROTECTION_filter_pass = rbind(HQ_or_LF_PROTECTION_filter_pass, output_x)
  }
  colnames(HQ_or_LF_PROTECTION_filter_pass) = c("ReachName","HQ_or_LF_protection_filter_pass_yes_no")
  # ------------- combine -------------
  Output_Steelhead_All = merge(Output_Steelhead_All  ,HQ_or_LF_PROTECTION_filter_pass, by = "ReachName", all.x= TRUE )
  
  #  ---------------------------------------------------------------------------------
  #
  #         Combine LF and HQ Pathway, and pull HQ Pct score
  #
  #  ---------------------------------------------------------------------------------
  
  #  ---------------------------------------------------------------------------------
  #                Restoration
  #  ---------------------------------------------------------------------------------
  # ------------------ Spring Chinook ---------------
  HQ_and_LF_Pathway_Restoration_Spring_Chinook = rbind(Habitat_Quality_Pathway_Restoration_Spring_Chinook,  Limiting_Factor_Pathway_Restoration_Spring_Chinook)
  HQ_and_LF_Pathway_Restoration_Spring_Chinook = HQ_and_LF_Pathway_Restoration_Spring_Chinook[!duplicated(HQ_and_LF_Pathway_Restoration_Spring_Chinook$ReachName), ]
  
  # ------------------ Steelhead ---------------
  HQ_and_LF_Pathway_Restoration_Steelhead = rbind(Habitat_Quality_Pathway_Restoration_Steelhead,  Limiting_Factor_Pathway_Restoration_Steelhead)
  HQ_and_LF_Pathway_Restoration_Steelhead = HQ_and_LF_Pathway_Restoration_Steelhead[!duplicated(HQ_and_LF_Pathway_Restoration_Steelhead$ReachName), ]
  
  # ------------------ Bull Trout ---------------
  if(exclude_bull_trout == "no"){
    HQ_and_LF_Pathway_Restoration_Bull_Trout = rbind(Habitat_Quality_Pathway_Restoration_Bull_Trout,  Limiting_Factor_Pathway_Restoration_Bull_Trout)
    HQ_and_LF_Pathway_Restoration_Bull_Trout = HQ_and_LF_Pathway_Restoration_Bull_Trout[!duplicated(HQ_and_LF_Pathway_Restoration_Bull_Trout$ReachName), ]
  }
  
  #  ---------------------------------------------------------------------------------
  #                Protection
  #  ---------------------------------------------------------------------------------
  # ------------------ Spring Chinook ---------------
  HQ_and_LF_Pathway_Protection_Spring_Chinook = rbind(Habitat_Quality_Pathway_Protection_Spring_Chinook,  Limiting_Factor_Pathway_Protection_Spring_Chinook)
  HQ_and_LF_Pathway_Protection_Spring_Chinook = HQ_and_LF_Pathway_Protection_Spring_Chinook[!duplicated(HQ_and_LF_Pathway_Protection_Spring_Chinook$ReachName), ]
  
  # ------------------ Steelhead ---------------
  HQ_and_LF_Pathway_Protection_Steelhead = rbind(Habitat_Quality_Pathway_Protection_Steelhead,  Limiting_Factor_Pathway_Protection_Steelhead)
  HQ_and_LF_Pathway_Protection_Steelhead = HQ_and_LF_Pathway_Protection_Steelhead[!duplicated(HQ_and_LF_Pathway_Protection_Steelhead$ReachName), ]
  
  # ------------------ Bull Trout ---------------
  if(exclude_bull_trout == "no"){
    HQ_and_LF_Pathway_Protection_Bull_Trout = rbind(Habitat_Quality_Pathway_Protection_Bull_Trout,  Limiting_Factor_Pathway_Protection_Bull_Trout)
    HQ_and_LF_Pathway_Protection_Bull_Trout = HQ_and_LF_Pathway_Protection_Bull_Trout[!duplicated(HQ_and_LF_Pathway_Protection_Bull_Trout$ReachName), ]
  }
  
  
  #  ---------------------------------------------------------------------------------
  #
  #         Add HQ Scores (for scoring)
  #
  #  ---------------------------------------------------------------------------------
  
  # ------------------ prep HQ data for merge -------
  Habitat_Quality_Scores_for_merge = Habitat_Quality_Scores[,c("ReachName","HQ_Pct")]
  # ---------- add the Okanogan ---------------
  Habitat_Quality_Scores_for_merge_Okanogan = PRCNT_Habitat_Quality_Okanogan_EDT[,c("ReachName","HQ_Score")]
  colnames(Habitat_Quality_Scores_for_merge_Okanogan)[2] = "HQ_Pct"
  for(i in 1:nrow(Habitat_Quality_Scores_for_merge_Okanogan)){
    x = which(Habitat_Quality_Scores_for_merge$ReachName == Habitat_Quality_Scores_for_merge_Okanogan$ReachName[i])
    Habitat_Quality_Scores_for_merge$HQ_Pct[x] = Habitat_Quality_Scores_for_merge_Okanogan$HQ_Pct[i]
  }
  
  #  ---------------------------------------------------------------------------------
  #                Restoration
  #  ---------------------------------------------------------------------------------
  # ------------------- Spring Chinook ---------------
  HQ_and_LF_Pathway_Restoration_Spring_Chinook = merge(HQ_and_LF_Pathway_Restoration_Spring_Chinook, Habitat_Quality_Scores_for_merge, by="ReachName", all.y=FALSE) 
  # ------------------- Spring Chinook ---------------
  HQ_and_LF_Pathway_Restoration_Steelhead = merge(HQ_and_LF_Pathway_Restoration_Steelhead, Habitat_Quality_Scores_for_merge, by="ReachName", all.y=FALSE) 
  # ------------------- Spring Chinook ---------------
  if(exclude_bull_trout == "no"){
    HQ_and_LF_Pathway_Restoration_Bull_Trout = merge(HQ_and_LF_Pathway_Restoration_Bull_Trout, Habitat_Quality_Scores_for_merge, by="ReachName", all.y=FALSE) 
  }
  
  #  ---------------------------------------------------------------------------------
  #                Protection
  #  ---------------------------------------------------------------------------------
  # ------------------- Spring Chinook ---------------
  HQ_and_LF_Pathway_Protection_Spring_Chinook = merge(HQ_and_LF_Pathway_Protection_Spring_Chinook, Habitat_Quality_Scores_for_merge, by="ReachName", all.y=FALSE) 
  # ------------------- Spring Chinook ---------------
  HQ_and_LF_Pathway_Protection_Steelhead = merge(HQ_and_LF_Pathway_Protection_Steelhead, Habitat_Quality_Scores_for_merge, by="ReachName", all.y=FALSE) 
  # ------------------- Spring Chinook ---------------
  if(exclude_bull_trout == "no"){
    HQ_and_LF_Pathway_Protection_Bull_Trout = merge(HQ_and_LF_Pathway_Protection_Bull_Trout, Habitat_Quality_Scores_for_merge, by="ReachName", all.y=FALSE) 
  }
  
  #  ---------------------------------------------------------------------------------
  #
  #          Confinement
  #
  #  ---------------------------------------------------------------------------------
  
  #  ---------------------------------------------------------------------------------
  #           Reach Confinement  - RESTORATION
  #  ---------------------------------------------------------------------------------
  
  # ------------------------- Confinement criteria --------------------
  Reach_Confinement_Criteria_Restoration_Reach_Rankings = Restoration_Reach_Scoring[which(Restoration_Reach_Scoring$Indicator == "Confinement" &
                                                                                                  Restoration_Reach_Scoring$Category_Stage == "filter"),c("Category_lower_limit","Category_upper_limit")]
  # ----------------------- filter out for Confinement Scores --------------
  Confinement_Scores_Restoration = Confinement_Scores %>%  
    filter(Confined_Pct    >=   Reach_Confinement_Criteria_Restoration_Reach_Rankings$Category_lower_limit  ) %>%
    filter(Confined_Pct    <   Reach_Confinement_Criteria_Restoration_Reach_Rankings$Category_upper_limit)
  # ------------------------ identify Reaches that pass through filter ----------
  HQ_and_LF_Pathway_Restoration_Spring_Chinook = HQ_and_LF_Pathway_Restoration_Spring_Chinook %>%  
    filter(ReachName   %in%   Confinement_Scores_Restoration$`ReachName`)
  HQ_and_LF_Pathway_Restoration_Steelhead = HQ_and_LF_Pathway_Restoration_Steelhead %>%  
    filter(ReachName   %in%   Confinement_Scores_Restoration$`ReachName`)
  print(paste("HQ Pathway-RESTORATION Spring Chinook - total reaches after reach confinement filter: ", nrow(HQ_and_LF_Pathway_Restoration_Spring_Chinook), sep=""))
  print(paste("HQ Pathway-RESTORATION Steelhead - total reaches after reach confinement filter: ", nrow(HQ_and_LF_Pathway_Restoration_Steelhead), sep=""))
  
  if(exclude_bull_trout == "no"){
    HQ_and_LF_Pathway_Restoration_Bull_Trout = HQ_and_LF_Pathway_Restoration_Bull_Trout %>%  
      filter(ReachName   %in%   Confinement_Scores_Restoration$`ReachName`)
    print(paste("HQ Pathway-RESTORATION Bull Trout- total reaches after reach confinement filter: ", nrow(HQ_and_LF_Pathway_Restoration_Bull_Trout), sep=""))
  }
  
  # -------- add to total output -----
  confinement_output = Confinement_Scores[,c("ReachName","Unconfined_Pct")]
  confinement_output$Unconfined_more_than_0 = "yes"
  confinement_output$Unconfined_more_than_0[which(confinement_output$Unconfined_Pct == 0)] = "no"
  Output_Spring_Chinook_All = merge(Output_Spring_Chinook_All  ,confinement_output, by = "ReachName" ) 
  Output_Steelhead_All = merge(Output_Steelhead_All  ,confinement_output, by = "ReachName" ) 
  
  
  #  ---------------------------------------------------------------------------------
  #           Reach Confinement  - PROTECTION
  #  ---------------------------------------------------------------------------------
  
  # ------------------------- Confinement criteria --------------------
  Reach_Confinement_Criteria_Protection_Reach_Rankings = Protection_Reach_Scoring[which(Protection_Reach_Scoring$Indicator == "Confinement" &
                                                                                          Protection_Reach_Scoring$Category_Stage == "filter"),c("Category_lower_limit","Category_upper_limit")]
  # ----------------------- filter out for Confinement scores --------------
  Confinement_Scores_Protection = Confinement_Scores %>%  
    filter(Confined_Pct    >=   Reach_Confinement_Criteria_Protection_Reach_Rankings$Category_lower_limit  ) %>%
    filter(Confined_Pct    <   Reach_Confinement_Criteria_Protection_Reach_Rankings$Category_upper_limit)
  # ------------------------ identify Reaches that pass through filter ----------
  HQ_and_LF_Pathway_Protection_Spring_Chinook = HQ_and_LF_Pathway_Protection_Spring_Chinook %>%  
    filter(ReachName   %in%   Confinement_Scores_Protection$`ReachName`)
  HQ_and_LF_Pathway_Protection_Steelhead = HQ_and_LF_Pathway_Protection_Steelhead %>%  
    filter(ReachName   %in%   Confinement_Scores_Protection$`ReachName`)
  print(paste("HQ Pathway-PROTECTION Spring Chinook - total reaches after reach confinement filter: ", nrow(HQ_and_LF_Pathway_Protection_Spring_Chinook), sep=""))
  print(paste("HQ Pathway-PROTECTION Steelhead - total reaches after reach confinement filter: ", nrow(HQ_and_LF_Pathway_Protection_Steelhead), sep=""))
  # ------------------  Bull Trout ----------
  if(exclude_bull_trout == "no"){
    HQ_and_LF_Pathway_Protection_Bull_Trout = HQ_and_LF_Pathway_Protection_Bull_Trout %>%  
      filter(ReachName   %in%   Confinement_Scores_Protection$`ReachName`)
    print(paste("HQ Pathway-PROTECTION Bull Trout - total reaches after reach confinement filter: ", nrow(HQ_and_LF_Pathway_Protection_Bull_Trout), sep=""))
  }
  
  #  ---------------------------------------------------------------------------------
  #
  #        Number of Life Stages Filter (Restoration and Protection)
  #
  #  ---------------------------------------------------------------------------------

  # ----------- generate life stage filter -------------
  Life_Stage_Priorities_AU_and_Reach_data_FILTER_Restoration = Restoration_Reach_Scoring[which(Restoration_Reach_Scoring$Indicator == "Number_Life_Stage_Presence" &
                                                                                                 Restoration_Reach_Scoring$Category_Stage == "filter"),c("Category_lower_limit","Category_upper_limit")]
  Life_Stage_Priorities_AU_and_Reach_data_FILTER_Protection = Protection_Reach_Scoring[which(Protection_Reach_Scoring$Indicator == "Number_Life_Stage_Presence" &
                                                                                               Protection_Reach_Scoring$Category_Stage == "filter"),c("Category_lower_limit","Category_upper_limit")]
  
  # -------------------- add additional column for this particular species life stage sum ---------------
  Life_Stage_Priorities_AU_and_Reach_data["Life_Stage_Sum_Column_Spring_Chinook"] = Life_Stage_Priorities_AU_and_Reach_data[life_stage_sum_column_Spring_Chinook]
  Life_Stage_Priorities_AU_and_Reach_data["Life_Stage_Sum_Column_Steelhead"] = Life_Stage_Priorities_AU_and_Reach_data[life_stage_sum_column_Steelhead]
  if(exclude_bull_trout == "no"){
    Life_Stage_Priorities_AU_and_Reach_data["Life_Stage_Sum_Column_Bull_Trout"] = Life_Stage_Priorities_AU_and_Reach_data[life_stage_sum_column_Bull_Trout]
  }
  # ----------------------- filter out number of life stages for each species --------------
  Life_Stage_Spring_Chinook_Reaches = Life_Stage_Priorities_AU_and_Reach_data[which(  Life_Stage_Priorities_AU_and_Reach_data["Life_Stage_Sum_Column_Spring_Chinook"]   >=   Life_Stage_Priorities_AU_and_Reach_data_FILTER_Restoration$Category_lower_limit), c("ReachName") ]
  Life_Stage_Steelhead_Reaches = Life_Stage_Priorities_AU_and_Reach_data[which(  Life_Stage_Priorities_AU_and_Reach_data["Life_Stage_Sum_Column_Steelhead"]   >=   Life_Stage_Priorities_AU_and_Reach_data_FILTER_Restoration$Category_lower_limit), c("ReachName") ]
  if(exclude_bull_trout == "no"){
    Life_Stage_Bull_Trout_Reaches = Life_Stage_Priorities_AU_and_Reach_data[which(  Life_Stage_Priorities_AU_and_Reach_data["Life_Stage_Sum_Column_Bull_Trout"]   >=   Life_Stage_Priorities_AU_and_Reach_data_FILTER_Restoration$Category_lower_limit), c("ReachName") ]
  }
  
  #  ---------------------------------------------------------------------------------
  #          Life Stage Sum - RESTORATION
  #  ---------------------------------------------------------------------------------
  # ----------------------- merge with current filtered data --------------
  HQ_and_LF_Pathway_Restoration_Spring_Chinook = HQ_and_LF_Pathway_Restoration_Spring_Chinook %>%  
    filter(ReachName   %in%   Life_Stage_Spring_Chinook_Reaches$`ReachName`)
  HQ_and_LF_Pathway_Restoration_Steelhead = HQ_and_LF_Pathway_Restoration_Steelhead %>%  
    filter(ReachName   %in%   Life_Stage_Steelhead_Reaches$`ReachName`)
  print(paste("HQ Pathway-RESTORATION Spring Chinook - total reaches after life stage sum filter: ", nrow(HQ_and_LF_Pathway_Restoration_Spring_Chinook), sep=""))
  print(paste("HQ Pathway-RESTORATION Steelhead - total reaches after life stage sum filter: ", nrow(HQ_and_LF_Pathway_Restoration_Steelhead), sep=""))
  
  # -------- add to total output -----
  life_stage_sum_output = Life_Stage_Priorities_AU_and_Reach_data[,c("ReachName","Life_Stage_Sum_Column_Spring_Chinook")]
  Output_Spring_Chinook_All = merge(Output_Spring_Chinook_All  ,life_stage_sum_output, by = "ReachName" ) 
  
  life_stage_sum_output = Life_Stage_Priorities_AU_and_Reach_data[,c("ReachName","Life_Stage_Sum_Column_Steelhead")]
  Output_Steelhead_All = merge(Output_Steelhead_All  ,life_stage_sum_output, by = "ReachName" ) 
  
  # ------------- write the Output of the Reaches --------
  # -------- Spring Chinook -------
  output_path_x =  paste(output_path,Output_ALL_Spring_Chinook_file, sep="")
  write_xlsx(Output_Spring_Chinook_All,output_path_x )
  # -------- Steelhead -------
  output_path_x =  paste(output_path,Output_ALL_Steelhead_file, sep="")
  write_xlsx(Output_Steelhead_All,output_path_x )
  
  # ------------------  Bull Trout ----------
  if(exclude_bull_trout == "no"){
    HQ_and_LF_Pathway_Restoration_Bull_Trout = HQ_and_LF_Pathway_Restoration_Bull_Trout %>%  
      filter(ReachName   %in%   Life_Stage_Bull_Trout_Reaches$`ReachName`)
    print(paste("HQ Pathway-RESTORATION Bull Trout - total reaches after life stage sum filter: ", nrow(HQ_and_LF_Pathway_Restoration_Bull_Trout), sep=""))
  }
  
  #  ---------------------------------------------------------------------------------
  #           Life Stage Sum  - PROTECTION
  #  ---------------------------------------------------------------------------------
  
  # ----------------------- merge with current filtered data --------------
  HQ_and_LF_Pathway_Protection_Spring_Chinook = HQ_and_LF_Pathway_Protection_Spring_Chinook %>%  
    filter(ReachName   %in%   Life_Stage_Spring_Chinook_Reaches$`ReachName`)
  HQ_and_LF_Pathway_Protection_Steelhead = HQ_and_LF_Pathway_Protection_Steelhead %>%  
    filter(ReachName   %in%   Life_Stage_Steelhead_Reaches$`ReachName`)
  print(paste("HQ Pathway-PROTECTION Spring Chinook - total reaches after life stage sum filter: ", nrow(HQ_and_LF_Pathway_Protection_Spring_Chinook), sep=""))
  print(paste("HQ Pathway-PROTECTION Steelhead - total reaches after life stage sum filter: ", nrow(HQ_and_LF_Pathway_Protection_Steelhead), sep=""))
  # ------------------  Bull Trout ----------
  if(exclude_bull_trout == "no"){
    HQ_and_LF_Pathway_Protection_Bull_Trout = HQ_and_LF_Pathway_Protection_Bull_Trout %>%  
      filter(ReachName   %in%   Life_Stage_Bull_Trout_Reaches$`ReachName`)
    print(paste("HQ Pathway-PROTECTION Bull Trout - total reaches after life stage sum filter: ", nrow(HQ_and_LF_Pathway_Protection_Bull_Trout), sep=""))
  }
  
  #  ---------------------------------------------------------------------------------
  #
  #       Combine Across Species
  #
  #  ---------------------------------------------------------------------------------
  
  #  ---------------------------------------------------------------------------------
  #               Restoration - Combine All Species
  #  ---------------------------------------------------------------------------------
  
  # -------------------------- COMBINE across the species -----------------------------
  Habitat_Quality_Pathway_Restoration = HQ_and_LF_Pathway_Restoration_Spring_Chinook
  # -------- reaches in Steelhead (that are not in Spring Chinook) -------
  reaches_additional_Steelhead = setdiff(HQ_and_LF_Pathway_Restoration_Steelhead$ReachName,
                                         HQ_and_LF_Pathway_Restoration_Spring_Chinook$ReachName)
  x = HQ_and_LF_Pathway_Restoration_Steelhead$ReachName %in% reaches_additional_Steelhead 
  Habitat_Quality_Pathway_Restoration_Steelhead_X = HQ_and_LF_Pathway_Restoration_Steelhead[x,]

  # ----------- combine ---------------
  Habitat_Quality_Pathway_Restoration = rbind(Habitat_Quality_Pathway_Restoration,Habitat_Quality_Pathway_Restoration_Steelhead_X )
  
  # --------------- add Bull Trout -----------------
  if(exclude_bull_trout == "no"){
    # -------- reaches in Bull Trout (that are not in Spring Chinook) -------
    reaches_additional_Bull_Trout = setdiff(HQ_and_LF_Pathway_Restoration_Bull_Trout$ReachName,
                                            Habitat_Quality_Pathway_Restoration$ReachName)
    x = HQ_and_LF_Pathway_Restoration_Bull_Trout$ReachName %in% reaches_additional_Bull_Trout 
    Habitat_Quality_Pathway_Restoration_Bull_Trout_X = HQ_and_LF_Pathway_Restoration_Bull_Trout[x,]
    # ----------- combine ---------------
    Habitat_Quality_Pathway_Restoration = rbind(Habitat_Quality_Pathway_Restoration,Habitat_Quality_Pathway_Restoration_Bull_Trout_X )
  }
  
  #  ---------------------------------------------------------------------------------
  #               Protection - Combine All Species
  #  ---------------------------------------------------------------------------------
  
  # -------------------------- COMBINE across the species -----------------------------
  Habitat_Quality_Pathway_Protection = HQ_and_LF_Pathway_Protection_Spring_Chinook
  # -------- reaches in Steelhead (that are not in Spring Chinook) -------
  reaches_additional_Steelhead = setdiff(HQ_and_LF_Pathway_Protection_Steelhead$ReachName,
                                         HQ_and_LF_Pathway_Protection_Spring_Chinook$ReachName)
  x = HQ_and_LF_Pathway_Protection_Steelhead$ReachName %in% reaches_additional_Steelhead 
  Habitat_Quality_Pathway_Protection_Steelhead_X = HQ_and_LF_Pathway_Protection_Steelhead[x,]
  
  # ----------- combine ---------------
  Habitat_Quality_Pathway_Protection = rbind(Habitat_Quality_Pathway_Protection,Habitat_Quality_Pathway_Protection_Steelhead_X )
  
  # --------------- add Bull Trout -----------------
  if(exclude_bull_trout == "no"){
    # -------- reaches in Bull Trout (that are not in Spring Chinook) -------
    reaches_additional_Bull_Trout = setdiff(HQ_and_LF_Pathway_Protection_Bull_Trout$ReachName,
                                            Habitat_Quality_Pathway_Protection$ReachName)
    x = HQ_and_LF_Pathway_Protection_Bull_Trout$ReachName %in% reaches_additional_Bull_Trout 
    Habitat_Quality_Pathway_Protection_Bull_Trout_X = HQ_and_LF_Pathway_Protection_Bull_Trout[x,]
    # ----------- combine ---------------
    Habitat_Quality_Pathway_Protection = rbind(Habitat_Quality_Pathway_Protection,Habitat_Quality_Pathway_Protection_Bull_Trout_X )
  }
  
  # ---------------------------------------------------------------------------------------
  #
  #
  #     Reach Ranks (within AU ranks)
  #
  #
  # ---------------------------------------------------------------------------------------
  
  # ----------------------------------------------------------------------------------- 
  #
  #         RESTORATION: Generate and Output Scores
  #
  # ----------------------------------------------------------------------------------- 
  
  
  if( nrow(Habitat_Quality_Pathway_Restoration) > 0 ){
    
    # ----------------------------------------------------------------------------------- 
    #        Initiate Data Frame
    # ----------------------------------------------------------------------------------- 
    Restoration_Scores_Output = Habitat_Quality_Pathway_Restoration[,c("ReachName","Basin","Assessment.Unit")]
    
    # ----------------------------------------------------------------------------------- 
    #
    #         Confinement Scores (Scoring for Ranks)
    #
    # ----------------------------------------------------------------------------------- 

    # ------------------ add Confinement Percent ----------
    Restoration_Scores_Output = left_join(Restoration_Scores_Output, 
                                          Confinement_Scores_Restoration[,c("ReachName","Unconfined_Pct")], by = c("ReachName" = "ReachName"))
    # ---------- rename Unconfined_Pct column to "Unconfined_Percent" ---------
    colnames(Restoration_Scores_Output)[which(colnames(Restoration_Scores_Output) == "Unconfined_Pct")] = "Unconfined_Percent"
    # ------------------ indicator ratings  -----------------------
    #confinement_metric_data = Restoration_Reach_Scoring[which(Restoration_Reach_Scoring$Indicator == "Confinement" &
    #                                                            Restoration_Reach_Scoring$Category_Stage == "indicator"),c("Category_lower_limit","Category_upper_limit", "Score")]
    # ----------------- generate score --------------------
    #Restoration_Scores_Output = Restoration_Scores_Output  %>%
    #  mutate(Confinement_SCORES = ifelse(Unconfined_Pct  >= confinement_metric_data$Category_lower_limit[1] & 
    #                                       Unconfined_Pct  <= confinement_metric_data$Category_upper_limit[1] , confinement_metric_data$Score[1],
    #                        ifelse(Unconfined_Pct  > confinement_metric_data$Category_lower_limit[2] & 
    #                                 Unconfined_Pct  <= confinement_metric_data$Category_upper_limit[2] , confinement_metric_data$Score[2],
    #                               ifelse(Unconfined_Pct  >= confinement_metric_data$Category_lower_limit[3] & 
    #                                        Unconfined_Pct  <= confinement_metric_data$Category_upper_limit[3] , confinement_metric_data$Score[3],
    #                               NA))))
    
    # ----------------------------------------------------------------------------------- 
    #
    #         Habitat Quality Scores (Scoring for Ranks)
    #
    # ----------------------------------------------------------------------------------- 
    

    # --------------- update HQ_Pct name if necessary ----
    if(any(colnames(Habitat_Quality_Pathway_Restoration) == "HQ_Pct.y")){ colnames(Habitat_Quality_Pathway_Restoration)[which(colnames(Habitat_Quality_Pathway_Restoration) == "HQ_Pct.y")] = "HQ_Pct"  }
    # ------------------ add Habitat Quality Scores ----------
    Habitat_Quality_Pathway_Restoration$Habitat_Quality_Percent = (1 - Habitat_Quality_Pathway_Restoration$HQ_Pct) *100  # need to establish as percent - established per rules from March 2021 PWG
    Restoration_Scores_Output = left_join(Restoration_Scores_Output, 
                                          Habitat_Quality_Pathway_Restoration[,c("ReachName","Habitat_Quality_Percent")], by = c("ReachName" = "ReachName"))
    # ---------------- rename the HQ column since the score is 1 - HQ Pct --------------
    colnames(Restoration_Scores_Output)[length( colnames(Restoration_Scores_Output))] = "Habitat_Degradation_Percent"
    # ------------------ indicator ratings  -----------------------
    #habitat_quality_metric_data = Restoration_Reach_Scoring[which(Restoration_Reach_Scoring$Indicator == "Habitat_Quality" &
    #                                                            Restoration_Reach_Scoring$Category_Stage == "indicator"),c("Category_lower_limit","Category_upper_limit", "Score")]
    # ----------------- generate score --------------------
    #Restoration_Scores_Output = Restoration_Scores_Output  %>%
    #  mutate(Habitat_Quality_SCORES = ifelse(Habitat_Quality_Percent  >= habitat_quality_metric_data$Category_lower_limit[1] & 
    #                                          Habitat_Quality_Percent  <= habitat_quality_metric_data$Category_upper_limit[1] , habitat_quality_metric_data$Score[1],
    #                        ifelse(Habitat_Quality_Percent  > habitat_quality_metric_data$Category_lower_limit[2] & 
    #                                 Habitat_Quality_Percent  <= habitat_quality_metric_data$Category_upper_limit[2] , habitat_quality_metric_data$Score[2],
    #                               ifelse(Habitat_Quality_Percent  >= habitat_quality_metric_data$Category_lower_limit[3] & 
    #                                        Habitat_Quality_Percent  <= habitat_quality_metric_data$Category_upper_limit[3] , habitat_quality_metric_data$Score[3],
    #                                      NA))))
    

    # ----------------------------------------------------------------------------------- 
    #
    #         Limiting Factors for High Priority Life Stages (Scoring for Ranks)
    #
    # ----------------------------------------------------------------------------------- 
    
    # ------------------ add Habitat Quality Scores ----------
    #columns_to_output = c("ReachName", "Limiting_Factor_Score_Percent" , "Habitat_Attribute_Present_List", "Habitat_Attribute_Missing_List" , "Habitat_Attribute_All_Total" ,"Habitat_Attribute_Percent_Data_Presence")
    # all potential: c("ReachName","Habitat_Attribute_Present_List", "Habitat_Attribute_Missing_List" , "Habitat_Attribute_Present_Total", "Habitat_Attribute_All_Total" ,"Habitat_Attribute_Percent_Data_Presence", "Total_Attribute_Score", "Species", "Limiting_Factor_Score_Percent" )
    
    # ------------------ function that outputs the entire row (so you can check) ------
    Limiting_Factor_Output = FUNCTION_calc_Limiting_Factor_Score(Habitat_Quality_Pathway_Restoration)
    
    # ------------------- reduce the row for the output column --------------------
    Limiting_Factor_Output_truncated = Limiting_Factor_Output[,c("ReachName", "Life_Stage_Habitat_Degradation")]
    
    Restoration_Scores_Output = merge(Restoration_Scores_Output, 
                                          Limiting_Factor_Output_truncated, by = "ReachName", all.x=TRUE)
    
    
    # ------------------ indicator ratings  -----------------------
    #limiting_factor_metric_data = Restoration_Reach_Scoring[which(Restoration_Reach_Scoring$Indicator == "Limiting_Factor_for_High_Priority_Life_Stages" &
    #                                                                Restoration_Reach_Scoring$Category_Stage == "indicator"),c("Category_lower_limit","Category_upper_limit", "Score")]
    # ----------------- generate score --------------------
    #Restoration_Scores_Output = Restoration_Scores_Output  %>%
    #  mutate(Limiting_Factor_SCORES = ifelse(Limiting_Factor_Score_Percent  >= limiting_factor_metric_data$Category_lower_limit[1] & 
    #                                          Limiting_Factor_Score_Percent  <= limiting_factor_metric_data$Category_upper_limit[1] , limiting_factor_metric_data$Score[1],
    #                                        ifelse(Limiting_Factor_Score_Percent  > limiting_factor_metric_data$Category_lower_limit[2] & 
    #                                                 Limiting_Factor_Score_Percent  <= limiting_factor_metric_data$Category_upper_limit[2] , limiting_factor_metric_data$Score[2],
    #                                               ifelse(Limiting_Factor_Score_Percent  >= limiting_factor_metric_data$Category_lower_limit[3] & 
    #                                                        Limiting_Factor_Score_Percent  <= limiting_factor_metric_data$Category_upper_limit[3] , limiting_factor_metric_data$Score[3],
    #                                                      NA))))
    # ----------- set NAs to 1 -----------
    #x = which(is.na(Restoration_Scores_Output$Limiting_Factor_SCORES ))
    #Restoration_Scores_Output$Limiting_Factor_SCORES[x] = 1
    
    # ----------------------------------------------------------------------------------- 
    #
    #         Number of Life Stages Present 
    #         NOTE - we decided to exclude
    #
    # ----------------------------------------------------------------------------------- 
    
    # ------------------------ get the maximum life stage sum number across species -------------------
    #if(exclude_bull_trout == "no"){
    #  Life_Stage_Priorities_AU_and_Reach_data$Life_Stage_Sum_Max <-apply(X=Life_Stage_Priorities_AU_and_Reach_data[,c("Life_Stage_Sum_Column_Spring_Chinook","Life_Stage_Sum_Column_Steelhead", "Life_Stage_Sum_Column_Bull_Trout")], MARGIN=1, FUN=max)
    #}else{
    #  Life_Stage_Priorities_AU_and_Reach_data$Life_Stage_Sum_Max <-apply(X=Life_Stage_Priorities_AU_and_Reach_data[,c("Life_Stage_Sum_Column_Spring_Chinook","Life_Stage_Sum_Column_Steelhead")], MARGIN=1, FUN=max)
    #}
    
    # ------------------ add Life Stage Sum ----------
    #Restoration_Scores_Output = left_join(Restoration_Scores_Output, 
    #                                      Life_Stage_Priorities_AU_and_Reach_data[,c("ReachName", "Life_Stage_Sum_Max")], by = c("ReachName" = "ReachName"))
    # ------- update Life Stage Column Names --------
    #colnames(Restoration_Scores_Output)[length(colnames(Restoration_Scores_Output))] = "Life_Stage_Sum"
    # ------------------ indicator ratings  -----------------------
    #life_stage_metric_data = Restoration_Reach_Scoring[which(Restoration_Reach_Scoring$Indicator == "Number_Life_Stage_Presence" &
    #                                                            Restoration_Reach_Scoring$Category_Stage == "indicator"),c("Category_lower_limit","Category_upper_limit", "Score")]
    # ----------------- generate score --------------------
    #Restoration_Scores_Output = Restoration_Scores_Output  %>%
    #  mutate(Life_Stage_Sum_SCORES = ifelse(Life_Stage_Sum  >= life_stage_metric_data$Category_lower_limit[1] & 
    #                                      Life_Stage_Sum  <= life_stage_metric_data$Category_upper_limit[1] , life_stage_metric_data$Score[1],
    #                                    ifelse(Life_Stage_Sum  > life_stage_metric_data$Category_lower_limit[2] & 
    #                                             Life_Stage_Sum  <= life_stage_metric_data$Category_upper_limit[2] , life_stage_metric_data$Score[2],
    #                                                  NA)))
    
    # ----------------------------------------------------------------------------------- 
    #
    #       Add all the Percents for a Total (Scoring for Ranks)
    #
    # ----------------------------------------------------------------------------------- 
    Restoration_Scores_Output$Reach_Rank_Total_Score = rowSums(Restoration_Scores_Output[ , c("Unconfined_Percent", "Habitat_Degradation_Percent", "Life_Stage_Habitat_Degradation")])
    
    # ----------------------------------------------------------------------------------- 
    #
    #       Initiated Reach Ranks
    #
    # ----------------------------------------------------------------------------------- 
    
    Restoration_Scores_Output$AU_level_Reach_Rank = NA
    
    # ---------------------------------------------------------------------------------------
    #
    #     Identify "off ramps" (e.g. reaches automatically assigned)
    #     Note: Greer and I decided reaches identified here would not be included in reach ranking calculation (13.May.2021)
    #
    # ---------------------------------------------------------------------------------------
    
    # -------------------------------------------------------
    #     Add Fish Barrier Columns (will fill out below)
    # -------------------------------------------------------
    # ------------- create fish barrier column -------
    Restoration_Scores_Output$Fish_Barrier_Filter = 0
    
    # ----------------- identify if reaches in fish barrier overlap ---------
    reaches_barrier_intersection = intersect(Restoration_Scores_Output$ReachName, Barriers_Pathways_Data$ReachName)
    reaches_barrier_different = setdiff( Barriers_Pathways_Data$ReachName, Restoration_Scores_Output$ReachName)
    
    # ----------- if reach barriers are in an existing reach in Restoration_Scores_Output ----------
    if( length(reaches_barrier_intersection)>0 ){
      for(reach_x in reaches_barrier_intersection){
        # ------------ identify reach ------------
        x = which(Restoration_Scores_Output$ReachName == reach_x)
        # -------------- mark filter as 1 ------------
        Restoration_Scores_Output$Fish_Barrier_Filter[x] = 1
        # -------------- make AU Rank as 1 ------------
        Restoration_Scores_Output$AU_level_Reach_Rank[x] = 1
      }
    }
    
    # ----------- if reach barriers IS NOT in an existing reach in Restoration_Scores_Output ----------
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
    }
    
    # -------------------------------------------------------
    #     Add De-Water Reach
    # -------------------------------------------------------
    # fill in, in the future
    Restoration_Scores_Output$De_Watering_Human_Activity = 0
    
    # ----------------------------------------------------------------------------------- 
    #
    #       Generate within AU scores: Methow, Wenatchee, Entiat
    #
    # ----------------------------------------------------------------------------------- 
    
    # ---------------- pull unique AU -------------------
    x_basins = which(Restoration_Scores_Output$Basin == "Methow" |
                       Restoration_Scores_Output$Basin == "Entiat" |
                       Restoration_Scores_Output$Basin == "Wenatchee" )
    unique_AU = unique(Restoration_Scores_Output$Assessment.Unit[x_basins])
    # -------------- start the AU Rank score ---------
    Restoration_Scores_Output$AU_level_Reach_Rank = NA
    # -------------- start the EDT reach rank (NOTE this will only be present in Okanogan) ---------
    Restoration_Scores_Output$EDT_reach_rank = NA
    for(AU_x in unique_AU){
      
      # --------------- pull scores for reaches in this AU ---------
      AU_data_frame = Restoration_Scores_Output[which(Restoration_Scores_Output$Assessment.Unit == AU_x), ]
      # --------------- initiate AU_level_Reach_Rank ------------
      AU_data_frame$AU_level_Reach_Rank = NA
      # ---------------- remove NA ------------
      reaches_to_rank = which(!is.na(AU_data_frame$Reach_Rank_Total_Score))
      # --------- remove Fish Barrier or DeWater reaches (don't include in rank) -----
      fish_barrier_or_dewater_reaches = which(AU_data_frame$Fish_Barrier_Filter == 1 | AU_data_frame$De_Watering_Human_Activity == 1)
      # ----- remove reaches with fish barrier or dewatering -------
      if( length(fish_barrier_or_dewater_reaches) > 0){
        
        # ------------ assign reach rank of 1 for fish barrier and dewatering -----
        AU_data_frame$AU_level_Reach_Rank[fish_barrier_or_dewater_reaches] = 1
        
        
        # --------- if reaches with no fish barriers or dewatering activities -----
        if(length(reaches_to_rank)>0){
          # ------- remove the fish barrier and dewatering reaches from ordering for ranking (assigned a 1 above) ----
          reaches_to_rank = reaches_to_rank[! reaches_to_rank %in% fish_barrier_or_dewater_reaches ] 
          
        # -------- if only reaches in this AU have fish barrier or dewatering issues -------
        }else{
          reaches_to_rank = c()
        }
      }
      
      # ------------- Rank reaches (if reaches present to rank)  -----------------
      if(length(reaches_to_rank) > 0){
        AU_rank_x = rank(AU_data_frame$Reach_Rank_Total_Score[reaches_to_rank])
        low_3 = length(AU_rank_x)/3
        mid_3 = low_3 * 2
        low_3_x = which(AU_rank_x <= low_3)
        mid_3_x = which(AU_rank_x <= mid_3 & AU_rank_x > low_3)
        hi_3_x = which(AU_rank_x > mid_3)
        # ------------- Score 1, 2, 3 -----------

        AU_data_frame$AU_level_Reach_Rank[reaches_to_rank[low_3_x]] = 3
        AU_data_frame$AU_level_Reach_Rank[reaches_to_rank[mid_3_x]] = 2
        AU_data_frame$AU_level_Reach_Rank[reaches_to_rank[hi_3_x]] = 1

      }
      
      # -------------- add the AU Rank to data -----------------
      for(reach_x in AU_data_frame$ReachName){
        AU_rank_x = AU_data_frame$AU_level_Reach_Rank[ which(AU_data_frame$ReachName == reach_x)]
        Restoration_Scores_Output$AU_level_Reach_Rank[ which(Restoration_Scores_Output$ReachName == reach_x)] = AU_rank_x
      }
      #Restoration_Scores_Output$AU_level_Reach_Rank[which(Restoration_Scores_Output$Assessment.Unit == AU_x)] = AU_data_frame$AU_level_Reach_Rank
      
      
    }
    
    
    # ----------------------------------------------------------------------------------- 
    #
    #       Generate within AU scores: Okanogan
    #
    # ----------------------------------------------------------------------------------- 
    
    # ---------------- pull unique AU -------------------
    x_basins = which(Restoration_Scores_Output$Basin == "Okanogan" )
    unique_AU = unique(Restoration_Scores_Output$Assessment.Unit[x_basins])

    for(AU_x in unique_AU){
      
      # --------------- pull scores for reaches in this AU (from prioritization) ---------
      AU_data_frame = Restoration_Scores_Output[which(Restoration_Scores_Output$Assessment.Unit == AU_x), ]
      # --------------- pull scores for reaches in this AU (from EDT) ---------
      AU_data_frame_EDT = Reach_and_Attribute_Rank_Restoration_Okanogan[which(Reach_and_Attribute_Rank_Restoration_Okanogan$`Assessment Unit` == AU_x), ]
      # --------------- initiate AU_level_Reach_Rank ------------
      AU_data_frame$AU_level_Reach_Rank = NA
      # ---------------- remove NA ------------
      #reaches_to_rank = rank_reach_x[which(!is.na(AU_data_frame_EDT$Reach_Rank[rank_reach_x]))]
      
      # ----------------- copy EDT reach ranks over to prioritization data ---------
      i = 0
      reaches_to_rank = c()
      for(reach_x in AU_data_frame$ReachName){
        i = i + 1
        # ------ get EDT reach ------
        reach_x_EDT = which(AU_data_frame_EDT$ReachName == reach_x)
        if(length(reach_x_EDT) > 0){
          # ------- add reach rank --------
          AU_data_frame$AU_level_Reach_Rank[i] = AU_data_frame_EDT$Reach_Rank[reach_x_EDT]
          reaches_to_rank = c(reaches_to_rank,i)
        }
      }
      
      
      # --------- remove Fish Barrier or DeWater reaches (don't include in rank) -----
      fish_barrier_or_dewater_reaches = which(AU_data_frame$Fish_Barrier_Filter == 1 | AU_data_frame$De_Watering_Human_Activity == 1)
      # ----- remove reaches with fish barrier or dewatering -------
      if( length(fish_barrier_or_dewater_reaches) > 0){
        
        # ------------ assign reach rank of 1 for fish barrier and dewatering -----
        AU_data_frame$AU_level_Reach_Rank[fish_barrier_or_dewater_reaches] = 1
        
        
        # --------- if reaches with no fish barriers or dewatering activities -----
        if(length(reaches_to_rank)>0){
          # ------- remove the fish barrier and dewatering reaches from ordering for ranking (assigned a 1 above) ----
          reaches_to_rank = reaches_to_rank[! reaches_to_rank %in% fish_barrier_or_dewater_reaches ] 
          
          # -------- if only reaches in this AU have fish barrier or dewatering issues -------
        }else{
          reaches_to_rank = c()
        }
      }
      
      # ------------- Rank reaches (if reaches present to rank)  -----------------
      if(length(reaches_to_rank) > 0){
        AU_rank_x = rank(AU_data_frame$AU_level_Reach_Rank[reaches_to_rank])
        low_3 = length(AU_rank_x)/3
        mid_3 = low_3 * 2
        low_3_x = which(AU_rank_x <= low_3)
        mid_3_x = which(AU_rank_x <= mid_3 & AU_rank_x > low_3)
        hi_3_x = which(AU_rank_x > mid_3)
        # ------------- Score 1, 2, 3 -----------
        
        AU_data_frame$AU_level_Reach_Rank[reaches_to_rank[low_3_x]] = 3
        AU_data_frame$AU_level_Reach_Rank[reaches_to_rank[mid_3_x]] = 2
        AU_data_frame$AU_level_Reach_Rank[reaches_to_rank[hi_3_x]] = 1
        
      }
      
      # -------------- add the AU Rank to data -----------------
      for(reach_x in AU_data_frame$ReachName){
        AU_rank_x = AU_data_frame$AU_level_Reach_Rank[ which(AU_data_frame$ReachName == reach_x)]
        Restoration_Scores_Output$AU_level_Reach_Rank[ which(Restoration_Scores_Output$ReachName == reach_x)] = AU_rank_x
      }
      #Restoration_Scores_Output$AU_level_Reach_Rank[which(Restoration_Scores_Output$Assessment.Unit == AU_x)] = AU_data_frame$AU_level_Reach_Rank
      
      # -------------- add Okanogan AU level rank ---------
      for(reach_x in AU_data_frame_EDT$ReachName){
        EDT_AU_rank_x = AU_data_frame_EDT$Reach_Rank[ which(AU_data_frame_EDT$ReachName == reach_x)]
        Restoration_Scores_Output$EDT_reach_rank[ which(Restoration_Scores_Output$ReachName == reach_x)] = EDT_AU_rank_x
      }
      #Restoration_Scores_Output$EDT_reach_rank[which(Restoration_Scores_Output$Assessment.Unit == AU_x)] = AU_data_frame_EDT$Reach_Rank 
      
    }
    
    # ----------------------------------------------------------------------------------- 
    #
    #       IF data is missing - put in "Missing Data"
    #
    # ----------------------------------------------------------------------------------- 
    AU_level_NA = which(is.na(Restoration_Scores_Output$AU_level_Reach_Rank))
    Restoration_Scores_Output$AU_level_Reach_Rank[AU_level_NA] = "Missing_Data"
    
    
    
  }else{
    
    print(paste("--- No Restoration Reaches generated for species: ", species_x, sep=""))
    Restoration_Scores_Output = NA
  }
  

  
  # -------------------------------------------------------
  #
  #     Calculate Scores and Sort into Tiers
  #
  # ------------------------------------------------------- 

  # ----------------------------------------------
  #    Rank them (across all reaches)
  # ----------------------------------------------
  Restoration_Scores_Output$Rank_Across_All_Reaches = rank( -Restoration_Scores_Output$Reach_Rank_Total_Score, ties.method= "min")

  # ----------------------------------------------------------------------------------- 
  #
  #         PROTECTION: Generate and Output Scores
  #
  # ----------------------------------------------------------------------------------- 
  
  
  if( nrow(Habitat_Quality_Pathway_Protection) > 0 ){
    
    # ----------------------------------------------------------------------------------- 
    #        Initiate Data Frame
    # ----------------------------------------------------------------------------------- 
    Protection_Scores_Output = Habitat_Quality_Pathway_Protection[,c("ReachName","Basin","Assessment.Unit")]
    
    # ----------------------------------------------------------------------------------- 
    #
    #         Confinement Scores
    #
    # ----------------------------------------------------------------------------------- 
    
    # ------------------ add Confinement Percent ----------
    Protection_Scores_Output = left_join(Protection_Scores_Output, 
                                         Confinement_Scores_Protection[,c("ReachName","Unconfined_Pct")], by = c("ReachName" = "ReachName"))
    # ---------- rename Unconfined_Pct column to "Unconfined_Percent" ---------
    colnames(Protection_Scores_Output)[which(colnames(Protection_Scores_Output) == "Unconfined_Pct")] = "Unconfined_Percent"
    # ------------------ indicator ratings  -----------------------
    #confinement_metric_data = Protection_Reach_Scoring[which(Protection_Reach_Scoring$Indicator == "Confinement" &
    #                                                           Protection_Reach_Scoring$Category_Stage == "indicator"),c("Category_lower_limit","Category_upper_limit", "Score")]
    # ----------------- generate score --------------------
    #Protection_Scores_Output = Protection_Scores_Output  %>%
    #  mutate(Confinement_SCORES = ifelse(Unconfined_Pct  >= confinement_metric_data$Category_lower_limit[1] & 
    #                                       Unconfined_Pct  <= confinement_metric_data$Category_upper_limit[1] , confinement_metric_data$Score[1],
    #                        ifelse(Unconfined_Pct  > confinement_metric_data$Category_lower_limit[2] & 
    #                                 Unconfined_Pct  <= confinement_metric_data$Category_upper_limit[2] , confinement_metric_data$Score[2],
    #                               ifelse(Unconfined_Pct  >= confinement_metric_data$Category_lower_limit[3] & 
    #                                        Unconfined_Pct  <= confinement_metric_data$Category_upper_limit[3] , confinement_metric_data$Score[3],
    #                               NA))))
    
    # ----------------------------------------------------------------------------------- 
    #
    #         Habitat Quality Scores
    #
    # ----------------------------------------------------------------------------------- 
    
    
    # --------------- update HQ_Pct name if necessary ----
    if(any(colnames(Habitat_Quality_Pathway_Protection) == "HQ_Pct.y")){ colnames(Habitat_Quality_Pathway_Protection)[which(colnames(Habitat_Quality_Pathway_Protection) == "HQ_Pct.y")] = "HQ_Pct"  }
    # ------------------ add Habitat Quality Scores ----------
    Habitat_Quality_Pathway_Protection$Habitat_Quality_Percent = (Habitat_Quality_Pathway_Protection$HQ_Pct) *100  # need to establish as percent - established per rules from March 2021 PWG
    Protection_Scores_Output = left_join(Protection_Scores_Output, 
                                         Habitat_Quality_Pathway_Protection[,c("ReachName","Habitat_Quality_Percent")], by = c("ReachName" = "ReachName"))
    # ------------------ indicator ratings  -----------------------
    #habitat_quality_metric_data = Protection_Reach_Scoring[which(Protection_Reach_Scoring$Indicator == "Habitat_Quality" &
    #                                                            Protection_Reach_Scoring$Category_Stage == "indicator"),c("Category_lower_limit","Category_upper_limit", "Score")]
    # ----------------- generate score --------------------
    #Protection_Scores_Output = Protection_Scores_Output  %>%
    #  mutate(Habitat_Quality_SCORES = ifelse(Habitat_Quality_Percent  >= habitat_quality_metric_data$Category_lower_limit[1] & 
    #                                          Habitat_Quality_Percent  <= habitat_quality_metric_data$Category_upper_limit[1] , habitat_quality_metric_data$Score[1],
    #                        ifelse(Habitat_Quality_Percent  > habitat_quality_metric_data$Category_lower_limit[2] & 
    #                                 Habitat_Quality_Percent  <= habitat_quality_metric_data$Category_upper_limit[2] , habitat_quality_metric_data$Score[2],
    #                               ifelse(Habitat_Quality_Percent  >= habitat_quality_metric_data$Category_lower_limit[3] & 
    #                                        Habitat_Quality_Percent  <= habitat_quality_metric_data$Category_upper_limit[3] , habitat_quality_metric_data$Score[3],
    #                                      NA))))
    
    
    # ----------------------------------------------------------------------------------- 
    #
    #         Threats (% of floodplain disturbance)
    #
    # ----------------------------------------------------------------------------------- 
    
    # ------------------ add Threats Score ----------
    Protection_Scores_Output = left_join(Protection_Scores_Output, 
                                         Degraded_Floodplain_Data[,c("ReachName","Degraded_Area_Percent")], by = c("ReachName" = "ReachName"))
    
    # ----------------------------------------------------------------------------------- 
    #
    #         Ownership (% private ownership)
    #
    # ----------------------------------------------------------------------------------- 
    
    # ------------------ add Threats Score ----------
    # NOTE - if you add - make sure to add below when columns are selected
    #Protection_Scores_Output$Ownership_Percent = NA
    
    
    # ----------------------------------------------------------------------------------- 
    #
    #         Connection (Connection to other existing protected areas)
    #
    # ----------------------------------------------------------------------------------- 
    
    # ------------------ add Threats Score ----------
    Protection_Scores_Output = left_join(Protection_Scores_Output, 
                                         Protected_Percentage_Data[,c("ReachName","Protected_Percent")], by = c("ReachName" = "ReachName"))
    
    # ----------------------------------------------------------------------------------- 
    #
    #       Add all the Percents for a Total
    #
    # ----------------------------------------------------------------------------------- 
    Protection_Scores_Output$Reach_Rank_Total_Score = rowSums(Protection_Scores_Output[ , c("Unconfined_Percent", "Habitat_Quality_Percent","Degraded_Area_Percent", "Protected_Percent" )])
    
    # ----------------------------------------------------------------------------------- 
    #
    #       Initiated Reach Ranks
    #
    # ----------------------------------------------------------------------------------- 
    
    Protection_Scores_Output$AU_level_Reach_Rank = NA
    
    # ---------------------------------------------------------------------------------------
    #
    #     Identify "off ramps" (e.g. reaches automatically assigned)
    #     Note: Greer and I decided reaches identified here would not be included in reach ranking calculation (13.May.2021)
    #
    # ---------------------------------------------------------------------------------------
    
    # -------------------------------------------------------
    #     Add 90-100% Protected Column
    # -------------------------------------------------------
    # ------------- create column -------
    Protection_Scores_Output$Protected_90_100_Pct = 0
    # --------- identify which reaches are above 90% protected -----
    above_90_Pct_x = which(Protection_Scores_Output$Protected_Percent>90)
    # ------------- set to 1 if reach is above 90% protected ------------------
    Protection_Scores_Output$Protected_90_100_Pct[above_90_Pct_x] = 1
    
    # ----------------------------------------------------------------------------------- 
    #
    #       Generate within AU scores: Methow, Wenatchee, Entiat
    #
    # ----------------------------------------------------------------------------------- 
    
    # ---------------- pull unique AU -------------------
    x_basins = which(Protection_Scores_Output$Basin == "Methow" |
                       Protection_Scores_Output$Basin == "Entiat" |
                       Protection_Scores_Output$Basin == "Wenatchee" )
    unique_AU = unique(Protection_Scores_Output$Assessment.Unit[x_basins])
    # -------------- start the AU Rank score ---------
    Protection_Scores_Output$AU_level_Reach_Rank = NA
    # -------------- start the EDT reach rank (NOTE this will only be present in Okanogan) ---------
    Protection_Scores_Output$EDT_reach_rank = NA
    for(AU_x in unique_AU){
      
      # --------------- pull scores for reaches in this AU ---------
      AU_data_frame = Protection_Scores_Output[which(Protection_Scores_Output$Assessment.Unit == AU_x), ]
      # --------------- initiate AU_level_Reach_Rank ------------
      AU_data_frame$AU_level_Reach_Rank = NA
      # ---------------- remove NA ------------
      reaches_to_rank = which(!is.na(AU_data_frame$Reach_Rank_Total_Score))
      # --------- remove Fish Barrier or DeWater reaches (don't include in rank) -----
      protected_90_100_off_ramp = which(AU_data_frame$Protected_90_100_Pct == 1 )
      # ----- remove reaches with fish barrier or dewatering -------
      if( length(protected_90_100_off_ramp) > 0){
        
        # ------------ assign reach rank of 1 for fish barrier and dewatering -----
        AU_data_frame$AU_level_Reach_Rank[protected_90_100_off_ramp] = 3
        
        
        # --------- if reaches with no fish barriers or dewatering activities -----
        if(length(reaches_to_rank)>0){
          # ------- remove the fish barrier and dewatering reaches from ordering for ranking (assigned a 1 above) ----
          reaches_to_rank = reaches_to_rank[! reaches_to_rank %in% protected_90_100_off_ramp ] 
          
          # -------- if only reaches in this AU have fish barrier or dewatering issues -------
        }else{
          reaches_to_rank = c()
        }
      }
      
      # ------------- Rank reaches (if reaches present to rank)  -----------------
      if(length(reaches_to_rank) > 0){
        AU_rank_x = rank(AU_data_frame$Reach_Rank_Total_Score[reaches_to_rank])
        low_3 = length(AU_rank_x)/3
        mid_3 = low_3 * 2
        low_3_x = which(AU_rank_x <= low_3)
        mid_3_x = which(AU_rank_x <= mid_3 & AU_rank_x > low_3)
        hi_3_x = which(AU_rank_x > mid_3)
        # ------------- Score 1, 2, 3 -----------
        
        AU_data_frame$AU_level_Reach_Rank[reaches_to_rank[low_3_x]] = 3
        AU_data_frame$AU_level_Reach_Rank[reaches_to_rank[mid_3_x]] = 2
        AU_data_frame$AU_level_Reach_Rank[reaches_to_rank[hi_3_x]] = 1
        
      }
      
      # -------------- add the AU Rank to data -----------------
      for(reach_x in AU_data_frame$ReachName){
        AU_rank_x = AU_data_frame$AU_level_Reach_Rank[ which(AU_data_frame$ReachName == reach_x)]
        Protection_Scores_Output$AU_level_Reach_Rank[ which(Protection_Scores_Output$ReachName == reach_x)] = AU_rank_x
      }
      #Protection_Scores_Output$AU_level_Reach_Rank[which(Protection_Scores_Output$Assessment.Unit == AU_x)] = AU_data_frame$AU_level_Reach_Rank
      
      
    }
    
    
    # ----------------------------------------------------------------------------------- 
    #
    #       Generate within AU scores: Okanogan
    #
    # ----------------------------------------------------------------------------------- 
    
    # ---------------- pull unique AU -------------------
    x_basins = which(Protection_Scores_Output$Basin == "Okanogan" )
    unique_AU = unique(Protection_Scores_Output$Assessment.Unit[x_basins])
    
    for(AU_x in unique_AU){
      
      # --------------- pull scores for reaches in this AU (from prioritization) ---------
      AU_data_frame = Protection_Scores_Output[which(Protection_Scores_Output$Assessment.Unit == AU_x), ]
      # --------------- pull scores for reaches in this AU (from EDT) ---------
      AU_data_frame_EDT = Reach_and_Attribute_Rank_Protection_Okanogan[which(Reach_and_Attribute_Rank_Protection_Okanogan$`Assessment Unit` == AU_x), ]
      # --------------- initiate AU_level_Reach_Rank ------------
      AU_data_frame$AU_level_Reach_Rank = NA
      # ---------------- remove NA ------------
      #reaches_to_rank = rank_reach_x[which(!is.na(AU_data_frame_EDT$Reach_Rank[rank_reach_x]))]
      
      # ----------------- copy EDT reach ranks over to prioritization data ---------
      
      reaches_to_rank = c()
      for(i in 1:nrow(AU_data_frame)){
        reach_x = AU_data_frame$ReachName[i]
        
        # ------ get EDT reach ------
        reach_x_EDT = which(AU_data_frame_EDT$ReachName == reach_x)
        if(length(reach_x_EDT) > 0){
          # ------- add reach rank --------
          AU_data_frame$AU_level_Reach_Rank[i] = AU_data_frame_EDT$Reach_Rank[reach_x_EDT]
          reaches_to_rank = c(reaches_to_rank,i)
        }
      }
      
      
      # --------- remove  reaches with >90% protection  (don't include in rank) -----
      protected_90_100_off_ramp = which(AU_data_frame$Protected_90_100_Pct == 1 )
      # ----- remove  reaches with >90% protection -------
      if( length(protected_90_100_off_ramp) > 0){
        
        # ------------ assign reach rank of 1 for fish barrier and dewatering -----
        AU_data_frame$AU_level_Reach_Rank[protected_90_100_off_ramp] = 3
        
        
        # --------- if reaches with no fish barriers or dewatering activities -----
        if(length(reaches_to_rank)>0){
          # ------- remove the reaches with >90% protection from ordering for ranking (assigned a 1 above) ----
          reaches_to_rank = reaches_to_rank[! reaches_to_rank %in% protected_90_100_off_ramp ] 
          
          # -------- if only reaches in this AU havereaches with >90% protection -------
        }else{
          reaches_to_rank = c()
        }
      }
      
      # ------------- Rank reaches (if reaches present to rank)  -----------------
      if(length(reaches_to_rank) > 0){
        AU_rank_x = rank(AU_data_frame$AU_level_Reach_Rank[reaches_to_rank])
        low_3 = length(AU_rank_x)/3
        mid_3 = low_3 * 2
        low_3_x = which(AU_rank_x <= low_3)
        mid_3_x = which(AU_rank_x <= mid_3 & AU_rank_x > low_3)
        hi_3_x = which(AU_rank_x > mid_3)
        # ------------- Score 1, 2, 3 -----------
        
        AU_data_frame$AU_level_Reach_Rank[reaches_to_rank[low_3_x]] = 3
        AU_data_frame$AU_level_Reach_Rank[reaches_to_rank[mid_3_x]] = 2
        AU_data_frame$AU_level_Reach_Rank[reaches_to_rank[hi_3_x]] = 1
        
      }
      
      # -------------- add the AU Rank to data -----------------
      for(reach_x in AU_data_frame$ReachName){
        AU_rank_x = AU_data_frame$AU_level_Reach_Rank[ which(AU_data_frame$ReachName == reach_x)]
        Protection_Scores_Output$AU_level_Reach_Rank[ which(Protection_Scores_Output$ReachName == reach_x)] = AU_rank_x
      }
      #Protection_Scores_Output$AU_level_Reach_Rank[which(Protection_Scores_Output$Assessment.Unit == AU_x)] = AU_data_frame$AU_level_Reach_Rank
      
      # -------------- add Okanogan AU level rank ---------
      for(reach_x in AU_data_frame_EDT$ReachName){
        EDT_AU_rank_x = AU_data_frame_EDT$Reach_Rank[ which(AU_data_frame_EDT$ReachName == reach_x)]
        Protection_Scores_Output$EDT_reach_rank[ which(Protection_Scores_Output$ReachName == reach_x)] = EDT_AU_rank_x
      }
      
      
      
    }
    
    # ----------------------------------------------------------------------------------- 
    #
    #       IF data is missing - put in "Missing Data"
    #
    # ----------------------------------------------------------------------------------- 
    AU_level_NA = which(is.na(Protection_Scores_Output$AU_level_Reach_Rank))
    Protection_Scores_Output$AU_level_Reach_Rank[AU_level_NA] = "Missing_Data"
    
  }else{
    
    print(paste("--- No Protection Reaches generated for species: ", species_x, sep=""))
    Protection_Scores_Output = NA
  }
  
  # ----------------- convert AU Reach Ranks to numeric -------
  Restoration_Scores_Output$AU_level_Reach_Rank = as.numeric(as.character(Restoration_Scores_Output$AU_level_Reach_Rank ))
  Protection_Scores_Output$AU_level_Reach_Rank = as.numeric(as.character(Protection_Scores_Output$AU_level_Reach_Rank ))
  
  # --------------------- Put Restoration and Protection Reach Rankings into a list --------------
  Reach_Rankings_Combined = list( 
    "Reach_Rankings_Restoration" = Restoration_Scores_Output,
    "Reach_Ranking_Protection" = Protection_Scores_Output
  )
  
  return(Reach_Rankings_Combined)

  
}


#output_path_x =  paste(output_path,'Protection_Reach_Ranking_Scores_Output.xlsx', sep="")
#write_xlsx(Protection_Scores_Output,output_path_x )
#output_path_x =  paste(output_path,'Restoration_Reach_Ranking_Scores_Output.xlsx', sep="")
#write_xlsx(Restoration_Scores_Output,output_path_x )

# get percent that have 100%
# x = which(Restoration_Scores_Output$Habitat_Attribute_Percent_Data_Presence == 1)
# length(x) / nrow(Restoration_Scores_Output)

# x = which(Restoration_Scores_Output$Habitat_Attribute_Percent_Data_Presence == 0)
# length(x) / nrow(Restoration_Scores_Output)





# --------------------------------------------------------------------------------------------------------------------------
#
#
#         Function to calculate Limiting Factors for Reach Rankings
#
#
# --------------------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------------------
#           Row functions 
# --------------------------------------------------------------------------------------------------------------------------

# R function to pull the number from a row
two_attribute_fxn = function(x, output) {
  # x is the row of type Character
  # access element in first column
  initial_attribute = x[1]
  # access element in second column
  second_attribute = x[2]
  #your code to process x
  if( !is.na(initial_attribute) ){
    output_x_i = initial_attribute
  }else{
    output_x_i = second_attribute
  }
  output_x_i  # return this value
}

# R function to pull the number from a row
two_attribute_T_or_F_fxn = function(x, output) {
  # x is the row of type Character
  # access element in first column
  initial_attribute = x[1]
  # access element in second column
  second_attribute = x[2]
  #your code to process x
  if( !is.na(initial_attribute) ){
    if(initial_attribute == TRUE){
      output_x_i = TRUE
    }else if(initial_attribute == FALSE){
      
      if(!is.na(second_attribute)){
        
        if(second_attribute == TRUE){
          output_x_i = TRUE
        }else if(second_attribute == FALSE){
          output_x_i = FALSE
        }
        
      }else{
        output_x_i = initial_attribute
      }
      
    }else{
      output_x_i = FALSE
    }
    
  }else{
    output_x_i = second_attribute
  }
  
  
  output_x_i  # return this value
}


# R function to pull the number from a row
pull_T_F_col_name_fxn = function(x, output) {
  # which columns were true or false
  row_t_f = which(!is.na(x))
  # which were true
  row_t = which(x == TRUE)
  # which were false
  row_f = which(x == FALSE)
  
  # ------ put together -----
  output_x_i =  t( as.data.frame( c( length(row_t_f), length(row_t), length(row_f) ) )  )
  
  output_x_i  # return this value
}


# function to calculate number of habitat attributes that are impaired 
function_total_impaired_habitat_attribute = function(x, output){
  
  # ------------------- identify which of the attributes are impaired --------
  x_2 = x[which(x == 1)]
  
  # ------------- get length (total habitat attributes) --------------
  output_x_i = length(x_2)
  
  # -------------- return ----------------
  output_x_i
}

#data_x = apply(Limiting_Factor_Reach_ALL_attributes[, 3:ncol(Limiting_Factor_Reach_ALL_attributes)], 1, function_total_impaired_habitat_attribute)


# --------------------------------------------------------------------------------------------------------------------------
#        Primary Function
# --------------------------------------------------------------------------------------------------------------------------

#data_x = Limiting_Factor_Reach_ALL_attributes[1:10,]
#apply(data_x[,2:ncol(data_x)], 1, function_total_impaired_habitat_attribute)

# Habitat_Quality_Pathway_Restoration_MASTER = Habitat_Quality_Pathway_Restoration

#FUNCTION_calc_Limiting_Factor_Score(Habitat_Quality_Pathway_Restoration) 

FUNCTION_calc_Limiting_Factor_Score = function(Habitat_Quality_Pathway_Restoration ){
  
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
  Limiting_Factor_Reach_Ranking_Scores = as.data.frame(Habitat_Quality_Pathway_Restoration[,'ReachName'])
  colnames(Limiting_Factor_Reach_Ranking_Scores) = "ReachName"
  # --------------- add columns ----------------
  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_List = NA
  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Missing_List = NA
  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_Total = 0
  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_All_Total = 0
  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Percent_Data_Presence = 0
  Limiting_Factor_Reach_Ranking_Scores$Total_Attribute_Score = 0
  Limiting_Factor_Reach_Ranking_Scores$Species = NA
  Limiting_Factor_Reach_Ranking_Scores$Priority_Life_Stages = NA
  
  # ---------------- start data frame to combine all habitat attributes -------
  Limiting_Factor_Reach_ALL_attributes = as.data.frame(Limiting_Factor_Reach_Ranking_Scores$ReachName)
  colnames(Limiting_Factor_Reach_ALL_attributes) = "ReachName"
  Limiting_Factor_Reach_ALL_attributes$Priority_Life_Stages = NA
  # ---------------- start data frame for True/False if habitat data present -------
  Limiting_Factor_Reach_ALL_attributes_T_F_Presence = as.data.frame(Limiting_Factor_Reach_Ranking_Scores$ReachName)
  colnames(Limiting_Factor_Reach_ALL_attributes_T_F_Presence) = "ReachName"
  
  # --------------------------------------------------------------------------
  #         Get Species
  # --------------------------------------------------------------------------
  # -------- get species list ----------
  if(exclude_bull_trout == "no"){
    Species_List = c("Spring Chinook", "Steelhead", "Bull Trout")
  }else{
    Species_List = c("Spring Chinook", "Steelhead")
  }
  
  reaches_unique_x = c() # just to output which reaches have high priority life stages
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
    if(species_x == "Bull Trout" & exclude_bull_trout == "no"){
      # ---------------- life stage priority names ---------
      life_stages_priorities_species_specific =  life_stages_priorities[['bull_trout_life_stages']]
      life_stages_prescence_species_specific =  life_stages_prescence[['bull_trout_life_stages']]
    }
    
    # --------------------------------------------------------------------------
    #         Generate Habitat Attribute Scores and Add to Limiting_Factor_Reach_Ranking_Scores
    # --------------------------------------------------------------------------
    # -------------- Get unique life stages ---------------------
    life_stages_unique = unique(Attribute_LifeStage_Crosswalk$`Life Stage`[which(Attribute_LifeStage_Crosswalk$Species == species_x)])
    life_stage_x = life_stages_unique[2]
    
    for(life_stage_x in life_stages_unique){
      print(life_stage_x)
      # ------------------------------------------
      #    Filter out if life stage is high priority in reaches
      # ---------------------------------------------
      # NOTE - just pulling 
      # --------------------- high priority reaches -----------------
      life_stages_priorities_species_specific_list = Life_Stage_Priorities_AU_and_Reach_data %>%
        filter(Life_Stage_Priorities_AU_and_Reach_data[life_stages_priorities_species_specific[[life_stage_x]]] ==  Life_Stage_Priority)
      # ----------------- life stage present in those high priority reaches --------------
      life_stages_priorities_species_specific_list = life_stages_priorities_species_specific_list %>%
        filter(life_stages_priorities_species_specific_list[life_stages_prescence_species_specific[[life_stage_x]]] ==  1)
      #life_stages_priorities_species_specific_list = Life_Stage_Priorities_AU_and_Reach_data %>%
      #  filter(Life_Stage_Priorities_AU_and_Reach_data[life_stages_prescence_species_specific[[life_stage_x]]] ==  1)
      
      # ------------------- just pull the reaches ----------------
      #Reaches_life_stage_high_priority = as.data.frame(life_stages_priorities_species_specific_list[,c("ReachName")])[['ReachName']]
      
      # ------------------ identify overlapping reaches with reaches already filtered ------------
      Reaches_life_stage_high_priority_Overlap = intersect( life_stages_priorities_species_specific_list$ReachName,
                                                           Habitat_Quality_Pathway_Restoration$ReachName )
      #Reaches_life_stage_high_priority_Overlap = Habitat_Quality_Pathway_Restoration$ReachName
      reaches_unique_x = unique(c(reaches_unique_x,Reaches_life_stage_high_priority_Overlap))
      
      # ---------------------------------------------
      #    Pull Habitat Attributes 
      # ---------------------------------------------
      habitat_attributes_reaches_x = Generate_individual_life_stage_habitat_attributes(Reaches_life_stage_high_priority_Overlap, species_x, 
                                                        life_stage_x, core_metric_use)
      # -------------------------------------------
      #   Merge these habitat attributes
      # ------------------------------------------
      if( !is.na(habitat_attributes_reaches_x)[1] ){
        colx = colnames(habitat_attributes_reaches_x)[2:ncol(habitat_attributes_reaches_x)][1]
        
        for(colx in colnames(habitat_attributes_reaches_x)[2:ncol(habitat_attributes_reaches_x)] ){
          

          # -------------- if this is habitat attribute is already present ------  
          if( any(colnames(Limiting_Factor_Reach_ALL_attributes) == colx) ){
            
            # ---------------------- fill in columns with 1, 3, 5 for the habitat attributes ----------------
            habitat_attributes_reaches_x2 = habitat_attributes_reaches_x[,c("ReachName",colx)]  # pull ReachName and habitat attribute column
            colnames(habitat_attributes_reaches_x2)[2] = "second"  # name the habitat attribute with the same name as second 
            Limiting_Factor_Reach_ALL_attributes = merge(Limiting_Factor_Reach_ALL_attributes, habitat_attributes_reaches_x2[,c("ReachName","second")], by="ReachName", all.x=TRUE)
            # ------------------ just pulling the number (taking minimum - but if it's a number it's the same number) -------
            Limiting_Factor_Reach_ALL_attributes[,colx] = apply(Limiting_Factor_Reach_ALL_attributes[,c(colx, "second")], 1, two_attribute_fxn)
            # ----------------- remove the "second" number ------------------
            Limiting_Factor_Reach_ALL_attributes = Limiting_Factor_Reach_ALL_attributes[, - which(colnames(Limiting_Factor_Reach_ALL_attributes) == "second")]
            # -------------------- add life-stage species combo ------
            
            # ------ get the index for each reach --------
            reaches_to_merge_index = c()
            for(reach_x in habitat_attributes_reaches_x2$ReachName){
              x = which(Limiting_Factor_Reach_ALL_attributes$ReachName == reach_x)
              if(length(x)>0){
                reaches_to_merge_index = rbind(reaches_to_merge_index, x)
              }
            }
            # ----- add life stage - species to column -----
            life_stage_species_x = paste(life_stage_x,species_x,sep="-")
            Limiting_Factor_Reach_ALL_attributes$Priority_Life_Stages[reaches_to_merge_index] = paste(Limiting_Factor_Reach_ALL_attributes$Priority_Life_Stages[reaches_to_merge_index], life_stage_species_x, sep=", " )
            
            # --------------- fill in True/False if present ---------
            habitat_attributes_reaches_x2$T_or_F = FALSE
            habitat_attributes_reaches_x2[which( !is.na(habitat_attributes_reaches_x2[,"second"]) ),"T_or_F"] = TRUE 
            habitat_attributes_reaches_x2 = habitat_attributes_reaches_x2[,-which( colnames(habitat_attributes_reaches_x2) == "second") ]
            # --------------------- merge -------------
            Limiting_Factor_Reach_ALL_attributes_T_F_Presence = merge(Limiting_Factor_Reach_ALL_attributes_T_F_Presence, habitat_attributes_reaches_x2[,c("ReachName","T_or_F")], by="ReachName", all.x=TRUE)
            # ------------------ just pulling the number (taking minimum - but if it's a number it's the same number) -------
            Limiting_Factor_Reach_ALL_attributes_T_F_Presence[,colx] = apply(Limiting_Factor_Reach_ALL_attributes_T_F_Presence[,c(colx, "T_or_F")], 1, two_attribute_T_or_F_fxn)
            # ----------------- remove the "second" number ------------------
            Limiting_Factor_Reach_ALL_attributes_T_F_Presence = Limiting_Factor_Reach_ALL_attributes_T_F_Presence[, - which(colnames(Limiting_Factor_Reach_ALL_attributes_T_F_Presence) == "T_or_F")]

            
          # ---------- if this habitat attribute is not in Limiting_Factor_Reach_ALL_attributes ------
          }else{
            # ---------------- habitat attributes ---------
            Limiting_Factor_Reach_ALL_attributes = merge(Limiting_Factor_Reach_ALL_attributes, habitat_attributes_reaches_x[,c("ReachName",colx)], by="ReachName", all.x=TRUE)

            # -------------- T/F data frame -----------------
            habitat_attributes_reaches_x2 = habitat_attributes_reaches_x[,c("ReachName",colx)]  # pull ReachName and habitat attribute column
            habitat_attributes_reaches_x2$T_or_F = FALSE
            habitat_attributes_reaches_x2[which( !is.na(habitat_attributes_reaches_x2[,colx]) ),"T_or_F"] = TRUE 
            habitat_attributes_reaches_x2 = habitat_attributes_reaches_x2[,-which( colnames(habitat_attributes_reaches_x2) == colx) ]
            colnames(habitat_attributes_reaches_x2)[2] = colx
            Limiting_Factor_Reach_ALL_attributes_T_F_Presence = merge(Limiting_Factor_Reach_ALL_attributes_T_F_Presence, habitat_attributes_reaches_x2[,c("ReachName",colx)], by="ReachName", all.x=TRUE)
          }
          # ---------------- pull Limiting_Factor_Reach_ALL_attributes with the same name ---------
        }
        
      }
 
      
      
      # ---------------- if habitat attribute data present for these reaches and life stages ----------------
      if( !is.na(habitat_attributes_reaches_x)[1] ){
        # ---------------------------------------------
        #   Combine with Data
        # ---------------------------------------------
        # ------------ get the unique reaches -------------
        reaches_Limiting_Factor_x = c()
        for(reach_x2 in habitat_attributes_reaches_x$ReachName){
          # ------- identify row (index value) with this reach ------
          x = which(Limiting_Factor_Reach_Ranking_Scores$ReachName == reach_x2)
          reaches_Limiting_Factor_x = rbind(reaches_Limiting_Factor_x,x )
          
        }
        reaches_Limiting_Factor_x = as.vector(reaches_Limiting_Factor_x) 

        # ----------------------- get unique habitat attributes --------------
        colnames_x = colnames(habitat_attributes_reaches_x)[2:length(colnames(habitat_attributes_reaches_x))]
        # ------------ add the life stage and species to the name -----------
        habitat_attributes_life_stage_species_x = paste(colnames_x,species_x,life_stage_x,sep="_")
        # ----------------- loop through each habitat attribute --------
        #colx = colnames_x[2]
        i = 0
        for(colx in colnames_x){
          i = i + 1
          # --------------- get reaches with data present and absent ----------------
          presence_x = which(habitat_attributes_reaches_x[,colx] > 0 )
          absence_x = which(is.na(habitat_attributes_reaches_x[,colx]) )
          # ----------------------- fill Habitat Attribute Presence List ----------------------
          Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_List[reaches_Limiting_Factor_x[presence_x] ] = paste( Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_List[reaches_Limiting_Factor_x[presence_x] ],  habitat_attributes_life_stage_species_x[i], sep=",")
          # ----------------------- fill Habitat Attribute Missing List ----------------------
          if( length(absence_x) > 0 ){
            Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Missing_List[reaches_Limiting_Factor_x[absence_x] ] = paste(Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Missing_List[reaches_Limiting_Factor_x[absence_x] ], habitat_attributes_life_stage_species_x[i], sep=",")
          }
          # ----------------------- add to Data Presence Total ----------------------
          Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_Total[reaches_Limiting_Factor_x[presence_x] ] =  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_Total[reaches_Limiting_Factor_x[presence_x] ] + 1
          # ----------------------- add to Data Total  ----------------------
          Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_All_Total[reaches_Limiting_Factor_x] =  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_All_Total[reaches_Limiting_Factor_x]  + 1
          # ----------------------- add to Present Presence ----------------------
          Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Percent_Data_Presence[reaches_Limiting_Factor_x] = Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_Total[reaches_Limiting_Factor_x ] /  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_All_Total[reaches_Limiting_Factor_x]
          # ----------------------- add to Total score----------------------
          Limiting_Factor_Reach_Ranking_Scores$Total_Attribute_Score[reaches_Limiting_Factor_x] = rowSums( cbind( Limiting_Factor_Reach_Ranking_Scores$Total_Attribute_Score[reaches_Limiting_Factor_x], habitat_attributes_reaches_x[,colx] ), na.rm=T ) 
          # -----------------------include species----------------------
          Limiting_Factor_Reach_Ranking_Scores$Species[reaches_Limiting_Factor_x] = paste(Limiting_Factor_Reach_Ranking_Scores$Species[reaches_Limiting_Factor_x], species_x) 
          # --------------------- add life stages -----------------
          Limiting_Factor_Reach_Ranking_Scores$Priority_Life_Stages[reaches_Limiting_Factor_x] = paste(Limiting_Factor_Reach_Ranking_Scores$Priority_Life_Stages[reaches_Limiting_Factor_x], life_stage_x) 
        }
        
      }

       # --------- remove T_or_F column ----------------
      if( any(colnames(Limiting_Factor_Reach_ALL_attributes_T_F_Presence) == "T_or_F")  ){
        Limiting_Factor_Reach_ALL_attributes_T_F_Presence = Limiting_Factor_Reach_ALL_attributes_T_F_Presence[ , -which(colnames(Limiting_Factor_Reach_ALL_attributes_T_F_Presence) == "T_or_F")]
      }
      
    }

  } # end species loop
  
  # -------------------------------------------------------------------------------------------
  #
  #     Calculate Totals
  #
  # -------------------------------------------------------------------------------------------
  
  Limiting_Factor_Reach_Ranking_Scores$Limiting_Factor_Score_Percent = (Limiting_Factor_Reach_Ranking_Scores$Total_Attribute_Score / (Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_All_Total*5) ) * 100
  # ------- all rows -----------
  # potential columns:  "Habitat_Attribute_Present_List", "Habitat_Attribute_Missing_List" , "Habitat_Attribute_Present_Total", "Habitat_Attribute_All_Total" ,"Habitat_Attribute_Percent_Data_Presence", "Total_Attribute_Score", "Species", "Limiting_Factor_Score_Percent"  
  
  # -------------------------------------------------------------------------------------------
  #
  #     IF no attributes (e.g. not a high priority life stage) - set to 0
  #
  # -------------------------------------------------------------------------------------------
  
  no_high_priority_life_stage_x = which( is.na(Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_List) &
                                           is.na(Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Missing_List) )
  # ------------ set to 0 -------------
  Limiting_Factor_Reach_Ranking_Scores$Limiting_Factor_Score_Percent[no_high_priority_life_stage_x] = 0
  
  # -------------------------------------------------------------------------------------------
  #
  #     Remove leading "NA,"
  #
  # -------------------------------------------------------------------------------------------
  
  # -------------------- remove leading "NA," from habitat attribute present list --------------------
  x_leading_NA = which(substr(Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_List,1,3) == "NA,")
  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_List[x_leading_NA] = substr(Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_List[x_leading_NA],4,nchar(Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Present_List[x_leading_NA]))
  # -------------------- remove leading "NA," from habitat attribute missing list --------------------
  x_leading_NA = which(substr(Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Missing_List,1,3) == "NA,")
  Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Missing_List[x_leading_NA] = substr(Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Missing_List[x_leading_NA],4,nchar(Limiting_Factor_Reach_Ranking_Scores$Habitat_Attribute_Missing_List[x_leading_NA]))
  
  # -------------------------------------------------------------------------------------------
  #
  #       Calculate total Habitat Attributes Present
  #
  # -------------------------------------------------------------------------------------------
  
  # --------------------- count the total unique habitat attributes, if present, and if not ----------
  Limiting_Factor_Reach_ALL_attributes_T_F_Presence_2 = apply(Limiting_Factor_Reach_ALL_attributes_T_F_Presence[,2:ncol(Limiting_Factor_Reach_ALL_attributes_T_F_Presence)], 1, pull_T_F_col_name_fxn)
  Limiting_Factor_Reach_ALL_attributes_T_F_Presence_2 = t(Limiting_Factor_Reach_ALL_attributes_T_F_Presence_2)
  colnames(Limiting_Factor_Reach_ALL_attributes_T_F_Presence_2) = c("total_potential_data", "data_present", "data_absent"  )
  Limiting_Factor_Reach_ALL_attributes_T_F_Presence_2 = as.data.frame(Limiting_Factor_Reach_ALL_attributes_T_F_Presence_2)
  
  # ------------------------ identify which are impaired --------------
  Limiting_Factor_Reach_ALL_attributes$Total_Impaired_Attributes = apply(Limiting_Factor_Reach_ALL_attributes[, 3:ncol(Limiting_Factor_Reach_ALL_attributes)], 1, function_total_impaired_habitat_attribute)
  # ------------------- percent of LF habitat attributes are impaired --------
  Limiting_Factor_Reach_ALL_attributes$Life_Stage_Habitat_Degradation = Limiting_Factor_Reach_ALL_attributes$Total_Impaired_Attributes / Limiting_Factor_Reach_ALL_attributes_T_F_Presence_2$data_present
  
  
  # -------------------------------------------------------------------------------------------
  #
  #     Add which habitat attributes are missing (BOTH put "Missing Data" and list in a column)
  #
  # -------------------------------------------------------------------------------------------
  # data_frame_x = cbind( Limiting_Factor_Reach_ALL_attributes[order(Limiting_Factor_Reach_ALL_attributes$ReachName),], Limiting_Factor_Reach_ALL_attributes_T_F_Presence[order(Limiting_Factor_Reach_ALL_attributes_T_F_Presence$ReachName),] )
  # ----------------------- calculate all the missing data --------------------
  #data_frame_x= Limiting_Factor_Reach_ALL_attributes_T_F_Presence[order(Limiting_Factor_Reach_ALL_attributes_T_F_Presence$ReachName),]
  Limiting_Factor_Reach_ALL_attributes_T_F_Presence_one_column = apply( Limiting_Factor_Reach_ALL_attributes_T_F_Presence[order(Limiting_Factor_Reach_ALL_attributes_T_F_Presence$ReachName),] , 1, function_missing_data_in_one_column)
  
  # -------- A) get one row with all missing data and B) add "Missing Data" to Limiting_Factor_Reach_ALL_attributes where data is missing ----
  list_of_missing_data = c()
  for(i in 1:nrow(Limiting_Factor_Reach_ALL_attributes_T_F_Presence)){
    reach_x = Limiting_Factor_Reach_ALL_attributes_T_F_Presence$ReachName[i]
    # ------------ pull columns that are false ----------
    false_columns = Limiting_Factor_Reach_ALL_attributes_T_F_Presence[i,which(Limiting_Factor_Reach_ALL_attributes_T_F_Presence[i,] == FALSE)]
    # ----------------- combine false column names into one column ---------------
    false_columns_one_column = colnames(false_columns)
    # ---------------------- combine into one element ------
    false_columns_one_single = paste(false_columns_one_column, collapse=",")
    # --------- add to list of all missing data -------------
    list_of_missing_data = c(list_of_missing_data, false_columns_one_single)
    
    # ---------------- in Limiting_Factor_Reach_ALL_attributes - add "missing" where missing data -----------
    row_x_output = which(Limiting_Factor_Reach_ALL_attributes$ReachName == reach_x)
    for(colx in false_columns_one_column){

      # ------------ for a habitat attribute (column) that is "False" - add "Missing Data" -----
      Limiting_Factor_Reach_ALL_attributes[row_x_output, colx] = "Missing Data"

    }
    
  }
  # ----------------- add missing data -----------
  missing_data_df =  cbind( Limiting_Factor_Reach_ALL_attributes_T_F_Presence$ReachName ,list_of_missing_data)
  colnames(missing_data_df) = c("ReachName","list_of_missing_data")
  Limiting_Factor_Reach_ALL_attributes = merge(Limiting_Factor_Reach_ALL_attributes, missing_data_df, by="ReachName", all.x=TRUE)
  
  # --------------------- make it so Life_Stage_Habitat_Degradation column is a percent (not a ratio) -------
  Limiting_Factor_Reach_ALL_attributes$Life_Stage_Habitat_Degradation = Limiting_Factor_Reach_ALL_attributes$Life_Stage_Habitat_Degradation * 100
  
  # -------------------------------------------------------------------------------------------
  #
  #     Clean up Of Priority Life Stage column
  #
  # -------------------------------------------------------------------------------------------
  # ------------------- Remove Leading NA ---------------
  x_leading_NA = which(substr(Limiting_Factor_Reach_ALL_attributes$Priority_Life_Stages,1,3) == "NA,")
  Limiting_Factor_Reach_ALL_attributes$Priority_Life_Stages[x_leading_NA] = substr(Limiting_Factor_Reach_ALL_attributes$Priority_Life_Stages[x_leading_NA],4,nchar(Limiting_Factor_Reach_ALL_attributes$Priority_Life_Stages[x_leading_NA]))
  # ----------------- only get unique life stage - species combos ------
  for(i in 1:nrow(Limiting_Factor_Reach_ALL_attributes)){
    life_stage_species_x = Limiting_Factor_Reach_ALL_attributes$Priority_Life_Stages[i]
    life_stage_species_x2 = unlist(strsplit(life_stage_species_x, ", "))
    # ------ remove white space from outsides --------
    life_stage_species_x2 = trimws(life_stage_species_x2)
    # ------------- get unique -----------------
    life_stage_species_x2 = unique(life_stage_species_x2)
    # --------------- combine into one element -----------
    life_stage_species_x2 = paste(life_stage_species_x2, collapse=", ")
    # ------------- re merge ------------
    Limiting_Factor_Reach_ALL_attributes$Priority_Life_Stages[i] = life_stage_species_x2
  }
  
  # ------------- IF NA - then missing a priority life stage ----
  x_NA = which( Limiting_Factor_Reach_ALL_attributes$Priority_Life_Stages == "NA" )
  Limiting_Factor_Reach_ALL_attributes$Priority_Life_Stages[x_NA] = "No High Priority Life Stages in reach"
  # ------ set Limiting Factor Percent Score for reach names with no high priority life stages to 0 ---------
  Limiting_Factor_Reach_ALL_attributes$Life_Stage_Habitat_Degradation[x_NA] = 0
  
  # -------------------------------------------------------------------------------------------
  #
  #     Return Data
  #
  # -------------------------------------------------------------------------------------------
  #Limiting_Factor_Reach_Ranking_Scores_Output = Limiting_Factor_Reach_Ranking_Scores[,columns_to_output ]
  
  
  return(Limiting_Factor_Reach_ALL_attributes)
  
}

# Limiting_Factor_Reach_ALL_attributes_T_F_Presence_3 = cbind(Limiting_Factor_Reach_ALL_attributes,  Limiting_Factor_Reach_ALL_attributes_T_F_Presence, Limiting_Factor_Reach_Ranking_Scores_Output)

# Limiting_Factor_Reach_ALL_attributes_T_F_Presence_3 =cbind(Limiting_Factor_Reach_ALL_attributes[order(Limiting_Factor_Reach_ALL_attributes$ReachName),], Limiting_Factor_Reach_ALL_attributes_T_F_Presence[order(Limiting_Factor_Reach_ALL_attributes_T_F_Presence$ReachName),] , Limiting_Factor_Reach_Ranking_Scores_Output[order(Limiting_Factor_Reach_Ranking_Scores_Output$ReachName),] ) 
# -------------------------------------------------------------------
#
#       Function to generate habitat attributes for individual species/life stage combo
#
# -------------------------------------------------------------------

test_x = FALSE
if(test_x){
  species = species_x
  life_stage_x = life_stage_x
  core_metric_use  = core_metric_use
  reaches_x = Reaches_life_stage_high_priority_Overlap
}



Generate_individual_life_stage_habitat_attributes = function(reaches_x, species_x, life_stage_x, core_metric_use){
  
  # -------------------------------------------------------------------
  #  Pull life stage attributes for this species
  # ------------------------------------------------------------------
  
  # -------------------- pull habitat attributes/life stages JUST for this species ---------
  Attribute_LifeStage_Crosswalk_Life_Stage = Attribute_LifeStage_Crosswalk %>%
    filter(Species  %in% species_x  )
  
  # ------------------ pull habitat attributes JUST for this life stage -------------------
  habitat_attributes_life_stage_list = Attribute_LifeStage_Crosswalk_Life_Stage %>%
    filter(Attribute_LifeStage_Crosswalk_Life_Stage$'Life Stage'  %in%  life_stage_x  )
  # -------------- IF pulling core metrics, filter based on that --------------
  if(core_metric_use == "yes"){
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
      if( is.null(nrow(Habitat_Attribute_Scores_for_individual_Life_Stage)) & nrow(Habitat_Attribute_Score_x) > 0 ){
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
    
    if( nrow(Habitat_Attribute_Scores_for_individual_Life_Stage) == 0 ){
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


# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#               Function to pull all the High Priority life stages 
#
# -----------------------------------------------------------------------------------------------------------------------------------------------
species_x = "Steelhead"

FUNCTION_pull_High_Priority_Life_Stages_for_a_reach = function(reach_x, species_x){
  
  # --------------- pull life stage priorities for this reach ------------
  reach_row_life_stages_x = Life_Stage_Priorities_AU_and_Reach_data[which(Life_Stage_Priorities_AU_and_Reach_data$ReachName == reach_x), ]
  
  life_stages_priority_list  = c()
  if(species_x == "Spring Chinook"){
    
    life_stages = Attribute_LifeStage_Crosswalk$`Life Stage`[Attribute_LifeStage_Crosswalk$Species == "Spring Chinook"]
    life_stages = unique(life_stages)
    for(life_stage_x in life_stages){
      colname_x = spring_chinook_life_stages[[life_stage_x]]
      priority_x = reach_row_life_stages_x[colname_x]
      if(priority_x == "High Priority"){
        life_stages_priority_list = c(life_stages_priority_list, life_stage_x)
      }
    }
  }else if(species_x == "Steelhead"){
    
    life_stages = Attribute_LifeStage_Crosswalk$`Life Stage`[Attribute_LifeStage_Crosswalk$Species == "Steelhead"]
    life_stages = unique(life_stages)
    for(life_stage_x in life_stages){
      colname_x = steelhead_life_stages[[life_stage_x]]
      priority_x = reach_row_life_stages_x[colname_x]
      if(priority_x == "High Priority"){
        life_stages_priority_list = c(life_stages_priority_list, life_stage_x)
      }
    }
  }else if(species_x == "Bull Trout"){
    
    life_stages = Attribute_LifeStage_Crosswalk$`Life Stage`[Attribute_LifeStage_Crosswalk$Species == "Bull Trout"]
    life_stages = unique(life_stages)
    for(life_stage_x in life_stages){
      colname_x = bull_trout_life_stages[[life_stage_x]]
      priority_x = reach_row_life_stages_x[colname_x]
      if(priority_x == "High Priority"){
        life_stages_priority_list = c(life_stages_priority_list, life_stage_x)
      }
    }
  }
  
  return(life_stages_priority_list)
}



# -------------------- just to see if any high priority life stage --
# NOTE - it's across all species - so Wenatchee River Tumwater 01 and 02 are High Priority for BT

run_high_priority_life_stage_test = FALSE
if(run_high_priority_life_stage_test){
  for(reach_x in Limiting_Factor_Reach_ALL_attributes$ReachName[which(is.na(Limiting_Factor_Reach_ALL_attributes$Life_Stage_Habitat_Degradation))] ){
    TRUE_x = any(Life_Stage_Priorities_AU_and_Reach_data[which(Life_Stage_Priorities_AU_and_Reach_data$ReachName == reach_x),] == "High Priority")
    if(TRUE_x){
      print(reach_x)
    }
  }
}

