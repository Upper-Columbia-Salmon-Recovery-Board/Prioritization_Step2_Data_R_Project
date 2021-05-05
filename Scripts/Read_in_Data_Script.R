
# ---------------------------------------------------------------------------
#
#      SCRIPT: Read in Data
#
#      R Project to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------


# library(DataEditR) # only use if want to use interactive data editing


# ---------------------------------------------------------------------------
#
#     Read in Data
#
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
#
#     Habitat Raw Data
#
# ---------------------------------------------------------------------------

habitat_raw_data = read_excel(  paste(habitat_data_path,'Habitat_Data_Raw.xlsx', sep="")   )

# ----------- update columns that are numeric to numeric ------------
cols.num <- c( 'Sand_occular_prcnt_INDICATOR_1',	'Gravel_occular_prcnt_INDICATOR_2',
              'Cobble_occular_prcnt_INDICATOR_3',	'Boulder_occular_prcnt_INDICATOR_4',	'Bedrock_occular_prcnt_INDICATOR_5',
              'Gravel_and_Cobble_occular_prcnt_INDICATOR_6',	'Clay_Silt_Sand_occular_prcnt_INDICATOR_7',
              'Sand_sieve_prcnt_INDICATOR_8',	'Gravel_sieve_prcnt_INDICATOR_9',	'Cobble_sieve_prcnt_INDICATOR_10',
              'Boulder_sieve_prcnt_INDICATOR_11',	'Bedrock_sieve_prcnt_INDICATOR_12',	'D50_sieve_size_prcnt_finer_mm_INDICATOR_13',
              'Silt_pebble_count_PRCNT_INDICATOR_14',	'Sand_pebble_count_PRCNT_INDICATOR_15',	'Gravel_pebble_count_PRCNT_INDICATOR_16',
              'Cobble_pebble_count_PRCNT_INDICATOR_17',	'Boulder_pebble_count_PRCNT_INDICATOR_18',	
              'Bedrock_pebble_count_PRCNT_INDICATOR_19',	'Gravel_and_Cobble_pebble_count_PRCNT_INDICATOR_20',	'Boulder_UCSRB_pct',
              'GravelCobble_UCSRB_pct',	'Pieces_per_mile_INDICATOR_1',
              'Small_pieces_per_mile_INDICATOR_2',	'Medium_pieces_per_mile_INDICATOR_3',	'Large_pieces_per_mile_INDICATOR_4',
              'Overstory_mature_tree_prcnt_INDICATOR_5','Pools_CATEGORY_1_NUMERIC',	'Pools_total_INDICATOR_1',
              'Pools_per_mile_INDICATOR_2',	'Pools_deeper_3_ft_prcnt_INDICATOR_3',	'Pools_deeper_3_ft_per_mile_INDICATOR_4',
              'Pools_deeper_5_ft_per_mile_INDICATOR_5',	'Pools_pool_depth_ft_INDICATOR_6','Connectivity_total_SC_INDICATOR_1',
              'Off_channel_habitat_prcnt_INDICATOR_2',	'Main_channel_prcnt_INDICATOR_3',	'Connectivity_fast_water_INDICATOR_4',
              'Side_channel_fast_prcnt_INDICATOR_5',	'Connectivity_slow_water_INDICATOR_6',	'Side_channel_slow_prcnt_INDICATOR_7',
              'Connectivity_cover_INDICATOR_8',	'Channel_Confinementor_or_Entrenchment_Ratio_INDICATOR_9',	'Channel_Bankfull_Width_to_Depth_Ratio_INDICATOR_10',
              'Structure_mature_tree_prcnt_INDICATOR_1',	'Structure_large_tree_prcnt_INDICATOR_2',	'Structure_small_tree_prcnt_INDICATOR_3',
              'Structure_small_tree_or_smaller_prcnt_INDICATOR_4',	'Structure_sapling_pole_prcnt_INDICATOR_5',	'Structure_shrub_seedling_prcnt_INDICATOR_6',
              'Structure_tall_grass_short_Shrub_prcnt_INDICATOR_7',	'Structure_grass_prcnt_INDICATOR_8',	'Structure_orchard_prcnt_INDICATOR_9',
              'Structure_bare_ground_prcnt_INDICATOR_10','PROSPER',	'NORWEST_Temperature','Canopy_Cover_NORWEST',
              'Undercut_Area_Pct_CHAMP',	'SubEstBldr_CHAMP',	'SubEstSandFines_CHAMP',	'LWFreq_Bf_CHAMP',
              'SC_Area_Pct_Average_CHAMP',	'FishCovNone_Average_CHAMP',	'GRVL_COBL_UCSRB_CHAMP',	'SubEmbed_Avg_Average_CHAMP', 
              'UCSRB_CanopyCoverPct',	'UCSRB_RiparianDisturbancePct',
              'UCSRB_OffChannel_Floodplain', 'UCSRB_OffChannel_SideChannels', 'UCSRB_ChannelStability', 'UCSRB_BankStability')
# ---- convert all columns listed above to numeric ----------              
habitat_raw_data[cols.num] <- sapply(habitat_raw_data[cols.num],as.numeric)

# ONLY for USE for interactive (COULD just have a person edit a CSV)
# habitat_raw_data_new <- data_edit(habitat_raw_data, save_as = "habitat_raw_data_updated.csv")

# ---------------------------------------------------------------------------
#
#    Barriers Pathways Output 
#     # these are copied over from output from barrier prioritization
#
# ---------------------------------------------------------------------------

Barriers_Pathways_Data = read_excel( paste(habitat_data_path,'Barriers_Pathway_Data.xlsx', sep="") )


# ---------------------------------------------------------------------------
#
#     AU Ranks
#
# ---------------------------------------------------------------------------

AU_Ranks_data = read_excel( paste(ranking_data_path,'AU_Ranks.xlsx', sep="") )

# ----------- update columns that are numeric to numeric ------------
cols.num = c('Spring Chinook_Restoration',	'SPCHNTier_Restoration',	'Steelhead_Restoration',	'STLTier_Restoration',
             'BullTrout_Restoration',	'BTTier_Restoration',	'Spring Chinook_Protection',	'SPCHNTier_Protection',
             'Steelhead_Protection',	'STLTier_Protection',	'BullTrout_Protection',	'BTTier_Protection')
AU_Ranks_data[cols.num] <- sapply(AU_Ranks_data[cols.num],as.numeric)

# ---------------------------------------------------------------------------
#
#     Life Stage Priorities - only AU level
#
# ---------------------------------------------------------------------------

Life_Stage_Priorities_AU_only_data = read_excel( paste(ranking_data_path,'LifeStagePriorities.xlsx', sep=""), skip=1 )

# ---------------------------------------------------------------------------
#
#     Life Stage Priorities - AU and Reach
#
# ---------------------------------------------------------------------------

Life_Stage_Priorities_AU_and_Reach_data = read_excel( paste(ranking_data_path,'LifeStagePriorities_AUandReach.xlsx', sep="") )

# ---------------------- match column names to life stages ------------------------------ 
spring_chinook_life_stages = list("Adult Migration" = "SPCH Adult Migration  AU LS Priority",	  "Holding"=	"SPCH Holding  AU LS Priority", "Holding and Maturation"=	"SPCH Holding  AU LS Priority",
                                "Spawning and Incubation" = "SPCH Spawning AU LS Priority",	"Fry Colonization" = "SPCH Fry Colonization  AU LS Priority", "Fry" = "SPCH Fry Colonization  AU LS Priority",
                                "Summer Rearing"  = "SPCH Summer Rearing  AU LS Priority", 	"Winter Rearing" = "SPCH Winter Rearing  AU LS Priority",
                                "Smolt Outmigration"="SPCH Smolt Emigration  AU LS Priority", "Holding and Maturation" = "SPCH Holding")

steelhead_life_stages =     life_stage_priority_list = list("Adult Migration"  = "SH Adult Migration  AU LS Priority","Holding"  =  "SH Holding  AU LS Priority",
                                                            "Holding and Maturation"  =  "SH Holding  AU LS Priority",	"Spawning and Incubation" ="SH Spawning AU LS Priority", 
                                                            "Fry" = "SH Fry Colonization  AU LS Priority", "Fry Colonization" = "SH Fry Colonization  AU LS Priority","Spawning and Incubation" = "SH Summer Rearing  AU LS Priority",
                                                            "Winter Rearing"  = "SH Winter Rearing  AU LS Priority",	"Smolt Outmigration" = "SH Smolt Emigration  AU LS Priority")

bull_trout_life_stages = list("Adult Migration"  = "BT Adult Migration  AU LS Priority",	"Holding and Maturation"= 	"BT Holding  AU LS Priority",
                                "Spawning and Incubation" =	"BT Spawning AU LS Priority",	"BT Natal Rearing" =  "BT Natal Rearing  AU LS Priority",
                                "Adult Non-Spawning" = "BT Adult Non-Spawning AU LS Priority", "BT Subadult Rearing"=  "BT Subadult Rearing  AU LS Priority")

life_stages_priorities = list("spring_chinook_life_stages" =  spring_chinook_life_stages,  
                              "steelhead_life_stages" = steelhead_life_stages, 
                              "bull_trout_life_stages" = bull_trout_life_stages )

# ---------------------- match column names to life stages ------------------------------ 
spring_chinook_life_stages_presence = list("Adult Migration" = "SPCH Adult Migration",	  "Holding"=	"SPCH Holding", "Holding and Maturation"=	"SPCH Holding",
                                  "Spawning and Incubation" = "SPCH Spawning",	"Fry Colonization" = "SPCH Fry Colonization", 	"Fry" = "SPCH Fry Colonization", 
                                  "Summer Rearing"  = "SPCH Summer Rearing", 	"Winter Rearing" = "SPCH Winter Rearing", 
                                  "Smolt Outmigration"= "SPCH Smolt Emigration"  )	
steelhead_life_stages_presence =     life_stage_priority_list = list("Adult Migration"  = "SH Adult Migration","Holding"  =  "SH Holding",
                                                            "Holding and Maturation"  =  "SH Holding",	"Spawning and Incubation" ="SH Spawning", 
                                                            "Fry" = "SH Fry Colonization","Fry Colonization" = "SH Fry Colonization",
                                                            "Spawning and Incubation" = "SH Spawning", "Summer Rearing" =  "SH Summer Rearing",
                                                            "Winter Rearing"  = "SH Winter Rearing",	"Smolt Outmigration" = "SH Smolt Emigration")

bull_trout_life_stages_presence = list("Adult Migration"  = "BT Adult Spawning Migration",	"Holding and Maturation"= 	"BT Holding and Maturation",
                                       "Spawning and Incubation" =	"BT Spawning",	"BT Natal Rearing" =  "BT Natal Rearing",
                                       "Adult Non-Spawning" = "BT_AdultNonspawning", "BT Subadult Rearing"=  "BT Subadult Rearing")


life_stages_prescence = list("spring_chinook_life_stages" =  spring_chinook_life_stages_presence,  
                              "steelhead_life_stages" = steelhead_life_stages_presence, 
                              "bull_trout_life_stages" = bull_trout_life_stages_presence )

# ---------------------------------------------------------------------------
#
#     Life Stages and accompanying Habitat Attributes for each Species
#
# ---------------------------------------------------------------------------

Attribute_LifeStage_Crosswalk = read_excel( paste(crosswalks_path,'Attribute_LifeStage_Crosswalk.xlsx', sep="") )
Attribute_LifeStage_Crosswalk$Habitat_Attribute_2 = gsub(" ", "", Attribute_LifeStage_Crosswalk$`Habitat Attribute`, fixed = TRUE)

# ---------------------------------------------------------------------------
#
#     Channel Habitat Unit Data
#
# ---------------------------------------------------------------------------

Channel_Unit_Raw = read_excel( paste(habitat_data_path,'Channel_Unit_Raw.xlsx', sep="") )

# ----------- update columns that are numeric to numeric ------------
cols.num = c('Riffle_Habitat_Prcnt_INDICATOR_1' , 'Rapid_Habitat_Prcnt_INDICATOR_2' , 'Glide_Habitat_Prcnt_INDICATOR_3',
      'Pool_Habitat_Prcnt_INDICATOR_4', 'Cascade_Habitat_Prcnt_INDICATOR_5', 'Side_Channel_Habitat_Prcnt_INDICATOR_6',
      'Braid_Habitat_Prcnt_INDICATOR_7',    'Bar_Habitat_Prcnt_INDICATOR_8')
Channel_Unit_Raw[cols.num] <- sapply(Channel_Unit_Raw[cols.num],as.numeric)

# ---------------------------------------------------------------------------
#
#     CHAMP data per reach
#
# ---------------------------------------------------------------------------

CHAMP_data_per_reach = read_excel( paste(habitat_data_path,'CHAMP_data_per_reach.xlsx', sep="") )

# ----------- update columns that are numeric to numeric ------------
cols.num = c('NumberofCHaMPDataPoints', 'SlowWater_Pct_Average',	'SlowWater_Pct_StandardDeviation',	'FstTurb_Pct_Average',
             'FstTurb_Pct_StandardDeviation',	'Grad_Average',	'Grad_StandardDeviation',	'Sin_Average',
             'Sin_StandardDeviation',	'Area_Wet_Average',	'Area_Wet_StandardDeviation',	'Area_Bf_Average',
             'Area_Bf_StandardDeviation',	'WetVol_Average',	'WetVol_StandardDeviation',	'DpthThlwg_UF_CV_Average',
             'DpthThlwg_UF_CV_StandardDeviation',	'DpthWet_SD_Average',	'DpthWet_SD_StandardDeviation',	
             'BfWdth_Avg_Average',	'BfWdth_Avg_StandardDeviation',	'WetSCL_Area_Average',	
             'WetSCL_Area_StandardDeviation',	'SCSm_Area_Average',	'SCSm_Area_StandardDeviation',
             'WetSC_Pct_Average',	'WetSC_Pct_StandardDeviation',	'SCSm_Freq_Average',	
             'SCSm_Freq_StandardDeviation',	'SCSm_Ct_Average',	'SCSm_Ct_StandardDeviation',	'SCSm_Vol_Average',
             'SCSm_Vol_StandardDeviation',	'SubEmbed_Avg_Average',	'SubEmbed_Avg_StandardDeviation',	
             'SubEmbed_SD_Average',	'SubEmbed_SD_StandardDeviation',	'SubD50_Average',	'SubD50_StandardDeviation',
             'RipCovBigTree_Average',	'RipCovBigTree_StandardDeviation',	'RipCovConif_Average',
             'RipCovConif_StandardDeviation',	'RipCovGrnd_Average',	'RipCovGrnd_StandardDeviation',	
             'RipCovNonWood_Average',	'RipCovNonWood_StandardDeviation',	'RipCovUstory_Average',	
             'RipCovUstory_StandardDeviation',	'RipCovWood_Average',	'RipCovWood_StandardDeviation',
             'LWVol_Wet_Average',	'LWVol_Wet_StandardDeviation',	'LWVol_Bf_Average',	'LWVol_Bf_StandardDeviation',
             'RipCovCanNone_Average',	'UCSRB_RipCanCover',	'RipCovCanNone_StandardDeviation',	'Ucut_Area_Average',
             'Ucut_Area_StandardDeviation',	'FishCovLW_Average',	'FishCovLW_StandardDeviation',	
             'SubEstSandFines_Average',	'SubEstSandFines_StandardDeviation',	'LWFreq_Wet_Average',
             'LWFreq_Wet_StandardDeviation',	'FishCovNone_Average',	'FishCovNone_StandardDeviation',
             'LWFreq_Bf_Average',	'LWFreq_Bf_StandardDeviation',	'FishCovAqVeg_Average',	
             'FishCovAqVeg_StandardDeviation',	'SubEstBldr_Average',	'SubEstBldr_StandardDeviation',
             'SubEstCbl_Average',	'SubEstGrvl_Average',	'FishCovTotal_Average',	'FishCovTotal_StandardDeviation',
             'UcutLgth_Pct_Average',	'UcutLgth_Pct_StandardDeviation',	'UcutArea_Pct_Average',	
             'UcutArea_Pct_StandardDeviation',	'SC_Area_Average',	'SC_Area_StandardDeviation',
             'SC_Area_Pct_Average',	'GRVL_COBL_UCSRB')
# ---- convert all columns listed above to numeric ----------              
CHAMP_data_per_reach[cols.num] <- sapply(CHAMP_data_per_reach[cols.num],as.numeric)

# -------------------------- CHAMP data only use 139 reaches, so add reaches not present as NA -------------
CHAMP_data_Updated =  habitat_raw_data[,c("ReachName","Basin","Assessment.Unit")]
CHAMP_data_Updated = merge(CHAMP_data_Updated, CHAMP_data_per_reach, by = "ReachName", 
                         all.x = TRUE, all.y = TRUE)

# ---------------------------------------------------------------------------
#
#    Reach Information
#
# ---------------------------------------------------------------------------

Reach_Information_data = read_excel( paste(habitat_data_path,'ReachInfo.xlsx', sep="") )


# ---------------------------------------------------------------------------
#
#  Habitat Quality and Geomorphic Potential Rating Criteria
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#    Read in main data
# ---------------------------------------------------------------------------

Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria = read_excel( paste(criteria_and_scoring_path,'Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria.xlsx', sep="") )
# ----------- update columns that are numeric to numeric ------------
cols.num = c('Category_lower', 'Category_upper')
Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria[cols.num] <- sapply(Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria[cols.num],as.numeric)

# ---------------------------------------------------------------------------
#    Generate data frame where each Data Source has a row (since some rows have multiple data sources)
# ---------------------------------------------------------------------------
Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria_Updated = data.frame()

for(rowx in rownames(Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria)){
  
  # ------------- choose each row -------------
  rowx2 = Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria[rowx, ]
  data_source_x = rowx2$Data_Sources
  
  # -------------- verify if multiple data sources ---------
  if(grepl( ",", data_source_x, fixed = TRUE)){
    
    list_data_sources = unlist(strsplit(data_source_x, ","))
    for(data_sources_x in list_data_sources){
      
      # ------------- create updated row with just one data sources --------
      rowx2_updated = rowx2
      rowx2_updated$Data_Sources = data_sources_x
      # ------------- append new row ------------
      Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria_Updated = 
        rbind(Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria_Updated, rowx2_updated)
    }
    
    # ----------- if only one data sources ---------------
  }else{
    Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria_Updated = 
      rbind(Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria_Updated, rowx2)
  }
  
}


# ---------------------------------------------------------------------------
#
#  Habitat Limiting Factor Rating Criteria
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#    Read in main data
# ---------------------------------------------------------------------------

Habitat_Limiting_Factor_Rating_Criteria = read_excel( paste(criteria_and_scoring_path,'Habitat_Limiting_Factor_Rating_Criteria.xlsx', sep="") )

# ----------- update columns that are numeric to numeric ------------
cols.num = c('Category_lower', 'Category_upper', 'Filter_value_lower_meters', 'Filter_value_upper_meters')
Habitat_Limiting_Factor_Rating_Criteria[cols.num] <- sapply(Habitat_Limiting_Factor_Rating_Criteria[cols.num],as.numeric)

# ---------------------------------------------------------------------------
#    Generate data frame where each Data Source has a row (since some rows have multiple data sources)
# ---------------------------------------------------------------------------
Habitat_Limiting_Factor_Rating_Criteria_Updated = data.frame()

for(rowx in rownames(Habitat_Limiting_Factor_Rating_Criteria)){
  
  # ------------- choose each row -------------
  rowx2 = Habitat_Limiting_Factor_Rating_Criteria[rowx, ]
  data_source_x = rowx2$Data_Sources
  
  # -------------- verify if multiple data sources ---------
  if(grepl( ",", data_source_x, fixed = TRUE)){
    
    list_data_sources = unlist(strsplit(data_source_x, ","))
    for(data_sources_x in list_data_sources){
      
      # ------------- create updated row with just one data sources --------
      rowx2_updated = rowx2
      rowx2_updated$Data_Sources = data_sources_x
      # ------------- append new row ------------
      Habitat_Limiting_Factor_Rating_Criteria_Updated = 
        rbind(Habitat_Limiting_Factor_Rating_Criteria_Updated, rowx2_updated)
    }
    
  # ----------- if only one data sources ---------------
  }else{
    Habitat_Limiting_Factor_Rating_Criteria_Updated = 
      rbind(Habitat_Limiting_Factor_Rating_Criteria_Updated, rowx2)
  }
  
}

# ---------------------------------------------------------------------------
#
#   Confinement Scores
#
# ---------------------------------------------------------------------------

# -------------------- Confinement Excel ------------------
Confinement_Scores = read_excel( paste(habitat_data_path,'Confinement_Scores.xlsx', sep="") )
# --------------------- Read in Confinement Scores -----------------
Confinement_Scores_Criteria = read_excel( paste(habitat_data_path,'Confinement_Scores.xlsx', sep="") )
# ----------------- Update Scores -------------


# ------------------- update geomorphic potential/confinement scores with criteria ---------------
source(paste(script_path, 'FUNCTIONS_for_Reading_Data.R', sep=""))

Geomorphic_Criteria = Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria_Updated[which(Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria_Updated$Habitat_Quality_Scoring_Metric == "Geomorphic Potential"),]
FUNCTION_update_Confinement_Scores(Confinement_Scores, Geomorphic_Criteria)

# ---------------------------------------------------------------------------
#
#   Habitat Quality Restoration and Protection Criteria
#
# ---------------------------------------------------------------------------

Habitat_Quality_Restoration_and_Protection_Scoring = read_excel( paste(criteria_and_scoring_path,'Habitat_Quality_Restoration_and_Protection_Scoring.xlsx', sep="") )

# --------------- divide up into Restoration and Protection -------------------------
Restoration_Scoring = Habitat_Quality_Restoration_and_Protection_Scoring %>%
  filter(Habitat_Quality_Score_Metric == 'Habitat Quality Scoring- Restoration')

Protection_Scoring = Habitat_Quality_Restoration_and_Protection_Scoring %>%
  filter(Habitat_Quality_Score_Metric == 'Habitat Quality Scoring- Protection')

# ---------------------------------------------------------------------------
#
#   Restoration and Protection Reach Scoring Criteria 
#
# ---------------------------------------------------------------------------

Reach_Scoring_Restoration_and_Protection_Scoring = read_excel( paste(criteria_and_scoring_path,'Criteria_Reach_Scoring_Restoration_and_Protection.xlsx', sep="") )

# ----------- update columns that are numeric to numeric ------------
cols.num <- c( 'Category_lower_limit',	'Category_upper_limit',	'Score')
Reach_Scoring_Restoration_and_Protection_Scoring[cols.num] <- sapply(Reach_Scoring_Restoration_and_Protection_Scoring[cols.num],as.numeric)


# --------------- divide up into Restoration and Protection -------------------------
Restoration_Reach_Scoring = Reach_Scoring_Restoration_and_Protection_Scoring %>%
  filter(Pathway == 'Restoration')

Protection_Reach_Scoring = Reach_Scoring_Restoration_and_Protection_Scoring %>%
  filter(Pathway == 'Protection')

# ---------------------------------------------------------------------------
#
#  Okanogan EDT data
#
# ---------------------------------------------------------------------------
# NOTE: additional data processing/prep for Okanogan EDT data is done in the Okanogan_EDT_data_input_prep.R script

# --------------- EDT Habitat Attribute Crosswalk --------------------
AttributeCrosswalk = read_excel( paste(Okanogan_EDT_path,'AttributeCrosswalk_Okanogan_EDT.xlsx', sep="") )
# --------------- simplify and prep for Limiting Factor Pathway -------
AttributeCrosswalk_simple = AttributeCrosswalk[,c("Level 2 Attribute", "RTT Habitat Attributes")]
colnames(AttributeCrosswalk_simple) = c("EDT Attribute", "RTT_Habitat_Attribute")

# --------------- EDT Habitat Attribute Crosswalk --------------------
# same data as below HabitatAttribute_Ratings_Level2 and HabitatAttribute_Ratings_Level3 (but the former were split out by levels)
# HabitatAttribute_Ratings = read_excel( paste(Okanogan_EDT_path,'HabitatAttribute_Ratings_Okanogan_EDT.xlsx', sep="") )

# --------------- EDT Habitat Attribute Crosswalk --------------------
HabitatAttribute_Ratings_Level2 = read_excel( paste(Okanogan_EDT_path,'HabitatAttribute_Ratings_Level2.xlsx', sep="") )

# --------------- EDT Habitat Attribute Crosswalk --------------------
HabitatAttribute_Ratings_Level3 = read_excel( paste(Okanogan_EDT_path,'HabitatAttribute_Ratings_Level3.xlsx', sep="") )

# --------------- EDT Habitat Attribute Crosswalk --------------------
Barriers_Okanogan_EDT = read_excel( paste(Okanogan_EDT_path,'Barriers_Okanogan_EDT.xlsx', sep="") )

# --------------- EDT Habitat Attribute Crosswalk --------------------
LifeStageCrosswalk_EDT = read_excel( paste(Okanogan_EDT_path,'LifeStageCrosswalk_Okanogan_EDT.xlsx', sep="") )

# ------------- EDT Crosswalk between Level 2 and Level 3 EDT habitat attribute names --------
Level2_Level3_EDT_Crosswalk = read_excel( paste(Okanogan_EDT_path,'Level2_Level3_EDT_Crosswalk.xlsx', sep="") )

# ------------------ Limiting Factors Okanogan EDT -----------
Limiting_Factors_Okanogan_EDT = read_excel( paste(Okanogan_EDT_path,'Limiting_Factors_Okanogan_EDT.xlsx', sep="") )

# ------------------ Limiting Factors Okanogan EDT -----------
PRCNT_Habitat_Quality_Okanogan_EDT = read_excel( paste(Okanogan_EDT_path,'PRCNT_Habitat_Quality_Okanogan_EDT.xlsx', sep="") )

# ------------------------------- AU Ranks for Okanogan ----------------------------------
AU_Ranks_Okanogan = read_excel( paste(Okanogan_EDT_path,'AU_Ranks_Okanogan_EDT.xlsx', sep="") )
# ----------- update columns that are numeric to numeric ------------
cols.num = c('AU Restoration Rank',	'AU Protection Rank')
AU_Ranks_Okanogan[cols.num] <- sapply(AU_Ranks_Okanogan[cols.num],as.numeric)

# ----------------- just to compare habitat attribute listed --------
# unique(AttributeCrosswalk$`RTT Habitat Attributes`)[order(unique(AttributeCrosswalk$`RTT Habitat Attributes`))]
# unique(HabitatAttribute_Ratings$`RTT Habitat Attribute`)[order(unique(HabitatAttribute_Ratings$`RTT Habitat Attribute`))]
# unique(Limiting_Factors_Okanogan_EDT$)
# missing from HabitatAttribute_Ratings: "Off-Channel- Floodplain"    "Off-Channel- Side-Channels" "Pool Quantity & Quality"

# ---------------------------------------------------------------------------
#
#  Okanogan Criteria Data
#
# ---------------------------------------------------------------------------

# --------------- Okanogan Criteria for scoring --------------------
Criteria_Okanogan_EDT_Scoring = read_excel( paste(criteria_and_scoring_path,'Criteria_Okanogan_EDT_Scoring.xlsx', sep="") )

# ------- generate specific criteria --------
Criteria_Okanogan_EDT_Scoring_Level_2 = Criteria_Okanogan_EDT_Scoring[  which(Criteria_Okanogan_EDT_Scoring$Indicator == "Level 2 Attribute Functional Condition Selector") ,  ]
Criteria_Okanogan_EDT_Scoring_Level_3 = Criteria_Okanogan_EDT_Scoring[  which(Criteria_Okanogan_EDT_Scoring$Indicator == "Level 3 Attribute Functional Condition Selector") ,  ]
Criteria_Okanogan_EDT_Scoring_Limiting_Factor_Level_3 = Criteria_Okanogan_EDT_Scoring[  which(Criteria_Okanogan_EDT_Scoring$Indicator == "Level 3 Life Stage Performance Effect Scoring") ,  ]
Criteria_Okanogan_EDT_Scoring_Barrires = Criteria_Okanogan_EDT_Scoring[  which(Criteria_Okanogan_EDT_Scoring$Indicator == "Level 3 Attribute - Obstructoins (Barrires)") ,  ]

# ---------------------------------------------------------------------------
#
# Professional Judgment Information
#
# ---------------------------------------------------------------------------

Habitat_Attribute_Notes_and_Professional_Judgement = read_excel( paste(habitat_data_path,'Habitat_Attribute_Notes_and_Professional_Judgement.xlsx', sep="") )


# ---------------------------------------------------------------------------
#
#  Reach Assessments Projects and Status
#
# ---------------------------------------------------------------------------

Crosswalk_Habitat_Attributes_and_Actions = read_excel( paste(crosswalks_path,'Crosswalk_Habitat_Attributes_and_Actions.xlsx', sep="/"), 
                                                       sheet = 'Sheet1')
Crosswalk_Habitat_Attributes_and_Actions$Habitat_Attribute_2 = gsub(" ", "", Crosswalk_Habitat_Attributes_and_Actions$`Habitat Attribute`, fixed = TRUE)
Crosswalk_Habitat_Attributes_and_Actions$Action_Category_2 = gsub(" ", "", Crosswalk_Habitat_Attributes_and_Actions$`Action Category`, fixed = TRUE) # get action category without spaces

Reach_Assessment_Project_Data = read_excel( paste(reach_assessment_projects_path,'Reach_Assessments_Projects_Table_05052020.xlsx', sep=""), 
                                                        sheet = 'Data_Entry')
colnames(Reach_Assessment_Project_Data)[colnames(Reach_Assessment_Project_Data) == "Reach_UCSRB"] <- "ReachName" # update ReachName to be consisten with code

Action_Category_Name_Crosswalk = read_excel( paste(reach_assessment_projects_path,'Reach_Assessments_Projects_Table_05052020.xlsx', sep=""), 
                                           sheet = 'Action_Lists')
# ------ generate list with two types of Action Category list ------
Action_Category_Name_Crosswalk_Simple = c()
for(i in 1:nrow(Action_Category_Name_Crosswalk)){
  Action_Category_Name_Crosswalk_Simple[as.character(Action_Category_Name_Crosswalk$Action_Category_from_Rating_Table[i])] =
    Action_Category_Name_Crosswalk$Action_Category_List[i]
}

# ---------------------------------------------------------------------------
#
#      Reading in data to do comparisons 
#
# ---------------------------------------------------------------------------

# --------------- just a loop to comprae EDT-RTT crosswalk habitat attributes and original habitat attribute-action categories attributes -------
compare_RTT_and_EDT_habitat_attributes_True_False = FALSE
if(compare_RTT_and_EDT_habitat_attributes_True_False){
  
  RTT_EDT_habitat_attributes = unique(AttributeCrosswalk$`RTT Habitat Attributes`)
  RTT_EDT_habitat_attributes = RTT_EDT_habitat_attributes[-2]
  
  RTT_orig_habitat_attributes = unique(Crosswalk_Habitat_Attributes_and_Actions$`Habitat Attribute`)
  
  for(hab_x in RTT_EDT_habitat_attributes){
    
    if(!is.na(hab_x)){
      if( any(RTT_orig_habitat_attributes == hab_x) ){
        print(paste("RTT_EDT in orig RTT: ", hab_x))
      }else{
        print(paste("-------- RTT_EDT NOT orig RTT: ", hab_x))
      }
    }
    
  }
  
  
  
}



# ---------------- comparing Reach layer and habitat_raw_data -------------

comparing_habitat_raw_data_and_reach_layer_T_F = FALSE
if(comparing_habitat_raw_data_and_reach_layer_T_F){

  reaches_GIS = read_csv( 'C:/Users/Ryan/Downloads/Reaches_March_2021/Reaches_0.csv')
  
  reaches_in_GIS_not_in_habitat_raw_data = c()
  for(reach_x in reaches_GIS$ReachName){
    
    if( any(habitat_raw_data$ReachName == reach_x) ){
    }else{
      
      #print("this reach is not in habitat raw data:")
      x = which(reaches_GIS$ReachName == reach_x)
      basin_x = reaches_GIS$Basin[x]
      if(basin_x != "Okanogan"){
        print(reach_x) 
        row_x = t(as.data.frame(c(basin_x, reach_x)))
        reaches_in_GIS_not_in_habitat_raw_data = rbind(reaches_in_GIS_not_in_habitat_raw_data,   row_x)
      }
    }
  }
  
  rownames(reaches_in_GIS_not_in_habitat_raw_data) = seq(1,nrow(reaches_in_GIS_not_in_habitat_raw_data))
  colnames(reaches_in_GIS_not_in_habitat_raw_data) = c("Basin","ReachName")

    
}
