
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

library(readxl)


# ---------------------------------------------------------------------------
#
#     Read in Data
#
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
#     Habitat Raw Data
# ---------------------------------------------------------------------------

habitat_raw_data = read_excel(  paste(data_path,'Habitat_Data_Raw.xlsx', sep="") ,   skip=1  )

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
              'SC_Area_Pct_Average_CHAMP',	'FishCovNone_Average_CHAMP',	'GRVL_COBL_UCSRB_CHAMP',	'SubEmbed_Avg_Average_CHAMP')
              
habitat_raw_data[cols.num] <- sapply(habitat_raw_data[cols.num],as.numeric)


# USE for interactive (COULD just have a person edit a CSV)
# habitat_raw_data_new <- data_edit(habitat_raw_data, save_as = "habitat_raw_data_updated.csv")

# ---------------------------------------------------------------------------
#     AU Ranks
# ---------------------------------------------------------------------------

AU_Ranks_data = read_excel( paste(data_path,'AU_Ranks.xlsx', sep="") )

# ----------- update columns that are numeric to numeric ------------
cols.num = c('Spring Chinook_Restoration',	'SPCHNTier_Restoration',	'Steelhead_Restoration',	'STLTier_Restoration',
             'BullTrout_Restoration',	'BTTier_Restoration',	'Spring Chinook_Protection',	'SPCHNTier_Protection',
             'Steelhead_Protection',	'STLTier_Protection',	'BullTrout_Protection',	'BTTier__Protection')
AU_Ranks_data[cols.num] <- sapply(AU_Ranks_data[cols.num],as.numeric)

# ---------------------------------------------------------------------------
#     Life Stage Priorities - only AU level
# ---------------------------------------------------------------------------

Life_Stage_Priorities_AU_only_data = read_excel( paste(data_path,'LifeStagePriorities.xlsx', sep=""), skip=1 )

# ---------------------------------------------------------------------------
#     Life Stage Priorities - only AU level
# ---------------------------------------------------------------------------

Life_Stage_Priorities_AU_and_Reach_data = read_excel( paste(data_path,'LifeStagePriorities_AUandReach.xlsx', sep=""), skip=1 )

# ---------------------------------------------------------------------------
#     Channel Habitat Unit Data
# ---------------------------------------------------------------------------

Channel_Unit_Raw = read_excel( paste(data_path,'Channel_Unit_Raw.xlsx', sep="") )

# ----------- update columns that are numeric to numeric ------------
cols.num = c('Riffle_Habitat_Prcnt_INDICATOR_1' , 'Rapid_Habitat_Prcnt_INDICATOR_2' , 'Glide_Habitat_Prcnt_INDICATOR_3',
      'Pool_Habitat_Prcnt_INDICATOR_4', 'Cascade_Habitat_Prcnt_INDICATOR_5', 'Side_Channel_Habitat_Prcnt_INDICATOR_6',
      'Braid_Habitat_Prcnt_INDICATOR_7',    'Bar_Habitat_Prcnt_INDICATOR_8')
Channel_Unit_Raw[cols.num] <- sapply(Channel_Unit_Raw[cols.num],as.numeric)

# ---------------------------------------------------------------------------
#     CHAMP data per reach
# ---------------------------------------------------------------------------

CHAMP_data_per_reach = read_excel( paste(data_path,'CHAMP_data_per_reach.xlsx', sep="") )

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
CHAMP_data_per_reach[cols.num] <- sapply(CHAMP_data_per_reach[cols.num],as.numeric)



# ---------------------------------------------------------------------------
#    Reach Information
# ---------------------------------------------------------------------------

Reach_Information_data = read_excel( paste(data_path,'ReachInfo.xlsx', sep="") )

# ---------------------------------------------------------------------------
#   Confinement Scores
# ---------------------------------------------------------------------------

confinement_scores = read_excel( paste(data_path,'Confinement_Scores.xlsx', sep="") )

# ---------------------------------------------------------------------------
#  Habitat Quality and Geomorphic Potential Rating Criteria
# ---------------------------------------------------------------------------

Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria = read_excel( paste(data_path,'Habitat_Quality_and_Geomorphic_Potential_Rating_Criteria.xlsx', sep="") )

# ---------------------------------------------------------------------------
#  Habitat Limiting Factor Rating Criteria
# ---------------------------------------------------------------------------

Habitat_Limiting_Factor_Rating_Criteria = read_excel( paste(data_path,'Habitat_Limiting_Factor_Rating_Criteria.xlsx', sep="") )

# ----------- update columns that are numeric to numeric ------------
cols.num = c('Category_lower', 'Category_upper', 'Filter_value_lower_meters', 'Filter_value_upper_meters')
Habitat_Limiting_Factor_Rating_Criteria[cols.num] <- sapply(Habitat_Limiting_Factor_Rating_Criteria[cols.num],as.numeric)


# ------------------- for each Data Source (since some rows have multipe) into an individual row for each Data Source -------
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
# Professional Judgment Information
# ---------------------------------------------------------------------------

Habitat_Attribute_Notes_and_Professional_Judgement = read_excel( paste(data_path,'Habitat_Attribute_Notes_and_Professional_Judgement.xlsx', sep="") )


# ---------------------------------------------------------------------------
#  Reach Assessments Projects and Status
# ---------------------------------------------------------------------------

Reach_Asessment_Project_Data = read_excel( paste(data_path,'Reach_Assessments_Projects_Table_05052020.xlsx', sep="/"), 
                                                        sheet = 'Data_Entry')

Reach_Asessment_Projects_Actions_List = read_excel( paste(data_path,'Reach_Assessments_Projects_Table_05052020.xlsx', sep="/"), 
                                           sheet = 'Action_Lists')

Reach_Assessment_Check_List = read_excel( paste(data_path,'Reach_Assessments_Projects_Table_05052020.xlsx', sep="/"), 
                                                    sheet = 'Reach_Assessment_Check_List')




# ---------------------------------------------------------------------------
#
#    END
#
# ---------------------------------------------------------------------------

