
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
              'Structure_bare_ground_prcnt_INDICATOR_10','PROSER',	'NORWEST_Temperature','Canopy_Cover_NORWEST',
              'Undercut_Area_Pct_CHAMP',	'SubEstBldr_CHAMP',	'SubEstSandFines_CHAMP',	'LWFreq_Bf_CHAMP',
              'SC_Area_Pct_Average_CHAMP',	'FishCovNone_Average_CHAMP',	'GRVL_COBL_UCSRB_CHAMP',	'SubEmbed_Avg_Average_CHAMP')
              
habitat_raw_data[cols.num] <- sapply(habitat_raw_data[cols.num],as.numeric)



# USE for interactive (COULD just have a person edit a CSV)
# habitat_raw_data_new <- data_edit(habitat_raw_data, save_as = "habitat_raw_data_updated.csv")

# ---------------------------------------------------------------------------
#     AU Ranks
# ---------------------------------------------------------------------------

AU_Ranks_data = read_excel( paste(data_path_2,'AU_Ranks.xlsx', sep="") )

# ---------------------------------------------------------------------------
#     Life Stage Priorities - only AU level
# ---------------------------------------------------------------------------

Life_Stage_Priorities_AU_only_data = read_excel( paste(data_path_2,'LifeStagePriorities.xlsx', sep=""), skip=1 )

# ---------------------------------------------------------------------------
#     Life Stage Priorities - only AU level
# ---------------------------------------------------------------------------

Life_Stage_Priorities_AU_and_Reach_data = read_excel( paste(data_path_2,'LifeStagePriorities_AUandReach.xlsx', sep="/"), skip=1 )

# ---------------------------------------------------------------------------
#    Reach Information
# ---------------------------------------------------------------------------

Reach_Information_data = read_excel( paste(data_path_2,'ReachInfo.xlsx', sep="/") )


# ---------------------------------------------------------------------------
#
#    END
#
# ---------------------------------------------------------------------------

