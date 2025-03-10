

# ---------------------------------------------------------------------------
#
#      SCRIPT: List of Data Sources for each Habitat Attributes
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
#   Habitat Attributes (Limiting Factors) - List of Attributes and Data Sources      
#
# ---------------------------------------------------------------------------

# ----------------------- list of habitat attribute names in habitat raw data data frame --------------
# a few data sources are in the Channel_Unit_Raw or CHAMP_data_per_reach data frames, see list below
# data frame: habitat_raw_data (each of these data sources is a column in habitat_raw_data)
# NOTE: order of the list is order data are pulled - so the first data source listed for each habitat attribute
#       is the first data source pulled for each reach
# 

Habitat_Attributes_List = list(
  '% Fines/Embeddedness' = c('Dominant_Substrate_CATEGORY_1', 'Clay_Silt_Sand_occular_prcnt_INDICATOR_7', 'D50_sieve_size_prcnt_finer_mm_INDICATOR_13', 'SubEstSandFines_CHAMP', 'EDT_Percent_finer_0.85_mm' , 'Average_PRCNT_Embeddedness_Cramer_2023', 'PRCNT_Fines_Cascade_Fisheries_2024'),
  'Bank Stability' = c('Bank_Stability_CATEGORY_1' , 'UCSRB_BankStability', 'Channel_Migration_REI_Cascade_Fisheries_2024' ),
  'Brook Trout' = c('PROFESSIONAL JUDGEMENT', 'Step1 results'),
  'Channel Stability' = c('Vertical_Channel_Stability_CATEGORY_1', 'UCSRB_ChannelStability', 'Vertical_channel_stability_REI_Cascade_Fisheries_2024'),
  'Coarse Substrate' = c('Dominant_Substrate_CATEGORY_1', 'GravelCobble_UCSRB_pct', 'GRVL_COBL_UCSRB_CHAMP', 'Cramer_Substrate_FIELD_REI', 'Average_PRCNT_Gravel_Cobble_Cramer_2023' , 'PRCNT_Gravel_Cobble_Cascade_Fisheries_2024' ),
  'Contaminants' = c( 'Contaminants_303d (not pH bacteria or DO listings)'),
  'Cover- Boulders'  = c( 'Boulder_UCSRB_pct',  'SubEstBldr_CHAMP'), 
  'Cover- Undercut Banks' = c('Bank_Stability_CATEGORY_1', 'Structure_CATEGORY_1', 'Undercut_Area_Pct_CHAMP' ), 
  'Cover- Wood' = c('Pieces_per_mile_CATEGORY_1', 'Pieces_per_mile_Medium_Large_INDICATOR_1', 'LWFreq_Bf_CHAMP', 'Cramer_Cover_Wood_FIELD_REI', 'Large_Wood_Per_Mile_Cramer_2023' , 'Large_wood_per_mile_Cascade_Fisheries_2024' ), 
  'Entrainment/Stranding' = c('PROFESSIONAL JUDGEMENT'),
  'Flow- Scour' = c( 'PROFESSIONAL JUDGEMENT'), 
  'Flow- Summer Base Flow' = c( 'RAWatershed_Rating_Flow', 'Flow_305bList', 'PROSPER', 'PROFESSIONAL JUDGEMENT', 'ATLAS Flow'), 
  'Food- Food Web Resources' = c( 'PROFESSIONAL JUDGEMENT'), 
  'Harassment' = c('PROFESSIONAL JUDGEMENT' ), 
  'Icing' = c( 'PROFESSIONAL JUDGEMENT'), 
  'Floodplain Connectivity' = c( 'Floodplain_Connectivity_CATEGORY_1', 'Entrenchment_CATEGORY_2', 'Channel_Confinementor_or_Entrenchment_Ratio_INDICATOR_9', 'UCSRB_OffChannel_Floodplain', 'Floodplain_Connectivity_REI_Cascade_Fisheries_2024', 'PRCNT_Reach_length_with_bank_armour_or_levee' ),   # Old: "Off-Channel- Floodplain"
  'Off-Channel/Side-Channels' = c('Connectivity_CATEGORY_1', 'Side_Channel_Habitat_Prcnt_INDICATOR_6', 'WetSC_Pct_Average', 'UCSRB_OffChannel_SideChannels', 'Off_Channel_Connectivity_REI_Rating_Cramer_2023', 'Off_Channel_habitat_REI_Cascade_Fisheries_2024', 'PRCNT_Area_Off_Channel_10_yr_floodplain_Aspect'  ),
  'Pool Quantity & Quality' = c( 'Pools_CATEGORY_1', 'Pool_Habitat_Prcnt_INDICATOR_4', 'Pools_per_mile_INDICATOR_2', 'Cramer_Pools_FIELD_REI', 'Pools_per_mile_Cramer_2023','Pools_per_mile_Cascade_Fisheries_2024'),  #  'Channel_widths_per_pool_Cascade_Fisheries_2024'
  'Pools- Deep Pools' = c('Pools_deeper_3_ft_prcnt_INDICATOR_3', 'Pools_deeper_3_ft_per_mile_INDICATOR_4', 'Pools_deeper_5_ft_per_mile_INDICATOR_5'  ), 
  'Predators- Adults' = c( 'PROFESSIONAL JUDGEMENT'), 
  'Predators- Juveniles' = c( 'PROFESSIONAL JUDGEMENT'),
  'Riparian- Canopy Cover' = c('Canopy_Cover_CATEGORY_1', 'UCSRB_CanopyCoverPct', 'Aspect_Riparian_Tree_Shrub_Cover_PCNT', 'Canopy_Cover_NORWEST'),
  'Riparian-Disturbance' = c('Disturbance_CATEGORY_1', 'UCSRB_RiparianDisturbancePct', 'Aspect_Riparian_Disturbance_PRCNT'),
  'Superimposition' = list('PROFESSIONAL JUDGEMENT' ), 
  'Temperature- Adult Holding' = c( 'NORWEST_Temperature', '305bListings_Temperature', 'RAWatershed_Rating_Temp' ), 
  'Temperature- Adult Spawning' = c('NORWEST_Temperature', '305bListings_Temperature', 'RAWatershed_Rating_Temp' ), 
  'Temperature- Rearing'  = c('NORWEST_Temperature', '305bListings_Temperature', 'RAWatershed_Rating_Temp' )
)


# ----------------------- list of Level 2 habitat attribute names in Okanogan EDT Results --------------

Habitat_Attributes_List_OKANOGAN = list(
  '% Fines/Embeddedness' = c('EDT_Level 2 Fine Sediment', 'EDT_Level 2 Embeddedness'),
  'Bank Stability' = c('EDT_Level 2 Confinement: Artificial'),
  'Brook Trout' = c('PROFESSIONAL JUDGEMENT', 'Step1 results'),
  'Channel Stability' = c('EDT_Level 2 Confinement: Artificial'),
  'Coarse Substrate' = c('EDT_UCSRBCoarseSub pct'),
  'Contaminants' = c( 'Contaminants_303d (not pH bacteria or DO listings)', 'EDT Level 2 Metals in Sediments Functional Condition','EDT Level 2 Metals in Water Functional Condition','EDT Level 2 Miscellaneous Toxins Functional Condition'),
  'Cover- Boulders'  = c( 'SubEstBldr_CHAMP'), 
  'Cover- Undercut Banks' = c( 'Undercut_Area_Pct_CHAMP' ), 
  'Cover- Wood' = c('EDT_Woody Debris' ), 
  'Entrainment/Stranding' = c('PROFESSIONAL JUDGEMENT'),
  'Flow- Scour' = c( 'EDT_Level 2 Bed scour','EDT_Level 2 Flow: Inter-Annual Low Flow Variation'), 
  'Flow- Summer Base Flow' = c( 'EDT_Width','EDT_Level 2 Flow: Inter-Annual Low Flow Variation', 'PROSPER' , 'PROFESSIONAL JUDGEMENT','ATLAS Flow'), 
  'Food- Food Web Resources' = c('PROFESSIONAL JUDGEMENT',  'EDT_Level 2 Benthic Richness', 'EDT_Temperature: Food Effect'), 
  'Harassment' = c('PROFESSIONAL JUDGEMENT' ), 
  'Icing' = c( 'PROFESSIONAL JUDGEMENT'), 
  'Floodplain Connectivity' = c( 'EDT_Floodplain pct'),
  'Off-Channel/Side-Channels' = c('EDT Level 2 Side Channel Functional Condition'  ),
  'Pool Quantity & Quality' = c( 'EDT_Scour Pool pct'), 
  'Pools- Deep Pools' = c(  'PROFESSIONAL JUDGEMENT' ),
  'Predators- Adults' = c( 'PROFESSIONAL JUDGEMENT'), 
  'Predators- Juveniles' = c( 'PROFESSIONAL JUDGEMENT', 'EDT_Level 2 Predation Risk'),
  'Riparian- Canopy Cover' = c( 'UCSRB_CanopyCoverPct' , 'Canopy_Cover_NORWEST'),
  'Riparian-Disturbance' = c('UCSRB_RiparianDisturbancePct' ),
  'Superimposition' = list('PROFESSIONAL JUDGEMENT' ), 
  'Temperature- Adult Holding' = c( 'NORWEST_Temperature', '305bListings_Temperature',  'EDT_Temperature: Daily Maximum' ), 
  'Temperature- Adult Spawning' = c('NORWEST_Temperature', '305bListings_Temperature',  'EDT_Temperature: Daily Maximum'), 
  'Temperature- Rearing'  = c('NORWEST_Temperature', '305bListings_Temperature',  'EDT_Temperature: Daily Maximum' )
)


# ---------hybrid EDT and empirical data ----------
Habitat_Attributes_List_OKANOGAN = list(
  '% Fines/Embeddedness' = c('CCT_OBMEP_embeddedness', 'CCT_OBMEP_fines_and_sands'),
  'Bank Stability' = c('EDT_Level 2 Confinement: Artificial'),
  'Brook Trout' = c('PROFESSIONAL JUDGEMENT', 'Step1 results'),
  'Channel Stability' = c('EDT_Level 2 Confinement: Artificial'),
  'Coarse Substrate' = c('CCT_OBMEP_gravel_and_small_cobbles', 'Cramer_Substrate_MODELED_sand_silt_clay_PRCNT' , 'Cramer_Substrate_MODELED_REI' ),
  'Contaminants' = c( 'Contaminants_303d (not pH bacteria or DO listings)', 'EDT Level 2 Metals in Sediments Functional Condition','EDT Level 2 Metals in Water Functional Condition','EDT Level 2 Miscellaneous Toxins Functional Condition'),
  'Cover- Boulders'  = c(  'SubEstBldr_CHAMP'), 
  'Cover- Wood' = c('CCT_OBMEP_LWM_pieces_per_mile', 'Pieces_per_mile_Medium_Large_INDICATOR_1', 'LWFreq_Bf_Average CHAMP', 'Cramer_Cover_Wood_FIELD_REI', 'Cramer_Cover_Wood_MODELED_REI' ), 
  'Cover- Undercut Banks' = c( 'Undercut_Area_Pct_CHAMP' ), 
  'Entrainment/Stranding' = c('PROFESSIONAL JUDGEMENT'),
  'Flow- Scour' = c( 'EDT_Level 2 Bed scour','EDT_Level 2 Flow: Inter-Annual Low Flow Variation'), 
  'Flow- Summer Base Flow' = c( 'EDT_Width','EDT_Level 2 Flow: Inter-Annual Low Flow Variation', 'PROSPER' ,'Flow_305bList', 'ATLAS Flow','PROFESSIONAL JUDGEMENT'), 
  'Food- Food Web Resources' = c('PROFESSIONAL JUDGEMENT',  'EDT_Level 2 Benthic Richness', 'EDT_Temperature: Food Effect'), 
  'Harassment' = c('PROFESSIONAL JUDGEMENT' ), 
  'Icing' = c( 'PROFESSIONAL JUDGEMENT'), 
  'Floodplain Connectivity' = c( 'EDT_Floodplain pct','PRCNT_Reach_length_with_bank_armour_or_levee'),
  'Off-Channel/Side-Channels' = c('CCT_OBMEP_channel_type_side_channel_and_off_channel_PRCNT'  ),
  'Pool Quantity & Quality' = c( 'CCT_OBMEP_pools_per_mile', 'Pool_Habitat_Prcnt_INDICATOR_4', 'Pools_per_mile_INDICATOR_2', 'Cramer_Pools_FIELD_REI', 'Cramer_Pools_MODELED_REI' ), 
  'Pools- Deep Pools' = c(  'PROFESSIONAL JUDGEMENT' ),
  'Predators- Adults' = c( 'PROFESSIONAL JUDGEMENT'), 
  'Predators- Juveniles' = c( 'PROFESSIONAL JUDGEMENT', 'EDT_Level 2 Predation Risk'),
  'Riparian- Canopy Cover' = c( 'UCSRB_CanopyCoverPct' , 'Canopy_Cover_NORWEST'),
  'Riparian-Disturbance' = c('UCSRB_RiparianDisturbancePct' ),
  'Superimposition' = list('PROFESSIONAL JUDGEMENT' ), 
  'Temperature- Adult Holding' = c( 'NORWEST_Temperature', '305bListings_Temperature',  'EDT_Temperature: Daily Maximum' ), 
  'Temperature- Adult Spawning' = c('NORWEST_Temperature', '305bListings_Temperature',  'EDT_Temperature: Daily Maximum'), 
  'Temperature- Rearing'  = c('NORWEST_Temperature', '305bListings_Temperature',  'EDT_Temperature: Daily Maximum' )
)

# ----------------------- list of Okanogan habitat attribute names in habitat raw data data frame --------------

if(Okanogan_direct_data_NOT_EDT){
  Habitat_Attributes_List_OKANOGAN = list(
    '% Fines/Embeddedness' = c('CCT_OBMEP_embeddedness', 'CCT_OBMEP_fines_and_sands'),
    'Bank Stability' = c('EDT_Level 2 Confinement: Artificial'),
    'Brook Trout' = c('PROFESSIONAL JUDGEMENT', 'Step1 results'),
    'Channel Stability' = c('EDT_Level 2 Confinement: Artificial'),
    'Coarse Substrate' = c('CCT_OBMEP_gravel_and_small_cobbles', 'Cramer_Substrate_MODELED_sand_silt_clay_PRCNT' , 'Cramer_Substrate_MODELED_REI' ),
    'Contaminants' = c( 'Contaminants_303d (not pH bacteria or DO listings)', 'EDT Level 2 Metals in Sediments Functional Condition','EDT Level 2 Metals in Water Functional Condition','EDT Level 2 Miscellaneous Toxins Functional Condition'),
    'Cover- Boulders'  = c(  'SubEstBldr_CHAMP'), 
    'Cover- Wood' = c('CCT_OBMEP_LWM_pieces_per_mile', 'Pieces_per_mile_Medium_Large_INDICATOR_1', 'LWFreq_Bf_Average CHAMP', 'Cramer_Cover_Wood_FIELD_REI', 'Cramer_Cover_Wood_MODELED_REI' ), 
    'Cover- Undercut Banks' = c( 'Undercut_Area_Pct_CHAMP' ), 
    'Entrainment/Stranding' = c('PROFESSIONAL JUDGEMENT'),
    'Flow- Scour' = c( 'EDT_Level 2 Bed scour','EDT_Level 2 Flow: Inter-Annual Low Flow Variation'), 
    'Flow- Summer Base Flow' = c( 'EDT_Width','EDT_Level 2 Flow: Inter-Annual Low Flow Variation', 'PROSPER' ,'Flow_305bList', 'ATLAS Flow','PROFESSIONAL JUDGEMENT'), 
    'Food- Food Web Resources' = c('PROFESSIONAL JUDGEMENT',  'EDT_Level 2 Benthic Richness', 'EDT_Temperature: Food Effect'), 
    'Harassment' = c('PROFESSIONAL JUDGEMENT' ), 
    'Icing' = c( 'PROFESSIONAL JUDGEMENT'), 
    'Floodplain Connectivity' = c( 'EDT_Floodplain pct','PRCNT_Reach_length_with_bank_armour_or_levee'),
    'Off-Channel/Side-Channels' = c('CCT_OBMEP_channel_type_side_channel_and_off_channel_PRCNT'  ),
    'Pool Quantity & Quality' = c( 'CCT_OBMEP_pools_per_mile', 'Pool_Habitat_Prcnt_INDICATOR_4', 'Pools_per_mile_INDICATOR_2', 'Cramer_Pools_FIELD_REI', 'Cramer_Pools_MODELED_REI' ), 
    'Pools- Deep Pools' = c(  'PROFESSIONAL JUDGEMENT' ),
    'Predators- Adults' = c( 'PROFESSIONAL JUDGEMENT'), 
    'Predators- Juveniles' = c( 'PROFESSIONAL JUDGEMENT', 'EDT_Level 2 Predation Risk'),
    'Riparian- Canopy Cover' = c( 'UCSRB_CanopyCoverPct' , 'Canopy_Cover_NORWEST'),
    'Riparian-Disturbance' = c('UCSRB_RiparianDisturbancePct' ),
    'Superimposition' = list('PROFESSIONAL JUDGEMENT' ), 
    'Temperature- Adult Holding' = c( 'NORWEST_Temperature', '305bListings_Temperature',  'EDT_Temperature: Daily Maximum' ), 
    'Temperature- Adult Spawning' = c('NORWEST_Temperature', '305bListings_Temperature',  'EDT_Temperature: Daily Maximum'), 
    'Temperature- Rearing'  = c('NORWEST_Temperature', '305bListings_Temperature',  'EDT_Temperature: Daily Maximum' )
  )
  
}

# ---------------------------------- IF Using Cramer Modeled data ----------------
# Cramer did not do surveys OR modeled output in Okanogan

if(Cramer_Remote_Sensing_yes_no){
  # -------------------- Coarse Substrate ---------------
  # ---------- Methow, Entiat, Wenatchee ------
  Habitat_Attributes_List = Habitat_Attributes_List[-which( names(Habitat_Attributes_List)  == "Coarse Substrate")]
  Habitat_Attributes_List2 = list( 'Coarse Substrate' = c('Dominant_Substrate_CATEGORY_1', 'GravelCobble_UCSRB_pct', 'GRVL_COBL_UCSRB_CHAMP', 'Cramer_Substrate_FIELD_REI' ,'Cramer_Substrate_MODELED_REI', 'Average_PRCNT_Gravel_Cobble_Cramer_2023','PRCNT_Gravel_Cobble_Cascade_Fisheries_2024') )
  Habitat_Attributes_List=  append(Habitat_Attributes_List, Habitat_Attributes_List2 )
  
  # ------------------ Cover- Wood -------------------------
  # ---------- Methow, Entiat, Wenatchee ------
  Habitat_Attributes_List = Habitat_Attributes_List[-which( names(Habitat_Attributes_List)  == "Cover- Wood")]
  Habitat_Attributes_List2 = list( 'Cover- Wood' =  c('Pieces_per_mile_CATEGORY_1', 'Pieces_per_mile_Medium_Large_INDICATOR_1', 'LWFreq_Bf_CHAMP', 'Cramer_Cover_Wood_FIELD_REI', 'Cramer_Cover_Wood_MODELED_REI','Large_Wood_Per_Mile_Cramer_2023','Large_wood_per_mile_Cascade_Fisheries_2024'))   #
  Habitat_Attributes_List=  append(Habitat_Attributes_List, Habitat_Attributes_List2 )

  # ------------------ Pool Quantity & Quality -------------------------
  # ---------- Methow, Entiat, Wenatchee ------
  Habitat_Attributes_List = Habitat_Attributes_List[-which( names(Habitat_Attributes_List)  == "Pool Quantity & Quality")]
  Habitat_Attributes_List2 = list( 'Pool Quantity & Quality' = c('Pools_CATEGORY_1', 'Pool_Habitat_Prcnt_INDICATOR_4', 'Pools_per_mile_INDICATOR_2','Cramer_Pools_MODELED_pools_per_mile', 'Cramer_Pools_FIELD_REI', 'Cramer_Pools_MODELED_REI','Pools_per_mile_Cramer_2023','Pools_per_mile_Cascade_Fisheries_2024') ) #, 'Channel_widths_per_pool_Cascade_Fisheries_2024'
  Habitat_Attributes_List=  append(Habitat_Attributes_List, Habitat_Attributes_List2 )

}



# ---------------------------------- Read in crosswalk between EDT names and names in HabitatAttribute_Ratings_Level2_updated ----------------
Level2_Data_Sources_Name_Crosswalk = read_excel( paste(Okanogan_EDT_path,'Level2_Data_Sources_Name_Crosswalk.xlsx', sep="") )


# ------------- habitat attribute AND habitat attribute number/location of data FROM Channel Unit Data -------
#    Data listed here should pull data from Channel_Unit_Raw data frame (not habitat_raw_data)
# data frame: Channel_Unit_Raw
# the number is the location in the list of data sources (so 2 means it is the second data pulled for that habitat attribute)
Channel_Unit_Raw_data_use = list('Off-Channel/Side-Channels' = 2,
                                 'Pool Quantity & Quality'  = 2)
Channel_Unit_Raw_data_sources = list('Off-Channel/Side-Channels' = 'Side_Channel_Habitat_Prcnt_INDICATOR_6',
                                 'Pool Quantity & Quality'  = 'Pool_Habitat_Prcnt_INDICATOR_4')

# ------------- habitat attribute AND habitat attribute number/location of data FROM CHAMP data table -------
#    Data listed here should pull data from Channel_Unit_Raw data frame (not habitat_raw_data)
# data frame: CHAMP_data_per_reach
# the number is the location in the list of data sources (so 3 means it is the third data pulled for that habitat attribute)
CHAMP_data_per_reach_data_use = list('Off-Channel/Side-Channels' = 3,
                                     'Coarse Substrate' = 4)
CHAMP_data_per_reach_data_sources = list('Off-Channel/Side-Channels' = 'WetSC_Pct_Average',
                                     'Coarse Substrate' = 'SubEstSandFines_Average' )  # TRIEd: 'GRVL_COBL_UCSRB_CHAMP' 
#------------------------- column names of Habitat_Quality_Scores - HQ scores output (generated scores for pathway) -------
habitat_quality_scores_colnames_for_sum = c("Stability_Mean" , "CoarseSubstrate_score" ,"Cover-Wood_score", "Flow-SummerBaseFlow_score",
                                            "FloodplainConnectivity_score", "Off-Channel/Side-Channels_score","PoolQuantity&Quality_score",
                                            "Riparian_Mean","Temperature-Rearing_score")

habitat_quality_scores_colnames_ALL = c("BankStability_score", "ChannelStability_score",'Stability_Mean','CoarseSubstrate_score','Cover-Wood_score',
                                            'Flow-SummerBaseFlow_score', 'FloodplainConnectivity_score',
                                            'Off-Channel/Side-Channels_score','PoolQuantity&Quality_score',"Riparian-Disturbance_score" , "Riparian-CanopyCover_score" ,
                                        'Riparian_Mean', 'Temperature-Rearing_score')

# ---------------------------------------------------------------------------
#
#   Habitat Quality - List of Attributes and Data Sources      
#
# ---------------------------------------------------------------------------

# ----------------------- list of habitat attribute names in habitat raw data data frame --------------
# NOTE: as of March 2021 - R Code pulls habitat attribute (limiting factor) scores for this HQ pathway, unless
#               a particular attribute is not present in the habitat attribute scores
# a few data sources are in the Channel_Unit_Raw or CHAMP_data_per_reach data frames, see list below
# data frame: habitat_raw_data (each of these data sources is a column in habitat_raw_data)
# NOTE: order of the list is order data are pulled - so the first data source listed for each habitat attribute
#       is the first data source pulled for each reach
# 
Habitat_Quality_Habitat_Attributes_List = list(
  'Bank Stability' = c('Bank_Stability_CATEGORY_1', 'UCSRB_BankStability'), 
  'Channel Stability' = c('Vertical_Channel_Stability_CATEGORY_1', 'UCSRB_ChannelStability'),
  'Coarse Substrate' = c('Dominant_Substrate_CATEGORY_1', 'GravelCobble_UCSRB_pct', 'GRVL_COBL_UCSRB_CHAMP'),
  'Cover- Wood' = c('Pieces_per_mile_CATEGORY_1', 'Pieces_per_mile_Medium_Large_INDICATOR_1', 'LWFreq_Bf_CHAMP' ), 
  'Flow- Summer Base Flow' = c( 'RAWatershed_Rating_Flow', 'Flow_305bList', 'PROSPER', 'PROFESSIONAL JUDGEMENT', 'ATLAS Flow'), 
  'Floodplain Connectivity' = c( 'Floodplain_Connectivity_CATEGORY_1', 'Entrenchment_CATEGORY_2', 'Channel_Confinementor_or_Entrenchment_Ratio_INDICATOR_9', 'UCSRB_OffChannel_Floodplain' ),   # Data Sources listes first HabAtr as Floodplain_Connectivity_CATEGORY_1
  'Off-Channel/Side-Channels' = c('Connectivity_CATEGORY_1', 'Side_Channel_Habitat_Prcnt_INDICATOR_6', 'WetSC_Pct_Average', 'UCSRB_OffChannel_SideChannels'  ),
  'Pool Quantity & Quality' = c( 'Pools_CATEGORY_1', 'Pool_Habitat_Prcnt_INDICATOR_4', 'Pools_per_mile_INDICATOR_2'), 
  'Riparian- Canopy Cover' = c( 'UCSRB_CanopyCoverPct' , 'Canopy_Cover_NORWEST'),
  'Riparian-Disturbance' = c('Disturbance_CATEGORY_1' , 'UCSRB_RiparianDisturbancePct' ),
  'Temperature- Rearing'  = c('NORWEST_Temperature', '305bListings_Temperature', 'RAWatershed_Rating_Temp' )
)

# ----------------------- Habitat Attributes to default to REI if those data are available ------
REI_Default_List = c(
  'Bank Stability',
  'Channel Stability',
  'Coarse Substrate' ,
  'Cover- Wood' , 
  'Floodplain Connectivity',
  'Off-Channel/Side-Channels' ,
  'Pool Quantity & Quality',
  'Riparian-Disturbance'
)


