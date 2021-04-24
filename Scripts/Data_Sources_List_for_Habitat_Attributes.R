

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
  '% Fines/Embeddedness' = c('Dominant_Substrate_CATEGORY_1', 'Clay_Silt_Sand_occular_prcnt_INDICATOR_7', 'D50_sieve_size_prcnt_finer_mm_INDICATOR_13', 'SubEstSandFines_CHAMP'),
  'Brook Trout' = c('PROFESSIONAL JUDGEMENT', 'Step1 results'),
  'Coarse Substrate' = c('Dominant_Substrate_CATEGORY_1', 'GravelCobble_UCSRB_pct', 'SubEmbed_Avg_Average_CHAMP', 'SubEstSandFines_Average'),
  'Contaminants' = c( 'Contaminants_303d'),
  'Cover- Boulders'  = c( 'Boulder_UCSRB_pct',  'SubEstBldr_CHAMP'), 
  'Cover- Undercut Banks' = c('Bank_Stability_CATEGORY_1', 'Structure_CATEGORY_1', 'Undercut_Area_Pct_CHAMP' ), 
  'Cover- Wood' = c('Pieces_per_mile_CATEGORY_1', 'Pieces_per_mile_INDICATOR_1', 'LWFreq_Bf_CHAMP' ), 
  'Entrainment/Stranding' = c('PROFESSIONAL JUDGEMENT'),
  'Flow- Scour' = c( 'PROFESSIONAL JUDGEMENT'), 
  'Flow- Summer Base Flow' = c( 'RAWatershed_Rating_Flow', 'Flow_305bList', 'PROSPER','ATLAS_Flow', 'PROFESSIONAL JUDGEMENT'), 
  'Food- Food Web Resources' = c( 'PROFESSIONAL JUDGEMENT'), 
  'Harassment' = c('PROFESSIONAL JUDGEMENT' ), 
  'Icing' = c( 'PROFESSIONAL JUDGEMENT'), 
  'Off-Channel- Floodplain' = c( 'Floodplain_Connectivity_CATEGORY_1', 'Entrenchment_CATEGORY_2', 'Channel_Confinementor_or_Entrenchment_Ratio_INDICATOR_9', 'UCSRB_OffChannel_Floodplain' ),   # Data Sources listes first HabAtr as Floodplain_Connectivity_CATEGORY_1
  'Off-Channel- Side-Channels' = c('Connectivity_CATEGORY_1', 'Side_Channel_Habitat_Prcnt_INDICATOR_6', 'WetSC_Pct_Average', 'UCSRB_OffChannel_SideChannels'  ),
  'Pool Quantity & Quality' = c( 'Pools_CATEGORY_1', 'Pool_Habitat_Prcnt_INDICATOR_4', 'Pools_per_mile_INDICATOR_2'), 
  'Pools- Deep Pools' = c('Pools_deeper_3_ft_prcnt_INDICATOR_3', 'Pools_deeper_3_ft_per_mile_INDICATOR_4'  ), 
  'Predators- Adults' = c( 'PROFESSIONAL JUDGEMENT'), 
  'Predators- Juveniles' = c( 'PROFESSIONAL JUDGEMENT'),
  'Superimposition' = list('PROFESSIONAL JUDGEMENT' ), 
  'Temperature- Adult Holding' = c( 'NORWEST_Temperature', '305bListings_Temperature', 'RAWatershed_Rating_Temp' ), 
  'Temperature- Adult Spawning' = c('NORWEST_Temperature', '305bListings_Temperature', 'RAWatershed_Rating_Temp' ), 
  'Temperature- Rearing'  = c('NORWEST_Temperature', '305bListings_Temperature', 'RAWatershed_Rating_Temp' )
)

# ------------- habitat attribute AND habitat attribute number/location of data FROM Channel Unit Data -------
#    Data listed here should pull data from Channel_Unit_Raw data frame (not habitat_raw_data)
# data frame: Channel_Unit_Raw
# the number is the location in the list of data sources (so 2 means it is the second data pulled for that habitat attribute)
Channel_Unit_Raw_data_use = list('Off-Channel- Side-Channels' = 2,
                                 'Pool Quantity & Quality'  = 2)
Channel_Unit_Raw_data_sources = list('Off-Channel- Side-Channels' = 'Side_Channel_Habitat_Prcnt_INDICATOR_6',
                                 'Pool Quantity & Quality'  = 'Pool_Habitat_Prcnt_INDICATOR_4')

# ------------- habitat attribute AND habitat attribute number/location of data FROM CHAMP data table -------
#    Data listed here should pull data from Channel_Unit_Raw data frame (not habitat_raw_data)
# data frame: CHAMP_data_per_reach
# the number is the location in the list of data sources (so 3 means it is the third data pulled for that habitat attribute)
CHAMP_data_per_reach_data_use = list('Off-Channel- Side-Channels' = 3,
                                     'Coarse Substrate' = 4)
CHAMP_data_per_reach_data_sources = list('Off-Channel- Side-Channels' = 'WetSC_Pct_Average',
                                     'Coarse Substrate' = 'SubEstSandFines_Average')
#------------------------- column names of Habitat_Quality_Scores - HQ scores output (generated scores for pathway) -------
habitat_quality_scores_colnames_for_sum = c('Stability_Mean','CoarseSubstrate_score','Cover-Wood_score',
                                    'Flow-SummerBaseFlow_score', 'Off-Channel-Floodplain_score',
                                    'Off-Channel-Side-Channels_score','PoolQuantity&Quality_score','Riparian_Mean',
                                    'Temperature-Rearing_score')

habitat_quality_scores_colnames_ALL = c("BankStability_score", "ChannelStability_score",'Stability_Mean','CoarseSubstrate_score','Cover-Wood_score',
                                            'Flow-SummerBaseFlow_score', 'Off-Channel-Floodplain_score',
                                            'Off-Channel-Side-Channels_score','PoolQuantity&Quality_score',"Riparian-Disturbance_score" , "Riparian-CanopyCover_score" ,
                                        'Riparian_Mean', 'Temperature-Rearing_score')

# ---------------------------------------------------------------------------
#
#   Habitat Quality - List of Attributes and Data Sources      
#
# ---------------------------------------------------------------------------

# ----------------------- list of habitat attribute names in habitat raw data data frame --------------
# NOTE: as of March 2021 - R Code pulls habitat attribute (limiting factor) scores for this HQ pathway, unless
#               a paritcular attribute is not present in the habitat attribute scores
# a few data sources are in the Channel_Unit_Raw or CHAMP_data_per_reach data frames, see list below
# data frame: habitat_raw_data (each of these data sources is a column in habitat_raw_data)
# NOTE: order of the list is order data are pulled - so the first data source listed for each habitat attribute
#       is the first data source pulled for each reach
# 
Habitat_Quality_Habitat_Attributes_List = list(
  'Bank Stability' = c('Bank_Stability_CATEGORY_1', 'UCSRB_BankStability'), 
  'Channel Stability' = c('Vertical_Channel_Stability_CATEGORY_1', 'UCSRB_ChannelStability'),
  'Coarse Substrate' = c('Dominant_Substrate_CATEGORY_1'),
  'Cover- Wood' = c('Pieces_per_mile_CATEGORY_1' ), 
  'Flow- Summer Base Flow' = c( 'RAWatershed_Rating_Flow', 'Flow_305bList', 'PROSPER', 'PROFESSIONAL JUDGEMENT'), 
  'Off-Channel- Floodplain' = c( 'Floodplain_Connectivity_CATEGORY_1' ),  
  'Off-Channel- Side-Channels' = c('Connectivity_CATEGORY_1'  ),
  'Pool Quantity & Quality' = c( 'Pools_CATEGORY_1'), 
  'Riparian-Disturbance'  = c( 'Disturbance_CATEGORY_1', 'UCSRB_RiparianDisturbancePct'), 
  'Riparian- Canopy Cover' = c('Canopy_Cover_CATEGORY_1', 'UCSRB_CanopyCoverPct'),
  'Temperature- Rearing'  = c('NORWEST_Temperature', '305bListings_Temperature', 'RAWatershed_Rating_Temp' )
)


