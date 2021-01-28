# ---------------------------------------------------------------------------
#
#      R Script to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------

library(tidyverse)

# ---------------------------------------------------------------------------
#
#      Directories of Input and Output data
#
# ---------------------------------------------------------------------------

# --------------- directory of scripts -----------
script_path = 'Scripts/'

# ----------- directory of data -------------------
data_path = 'Data/'

# ----------- directory for output ---------
output_path = 'Output/'

# Old location of the reach attribute (NOT Raw) data:  'Y:/UCRTT/Prioritization/Tables for Tools/'

# ---------------------------------------------------------------------------
#
#     Read in Data
#
# ---------------------------------------------------------------------------

source(paste(script_path, 'Read_in_data_Script.R', sep=""))

# ---------------------------------------------------------------------------
#
#      Criteria for Filters   
#
# ---------------------------------------------------------------------------

source(paste(script_path, 'Criteria_Script.R', sep=""))


# ---------------------------------------------------------------------------
#
#   Generate Habitat Attribute Table
#
# ---------------------------------------------------------------------------

source(paste(script_path, 'Habitat_Quality_Scores_Generate_Script.R', sep=""))



# ---------------------------------------------------------------------------
#
#   Generate Habitat Quality Restoration and Protection Score
#
# ---------------------------------------------------------------------------

source(paste(script_path, 'Habitat_Attribute_Scores_Generate_Script.R', sep=""))












summary(habitat_raw_data )


plot( Pieces_per_mile_INDICATOR_1  ~  as.factor(Disturbance_CATEGORY_1 ), habitat_raw_data )
plot( Pieces_per_mile_INDICATOR_1  ~  GravelCobble_UCSRB_pct, habitat_raw_data )
plot( Pools_deeper_3_ft_per_mile_INDICATOR_4  ~  GravelCobble_UCSRB_pct, habitat_raw_data )


hist(as.numeric(habitat_raw_data$Pools_deeper_5_ft_per_mile_INDICATOR_5))

 
filter(habitat_raw_data,  Pools_deeper_3_ft_per_mile_INDICATOR_4   < 200 )

habitat_raw_data %>%  filter(Pools_deeper_3_ft_per_mile_INDICATOR_4   >10)

habitat_raw_data %>%
  filter(Basin  == "Wenatchee", ) %>%
  select(ReachName, Pools_deeper_3_ft_per_mile_INDICATOR_4)


habitat_raw_data %>%  select(ReachName, Pools_deeper_3_ft_per_mile_INDICATOR_4 ) %>% head(100)

species_reach = 'Steelhead.Reach'
species_reaches_true = 'yes'


habitat_raw_data %>%
  filter(ReachName  == "White River Upper 01" ) %>%
  select(ReachName, PROSER)

