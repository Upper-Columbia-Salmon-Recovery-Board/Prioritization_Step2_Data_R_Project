# ---------------------------------------------------------------------------
#
#      R Script to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
#
#      Directories of Input and Output data
#
# ---------------------------------------------------------------------------

# --------------- directory of scripts -----------
script_path = 'Scripts/'

# ----------- directory of data -------------------
data_path = 'Data/'
#setwd(data_path)

data_path_2 = 'Y:/UCRTT/Prioritization/Tables for Tools/'




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
#   
#
# ---------------------------------------------------------------------------





# ---------------------------------------------------------------------------
#
#   
#
# ---------------------------------------------------------------------------











summary(habitat_raw_data )


plot( Pieces_per_mile_INDICATOR_1  ~  as.factor(Disturbance_CATEGORY_1 ), habitat_raw_data )


hist(as.numeric(habitat_raw_data$Pools_deeper_5_ft_per_mile_INDICATOR_5))


filter(habitat_raw_data,  Boulder_occular_prcnt_INDICATOR_4  > 5 )


