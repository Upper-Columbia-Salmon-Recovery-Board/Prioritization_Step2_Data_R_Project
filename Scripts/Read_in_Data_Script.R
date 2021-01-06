
# ---------------------------------------------------------------------------
#
#      Script: Read in Data
#
#      R Script to generate Priority Action Categories Based on Habitat Quality 
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

# ----------- update to be numeric ------------
class(habitat_raw_data[ c('Sand_occular_prcnt_INDICATOR_1'  , 'Gravel_occular_prcnt_INDICATOR_2'  ) ])<-"numeric"


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

