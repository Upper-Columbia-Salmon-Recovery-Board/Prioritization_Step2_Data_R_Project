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
library(xlsx)
library(writexl)
library(readxl)

time1 <- proc.time()[3] # for timing the total time to run the tool

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
#   Generate Habitat Quality and Habitat Attribute Scores Table
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#   Generate Habitat Quality Restoration and Protection Score 
# ---------------------------------------------------------------------------

source(paste(script_path, 'Habitat_Quality_Scores_Generate_Script.R', sep=""))

# ---------------------------------------------------------------------------
#   Generate Habitat Attribute Table (used in Limiting Factor Pathway)
# ---------------------------------------------------------------------------

source(paste(script_path, 'Habitat_Attribute_Scores_Generate_Script.R', sep=""))

# ONLY if you want to skip this step and read in data that was already generated
#Habitat_Attribute_Scores  = read_excel(  paste(output_path,'Habitat_Attribute_Scores.xlsx', sep="")  )
#cols.num = c("HabitatAttributeScore1" , "HabitatAttributeScore2","HabitatAttributeScore3" , "HabitatAttributeScore4", "Habitat_Attribute_Score")
#Habitat_Attribute_Scores [cols.num] <- sapply(Habitat_Attribute_Scores[cols.num],as.numeric)


# ---------------------------------------------------------------------------
#
#   Generate Priority Reaches and Habitat Attributes
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#   Apply Habitat Quality Pathway Filters
#     NOTE: the function below runs HQ Pathway for Restoration and Protectoin
# ---------------------------------------------------------------------------

source(paste(script_path, 'Habitat_Quality_Pathway_Filter.R', sep=""))

basins_to_include = c("Methow",  "Entiat","Wenatchee")

Habitat_Quality_Pathway_Spring_Chinook = Generate_Habitat_Quality_Output_Table("Spring Chinook", basins_to_include )
Habitat_Quality_Pathway_Steelhead = Generate_Habitat_Quality_Output_Table("Steelhead", basins_to_include )
Habitat_Quality_Pathway_Bull_Trout = Generate_Habitat_Quality_Output_Table("Bull Trout", basins_to_include )

#View(Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']])
#View(Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']])

# ---------------------------------------------------------------------------
#   Apply Limiting Factor Pathway Filters
#     NOTE: the function below runs HQ Pathway for Restoration and Protection
# ---------------------------------------------------------------------------

#source(paste(script_path, 'Limiting_Factors_Filter.R', sep=""))
source(paste(script_path, 'Limiting_Factor_Pathway_Filter.R', sep=""))

Limiting_Factor_Pathway_Spring_Chinook = Generate_Limiting_Factor_Output_Table("Spring Chinook", basins_to_include)
Limiting_Factor_Pathway_Steelhead = Generate_Limiting_Factor_Output_Table("Steelhead", basins_to_include)
Limiting_Factor_Pathway_Bull_Trout = Generate_Limiting_Factor_Output_Table("Bull Trout", basins_to_include)


#View(Limiting_Factor_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']])
#View(Limiting_Factor_Bull_Trout[['Limiting_Factor_Pathway_Protection']])
#unique(Limiting_Factor_Bull_Trout[['Limiting_Factor_Pathway_Restoration']]$unacceptable_and_at_risk_1_3_indiv_habitat_attributes)


# ---------------------------------------------------------------------------
#
#   Generate Action Categories
#
# ---------------------------------------------------------------------------

# NOTE: 1) fix action_categories_output so you can add it to any data frame, 
#       2) generate outputs for meeting


source(paste(script_path, 'FUNCTIONS_for_Generating_Action_Categories.R', sep=""))

# ----------------------------------------
#    Habitat Quality Pathway 
# ----------------------------------------
Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']])
Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Protection']]  =  FUNCTION_to_generate_Action_Categories(Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Protection']])
Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']])
Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Protection']]  =  FUNCTION_to_generate_Action_Categories(Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Protection']])
Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']])
Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Protection']]  =  FUNCTION_to_generate_Action_Categories(Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Protection']])


# ----------------------------------------
#    Limiting Factors Pathway 
# ----------------------------------------


Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']])
Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Protection']]  =  FUNCTION_to_generate_Action_Categories(Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Protection']])
Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']])
Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Protection']]  =  FUNCTION_to_generate_Action_Categories(Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Protection']])
Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Restoration']])
Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Protection']]  =  FUNCTION_to_generate_Action_Categories(Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Protection']])




# ---------------------------------------------------------------------------
#
#   Combine Action Categories Across Species and Pathways 
#
# ---------------------------------------------------------------------------

source(paste(script_path, 'FUNCTIONS_for_Combining_Action_Tables.R', sep=""))

# ----------------------- combine within the two pathways -----------------------
Habitat_Quality_Restoration_Unacceptable = FUNCTION_combine_Habitat_Quality_Action_Categories_PER_REACH(1, "restoration")
Habitat_Quality_Restoration_Unacceptable_and_At_Risk = FUNCTION_combine_Habitat_Quality_Action_Categories_PER_REACH(3, "restoration")
Habitat_Quality_Protection_Unacceptable = FUNCTION_combine_Habitat_Quality_Action_Categories_PER_REACH(1, "protection")
Habitat_Quality_Protection_Unacceptable_and_At_Risk = FUNCTION_combine_Habitat_Quality_Action_Categories_PER_REACH(3, "protection")

Limiting_Factor_Restoration_Unacceptable = FUNCTION_combine_Limiting_Factor_Action_Categories_PER_REACH(1, "restoration")
Limiting_Factor_Restoration_Unacceptable_and_At_Risk = FUNCTION_combine_Limiting_Factor_Action_Categories_PER_REACH(3, "restoration")
Limiting_Factor_Protection_Unacceptable = FUNCTION_combine_Limiting_Factor_Action_Categories_PER_REACH(1, "protection")
Limiting_Factor_Protection_Unacceptable_and_At_Risk = FUNCTION_combine_Limiting_Factor_Action_Categories_PER_REACH(3, "protection")



# ----------------------- combine across pathways -----------------------


Restoration_Unaccepable = FUNCTION_combine_across_pathways(Habitat_Quality_Restoration_Unacceptable, Limiting_Factor_Restoration_Unacceptable)
Restoration_Unaccepable_and_At_Risk = FUNCTION_combine_across_pathways(Habitat_Quality_Restoration_Unacceptable_and_At_Risk, Limiting_Factor_Restoration_Unacceptable_and_At_Risk)

# ------------------------- export data ----------------------
output_path_x =  paste(output_path,'Action_Categories_and_Pathways_Restoration_Unaccepable.xlsx', sep="")
write_xlsx(Restoration_Unaccepable,output_path_x )
output_path_x =  paste(output_path,'Action_Categories_and_Pathways_Restoration_Unaccepable_and_At_Risk.xlsx', sep="")
write_xlsx(Restoration_Unaccepable_and_At_Risk,output_path_x )


print(paste("Time to complete ENTIRE tool: ", paste(round((proc.time()[3] - time1)/60, 2), " minutes")    ))












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

