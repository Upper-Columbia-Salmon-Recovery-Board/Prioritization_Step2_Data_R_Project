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

output_Habitat_Quality_and_Habitat_Attribute_Scores = "no"  # enter "yes" or "no" if you want this output

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
print("----------------------------------------- READ IN THE DATA --------------------------------------------")
source(paste(script_path, 'Read_in_data_Script.R', sep=""))

# ---------------------------------------------------------------------------
#
#      Criteria for Filters   
#
# ---------------------------------------------------------------------------
print("----------------------------------------- ASSIGN CRITERIA --------------------------------------------")
source(paste(script_path, 'Criteria_Script.R', sep=""))

# ---------------------------------------------------------------------------
#
#   Generate Habitat Quality and Habitat Attribute Scores Table
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#   Generate Habitat Quality Restoration and Protection Score 
# ---------------------------------------------------------------------------
print("----------------------------------------- GENERATE HABITA QUALITY SCORES --------------------------------------------")

source(paste(script_path, 'Habitat_Quality_Scores_Generate_Script.R', sep=""))
# output is Habitat_Quality_Pathway_Output
# View(Habitat_Quality_Pathway_Output[['Habitat_Quality_Pathway_Protection']])

# ---------------------------------------------------------------------------
#   Generate Habitat Attribute Table (used in Limiting Factor Pathway)
# ---------------------------------------------------------------------------
print("----------------------------------------- GENERATE HABITAT ATTRIBUTE SCORES (for Limtiting Factor Pathway) --------------------------------------------")

source(paste(script_path, 'Habitat_Attribute_Scores_Generate_Script.R', sep=""))
# OUTPUT is Habitat_Attribute_Scores

# ---------------------------------------------------------------------------
#
#   Generate Priority Reaches and Habitat Attributes
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#   Apply Habitat Quality Pathway Filters
#     NOTE: the function below runs HQ Pathway for Restoration and Protection
# ---------------------------------------------------------------------------
print("----------------------------------------- APPLY HABITAT QUALITY FILTERS FOR PRIORITIZATION --------------------------------------------")

source(paste(script_path, 'Habitat_Quality_Pathway_Filter.R', sep=""))

basins_to_include = c("Methow",  "Entiat","Wenatchee")

# ----- set names of Habitat Quality Scores to sum ------
habitat_quality_scores_colnames_for_sum = c("Stability_Mean" , "CoarseSubstrate_score" ,"Cover-Wood_score", "Flow-SummerBaseFlow_score",
                                            "Off-Channel-Floodplain_score", "Off-Channel-Side-Channels_score","PoolQuantity&Quality_score",
                                            "Riparian_Mean","Temperature-Rearing_score")

Habitat_Quality_Pathway_Spring_Chinook = Generate_Habitat_Quality_Output_Table("Spring Chinook", basins_to_include, habitat_quality_scores_colnames_for_sum )
Habitat_Quality_Pathway_Steelhead = Generate_Habitat_Quality_Output_Table("Steelhead", basins_to_include, habitat_quality_scores_colnames_for_sum )
Habitat_Quality_Pathway_Bull_Trout = Generate_Habitat_Quality_Output_Table("Bull Trout", basins_to_include, habitat_quality_scores_colnames_for_sum )

#View(Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']])
#View(Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']])

# ---------------------------------------------------------------------------
#   Apply Limiting Factor Pathway Filters
#     NOTE: the function below runs LF Pathway for Restoration and Protection
# ---------------------------------------------------------------------------
# NOTE: Protection output includes habitat attributes but does not filter based on habitat attributes
print("----------------------------------------- APPLY LIMITING FACTOR FILTERS FOR PRIORITIZATION --------------------------------------------")

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
#         (do not do this for Protection since no Actions generated for Protection)
#
# ---------------------------------------------------------------------------

# NOTE: 1) fix action_categories_output so you can add it to any data frame, 
#       2) generate outputs for meeting
print("----------------------------------------- GENERATE ACTIONS CATEGORIES FOR HQ AND LF PATHWAY --------------------------------------------")

source(paste(script_path, 'FUNCTIONS_for_Generating_Action_Categories.R', sep=""))

# ------------------------------------------------------------------------------------
#                     RESTORATION
# ------------------------------------------------------------------------------------
# ----------------------------------------
#    Habitat Quality Pathway  
# ----------------------------------------
Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']])
Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']])
Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']])

# ----------------------------------------
#    Limiting Factors Pathway 
# ----------------------------------------
Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']])
Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']])
Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Restoration']])

# ------------------------------------------------------------------------------------
#                     PROTECTION
# -----------------------------------------------------------------------------------
# NOTE - DO NOT need to generate action categories since no specific actions for protection

# ----------------------------------------
#    Habitat Quality Pathway  
# ----------------------------------------

#Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Protection']]  =  FUNCTION_to_generate_Action_Categories(Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Protection']])
#Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Protection']]  =  FUNCTION_to_generate_Action_Categories(Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Protection']])
#Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Protection']]  =  FUNCTION_to_generate_Action_Categories(Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Protection']])
#
#Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Protection']]  =  FUNCTION_to_generate_Action_Categories(Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Protection']])
#Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Protection']]  =  FUNCTION_to_generate_Action_Categories(Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Protection']])
#Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Protection']]  =  FUNCTION_to_generate_Action_Categories(Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Protection']])

# ---------------------------------------------------------------------------
#
#  RESTORATION: Summarize Habitat Attributes and Action Categories for each Reach within each Species and Score (Unnacceptable, At Risk, etc.)
#
# ---------------------------------------------------------------------------

print("----------------------------------------- COMBINE HQ AND LF OUTPUT --------------------------------------------")

source(paste(script_path, 'FUNCTIONS_for_Combining_Action_Tables.R', sep=""))

# ----------------------- summarize within a single pathway AND score category (Unacceptable, At Risk -----------------------

Habitat_Quality_Restoration_Unacceptable = FUNCTION_combine_Habitat_Quality_Action_Categories_PER_REACH("one", "restoration")
Habitat_Quality_Restoration_At_Risk = FUNCTION_combine_Habitat_Quality_Action_Categories_PER_REACH("two and three", "restoration")
Habitat_Quality_Restoration_Unacceptable_and_At_Risk = FUNCTION_combine_Habitat_Quality_Action_Categories_PER_REACH("one thru three", "restoration")

Limiting_Factor_Restoration_Unacceptable = FUNCTION_combine_Limiting_Factor_Action_Categories_PER_REACH("one", "restoration")
Limiting_Factor_Restoration_At_Risk = FUNCTION_combine_Limiting_Factor_Action_Categories_PER_REACH("two and three", "restoration")
Limiting_Factor_Restoration_Unacceptable_and_At_Risk = FUNCTION_combine_Limiting_Factor_Action_Categories_PER_REACH("one thru three", "restoration")

# ---------------------------------------------------------------------------
#  Combine across pathways into Score categories (Unacceptable, At Risk, Both)
# ---------------------------------------------------------------------------

# ----------------------- combine across pathways -----------------------
columns_info = c( "ReachName","Basin","Assessment.Unit" )
columns_to_combine_text = c(  "Pathways" ,  "Impaired_Habitat_Attributes_All_Species" , "Impaired_Habitat_Attributes_SpringChinook", "Action_Categories_All_Species",   "Action_Categories_SpringChinook"    )
columns_to_combine_text_LF_only = c(   "Life_Stages", "Life_Stages_SpringChinook"   )
columns_to_combine_yes_no = c( "Spring_Chinook_Actions_Present_Yes_No","SprCh_STLD_BullTr_All_Present_Yes_No" )
columns_to_combine_count_unique = c( "Impaired_Habitat_Attributes_All_Species", "Impaired_Habitat_Attributes_SpringChinook", "Action_Categories_All_Species",   "Action_Categories_SpringChinook" ) # the unique occurences of these are then counted and a number is produced
columns_to_combine_numeric = c("Number_of_Pathways"  )
columns_to_combine_numeric_LF_only = c("Number_of_Life_Stages", "Number_Life_Stages_SpringChinook"  )

Restoration_Unacceptable = FUNCTION_combine_across_pathways(Habitat_Quality_Restoration_Unacceptable, Limiting_Factor_Restoration_Unacceptable)
Restoration_At_Risk = FUNCTION_combine_across_pathways(Habitat_Quality_Restoration_At_Risk, Limiting_Factor_Restoration_At_Risk)
Restoration_Unacceptable_and_At_Risk = FUNCTION_combine_across_pathways(Habitat_Quality_Restoration_Unacceptable_and_At_Risk, Limiting_Factor_Restoration_Unacceptable_and_At_Risk)

# ---------------------------------------------------------------------------
#  Combine into ONE Data frame across all pathways and scores
# ---------------------------------------------------------------------------
Restoration_Prioritization_Output = FUNCTION_combine_across_Unacceptable_and_AtRisk(Restoration_Unacceptable, Restoration_At_Risk, Restoration_Unacceptable_and_At_Risk)
# ---------------------------------------------------------------------------
#  Add Barrier Prioritization Info
# ---------------------------------------------------------------------------
Restoration_Prioritization_Output = FUNCTION_Add_Barrier_Data(Restoration_Prioritization_Output, Barriers_Pathways_Data)

# ---------------------------------------------------------------------------
#
#  PROTECTION: prep to output
#
# ---------------------------------------------------------------------------

print("----------------------------------------- OUTPUT THE RESULTS --------------------------------------------")


source(paste(script_path, 'FUNCTIONS_for_Protection_Output.R', sep=""))

Protection_Prioritization_Output = FUNCTION_Combine_Protection_Output(Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Protection']],
                                                                      Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Protection']],
                                                                      Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Protection']],
                                                                      Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Protection']],
                                                                      Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Protection']],
                                                                      Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Protection']] )

# ---------------------------------------------------------------------------
#
#  Export the Data
#
# ---------------------------------------------------------------------------

# -----------------------------------------------------------------
#       Restoration
# -----------------------------------------------------------------
output_path_x =  paste(output_path,'Reach_Actions_Restoration_Unacceptable_and_AtRisk.xlsx', sep="")
write_xlsx(Restoration_Prioritization_Output,output_path_x )


output_path_x =  paste(output_path,'Action_Categories_and_Pathways_Restoration_Unacceptable.xlsx', sep="")
write_xlsx(Restoration_Unacceptable,output_path_x )
output_path_x =  paste(output_path,'Action_Categories_and_Pathways_Restoration_At_Risk.xlsx', sep="")
write_xlsx(Restoration_At_Risk,output_path_x )
output_path_x =  paste(output_path,'Action_Categories_and_Pathways_Restoration_Unacceptable_and_At_Risk.xlsx', sep="")
write_xlsx(Restoration_Unacceptable_and_At_Risk,output_path_x )

# -----------------------------------------------------------------
#      Protection
# -----------------------------------------------------------------
output_path_x =  paste(output_path,'Reach_Actions_Protection.xlsx', sep="")
write_xlsx(Protection_Prioritization_Output,output_path_x )


print(paste("Time to complete ENTIRE tool: ", paste(round((proc.time()[3] - time1)/60, 2), " minutes")    ))

