# ---------------------------------------------------------------------------
#
#      R Script to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   - - - - - - - - -  Packages, Script Criteria, and Directories of Scripts, Data, and Output  - - - - - - - - - 
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#  import R Packages
# ---------------------------------------------------------------------------
library(tidyverse)
#library(xlsx)
library(openxlsx)
#library(writexl)
library(readxl)

# ---------------------------------------------------------------------------
#  Script Criteria for output
# ---------------------------------------------------------------------------
read_MASTER_directly = TRUE # if TRUE - read MASTER from UCSRB servers, if FALSE - read from local 
write_MASTER_locally = TRUE # if TRUE -  write tabs in MASTER from UCSRB servers, if FALSE - do not write
basins_to_include = c("Methow",  "Entiat","Wenatchee" , "Okanogan")  # basins to include in simulation    
exclude_bull_trout = "no"  # if "yes" -> remove bull trout for WebMap applications
output_Habitat_Quality_and_Habitat_Attribute_Scores = "no"  # enter "yes" or "no" if you want the "flat table" Habitat Attribute output (doubles time to run script)
update_Okanogan_reach_names = "no"  # if "yes" - update Okanogan reach names (should not have to run again - since on 5.Apr.2021 Ryan updated names)
HQ_add_life_stage = "no"   # IF "yes" generate life stages for HQ pathway based on life stage presence in reaches, for combining into ONE Data frame across all pathways and scores
HQ_priority_life_stages = "yes"  # "yes" if use AU Life stages priority reach layer to generate life stages for habitat quality pathway
EDT_convert_Level3_Flow_to_Flow_Variability = "yes" # Level2_Level3_EDT_Crosswalk has "Flow Variability", Limiting_Factors_Okanogan_EDT and HabitatAttribute_Ratings_Level3
Okanogan_LF_Pathway_Level2_to_Level_3_yes_no = "no" # if yes, for Okanogan LF Pathway pull Level 3 then use crosswalk to get to Level 2, if "no" - just go straight to Level 2
core_metric_missing_data_species = c("Steelhead", "Spring Chinook") # species to use for core metrics in missing data (based on data layer Attribute_LifeStage_Crosswalk)
generate_reach_level_AU_scores = FALSE # True/False to generate AU scores with reach-level HQ scores
HQ_sensitivity_analysis_true_false = FALSE # IF you want to run the HQ sensitivity analysis
Cramer_Remote_Sensing_yes_no = FALSE # True/False whether to use Cramer Fish Sciences modeled data

# -----------------------------------------------------------------------------------------------------------------------------------------------
#   Directories of Input and Output data  
# -----------------------------------------------------------------------------------------------------------------------------------------------

time1 <- proc.time()[3] # for timing the total time to run the tool

# --------------- directory of scripts (where all the R scripts are located) -----------
script_path = 'Scripts/'

# ----------- directory of data -------------------
master_path = 'Data/'
habitat_data_path = paste(master_path,"Habitat_Data/", sep="")
ranking_data_path = paste(master_path,"Ranking_Data/", sep="")
crosswalks_path = paste(master_path,"Crosswalks/", sep="")  # various crosswalks 
criteria_and_scoring_path = paste(master_path,"Criteria_and_Scoring/", sep="") # Criteria and Scores for prioritization (Restoration and Protection)
Okanogan_EDT_path = paste(master_path,'Okanogan_EDT/', sep="")   # Data from Okanogan EDT results
reach_assessment_projects_path = paste(master_path,'Reach_Assessment_Projects/', sep="")  # data for projects from Reach Assessments

# -------------- MASTER path and file ----------
folder_x = "Y:/UCRTT/Prioritization/Step 2/Habitat Evaluation/"
#master_file = "MASTER_Step2_FINALDRFT_11052021.xlsx"
#master_file = "MASTER_Step2_FINALDRFT_09142022.xlsx"
master_file = "MASTER_Step2_FINALDRFT_2023_Updates.xlsx"
MASTER_Data_path = paste(folder_x,master_file, sep="")   # Data from Okanogan EDT results

# ----------- directory for output (where results are saved) ---------
output_path = 'Output/'

# Old location of the reach attribute (NOT Raw) data:  'Y:/UCRTT/Prioritization/Tables for Tools/'

# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   - - - - - - - - - Read in Data and Criteria   - - - - - - - - - 
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#     Read in Data
# ---------------------------------------------------------------------------
print("----------------------------------------- READ IN THE DATA --------------------------------------------")
source(paste(script_path, 'Read_in_data_Script.R', sep=""))

#print("----------------------------------------- Update Okanogan Reach Names (if necessary) --------------------------------------------")
#if(update_Okanogan_reach_names == "yes"){
#  source(paste(script_path, 'FUNCTION_update_names_in_data_frames.R', sep=""))
#}
  
# ---------------------------------------------------------------------------
#      Criteria for Filters   
# ---------------------------------------------------------------------------
print("----------------------------------------- ASSIGN CRITERIA --------------------------------------------")
source(paste(script_path, 'Criteria_Script.R', sep=""))

# ---------------------------------------------------------------------------
#   Prepare Okanogan EDT data
# ---------------------------------------------------------------------------

print("----------------------------------------- Prepare Okanogan EDT prep --------------------------------------------")
source(paste(script_path, 'Okanogan_EDT_data_input_prep.R', sep=""))


# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   - - - - - - - - - "Sub-Main" Script - to generate scores and organize output - - - - - - - - - 
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

source(paste(script_path, 'SubMain_Script_Generate_All_Reaches_and_Scores_and_Organize_Output.R', sep="")  )


# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   - - - - - - - - -  Misc Scripts  - - - - - - - - - 
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

# FUNCTION_print_reaches_in_Tier_1.R  - identify which reaches in a list are within or not

# 
# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   - - - - - - - - -  Export the Data  - - - - - - - - - 
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

# Note Habitat Data gaps/missing data output is done in Generate_Habitat_Quality_Scores_Missing_Data_Layer.R
# -----------------------------------------------------------------
#       Restoration
# -----------------------------------------------------------------
output_path_x =  paste(output_path,'Reach_Actions_Restoration_Unacceptable_and_AtRisk.xlsx', sep="")
write.xlsx(Restoration_Prioritization_Output,output_path_x )

# --------------------------
#     For WebMap
# --------------------------

# ------------- Output for WebMap ------------------
# ------- change "Habitat_Attribute" name to "Limiting_Factor"
colnames(Reach_Habitat_Attribute_Life_Stage_Restoration_Output)[colnames(Reach_Habitat_Attribute_Life_Stage_Restoration_Output) == "Habitat_Attribute"] <- "Limiting_Factor"
output_path_x =  paste(output_path,'Reach_Habitat_Attribute_Life_Stage_Restoration_Output.xlsx', sep="")
write.xlsx(Reach_Habitat_Attribute_Life_Stage_Restoration_Output,output_path_x )
# ------------- Output for WebMap ------------------
colnames(Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output)[colnames(Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output) == "Habitat_Attribute"] <- "Limiting_Factor"
output_path_x =  paste(output_path,'Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output.xlsx', sep="")
write.xlsx(Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output,output_path_x )

# ----------- Outward Facing Table (pops up when reach is clicked on AND in tab below) - RESTORATION -----------
#     ALSO for the excel 
output_path_x =  paste(output_path,'Restoration_Prioritization_Output_for_WebMap_Table.xlsx', sep="")
write.xlsx(Restoration_Prioritization_Output_for_WebMap,output_path_x )
# ----------- Outward Facing Table (pops up when reach is clicked on AND in tab below) - PROTECTION -----------
output_path_x =  paste(output_path,'Protection_Prioritization_Output_for_WebMap_Table.xlsx', sep="")
write.xlsx(Protection_Prioritization_Output_for_WebMap,output_path_x )

# ----------- Outward Facing Table - For individual species - RESTORATION --------
output_path_x =  paste(output_path,'Restoration_Prioritization_Output_SPRING_CHINOOK_for_WebMap_Table.xlsx', sep="")
write.xlsx(Restoration_Prioritization_Output_Spring_Chinook,output_path_x )
output_path_x =  paste(output_path,'Restoration_Prioritization_Output_STEELHEAD_for_WebMap_Table.xlsx', sep="")
write.xlsx(Restoration_Prioritization_Output_Steelhead,output_path_x )
output_path_x =  paste(output_path,'Restoration_Prioritization_Output_BULL_TROUT_for_WebMap_Table.xlsx', sep="")
write.xlsx(Restoration_Prioritization_Output_Bull_Trout,output_path_x )

# ----------- Outward Facing Table - For individual species - PROTECTION --------
output_path_x =  paste(output_path,'Protection_Prioritization_Output_SPRING_CHINOOK_for_WebMap_Table.xlsx', sep="")
write.xlsx(Protection_Prioritization_Output_Spring_Chinook,output_path_x )
output_path_x =  paste(output_path,'Protection_Prioritization_Output_STEELHEAD_for_WebMap_Table.xlsx', sep="")
write.xlsx(Protection_Prioritization_Output_Steelhead,output_path_x )
output_path_x =  paste(output_path,'Protection_Prioritization_Output_BULL_TROUT_for_WebMap_Table.xlsx', sep="")
write.xlsx(Protection_Prioritization_Output_Bull_Trout,output_path_x )


# ----------- Habitat Attributes Table w/ Ratings (to put in WebMap) -----------
output_path_x =  paste(output_path,'Habitat_Attributes_Ratings_Table_for_WebMap.xlsx', sep="")
write.xlsx(Habitat_Attributes_Ratings_Table,output_path_x )


# ------------ output Action Categories ----------------
output_path_x =  paste(output_path,'Action_Categories_and_Pathways_Restoration_Unacceptable.xlsx', sep="")
write.xlsx(Restoration_Unacceptable,output_path_x )
output_path_x =  paste(output_path,'Action_Categories_and_Pathways_Restoration_At_Risk.xlsx', sep="")
write.xlsx(Restoration_At_Risk,output_path_x )
output_path_x =  paste(output_path,'Action_Categories_and_Pathways_Restoration_Unacceptable_and_At_Risk.xlsx', sep="")
write.xlsx(Restoration_Unacceptable_and_At_Risk,output_path_x )

# ----------------- output Barriers data -----------------------
output_path_x = paste(output_path,'Barriers_Pathway_Output.xlsx', sep="")
write.xlsx(Barriers_Pathways_Data, output_path_x)

# ------------------ output Habitat Quality Scores for WebMap ----------------
output_path_x = paste(output_path,'Habitat_Quality_Scores_for_WebMap.xlsx', sep="")
write.xlsx(Habitat_Quality_Scores_for_WebMap, output_path_x)

# ------------------ output Basin Reach Information for WebMap ---------------- 
Reach_Information_data_for_WebMap = Reach_Information_data[,Reach_Information_data_columns_to_pull]
colnames(Reach_Information_data_for_WebMap) = Reach_Information_data_columns_new_names
output_path_x = paste(output_path,'Reach_Information_Data_for_WebMap.xlsx', sep="")
write.xlsx(Reach_Information_data_for_WebMap, output_path_x)


# -----------------------------------------------------------------
#     Projects Output
# -----------------------------------------------------------------

output_path_x =  paste(output_path,'Reach_Assessment_Project_Data_Habitat_Attributes.xlsx', sep="")
write.xlsx(Reach_Assessment_Project_Data_Habitat_Attributes,output_path_x )
output_path_x =  paste(output_path,'Reach_Assessment_Project_Data_per_Reach.xlsx', sep="")
write.xlsx(Reach_Assessment_Project_Data_per_Reach,output_path_x )
output_path_x =  paste(output_path,'Reach_Assessment_Project_Data_Habitat_Attributes_Priority_Reaches.xlsx', sep="")
write.xlsx(Reach_Assessment_Project_Data_Habitat_Attributes_Priority_Reaches,output_path_x )

# -----------------------------------------------------------------
#      Protection
# -----------------------------------------------------------------
output_path_x =  paste(output_path,'Reach_Actions_Protection.xlsx', sep="")
write.xlsx(Protection_Prioritization_Output,output_path_x )

# -----------------------------------------------------------------
#     Reach Rankings
# -----------------------------------------------------------------
output_path_x =  paste(output_path,'Restoration_Reach_Ranking_Scores_Output.xlsx', sep="")
write.xlsx(Reach_Rankings_Output_Restoration,output_path_x )
output_path_x =  paste(output_path,'Protection_Reach_Ranking_Scores_Output.xlsx', sep="")
write.xlsx(Reach_Rankings_Output_Protection,output_path_x )
output_path_x =  paste(output_path,'Output_Reach_Rank_ALL_species_and_reaches.xlsx', sep="")
write.xlsx(Output_ALL_species_and_reaches,output_path_x )



# -----------------------------------------------------------------
#      Combine into one MASTER excel
# -----------------------------------------------------------------

output_path_x =  paste(output_path,'Step2_Prioritization_Output.xlsx', sep="")


write.xlsx(data_output_x,   file = output_path_x,  sheetName = sheet_name ,  append = TRUE)


print(paste("Time to complete ENTIRE tool: ", paste(round((proc.time()[3] - time1)/60, 2), " minutes")    ))

