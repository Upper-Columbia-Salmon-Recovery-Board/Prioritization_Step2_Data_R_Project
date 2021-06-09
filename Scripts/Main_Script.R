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
library(xlsx)
library(writexl)
library(readxl)

# ---------------------------------------------------------------------------
#  Script Criteria for output
# ---------------------------------------------------------------------------
basins_to_include = c("Methow",  "Entiat","Wenatchee" , "Okanogan")  # basins to include in simulation    
exclude_bull_trout = "yes"  # if "yes" -> remove bull trout for WebMap applications
output_Habitat_Quality_and_Habitat_Attribute_Scores = "no"  # enter "yes" or "no" if you want the "flat table" Habitat Attribute output (doubles time to run script)
update_Okanogan_reach_names = "no"  # if "yes" - update Okanogan reach names (should not have to run again - since on 5.Apr.2021 Ryan updated names)
HQ_add_life_stage = "no"   # IF "yes" generate life stages for HQ pathway based on life stage presence in reaches, for combining into ONE Data frame across all pathways and scores
HQ_priority_life_stages = "yes"  # "yes" if use AU Life stages priority reach layer to generate life stages for habitat quality pathway
EDT_convert_Level3_Flow_to_Flow_Variability = "yes" # Level2_Level3_EDT_Crosswalk has "Flow Variability", Limiting_Factors_Okanogan_EDT and HabitatAttribute_Ratings_Level3

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

print("----------------------------------------- Update Okanogan Reach Names (if necessary) --------------------------------------------")
if(update_Okanogan_reach_names == "yes"){
  source(paste(script_path, 'FUNCTION_update_names_in_data_frames.R', sep=""))
}
  
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
#   - - - - - - - - -  Generate Habitat Attribute and Habitat Quality Scores Table  - - - - - - - - - 
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

# NOTE: running Limiting Factor pathway first since we decided to generate all habitat attribute scores with the Habitat Attribute Scores
#        script, then for the HQ Pathway, pull from those scores

# ---------------------------------------------------------------------------
#   LIMITING FACTOR PATHWAY: Generate Habitat Attribute Table (used in Limiting Factor Pathway)
# ---------------------------------------------------------------------------
print("----------------------------------------- GENERATE HABITAT ATTRIBUTE SCORES (for Limtiting Factor Pathway) --------------------------------------------")
# some of HQ filters are used in Habitat Attribute generation
source( paste(script_path, 'FUNCTIONS_for_Habitat_Quality_Filters.R', sep="")  )

# script to generate habitat attribute scores
source(paste(script_path, 'Habitat_Attribute_Scores_Generate_Script.R', sep="") )
# OUTPUT is Habitat_Attribute_Scores


# ---------------------------------------------------------------------------
#   HABITAT QUALITY PATHWAY: Generate Habitat Quality Restoration and Protection Score 
# ---------------------------------------------------------------------------

print("----------------------------------------- GENERATE HABITAT QUALITY SCORES --------------------------------------------")
source( paste(script_path, 'Habitat_Quality_Scores_Generate_Script.R', sep="") )
# output for all scores is: Habitat_Quality_Scores
# View(Habitat_Quality_Pathway_Output[['Habitat_Quality_Pathway_Restoration']])
# View(Habitat_Quality_Pathway_Output[['Habitat_Quality_Pathway_Protection']])

# ---------------------------------------------------------------------------
#   OKANOGAN: Habitat Quality AND Limiting Factor Pathway
# ---------------------------------------------------------------------------

print("----------------------------------------- GENERATE OKANOGAN HABITAT QUALITY AND LIMTING FACTOR SCORES --------------------------------------------")
# Okanogan: generate Habitat Attribute Scores for Habitat Quality (just Level 2) and Limiting Factor (Level 3 filter 1s and 3s to generate Level 2s)
# NOTE: Habitat_Quality_Scorse_Okanogan and Habiat_Attribute_Scores_Okanogan are generated by this script
source( paste(script_path, 'FUNCTIONS_Okanogan_EDT_Habitat_Attribute_Habitat_Quality_Scripts.R', sep="") )
# NOTE: these output should include all reaches in the Okanogan - no filters
# HQ Pathway: Habitat_Quality_Scores_Okanogan
# LF Pathway: Habitat_Attribute_Scores_Okanogan
# Individual Life stages for the Okanogan: Adult_Migration_LF_Okanogan, Fry_LF_Okanogan, Holding_and_Maturation_LF_Okanogan,
#                   Smolt_Outmigration_LF_Okanogan, Spawning_and_Incubation_LF_Okanogan, Summer_Rearing_LF_Okanogan, Winter_Rearing_LF_Okanogan

# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   - - - - - - - - -  Generate Priority Reaches and Habitat Attributes (HQ and LF Pathway) - - - - - - - - - 
#
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#   HABITAT QUALITY PATHWAY: Apply Habitat Quality Pathway Filters
# ---------------------------------------------------------------------------
# NOTE: the function below runs HQ Pathway for Restoration and Protection
print("----------------------------------------- APPLY HABITAT QUALITY FILTERS FOR PRIORITIZATION --------------------------------------------")
# ------- Habitat Quality Pathway Filter for the Methow-Entiat-Wenatchee ---------------
source(paste(script_path, 'Habitat_Quality_Pathway_Filter.R', sep=""))  # for Methow-Wenatchee-Entiat AND Okanogan functions
# ------- Habitat Quality Pathway Filter for the Okanogan ---------------
source(paste(script_path, 'Habitat_Quality_Pathway_Filter_OKANOGAN.R', sep=""))  # for Methow-Wenatchee-Entiat AND Okanogan functions


# ----- set names of Habitat Quality Scores to sum ------
habitat_quality_scores_colnames_for_sum = c("Stability_Mean" , "CoarseSubstrate_score" ,"Cover-Wood_score", "Flow-SummerBaseFlow_score",
                                            "Off-Channel-Floodplain_score", "Off-Channel-Side-Channels_score","PoolQuantity&Quality_score",
                                            "Riparian_Mean","Temperature-Rearing_score")

# --------------- generate for all basins except Okanogan ---------------
Habitat_Quality_Pathway_Spring_Chinook = Generate_Habitat_Quality_Output_Table("Spring Chinook", basins_to_include, habitat_quality_scores_colnames_for_sum )
Habitat_Quality_Pathway_Steelhead = Generate_Habitat_Quality_Output_Table("Steelhead", basins_to_include, habitat_quality_scores_colnames_for_sum )
Habitat_Quality_Pathway_Bull_Trout = Generate_Habitat_Quality_Output_Table("Bull Trout", basins_to_include, habitat_quality_scores_colnames_for_sum )

# --------------- generate for Okanogan ---------------
colnames_HQ_output = colnames(Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']])
colnames_HQ_habitat_attributes = colnames_HQ_output[7:19]
# NOTE: just need to apply filters to Habitat_Quality_Scores_Okanogan - pull those directly
Habitat_Quality_Pathway_Steelhead_OKANOGAN = Generate_Habitat_Quality_Output_Table_Okanogan("Steelhead" , colnames_HQ_output, colnames_HQ_habitat_attributes)
# View(Habitat_Quality_Pathway_Steelhead_OKANOGAN[['Habitat_Quality_Pathway_Restoration']])
# View(Habitat_Quality_Pathway_Steelhead_OKANOGAN[['Habitat_Quality_Pathway_Protection']])

# ---------------- Restoration: add Okanogan to Methow-Wenatchee-Okanogan HQ Output ------------
habitat_quality_scores_colnames_for_combo = colnames(Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']])[7:(ncol(Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']])-7)]
Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']] = Combine_MetEntWen_and_Okanogan_Habitat_Quality_Output(Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']],
                                      Habitat_Quality_Pathway_Steelhead_OKANOGAN [['Habitat_Quality_Pathway_Restoration']],  
                                      habitat_quality_scores_colnames_for_combo)
# ---------------- Protection: add Okanogan to Methow-Wenatchee-Okanogan HQ Output ------------
habitat_quality_scores_colnames_for_combo = colnames(Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Protection']])[7:(ncol(Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Protection']])-4)]
Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Protection']] = Combine_MetEntWen_and_Okanogan_Habitat_Quality_Output_Protection(Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Protection']],
                                                                                                                                   Habitat_Quality_Pathway_Steelhead_OKANOGAN[['Habitat_Quality_Pathway_Protection']],  
                                                                                                                                   habitat_quality_scores_colnames_for_combo)

# ------------------- Compare EDT and RTT Output ----------------------
source(paste(script_path, 'Compare_EDT_and_RTT_output_data.R', sep=""))  # for Methow-Wenatchee-Entiat AND Okanogan functions

# Use these to View the various outputs
# View(Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']])
# View(Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Protection']])
# View(Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']])
# View(Habitat_Quality_Pathway_Steelhead_OKANOGAN[['Habitat_Quality_Pathway_Restoration']])


# ---------------  Combine Habitat_Quality_Scores with all filters (for all basins) ------
source(paste(script_path, 'Generate_Habitat_Quality_Scores_ALL_FILTERS.R', sep=""))  
habitat_quality_scores_colnames_output = c("BankStability_score" ,"ChannelStability_score", "Stability_Mean" , "CoarseSubstrate_score" ,"Cover-Wood_score", "Flow-SummerBaseFlow_score",
                                            "Off-Channel-Floodplain_score", "Off-Channel-Side-Channels_score","PoolQuantity&Quality_score",
                                           "Riparian-Disturbance_score", "Riparian-CanopyCover_score", "Riparian_Mean","Temperature-Rearing_score")
Habitat_Quality_Scores_ALL_Spring_Chinook = Generate_Habitat_Quality_Output_Table_WITH_FILTERS("Spring Chinook",basins_to_include, habitat_quality_scores_colnames_output)
Habitat_Quality_Scores_ALL_Steelhead = Generate_Habitat_Quality_Output_Table_WITH_FILTERS("Steelhead",basins_to_include, habitat_quality_scores_colnames_output)
if(exclude_bull_trout == "no"){
  Habitat_Quality_Scores_ALL_Bull_Trout = Generate_Habitat_Quality_Output_Table_WITH_FILTERS("Spring Chinook",basins_to_include, habitat_quality_scores_colnames_output)
}

# ------------ combine individual species into one -----------
write_to_xls_x = TRUE
Habitat_Quality_Scores_ALL_SPECIES = FUNCTION_combine_HQ_ALL_Filters_no_Bull_Trout(Habitat_Quality_Scores_ALL_Spring_Chinook,  Habitat_Quality_Scores_ALL_Steelhead ,write_to_xls_x )

# ---------------------------------------------------------------------------
#   LIMITING FACTORS PATHWAY: Apply Limiting Factor Pathway Filters (Restoration and Protection)
# ---------------------------------------------------------------------------
# NOTE: Protection output includes habitat attributes but does not filter based on habitat attributes
print("----------------------------------------- APPLY LIMITING FACTOR FILTERS FOR PRIORITIZATION --------------------------------------------")
# ------- Limiting Factor Pathway Filter for the Methow-Entiat-Wenatchee ---------------
source(paste(script_path, 'Limiting_Factor_Pathway_Filter.R', sep=""))
# ------- Limiting Factor Pathway Filter for the Okanogan ---------------
source( paste(script_path, 'Limiting_Factor_Pathway_Filter_OKANOGAN.R', sep="") )

# ---------------- Generate Limiting Factor Output for Each Species ----------------
Limiting_Factor_Pathway_Spring_Chinook = Generate_Limiting_Factor_Output_Table("Spring Chinook", basins_to_include)
Limiting_Factor_Pathway_Steelhead = Generate_Limiting_Factor_Output_Table("Steelhead", basins_to_include)
if(exclude_bull_trout == "no"){ Limiting_Factor_Pathway_Bull_Trout = Generate_Limiting_Factor_Output_Table("Bull Trout", basins_to_include) }
# --------------- generate for Okanogan ---------------
Limiting_Factor_Pathway_Steelhead_OKANOGAN = Generate_Limiting_Factor_Output_Table_Okanogan("Steelhead", "Okanogan" )
Limiting_Factor_Pathway_Steelhead_OKANOGAN_no_level3 = Generate_Limiting_Factor_Output_Table_Okanogan_no_level3("Steelhead", "Okanogan")

# ------------------- combine -------------------
Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']] = rbind( Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']],
                                                                                   Limiting_Factor_Pathway_Steelhead_OKANOGAN[['Limiting_Factor_Pathway_Restoration']] )
Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Protection']] = rbind( Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Protection']],
                                                                                    Limiting_Factor_Pathway_Steelhead_OKANOGAN[['Limiting_Factor_Pathway_Protection']] )

# --------------------------------------------------
# Generate life stage tables for ALL reaches for each species (JUST for QAQC purposes)
# --------------------------------------------------
test_x = FALSE
if(test_x){
  Limiting_Factor_Scores_all_life_stages_reaches_Spring_Chinook = Generate_Species_Output_Table_for_ALL_REACHES_and_ALL_LIFE_STAGES("Spring Chinook")
  Limiting_Factor_Scores_all_life_stages_reaches_Steelhead = Generate_Species_Output_Table_for_ALL_REACHES_and_ALL_LIFE_STAGES("Steelhead")
  if(exclude_bull_trout == "no"){  Limiting_Factor_Scores_all_life_stages_reaches_Bull_Trout = Generate_Species_Output_Table_for_ALL_REACHES_and_ALL_LIFE_STAGES("Bull Trout") }
  
}

# -- for viewing data -----
# View(Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']])
# View(Limiting_Factor_Spring_Chinook[['Limiting_Factor_Pathway_Protection']])
#View(Limiting_Factor_Spring_Chinook[['Limiting_Factor_Pathway_Protection']][c('ReachName','LF_Sum','LF_Pct','LF_Score_Protection')])
#unique(Limiting_Factor_Bull_Trout[['Limiting_Factor_Pathway_Restoration']]$unacceptable_and_at_risk_1_3_indiv_habitat_attributes)

# View(Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Protection']][which(Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Protection']]$Basin == "Okanogan"),])
# View(Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Protection']][which(Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Protection']]$Basin == "Okanogan"),])


# View(Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']][,colnames(Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']])[c(1,2,21,22,24,25,26)] ])
# View(Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Restoration']])

# View(Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Protection']])
# View(Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Protection']])


# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   - - - - - - - - -  Generate Action Categories  - - - - - - - - - 
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------
# NOTE: do not do this for Protection since no Actions generated for Protection

# NOTE: 1) fix action_categories_output so you can add it to any data frame
print("----------------------------------------- GENERATE ACTIONS CATEGORIES FOR HQ AND LF PATHWAY --------------------------------------------")

source(paste(script_path, 'FUNCTIONS_for_Generating_Action_Categories.R', sep=""))

# ------------------------------------------------------------------------------------
#              Generate Action Categories
# ------------------------------------------------------------------------------------
# -----------------  Habitat Quality Pathway  -----------------------
Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']])
Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']])
if(exclude_bull_trout == "no"){
  Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']])
}
# View(Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']][which(Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']]$Basin == "Okanogan"),])
# --------------------  Limiting Factors Pathway  --------------------
Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']])
Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']])
if(exclude_bull_trout == "no"){
  Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Restoration']]  =  FUNCTION_to_generate_Action_Categories(Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Restoration']])
}
# View(Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']][which(Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']]$Basin == "Okanogan"),])

# ---------------------------------------------------------------------------
#     Summarize Habitat Attributes and Action Categories for each Reach within each Species and Score (Unacceptable, At Risk, etc.)
# ---------------------------------------------------------------------------
# NOTE: just for Restoration 
print("----------------------------------------- COMBINE HQ AND LF OUTPUT --------------------------------------------")

# -------------- Generate Functions ---------------
source(paste(script_path, 'FUNCTIONS_for_Combining_Action_Tables.R', sep=""))
source(paste(script_path, 'FUNCTIONS_for_Combining_Reach_Habitat_Attribute_combos.R', sep=""))

# ---------------- Run Functions to summarize within a single pathway AND score category (Unacceptable, At Risk) -----------------------
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
columns_to_combine_text = c(  "Pathways" ,  "Impaired_Habitat_Attributes_All_Species" , "Impaired_Habitat_Attributes_SpringChinook", "Impaired_Habitat_Attributes_Steelhead","Impaired_Habitat_Attributes_BullTrout",
                              "Action_Categories_All_Species",   "Action_Categories_SpringChinook",  "Action_Categories_Steelhead",  "Action_Categories_BullTrout"    )
columns_to_combine_text_LF_only = c(   "Life_Stages", "Life_Stages_SpringChinook"   )
columns_to_combine_yes_no = c( "Spring_Chinook_Actions_Present_Yes_No","SprCh_STLD_BullTr_All_Present_Yes_No" )
columns_to_combine_count_unique = c( "Impaired_Habitat_Attributes_All_Species", "Impaired_Habitat_Attributes_SpringChinook", "Impaired_Habitat_Attributes_Steelhead", "Impaired_Habitat_Attributes_BullTrout",
                                     "Action_Categories_All_Species",   "Action_Categories_SpringChinook","Action_Categories_Steelhead",  "Action_Categories_BullTrout" ) # the unique occurences of these are then counted and a number is produced
columns_to_combine_numeric = c("Number_of_Pathways"  )
columns_to_combine_numeric_LF_only = c("Number_of_Life_Stages", "Number_Life_Stages_SpringChinook"  )

Restoration_Unacceptable = FUNCTION_combine_across_pathways(Habitat_Quality_Restoration_Unacceptable, Limiting_Factor_Restoration_Unacceptable)
Restoration_At_Risk = FUNCTION_combine_across_pathways(Habitat_Quality_Restoration_At_Risk, Limiting_Factor_Restoration_At_Risk)
Restoration_Unacceptable_and_At_Risk = FUNCTION_combine_across_pathways(Habitat_Quality_Restoration_Unacceptable_and_At_Risk, Limiting_Factor_Restoration_Unacceptable_and_At_Risk)

# ---------------------------------------------------------------------------
#  Combine into ONE Data frame across all pathways and scores
# ---------------------------------------------------------------------------
columns_info = c( "ReachName" ) # columns to automatically add to beginning (left side) of output
# Note - only include the Habitat_Quality_Restoration_Unacceptable_and_At_Risk 
Restoration_Prioritization_Output = FUNCTION_combine_across_Unacceptable_and_AtRisk(Restoration_Unacceptable, Restoration_At_Risk, Restoration_Unacceptable_and_At_Risk, Habitat_Quality_Restoration_Unacceptable_and_At_Risk, columns_info, exclude_bull_trout, HQ_add_life_stage)

# ---------------------------------------------------------------------------
#  Add Barrier Prioritization Info
# ---------------------------------------------------------------------------
columns_info = c( "ReachName","Basin","Assessment.Unit" ) # columns to automatically add to beginning (left side) of output
Restoration_Prioritization_Output = FUNCTION_Add_Barrier_Data(Restoration_Prioritization_Output, Barriers_Pathways_Data, exclude_bull_trout)

# ---------------------------------------------------------------------------
#  Reduce for "Outward Facing" table in WebMap
# ---------------------------------------------------------------------------
colnames_outward_facing_WebMap = c("ReachName","Assessment.Unit","Species","Actions", "Life_Stages","Impaired_Habitat_Attributes_All_Species","Action_Categories_All_Species" )
colnames_reach_info = c("RM_Start", "RM_End")  # data that is in the reach geospatial layer to add to these data
colnames_outward_facing_WebMap_ORDER = c("ReachName","Assessment.Unit","Actions","Species", "Life_Stages","Impaired_Habitat_Attributes_All_Species","Action_Categories_All_Species" )
colnames_outward_facing_WebMap_UPDATED = c("Reach Name","Assessment Unit","Priority Actions","Priority Species","Priority Life Stages","Limiting Factors","Action Categories" )
# ------- filter out for specific columns ----------
Restoration_Prioritization_Output_for_WebMap = Restoration_Prioritization_Output[,colnames_outward_facing_WebMap]
# ----------- add Reach information ------------
# NOTE: this was to add river miles - BUT updated Okanogan reach names are not in the GIS layer with river miles
#Restoration_Prioritization_Output_for_WebMap  =  FUNCTION_add_reach_information(Restoration_Prioritization_Output_for_WebMap,  colnames_reach_info)
# ------------ do MISC processing for output ---------
Restoration_Prioritization_Output_for_WebMap = FUNCTION_prepare_outward_facing_table( Restoration_Prioritization_Output_for_WebMap , colnames_outward_facing_WebMap_ORDER, colnames_outward_facing_WebMap_UPDATED, exclude_bull_trout)


# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   - - - - - - - - -  PROTECTION: Prep Output  - - - - - - - - - 
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

print("----------------------------------------- OUTPUT THE RESULTS --------------------------------------------")

# -------------- Generate Function ---------------
source(paste(script_path, 'FUNCTIONS_for_Protection_Output.R', sep=""))

# -------------- Run Function to generate Protection output -----------
Protection_Prioritization_Output = FUNCTION_Combine_Protection_Output(Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Protection']],
                                                                      Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Protection']],
                                                                      Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Protection']],
                                                                      Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Protection']],
                                                                      Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Protection']],
                                                                      Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Protection']],
                                                                      exclude_bull_trout)

# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   - - - - - - - - - Generate Reach Ranking Score  - - - - - - - - - 
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

source(paste(script_path, "Reach_Rankings_Restoration_and_Protection.R", sep=""))
# ------  run the function to generate restoration rankings -------
Reach_Rankings_Output = Generate_Restoration_or_Protection_Reach_Rankings_Table(basins_to_include )
# ----------------- separate into Restoration and Protection ----------------
Reach_Rankings_Output_Restoration = Reach_Rankings_Output[['Reach_Rankings_Restoration']]
Reach_Rankings_Output_Protection = Reach_Rankings_Output[['Reach_Ranking_Protection']]

# View(Reach_Rankings_Output_Restoration[which(Reach_Rankings_Output_Restoration$Basin == "Okanogan"),colnames(Reach_Rankings_Output_Restoration)[c(1,2,4,5,6,11,12:13,15)]])
# View(Reach_Rankings_Output_Restoration[which(Reach_Rankings_Output_Restoration$Species == "Okanogan"),colnames(Reach_Rankings_Output_Restoration)[c(1,2,4,5,6,11,12:13,15)]])


# print what reaches overlap between restoration and protection 
intersect(Reach_Rankings_Output_Restoration$ReachName, Reach_Rankings_Output_Protection$ReachName)

setdiff(Reach_Rankings_Output_Restoration$ReachName, Restoration_Prioritization_Output$ReachName)

# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   - - - - - - - - -  RESTORATION - flat tables for WebMaps  - - - - - - - - - 
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#  Reach-Habitat Attributes-Life Stage per row
# ---------------------------------------------------------------------------
columns_info = c( "ReachName","Basin","Assessment.Unit" ) # columns to automatically add to beginning (left side) of output
Reach_Habitat_Attribute_Life_Stage_Restoration_Output = FUNCTION_combine_by_Reach_AND_Habitat_Attribute_Life_Stage(  Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']], 
                                                Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']], 
                                                Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']], 
                                                Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']],
                                                Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']], 
                                                Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Restoration']], columns_info, exclude_bull_trout)


# ---------------------------------------------------------------------------
#  Reach-Habitat Attributes-Life Stage-Species per row
# ---------------------------------------------------------------------------
Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output = FUNCTION_combine_by_Reach_AND_Habitat_Attribute_Life_Stage_Species( Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']], 
                                                                   Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']], 
                                                                   Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']], 
                                                                   Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']],
                                                                   Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']], 
                                                                   Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Restoration']], columns_info, exclude_bull_trout, HQ_priority_life_stages)


# ------------------ just to test/compare output --------------
# note these should be the same - except some of the HQ output is different (Stability, Riparian)
reach_test = "Chiwawa River Lower 01"
hab_ats_1 = unlist( strsplit(Restoration_Prioritization_Output_for_WebMap[which(Restoration_Prioritization_Output_for_WebMap$`Reach Name` == reach_test),]$`Limiting Factor`, ",") )
hab_ats_2 = unique(Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output[which(Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output$ReachName == reach_test),]$Habitat_Attribute)
setdiff(hab_ats_1, hab_ats_2)
setdiff(hab_ats_2, hab_ats_1)

unlist( strsplit(Restoration_Prioritization_Output_for_WebMap$`Priority Species`[which(Restoration_Prioritization_Output_for_WebMap$`Reach Name` == reach_test)], ",") )
unique(Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output[which(Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output$ReachName == reach_test),]$Species)

unlist(strsplit(Restoration_Prioritization_Output_for_WebMap[which(Restoration_Prioritization_Output_for_WebMap$`Reach Name` == reach_test),]$`Priority Life Stages`, ","))[order(unlist(strsplit(Restoration_Prioritization_Output_for_WebMap[which(Restoration_Prioritization_Output_for_WebMap$`Reach Name` == reach_test),]$`Priority Life Stages`, ",")))]
unique(Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output[which(Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output$ReachName == reach_test),]$Life_Stage)[order(unique(Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output[which(Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output$ReachName == reach_test),]$Life_Stage))]



# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   - - - - - - - - -  Generate Output of all reaches with habitat data (to include in Webmap)  - - - - - - - - - 
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

Habitat_Attribute_Scores_columns_to_pull  = c("% Fines/Embeddedness", "Brook Trout", "Coarse Substrate" ,"Contaminants",
                                           "Cover- Boulders", "Cover- Undercut Banks", "Cover- Wood", "Entrainment/Stranding", "Flow- Scour",
                                           "Flow- Summer Base Flow", "Food- Food Web Resources", "Harassment", "Icing", "Off-Channel- Floodplain",
                                           "Off-Channel- Side-Channels","Pool Quantity & Quality" , "Pools- Deep Pools", "Predators- Adults", "Predators- Juveniles",
                                           "Superimposition", "Temperature- Adult Holding", "Temperature- Adult Spawning", "Temperature- Rearing")
Habitat_Quality_Scores_columns_to_pull = c("BankStability_score","ChannelStability_score","Stability_Mean","CoarseSubstrate_score","Cover-Wood_score","Flow-SummerBaseFlow_score",
                                             "Off-Channel-Floodplain_score","Off-Channel-Side-Channels_score","PoolQuantity&Quality_score","Riparian-Disturbance_score",
                                             "Riparian-CanopyCover_score","Riparian_Mean","Temperature-Rearing_score")
# ORDER: REI ratings -> core metrics -> then the rest of them
Order_of_Habitat_Attribute_Rating_Table_Columns = c("Coarse Substrate","% Fines/Embeddedness", "Cover- Wood","Pool Quantity & Quality", # <- REI Values
                                                    "Off-Channel- Floodplain", "Off-Channel- Side-Channels", "Cover- Undercut Banks", #  <- REI Values
                                                    "ChannelStability_score", "Stability_Mean", "Riparian-Disturbance_score",         # <- HQ scores based on REI Values
                                                    "Riparian-CanopyCover_score", "Riparian_Mean",                                   # <- HQ scores based on REI Values
                                                    "Contaminants","Entrainment/Stranding","Predators- Juveniles",                  # <- Spr Chn and STLDH core metrics
                                                    "Cover- Boulders", "Flow- Scour", "Flow- Summer Base Flow","Food- Food Web Resources",  # <- one species core metric
                                                    "Harassment", "Icing", "Superimposition", "Temperature- Adult Holding",               # <- one species core metric
                                                    "Temperature- Adult Spawning", "Temperature- Rearing",                              # <- one species core metric
                                                    "Brook Trout", "Pools- Deep Pools", "Predators- Adults")                           # <- not a core metric

# just runs script - output is Habitat_Attributes_Ratings_Table
source(paste(script_path, "FUNCTIONS_for_Habitat_Attribute_Rating_Table_for_WebMap.R", sep=""))




# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   - - - - - - - - - PROJECTS: generate project layer - both for all projects and for project benefiting priority reaches  - - - - - - - - - 
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------

source(paste(script_path, "FUNCTIONS_for_Reach_Assessment_Projects_Processing.R", sep=""))

# ---------------------------------------------------------------------------
#       Generate List of Projects, Action Categories, and Habitat Attributes for All Reaches
# ---------------------------------------------------------------------------

column_order = c("ReachName", "Reach Assessment", "ProjectName", "Reach_ID_in_assessment","Action_Type","Action_Category", "Habitat_Attribute", "Action_Description")
Reach_Assessment_Project_Data_Habitat_Attributes = FUNCTION_add_habitat_attributes_to_Projects(Crosswalk_Habitat_Attributes_and_Actions, Reach_Assessment_Project_Data, column_order)

# ---------------------------------------------------------------------------
#       Generate List of Projects, Action Categories, and Habitat Attributes for EACH Reach
# ---------------------------------------------------------------------------

Reach_Assessment_Project_Data_per_Reach = FUNCTION_projects_one_row_per_reach(Reach_Assessment_Project_Data_Habitat_Attributes)

# ---------------------------------------------------------------------------
#   Generate List of Projects, Action Categories, and Habitat Attributes for Prioritized Reaches
# ---------------------------------------------------------------------------
Reach_Assessment_Project_Data_Habitat_Attributes_Priority_Reaches = FUNCTION_output_actions_for_priority_reaches(Reach_Assessment_Project_Data_Habitat_Attributes, Restoration_Prioritization_Output_for_WebMap )


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

# -----------------------------------------------------------------
#       Restoration
# -----------------------------------------------------------------
output_path_x =  paste(output_path,'Reach_Actions_Restoration_Unacceptable_and_AtRisk.xlsx', sep="")
write_xlsx(Restoration_Prioritization_Output,output_path_x )

# --------------------------
#     For WebMap
# --------------------------

# ------------- Output for WebMap ------------------
# ------- change "Habitat_Attribute" name to "Limiting_Factor"
colnames(Reach_Habitat_Attribute_Life_Stage_Restoration_Output)[colnames(Reach_Habitat_Attribute_Life_Stage_Restoration_Output) == "Habitat_Attribute"] <- "Limiting_Factor"
output_path_x =  paste(output_path,'Reach_Habitat_Attribute_Life_Stage_Restoration_Output.xlsx', sep="")
write_xlsx(Reach_Habitat_Attribute_Life_Stage_Restoration_Output,output_path_x )
# ------------- Output for WebMap ------------------
colnames(Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output)[colnames(Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output) == "Habitat_Attribute"] <- "Limiting_Factor"
output_path_x =  paste(output_path,'Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output.xlsx', sep="")
write_xlsx(Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output,output_path_x )
# ----------- Outward Facing Table (pops up when reach is clicked on) -----------
output_path_x =  paste(output_path,'Restoration_Prioritization_Output_for_WebMap_Table.xlsx', sep="")
write_xlsx(Restoration_Prioritization_Output_for_WebMap,output_path_x )

# ----------- Habitat Attributes Table w/ Ratings (to put in WebMap) -----------
output_path_x =  paste(output_path,'Habitat_Attributes_Ratings_Table.xlsx', sep="")
write_xlsx(Habitat_Attributes_Ratings_Table,output_path_x )


# ------------ output Action Categories ----------------
output_path_x =  paste(output_path,'Action_Categories_and_Pathways_Restoration_Unacceptable.xlsx', sep="")
write_xlsx(Restoration_Unacceptable,output_path_x )
output_path_x =  paste(output_path,'Action_Categories_and_Pathways_Restoration_At_Risk.xlsx', sep="")
write_xlsx(Restoration_At_Risk,output_path_x )
output_path_x =  paste(output_path,'Action_Categories_and_Pathways_Restoration_Unacceptable_and_At_Risk.xlsx', sep="")
write_xlsx(Restoration_Unacceptable_and_At_Risk,output_path_x )

# ----------------- output Barriers data -----------------------
output_path_x = paste(output_path,'Barriers_Pathway_Output.xlsx', sep="")
write_xlsx(Barriers_Pathways_Data, output_path_x)

# -----------------------------------------------------------------
#     Projects Output
# -----------------------------------------------------------------

output_path_x =  paste(output_path,'Reach_Assessment_Project_Data_Habitat_Attributes.xlsx', sep="")
write_xlsx(Reach_Assessment_Project_Data_Habitat_Attributes,output_path_x )
output_path_x =  paste(output_path,'Reach_Assessment_Project_Data_per_Reach.xlsx', sep="")
write_xlsx(Reach_Assessment_Project_Data_per_Reach,output_path_x )
output_path_x =  paste(output_path,'Reach_Assessment_Project_Data_Habitat_Attributes_Priority_Reaches.xlsx', sep="")
write_xlsx(Reach_Assessment_Project_Data_Habitat_Attributes_Priority_Reaches,output_path_x )

# -----------------------------------------------------------------
#      Protection
# -----------------------------------------------------------------
output_path_x =  paste(output_path,'Reach_Actions_Protection.xlsx', sep="")
write_xlsx(Protection_Prioritization_Output,output_path_x )

# -----------------------------------------------------------------
#     Reach Rankings
# -----------------------------------------------------------------
output_path_x =  paste(output_path,'Restoration_Reach_Ranking_Scores_Output.xlsx', sep="")
write_xlsx(Reach_Rankings_Output_Restoration,output_path_x )
output_path_x =  paste(output_path,'Protection_Reach_Ranking_Scores_Output.xlsx', sep="")
write_xlsx(Reach_Rankings_Output_Protection,output_path_x )

# -----------------------------------------------------------------
#      Combine into one MASTER excel
# -----------------------------------------------------------------

output_path_x =  paste(output_path,'Step2_Prioritization_Output.xlsx', sep="")


write.xlsx(data_output_x,   file = output_path_x,  sheetName = sheet_name ,  append = TRUE)


print(paste("Time to complete ENTIRE tool: ", paste(round((proc.time()[3] - time1)/60, 2), " minutes")    ))

