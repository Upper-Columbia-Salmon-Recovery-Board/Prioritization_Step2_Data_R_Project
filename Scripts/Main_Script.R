# ---------------------------------------------------------------------------
#
#      R Script to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Auth++or: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------

library(tidyverse)
library(xlsx)
library(writexl)
library(readxl)

# ---------------------------------------------------------------------------
#  simple Criteria for output
# ---------------------------------------------------------------------------

basins_to_include = c("Methow",  "Entiat","Wenatchee" )  # basins to include in simulation    "OKanogan"
exclude_bull_trout = "yes"  # if "yes" -> remove bull trout for WebMap applications
output_Habitat_Quality_and_Habitat_Attribute_Scores = "no"  # enter "yes" or "no" if you want the "flat table" Habitat Attribute output (doubles time to run script)
update_Okanogan_reach_names = "no"  # if "yes" - update Okanogan reach names (should not have to run again - since on 5.Apr.2021 Ryan updated names)

# ---------------------------------------------------------------------------
#
#      Directories of Input and Output data
#
# ---------------------------------------------------------------------------

time1 <- proc.time()[3] # for timing the total time to run the tool

# --------------- directory of scripts -----------
script_path = 'Scripts/'

# ----------- directory of data -------------------
master_path = 'Data/'
habitat_data_path = paste(master_path,"Habitat_Data/", sep="")
ranking_data_path = paste(master_path,"Ranking_Data/", sep="")
crosswalks_path = paste(master_path,"Crosswalks/", sep="")
criteria_and_scoring_path = paste(master_path,"Criteria_and_Scoring/", sep="")
Okanogan_EDT_path = paste(master_path,'Okanogan_EDT/', sep="")
reach_assessment_projects_path = paste(master_path,'Reach_Assessment_Projects/', sep="")

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

print("----------------------------------------- Update Okanogan Reach Names (if necessary) --------------------------------------------")
if(update_Okanogan_reach_names == "yes"){
  source(paste(script_path, 'FUNCTION_update_names_in_data_frames.R', sep=""))
}
  
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
#   Generate Habitat Attribute Table (used in Limiting Factor Pathway)
# ---------------------------------------------------------------------------
print("----------------------------------------- GENERATE HABITAT ATTRIBUTE SCORES (for Limtiting Factor Pathway) --------------------------------------------")
source( paste(script_path, 'FUNCTIONS_for_Habitat_Quality_Filters.R', sep="")  )

source(paste(script_path, 'Habitat_Attribute_Scores_Generate_Script.R', sep=""))
# OUTPUT is Habitat_Attribute_Scores

# ---------------------------------------------------------------------------
#   Generate Habitat Quality Restoration and Protection Score 
# ---------------------------------------------------------------------------
print("----------------------------------------- GENERATE HABITAT QUALITY SCORES --------------------------------------------")

source( paste(script_path, 'Habitat_Quality_Scores_Generate_Script.R', sep="") )
# output is Habitat_Quality_Pathway_Output
# View(Habitat_Quality_Pathway_Output[['Habitat_Quality_Pathway_Protection']])

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

# ----- set names of Habitat Quality Scores to sum ------
habitat_quality_scores_colnames_for_sum = c("Stability_Mean" , "CoarseSubstrate_score" ,"Cover-Wood_score", "Flow-SummerBaseFlow_score",
                                            "Off-Channel-Floodplain_score", "Off-Channel-Side-Channels_score","PoolQuantity&Quality_score",
                                            "Riparian_Mean","Temperature-Rearing_score")

Habitat_Quality_Pathway_Spring_Chinook = Generate_Habitat_Quality_Output_Table("Spring Chinook", basins_to_include, habitat_quality_scores_colnames_for_sum )
Habitat_Quality_Pathway_Steelhead = Generate_Habitat_Quality_Output_Table("Steelhead", basins_to_include, habitat_quality_scores_colnames_for_sum )
Habitat_Quality_Pathway_Bull_Trout = Generate_Habitat_Quality_Output_Table("Bull Trout", basins_to_include, habitat_quality_scores_colnames_for_sum )

# View(Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']])
# View(Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Protection']])
# View(Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']])

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

# View(Limiting_Factor_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']])
# View(Limiting_Factor_Spring_Chinook[['Limiting_Factor_Pathway_Protection']])
#View(Limiting_Factor_Spring_Chinook[['Limiting_Factor_Pathway_Protection']][c('ReachName','LF_Sum','LF_Pct','LF_Score_Protection')])
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

# ---------------------------------------------------------------------------
#
#  RESTORATION: Summarize Habitat Attributes and Action Categories for each Reach within each Species and Score (Unnacceptable, At Risk, etc.)
#
# ---------------------------------------------------------------------------

print("----------------------------------------- COMBINE HQ AND LF OUTPUT --------------------------------------------")

source(paste(script_path, 'FUNCTIONS_for_Combining_Action_Tables.R', sep=""))

source(paste(script_path, 'FUNCTIONS_for_Combining_Reach_Habitat_Attribute_combos.R', sep=""))


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
HQ_add_life_stage = "yes"   # IF generate life stages for HQ pathway based on life stage presence in reaches
columns_info = c( "ReachName","Basin","Assessment.Unit" ) # columns to automatically add to beginning (left side) of output
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
colnames_outward_facing_WebMap_ORDER = c("ReachName","RM_Start", "RM_End","Assessment.Unit","Species", "Life_Stages","Impaired_Habitat_Attributes_All_Species","Actions","Action_Categories_All_Species" )
colnames_outward_facing_WebMap_UPDATED = c("Reach Name","River Mile - Start", "River Mile - End","Assessment Unit","Species","Priority Life Stages","Limiting Factor","Action Pathways","Action Categories" )
# ------- filter out for specific columns ----------
Restoration_Prioritization_Output_for_WebMap = Restoration_Prioritization_Output[,colnames_outward_facing_WebMap]
# ----------- add Reach information ------------
Restoration_Prioritization_Output_for_WebMap  =  FUNCTION_add_reach_information(Restoration_Prioritization_Output_for_WebMap,  colnames_reach_info)
# ------------ do MISC processing for output ---------
Restoration_Prioritization_Output_for_WebMap = FUNCTION_prepare_outward_facing_table( Restoration_Prioritization_Output_for_WebMap , colnames_outward_facing_WebMap_ORDER, colnames_outward_facing_WebMap_UPDATED, exclude_bull_trout)

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
#  RESTORATION - flat tables for WebMaps of 
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#  Reach-Habitat Attributes - Life Stage per row
# ---------------------------------------------------------------------------

columns_info = c( "ReachName","Basin","Assessment.Unit" ) # columns to automatically add to beginning (left side) of output
Reach_Habitat_Attribute_Life_Stage_Restoration_Output = FUNCTION_combine_by_Reach_AND_Habitat_Attribute_Life_Stage(  Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']], 
                                                Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']], 
                                                Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']], 
                                                Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']],
                                                Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']], 
                                                Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Restoration']], columns_info, exclude_bull_trout)


# ---------------------------------------------------------------------------
#  Reach-Habitat Attributes - Life Stage per row
# ---------------------------------------------------------------------------
HQ_life_stages = "yes"  # "yes" if use AU Life stages reach layer to generate life stages for habitat quality pathway
Reach_Habitat_Attribute_Life_Stage__Species_Restoration_Output = FUNCTION_combine_by_Reach_AND_Habitat_Attribute_Life_Stage_Species( Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']], 
                                                                   Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']], 
                                                                   Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']], 
                                                                   Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']],
                                                                   Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']], 
                                                                   Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Restoration']], columns_info, exclude_bull_trout, HQ_life_stages)


# ------------------ just test output --------------
# note these should be the same - except som eof the HQ output is different (Stability, Riparian)
reach_test = "Twisp River Lower 01"
strsplit(Restoration_Prioritization_Output_for_WebMap[which(Restoration_Prioritization_Output_for_WebMap$`Reach Name` == reach_test),]$`Limiting Factor`, ",")
unique(Reach_Habitat_Attribute_Life_Stage__Species_Restoration_Output[which(Reach_Habitat_Attribute_Life_Stage__Species_Restoration_Output$ReachName == reach_test),]$Habitat_Attribute)

strsplit(Restoration_Prioritization_Output_for_WebMap[which(Restoration_Prioritization_Output_for_WebMap$`Reach Name` == reach_test),]$`Species`, ",")
unique(Reach_Habitat_Attribute_Life_Stage__Species_Restoration_Output[which(Reach_Habitat_Attribute_Life_Stage__Species_Restoration_Output$ReachName == reach_test),]$Species)

strsplit(Restoration_Prioritization_Output_for_WebMap[which(Restoration_Prioritization_Output_for_WebMap$`Reach Name` == reach_test),]$`Priority Life Stages`, ",")
unique(Reach_Habitat_Attribute_Life_Stage__Species_Restoration_Output[which(Reach_Habitat_Attribute_Life_Stage__Species_Restoration_Output$ReachName == reach_test),]$Life_Stage)

# ---------------------------------------------------------------------------
#
#  Output of all reaches with habitat data (to include in Webmap)
#
# ---------------------------------------------------------------------------


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



# ---------------------------------------------------------------------------
#
#  PROJECTS - generate project layer - both for all projects and for project benefiting priority reaches
#
# ---------------------------------------------------------------------------

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

# ---------------------------------------------------------------------------
#
#   Generate Reach Ranking Score
#
# ---------------------------------------------------------------------------

source(paste(script_path, "Reach_Rankings_Restoration_and_Protection.R", sep=""))

Restoration_Scores_Output = Generate_Restoration_or_Protection_Reach_Rankings_Table()

# ----- but in export section -----------
output_path_x =  paste(output_path,'Reach_Rankings_Restoration.xlsx', sep="")
write_xlsx(Restoration_Scores_Output,output_path_x )

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

# ------------- Output for WebMap ------------------
# ------- change "Habitat_Attribute" name to "Limiting_Factor"
colnames(Reach_Habitat_Attribute_Life_Stage_Restoration_Output)[colnames(Reach_Habitat_Attribute_Life_Stage_Restoration_Output) == "Habitat_Attribute"] <- "Limiting_Factor"
output_path_x =  paste(output_path,'Reach_Habitat_Attribute_Life_Stage_Restoration_Output.xlsx', sep="")
write_xlsx(Reach_Habitat_Attribute_Life_Stage_Restoration_Output,output_path_x )
# ------------- Output for WebMap ------------------
colnames(Reach_Habitat_Attribute_Life_Stage__Species_Restoration_Output)[colnames(Reach_Habitat_Attribute_Life_Stage__Species_Restoration_Output) == "Habitat_Attribute"] <- "Limiting_Factor"
output_path_x =  paste(output_path,'Reach_Habitat_Attribute_Life_Stage_Species_Restoration_Output.xlsx', sep="")
write_xlsx(Reach_Habitat_Attribute_Life_Stage__Species_Restoration_Output,output_path_x )
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



# -----------------------------------------------------------------
#     Actions
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
#      Combine into one MASTER excel
# -----------------------------------------------------------------

output_path_x =  paste(output_path,'Step2_Prioritization_Output.xlsx', sep="")


write.xlsx(data_output_x,   file = output_path_x,  sheetName = sheet_name ,  append = TRUE)


print(paste("Time to complete ENTIRE tool: ", paste(round((proc.time()[3] - time1)/60, 2), " minutes")    ))

