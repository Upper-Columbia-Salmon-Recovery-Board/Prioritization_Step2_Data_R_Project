

# ---------------------------------------------------------------------------
#
#      SCRIPT: Okangaon-EDT Habitat Attribute and Habitat Qualty Functions
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
#  Generate a "habitat_raw_data" type data frame for the HABITAT QUALITY Pathway -  Okanogan
#
# ---------------------------------------------------------------------------
# NOTE - for the HQ Pathway - directly pull Level 2 (LF pathway FIRST filters through Level 3)



# ----------------- initiate reach info ----------------
Okanogan_Basic_Reach_Info= habitat_raw_data[which(habitat_raw_data$Basin == "Okanogan"),c(1:3,5) ]

# -------------- add columns for all attributes -------------
for( habitat_attribute_x in names(Habitat_Attributes_List_OKANOGAN) ){

  # ----------------- list all the individual data sources for this habitat attribute ----------
  data_sources_x = Habitat_Attributes_List_OKANOGAN[[habitat_attribute_x]]
  
  # -------------- loop through data sources and add a column (IF not "PROFESSOINAL JUDGEMENT")
  for(data_source_x in data_sources_x){
    
    # ------------------- skip if professional judgment --------------
    if(data_source_x == "PROFESSIONAL JUDGEMENT"){ next }
    
    # ------------------- if data already read in - skip ----------
    if( any( colnames(Okanogan_Basic_Reach_Info) == data_source_x) ){ next }
    
    # ------------------ pull EDT data name (based on data sources names) ---------------
    data_source_level_2_index_x = which(Level2_Data_Sources_Name_Crosswalk$EDT_Level_2_names == data_source_x)
    data_source_level_2_x = Level2_Data_Sources_Name_Crosswalk$EDT_Level_2_habitat_attribute[data_source_level_2_index_x]
    
    # --------------------------------------
    #    IF present in EDT Results
    # --------------------------------------
    if( length(data_source_level_2_x)>0 ){
      # ---------------- pull all the data from the Level 2 EDT data (Functional Condition) -------
      data_source_all_index_x = which(HabitatAttribute_Ratings_Level2_updated$`EDT Attribute` == data_source_level_2_x)
      data_source_all_x = HabitatAttribute_Ratings_Level2_updated[data_source_all_index_x,]
      data_source_all_x_add = data_source_all_x[,c("Reach", "Level 2 Functional Condition")]
      colnames(data_source_all_x_add) = c("ReachName", data_source_x)
      
      # ------------- add Level 2 functional condition score for all habitat attributes to data frame -----------
      Okanogan_Basic_Reach_Info = merge(Okanogan_Basic_Reach_Info,data_source_all_x_add, by="ReachName", all.x=TRUE )
      
    # --------------------------------------
    #    IF present in habitat_raw_data Results
    # --------------------------------------
    }else if( any(colnames(habitat_raw_data) == data_source_x) ){
      
      # ----------------- generate scores --------------------
      habitat_raw_data_output_x = FUNCTION_generate_habitat_attribute_score_from_Habitat_Data_Raw(habitat_attribute_x, data_source_x, "HQ")
      # -------- if data not in HQ scoring criteria data ----------
      if( length(which(is.na(habitat_raw_data_output_x$metric_data))) == nrow(habitat_raw_data_output_x)){
        habitat_raw_data_output_x = FUNCTION_generate_habitat_attribute_score_from_Habitat_Data_Raw(habitat_attribute_x, data_source_x, "LF")
      }
      # ------------------- generate simple data frame with both habitat data and reach name -----------------
      habitat_raw_data_output_x = habitat_raw_data_output_x[,c("ReachName","score")]
      colnames(habitat_raw_data_output_x)[2] = data_source_x
      # ------------- add data from habitat_raw_data to data frame -----------
      Okanogan_Basic_Reach_Info = merge(Okanogan_Basic_Reach_Info,habitat_raw_data_output_x, by="ReachName", all.x=TRUE )
     
    # --------------------------------------
    #    IF is "NA" in the Crosswalk ( Level2_Data_Sources_Name_Crosswalk )
    # --------------------------------------  
    }else{
      Okanogan_Basic_Reach_Info[,data_source_x] = NA
      
      print(paste(c("No data source for: ", data_source_x)))
    }
    
  }
}
# ------- give it a different name ---------
Okanogan_Habitat_Quality_Output = Okanogan_Basic_Reach_Info

if(output_Habitat_Quality_and_Habitat_Attribute_Scores == "yes"){
  write.xlsx(Okanogan_Habitat_Quality_Output, file=paste(output_path,"Habitat_Quality_Pathway_Okanogan_All_Reaches.xlsx",sep=""), sheetName="Adult_Migration", row.names=FALSE)
}

EDT_level2_data_no_in_EDT_excel = c("Total Suspended Solids", "Large Cobble Riffle", "Small Cobble Riffle","Metals in Sediment" , "Metals in Water",
                                    "Miscellaneous Toxins","Backwater Pools", "Floodplain Ponds" ,"Seasonally Inundated Floodplain","Side Channel",
                                    "Beaver Ponds", "Scour Pools")

# "Total Suspended Solids": listed in AttributeCrosswalk and Level2->Level3 Crosswalk - NOT in any data tables
# "Large Cobble Riffle" and "Small Cobble Riffle": listed in AttributeCrosswalk and Level2->Level3 Crosswalk - NOT in any data tables
# "Metals in Sediment" and "Metals in Water": listed in AttributeCrosswalk and Level2->Level3 Crosswalk - NOT in any data tables
# "Miscellaneous Toxins": listed in AttributeCrosswalk and Level2->Level3 Crosswalk - NOT in any data tables
# "Backwater Pools": listed in AttributeCrosswalk and Level2->Level3 Crosswalk - NOT in any data tables
# "Floodplain Ponds": listed in AttributeCrosswalk and Level2->Level3 Crosswalk - NOT in any data tables
# "Seasonally Inundated Floodplain": listed in AttributeCrosswalk and Level2->Level3 Crosswalk - NOT in any data tables
# "Side Channel": listed in AttributeCrosswalk and Level2->Level3 Crosswalk - NOT in any data tables
# "Beaver Ponds": listed in AttributeCrosswalk and Level2->Level3 Crosswalk - NOT in any data tables
# "Scour Pools": listed in AttributeCrosswalk and Level2->Level3 Crosswalk - NOT in any data tables


# ---------------------------------------------------------------------------
#
#   LIMITING FACTOR AND HABITAT QUALITY PATHWAY
#
#  Generate Okanogan Habitat Attribute Scores table -  Okanogan
#     pull these scores for the HQ Pathway 
#
# ---------------------------------------------------------------------------

# ----------- just for my notes comparing where Professional Judgment  --------------
data_in_prof_judgement = c("Food- Food Web Resources", "Predators- Juveniles","Brook Trout","Entrainment/Stranding","Flow- Scour","Harassment" ,"Icing", "Superimposition"  )
data_source_prof = c('Entrainment/Stranding' , "Flow- Summer Base Flow",'Food- Food Web Resources',"Harrassment", "Icing", 'Predators- Juveniles',"Superimposition")
data_in_prof_judgement[order(data_in_prof_judgement)]
data_source_prof[order(data_source_prof)]

# --------------- basic info table ----------------
Okanogan_Habitat_Quality_Output_Info = Okanogan_Habitat_Quality_Output[,1:3]

# ------------------- start table --------------
Habitat_Attribute_Scores_Okanogan = c()

# ---------------- unique habitat attributes for HQ Pathway ----------------
habitat_attribute_list = names(Habitat_Attributes_List_OKANOGAN)

# ------------ loop through --------------
for(habitat_attribute_x in habitat_attribute_list){
  
  # --------------------------------------------------------
  #     Initiate the data frame
  # --------------------------------------------------------
  # ---------------- put in habitat attribute -------------
  Okanogan_Habitat_Quality_Output_X = Okanogan_Habitat_Quality_Output_Info
  Okanogan_Habitat_Quality_Output_X$Habitat_Attribute = habitat_attribute_x
  
  # --------------------------------------------------------
  #     Pull in Data Sources
  # --------------------------------------------------------
  # ----------------- pull in data sources -------------------
  data_sources_x = Habitat_Attributes_List_OKANOGAN[[habitat_attribute_x ]]
  data_sources_combined_x = c()
  i = 0
  for(data_source_x in data_sources_x){
    i = i + 1
    data_source_x2 = paste( c(data_source_x,"(HabitatAttributeScore",i,"),"   ), collapse="" )
    data_sources_combined_x = paste( data_sources_combined_x,data_source_x2, sep="" )
  }
  # ------ remove last comma ------------
  data_sources_combined_x = substr( data_sources_combined_x, 0, nchar(data_sources_combined_x) - 1 )
  
  # -------------- IF no data sources ------------
  if(length(data_sources_combined_x) == 0){
    Okanogan_Habitat_Quality_Output_X$Data_Sources = "no data sources"
    
  # ---------------- if data sources to add --------------
  }else{
    # --------------- combine with all of them -------------
    Okanogan_Habitat_Quality_Output_X$Data_Sources = data_sources_combined_x
  }

  
  # --------------------------------------------------------
  #    Add Habitat Attributes Columns
  # --------------------------------------------------------
  Okanogan_Habitat_Quality_Output_X$HabitatAttributeScore1 = NA
  Okanogan_Habitat_Quality_Output_X$HabitatAttributeScore2 = NA
  Okanogan_Habitat_Quality_Output_X$HabitatAttributeScore3 = NA
  Okanogan_Habitat_Quality_Output_X$HabitatAttributeScore4 = NA
  Okanogan_Habitat_Quality_Output_X$HabitatAttributeScore5 = NA
  Okanogan_Habitat_Quality_Output_X$HabitatAttributeScore6 = NA
  Okanogan_Habitat_Quality_Output_X$Habitat_Attribute_Score = NA
  
  # --------------------------------------------------------
  #    Loop through data sources and add to HabitatAttriubteScore columns
  # --------------------------------------------------------
  col_habitat_attribute_score_x = c("HabitatAttributeScore1", "HabitatAttributeScore2" ,"HabitatAttributeScore3","HabitatAttributeScore4","HabitatAttributeScore5" , "HabitatAttributeScore6")
  i = 0
  for(data_source_x in data_sources_x){
    i = i + 1
    # ---------------- IF pulling in professional judgement ------------
    if(data_source_x == "PROFESSIONAL JUDGEMENT"){
      
      Habitat_Attribute_Notes_and_Professional_Judgement_Habitat_Attribute_x = Habitat_Attribute_Notes_and_Professional_Judgement[which(Habitat_Attribute_Notes_and_Professional_Judgement$Habitat_Attribute == habitat_attribute_x),]
      if(nrow(Habitat_Attribute_Notes_and_Professional_Judgement_Habitat_Attribute_x) > 0){
        
        # ---------------- generate simple data frame-----------
        Okanogan_Habitat_Quality_Output_X_professional_judgement = Okanogan_Habitat_Quality_Output_X[,c("ReachName","Basin")]
        # ----------------- generate simple data frame -----------
        Habitat_Attribute_Notes_and_Professional_Judgement_Habitat_Attribute_x = Habitat_Attribute_Notes_and_Professional_Judgement_Habitat_Attribute_x[,c("ReachName","Updated_Value")]
        # ------------------- merge the two ------------------
        Okanogan_Habitat_Quality_Output_X_2 = merge(Okanogan_Habitat_Quality_Output_X_professional_judgement, Habitat_Attribute_Notes_and_Professional_Judgement_Habitat_Attribute_x, by="ReachName", all.x = TRUE )
        # ---------------- add to data frame --------------
        Okanogan_Habitat_Quality_Output_X[,col_habitat_attribute_score_x[i]] = Okanogan_Habitat_Quality_Output_X_2[,3]
      }else{
        Okanogan_Habitat_Quality_Output_X[,col_habitat_attribute_score_x[i]] = NA
      }
      
      
      
    # ---------------- IF simply pulling in from Okanogan_Habitat_Quality_Output ------------
    }else{
      # ----------------- pull data source column from Okanogan_Habitat_Quality_Output --------
      habitat_attribute_column_x = Okanogan_Habitat_Quality_Output[,c("ReachName",data_source_x )]
      # ----------- merge with data --------------
      Okanogan_Habitat_Quality_Output_X_2 = merge(Okanogan_Habitat_Quality_Output_X,habitat_attribute_column_x, by="ReachName", all.x=TRUE )
      Okanogan_Habitat_Quality_Output_X[,col_habitat_attribute_score_x[i]] = Okanogan_Habitat_Quality_Output_X_2[,ncol(Okanogan_Habitat_Quality_Output_X_2)]
    }

  }
  
  # --------------------------------------------------------
  #    Get lowest score across all Habitat Attribute Scores
  # --------------------------------------------------------
  # ------ convert to numeric ----
  for(colx in col_habitat_attribute_score_x){
    Okanogan_Habitat_Quality_Output_X[,colx] = as.numeric(as.character(Okanogan_Habitat_Quality_Output_X[,colx]))
  }
  Okanogan_Habitat_Quality_Output_X$Habitat_Attribute_Score = apply(Okanogan_Habitat_Quality_Output_X[,col_habitat_attribute_score_x], 1, min, na.rm=TRUE)
  # ------------- change "Inf" to NA ------------
  Inf_x = which(Okanogan_Habitat_Quality_Output_X$Habitat_Attribute_Score == "Inf")
  Okanogan_Habitat_Quality_Output_X$Habitat_Attribute_Score[Inf_x] = NA
  
  # --------------------------------------------------------
  #   add professional judgment notes
  # --------------------------------------------------------
  Habitat_Attribute_Notes_and_Professional_Judgement_Habitat_Attribute_x = Habitat_Attribute_Notes_and_Professional_Judgement[which(Habitat_Attribute_Notes_and_Professional_Judgement$Habitat_Attribute == habitat_attribute_x  & Habitat_Attribute_Notes_and_Professional_Judgement$Subbasin == "Okanogan"),]
  if(nrow(Habitat_Attribute_Notes_and_Professional_Judgement_Habitat_Attribute_x) > 0){
    
    # ---------------- generate simple data frame-----------
    Okanogan_Habitat_Quality_Output_X_2 = Okanogan_Habitat_Quality_Output_X[,c("ReachName","Basin")]
    # ----------------- generate simple data frame -----------
    Habitat_Attribute_Notes_and_Professional_Judgement_Habitat_Attribute_x = Habitat_Attribute_Notes_and_Professional_Judgement_Habitat_Attribute_x[,c("ReachName","Notes_or_Professional_Judgement")]
    # ------------------- merge the two ------------------
    Okanogan_Habitat_Quality_Output_X_2 = merge(Okanogan_Habitat_Quality_Output_X_2,Habitat_Attribute_Notes_and_Professional_Judgement_Habitat_Attribute_x )
    # ------------ just pull ReachName and Notes_or_Professional_Judgement -----------
    Okanogan_Habitat_Quality_Output_X_2 = Okanogan_Habitat_Quality_Output_X_2[,c("ReachName","Notes_or_Professional_Judgement")]
    # ----------------- add to data frame ---------------
    Okanogan_Habitat_Quality_Output_X = merge(Okanogan_Habitat_Quality_Output_X, Okanogan_Habitat_Quality_Output_X_2, by="ReachName", all.x = TRUE)
    
  }else{
    Okanogan_Habitat_Quality_Output_X$Notes_or_Professional_Judgement = NA
  }
  
  
  # --------------------------------------------------------
  #    combine with main data frame 
  # --------------------------------------------------------
  
  Habitat_Attribute_Scores_Okanogan = rbind(Habitat_Attribute_Scores_Okanogan, Okanogan_Habitat_Quality_Output_X)
}

if(output_Habitat_Quality_and_Habitat_Attribute_Scores == "yes"){
  write.xlsx(Habitat_Attribute_Scores_Okanogan, file=paste(output_path,"Habitat_Attribute_Scores_Okanogan.xlsx",sep=""),  row.names=FALSE)
}


# ---------------------------------------------------------------------------
#
#  Generate Habitat Quality Pathway Output
#
# ---------------------------------------------------------------------------

# ----------- habitat attributes for HQ output --------
HQ_columns = c("Bank Stability", "Channel Stability", "Coarse Substrate","Cover- Wood", 	"Flow- Summer Base Flow",
               "Off-Channel- Floodplain","Off-Channel- Side-Channels", "Pool Quantity & Quality",
               	"Riparian-Disturbance",	"Riparian- Canopy Cover",		"Temperature- Rearing")

# columns to do sums: HQ_columns_sum = c("Riparian", "Stability")

# ---------- initiate data frame -----------
Habitat_Quality_Scores_Okanogan = Okanogan_Habitat_Quality_Output[,c("ReachName","Basin", "Assessment.Unit", "Steelhead.Reach" )]
Habitat_Quality_Scores_Okanogan$"Spring.Chinook.Reach" = NA
Habitat_Quality_Scores_Okanogan$"Bull.Trout.Reach"  = NA
Habitat_Quality_Scores_Okanogan = Habitat_Quality_Scores_Okanogan[,c("ReachName","Basin", "Assessment.Unit","Spring.Chinook.Reach","Steelhead.Reach","Bull.Trout.Reach"  )]

for(habitat_attribute_x in HQ_columns){
  
  # ---------------- pull the data from the Habitat_Attribute_Scores_Okanogan ----------
  habitat_attribute_scores_x = Habitat_Attribute_Scores_Okanogan[which(Habitat_Attribute_Scores_Okanogan$Habitat_Attribute == habitat_attribute_x), c("ReachName","Habitat_Attribute_Score")]
  # ------------------ new column name -------------
  colnames(habitat_attribute_scores_x)[2] = habitat_attribute_x
  # ---------------- make is so column is numeric ---------
  habitat_attribute_scores_x[,2] = as.numeric(as.character( habitat_attribute_scores_x[,2]))
  # ------------- add to Habitat_Quality_Scores_Okanogan -----
  Habitat_Quality_Scores_Okanogan = merge(Habitat_Quality_Scores_Okanogan,habitat_attribute_scores_x, by="ReachName", all.x=TRUE )
  
  # ----------- get average Riparian score)
  if(habitat_attribute_x == "Riparian- Canopy Cover"){
    ncolx = ncol(Habitat_Quality_Scores_Okanogan)
    Habitat_Quality_Scores_Okanogan$Riparian = rowMeans(Habitat_Quality_Scores_Okanogan[,(ncolx-1):ncolx], na.rm=T)
  }
  
  # ----------- get average Riparian score)
  if(habitat_attribute_x == "Channel Stability"){
    ncolx = ncol(Habitat_Quality_Scores_Okanogan)
    Habitat_Quality_Scores_Okanogan$Stability = rowMeans(Habitat_Quality_Scores_Okanogan[,(ncolx-1):ncolx], na.rm=T)
  }
}

# ------------------- calculate HQ SCore ---------
HQ_Score_Columns = c("Stability", "Coarse Substrate","Cover- Wood", 	"Flow- Summer Base Flow",
               "Off-Channel- Floodplain","Off-Channel- Side-Channels", "Pool Quantity & Quality",
               "Riparian",		"Temperature- Rearing")

# ------------------------------------------------------------------------------------- 
#                 calculate HQ Sum and Pct
# -------------------------------------------------------------------------------------
# NOTE - for EDT the HQ_Pct was calculated from the % of Template in EDT

Habitat_Quality_Scores_Okanogan$HQ_Sum = NA # other basins: rowSums(Habitat_Quality_Scores_Okanogan[,HQ_Score_Columns]) 
PRCNT_Habitat_Quality_Okanogan_EDT_SHORTENED = PRCNT_Habitat_Quality_Okanogan_EDT[,c("ReachName","HQ_Score")]
colnames(PRCNT_Habitat_Quality_Okanogan_EDT_SHORTENED)[2] = c("HQ_Pct")
Habitat_Quality_Scores_Okanogan = merge(Habitat_Quality_Scores_Okanogan,PRCNT_Habitat_Quality_Okanogan_EDT_SHORTENED, by="ReachName", all.x=TRUE )

# ------------------------------------------------------------------------------------- 
#                 calculate HQ Restoration and Protection Score
# ------------------------------------------------------------------------------------- 

Habitat_Quality_Scores_Okanogan = Habitat_Quality_Scores_Okanogan  %>%
  mutate(HQ_Score_Restoration = ifelse(HQ_Pct  > Restoration_Scoring$Category_Lower[1] & 
                                         HQ_Pct  < Restoration_Scoring$Category_Upper[1] , Restoration_Scoring$Score[1],
                                       ifelse(HQ_Pct  >= Restoration_Scoring$Category_Lower[2] & 
                                                HQ_Pct  <= Restoration_Scoring$Category_Upper[2] , Restoration_Scoring$Score[2],
                                              ifelse(HQ_Pct  > Restoration_Scoring$Category_Lower[3] & 
                                                       HQ_Pct  <= Restoration_Scoring$Category_Upper[3] , Restoration_Scoring$Score[3],
                                                     NA))))
Habitat_Quality_Scores_Okanogan = Habitat_Quality_Scores_Okanogan  %>%
  mutate(HQ_Score_Protection = ifelse(HQ_Pct  > Protection_Scoring$Category_Lower [1] & 
                                        HQ_Pct  < Protection_Scoring$Category_Upper[1] , Protection_Scoring$Score[1],
                                      ifelse(HQ_Pct  >= Protection_Scoring$Category_Lower[2] & 
                                               HQ_Pct  <= Protection_Scoring$Category_Upper[2] , Protection_Scoring$Score[2],
                                             ifelse(HQ_Pct  > Protection_Scoring$Category_Lower[3] & 
                                                      HQ_Pct  <= Protection_Scoring$Category_Upper[3] , Protection_Scoring$Score[3],
                                                    NA))))

# ------------------------------------------------------------------------------------- 
#                merge existing Habitat_Quality_Scores with Okanogan 
# ------------------------------------------------------------------------------------- 

# -----------------------  -----------
for(row_OK in 1:nrow(Habitat_Quality_Scores_Okanogan) ){
  # ----------- identify Habitat_Quality_Scores row for this reach ------
  x = which(Habitat_Quality_Scores$ReachName == Habitat_Quality_Scores_Okanogan$ReachName[row_OK])
  # ---------- replace this row for new Okangoan row -----
  Habitat_Quality_Scores[x,] = Habitat_Quality_Scores_Okanogan[row_OK,]
}

# ------------------ output data -------------------------
if(output_Habitat_Quality_and_Habitat_Attribute_Scores == "yes"){
  Habitat_Quality_Scores = as.data.frame(Habitat_Quality_Scores)
  output_path_x =  paste(output_path,'Habitat_Quality_Scores.xlsx', sep="")
  write.xlsx(
    Habitat_Quality_Scores,
    output_path_x,
    col.names = TRUE,
    row.names = FALSE,
    append = FALSE,
    showNA = TRUE,
    password = NULL
  )
}


# ------------------------------------------------------------------------------------- 
#                 Save the data
# ------------------------------------------------------------------------------------- 
# ---------- so column names match Habitat_Quality_Scores --------
colnames(Habitat_Quality_Scores_Okanogan) = colnames(Habitat_Quality_Scores)

if(output_Habitat_Quality_and_Habitat_Attribute_Scores == "yes"){
  write.xlsx(Habitat_Quality_Scores_Okanogan, file=paste(output_path,"Habitat_Quality_Scores_Okanogan.xlsx",sep=""), row.names=FALSE)
}



# ---------------------------------------------------------------------------
#
#  Generate Life Stage tables for the Limiting Factor Pathway -  Okanogan
#     NOTE - this is mergable with the Habitat Attribute Scores data frame
#     ALSO Note - this applies no filters
#
# ---------------------------------------------------------------------------

# OKanogan EDT Limiting Factors Steps:
# 1. Pull the Level 3 that are limiting factors for the life stage (1 or 3)
# 2. Look up the Level 2 attributes under that Level 3 and pull any that are 1 or 3
# 3. Filter out any Level 2 that are not listed by the RTT for that life stage

# --------------------------------------------
#      All Scores generated for all Level 3s
# ----------------------------------------------
# Output: Limiting_Factors_Okanogan_EDT


# ------------------------ pull unique RTT life stages in the Limiting_Factors_Okanogan_EDT ---------------
unique_RTT_life_stages = unique(Limiting_Factors_Okanogan_EDT$`RTT Life Stage`)
unique_RTT_life_stages = unique_RTT_life_stages[order(unique_RTT_life_stages)]

# ----------- Basic Data frame --------------------
Okanogan_Basic_Reach_Info_LF_initial = Okanogan_Habitat_Quality_Output[,c("ReachName","Assessment.Unit","Steelhead.Reach" )]

# --------------- prep life stages output data ---------
Adult_Migration_LF_Okanogan = c()
Fry_LF_Okanogan = c() 
Holding_and_Maturation_LF_Okanogan = c()
Smolt_Outmigration_LF_Okanogan = c() 
Spawning_and_Incubation_LF_Okanogan = c()
Summer_Rearing_LF_Okanogan = c() 
Winter_Rearing_LF_Okanogan = c()

# ----------------- data not in data sources data frame -----------
data_not_in_data_sources_list_no_data_source_listed_not_in_HabAtrRat = c()
data_not_in_data_sources_list_no_data_source_listed_EXIST_in_HabAtrRat = c()

for(RTT_life_stage_x in unique_RTT_life_stages){
  
  # ---------------------- create data frame for this RTT life stage -------------
  Okanogan_Basic_Reach_Info_LF_initial_RTT = Okanogan_Basic_Reach_Info_LF_initial
  Okanogan_Basic_Reach_Info_LF_initial_RTT$'RTT Life Stage' = RTT_life_stage_x
  
  # ------------------- pull rows for this life stage -----------
  Limiting_Factors_Okanogan_EDT_LIFE_STAGE = Limiting_Factors_Okanogan_EDT[which(Limiting_Factors_Okanogan_EDT$`RTT Life Stage` == RTT_life_stage_x ),]
  
  # ---------------- loup through each EDT life stage ------------
  unique_EDT_life_stages = unique(Limiting_Factors_Okanogan_EDT_LIFE_STAGE$`EDT Life Stage`)
  
  for(EDT_life_stage_x in unique_EDT_life_stages){
    
    # ---------------------- create data frame for this RTT life stage -------------
    Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT = Okanogan_Basic_Reach_Info_LF_initial_RTT
    Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT$'EDT Life Stage' = EDT_life_stage_x
    
    # ------------------- pull rows for this life stage -----------
    Limiting_Factors_Okanogan_EDT_LIFE_STAGE_EDT = Limiting_Factors_Okanogan_EDT_LIFE_STAGE[which(Limiting_Factors_Okanogan_EDT_LIFE_STAGE$`EDT Life Stage` == EDT_life_stage_x ),]
    
    # --------------------- pull unique Level 3 habitat attributes for this EDT life stage ----------
    unique_level3_habitat_attributes_x = unique(Limiting_Factors_Okanogan_EDT_LIFE_STAGE_EDT$Attribute)
    
    # ------------------- pull rows for this level 3 attribute -----------
    for(level3_habitat_attribute_x in unique_level3_habitat_attributes_x){
      
      # ------------------- pull rows for this life stage -----------
      Limiting_Factors_Okanogan_EDT_LIFE_STAGE_EDT_Level3 = Limiting_Factors_Okanogan_EDT_LIFE_STAGE_EDT[which(Limiting_Factors_Okanogan_EDT_LIFE_STAGE_EDT$Attribute == level3_habitat_attribute_x ),]
      
      # --------------------------- add column name --------------
      Limiting_Factors_Okanogan_EDT_LIFE_STAGE_EDT_Level3$Level_3_Habitat_Attribute = level3_habitat_attribute_x
      
      # ---------------------- create data frame for this RTT life stage -------------
      Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3 = Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT
      Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT$'EDT Life Stage' = EDT_life_stage_x
      
      # -------------- Add Performance Effect and Score for Level 3 -----------
      Limiting_Factors_Okanogan_EDT_LIFE_STAGE_EDT_Level3_Output = Limiting_Factors_Okanogan_EDT_LIFE_STAGE_EDT_Level3[,c("Reach","Level_3_Habitat_Attribute", "Performance Effect","RTT_Limiting_Factor_Score")]
      colnames(Limiting_Factors_Okanogan_EDT_LIFE_STAGE_EDT_Level3_Output)[1] = "ReachName"
      Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3 = merge(Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3,  
                                                                   Limiting_Factors_Okanogan_EDT_LIFE_STAGE_EDT_Level3_Output, by = "ReachName", all.x = TRUE )
        
      # -------------------------------- get unique Level 2 habitat attributes ---------------
      unique_level2_habitat_attributes_x = Limiting_Factors_Okanogan_EDT_LIFE_STAGE_EDT_Level3$EDT_Attribute_Level_2[1]
      unique_level2_habitat_attributes_x = unlist(strsplit(unique_level2_habitat_attributes_x, ","))
      # NOW - write a FOR loop to loop through every Limiting_Factors_Okanogan_EDT_LIFE_STAGE_EDT_Level3$EDT_Attribute_Level_2 and pull from the HQ
      
      # ---------------- if no level 2 habitat attribute (it is NA) -------------
      if( is.na(unique_level2_habitat_attributes_x[1]) ){
        
        # ----------------- re-start the data frame ----------------
        Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3 = Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3[,c(1:8)]
        # -------------------- add NA ---------------
        Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3$Level_2_Habitat_Attribute = NA
        Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3$Functional_Condition_Score = NA
        
        
        if( RTT_life_stage_x == "Adult Migration" ){
          Adult_Migration_LF_Okanogan  = rbind(Adult_Migration_LF_Okanogan , Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3)
        }else if( RTT_life_stage_x == "Fry"  ){
          Fry_LF_Okanogan  = rbind(Fry_LF_Okanogan , Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3)
        }else if( RTT_life_stage_x == "Holding and Maturation"  ){
          Holding_and_Maturation_LF_Okanogan  = rbind(Holding_and_Maturation_LF_Okanogan , Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3)
        }else if( RTT_life_stage_x == "Smolt Outmigration"  ){
          Smolt_Outmigration_LF_Okanogan  = rbind(Smolt_Outmigration_LF_Okanogan , Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3)
        }else if( RTT_life_stage_x == "Spawning and Incubation"  ){
          Spawning_and_Incubation_LF_Okanogan  = rbind(Spawning_and_Incubation_LF_Okanogan , Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3)
        }else if( RTT_life_stage_x == "Summer Rearing"  ){
          Summer_Rearing_LF_Okanogan  = rbind(Summer_Rearing_LF_Okanogan , Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3)
        }else if( RTT_life_stage_x == "Winter Rearing"  ){
          Winter_Rearing_LF_Okanogan  = rbind(Winter_Rearing_LF_Okanogan , Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3)
        }
        
      
      # ------------------ if there is a level 2 habitat attribute --------------
      }else{
        
        
        for(level2_habitat_attribute_x in unique_level2_habitat_attributes_x){
          
          # ----------------- re-start the data frame ----------------
          Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3 = Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3[,c(1:8)]
          
          # ----------------------- IF there is level 2 in HQ output --------------
          if( any(Level2_Data_Sources_Name_Crosswalk$EDT_Level_2_habitat_attribute == level2_habitat_attribute_x) ){
            
            # --------------- add each Level 2 attribute to Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3 ------
            
            # ---------------------- pull Level 2 data from Okanogan_Habitat_Quality_Output -----
            col_attribute_short_x = which(Level2_Data_Sources_Name_Crosswalk$EDT_Level_2_habitat_attribute == level2_habitat_attribute_x)
            col_attribute_long_x = Level2_Data_Sources_Name_Crosswalk$EDT_Level_2_names[col_attribute_short_x]
            
            # ---------------- IF no crosswalk for level 2 -------------
            if( is.na(col_attribute_long_x)  | col_attribute_long_x == "NA" ){
              # -------------------- add level 2 habitat attribute and NA ---------------
              Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3$Level_2_Habitat_Attribute = level2_habitat_attribute_x
              
              # ---------------- IF Level 2 data present in the HabitatAttribute_Ratings_Level2 data frame (but NOT in the data sources listed) -----------
              if( any(HabitatAttribute_Ratings_Level2$`EDT Attribute` == level2_habitat_attribute_x )){
                #print(paste("level2_habitat_attribute_x not in data sources: ", level2_habitat_attribute_x))
                
                # -------------- combine data that is not in data sources listed ----------
                data_not_in_data_sources_list_x = t(as.data.frame(c("level2 in HabAtrRat, not in data sources",level2_habitat_attribute_x)))
                data_not_in_data_sources_list_no_data_source_listed_EXIST_in_HabAtrRat  = rbind(data_not_in_data_sources_list_no_data_source_listed_EXIST_in_HabAtrRat , data_not_in_data_sources_list_x)
                
                # ------------- pull data from HabitatAttribute_Ratings_Level2 --------------
                HabitatAttribute_Ratings_Level2_output_x = HabitatAttribute_Ratings_Level2[which(HabitatAttribute_Ratings_Level2$`EDT Attribute` == level2_habitat_attribute_x ), ]
                HabitatAttribute_Ratings_Level2_output_x = HabitatAttribute_Ratings_Level2_output_x[, c("Reach","Level 2 Functional Condition")]
                colnames(HabitatAttribute_Ratings_Level2_output_x) = c("ReachName", "Functional_Condition_Score")
                Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3 = merge(Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3,
                                                                             HabitatAttribute_Ratings_Level2_output_x, by ="ReachName", all.x=TRUE)
              }else{
                Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3$Functional_Condition_Score = NA
              }
              
              
              # ---------------- IF level 2 exists - pull from Okanogan_Habitat_Quality_Output -------------
            }else{
              
              # ------------------ add level 2 habitat attribute name --------------------
              Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3$Level_2_Habitat_Attribute = level2_habitat_attribute_x
              
              # ------------- pull data fro Habitat Quality (Okanogan) output ----------
              HQ_level2_data_x = Okanogan_Habitat_Quality_Output[,c("ReachName",col_attribute_long_x)]
              colnames(HQ_level2_data_x)[2] = level2_habitat_attribute_x
              Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3 = merge(Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3  ,HQ_level2_data_x, by = "ReachName", all.x = TRUE)
              colnames(Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3)[10] = "Functional_Condition_Score" 
              
            }
            
            if( RTT_life_stage_x == "Adult Migration" ){
              Adult_Migration_LF_Okanogan  = rbind(Adult_Migration_LF_Okanogan , Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3)
            }else if( RTT_life_stage_x == "Fry"  ){
              Fry_LF_Okanogan  = rbind(Fry_LF_Okanogan , Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3)
            }else if( RTT_life_stage_x == "Holding and Maturation"  ){
              Holding_and_Maturation_LF_Okanogan  = rbind(Holding_and_Maturation_LF_Okanogan , Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3)
            }else if( RTT_life_stage_x == "Smolt Outmigration"  ){
              Smolt_Outmigration_LF_Okanogan  = rbind(Smolt_Outmigration_LF_Okanogan , Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3)
            }else if( RTT_life_stage_x == "Spawning and Incubation"  ){
              Spawning_and_Incubation_LF_Okanogan  = rbind(Spawning_and_Incubation_LF_Okanogan , Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3)
            }else if( RTT_life_stage_x == "Summer Rearing"  ){
              Summer_Rearing_LF_Okanogan  = rbind(Summer_Rearing_LF_Okanogan , Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3)
            }else if( RTT_life_stage_x == "Winter Rearing"  ){
              Winter_Rearing_LF_Okanogan  = rbind(Winter_Rearing_LF_Okanogan , Okanogan_Basic_Reach_Info_LF_initial_RTT_EDT_Level_3)
            }
            
            
          }else{
            #print( paste(c("no level 2 short habitat attribute in crosswalk: ",level2_habitat_attribute_x)))
            # -------------- combine data that is not in data sources listed ----------
            data_not_in_data_sources_list_x = t(as.data.frame(c("no level 2 short habitat attribute in crosswalk",level2_habitat_attribute_x)))
            data_not_in_data_sources_list_no_data_source_listed_not_in_HabAtrRat  = rbind(data_not_in_data_sources_list_no_data_source_listed_not_in_HabAtrRat , data_not_in_data_sources_list_x)
            
          }
          
          
        } #end of level 2 for loop 
        
      }
      


    } # end of level 3 for loop
    
  } # end of loop through EDT for loop
  
}

  
# ---------------------------------------------------------------------------
#           Output these into individual tabs
# ---------------------------------------------------------------------------
# NOTE: had issues outputing all tabs - kept running into memory issues
if(output_Habitat_Quality_and_Habitat_Attribute_Scores == "yes"){
  write.xlsx(Adult_Migration_LF_Okanogan, file=paste(output_path,"Limiting_Factor_Pathway_Okanogan_All_Reaches.xlsx",sep=""), sheetName="Adult_Migration", row.names=FALSE)
  write.xlsx(Fry_LF_Okanogan, file=paste(output_path,"Limiting_Factor_Pathway_Okanogan_All_Reaches.xlsx",sep=""), sheetName="Fry", append=TRUE, row.names=FALSE)
  write.xlsx(Holding_and_Maturation_LF_Okanogan, file=paste(output_path,"Limiting_Factor_Pathway_Okanogan_All_Reaches.xlsx",sep=""), sheetName="Holding_and_Maturation", append=TRUE, row.names=FALSE)
  write.xlsx(Smolt_Outmigration_LF_Okanogan, file=paste(output_path,"Limiting_Factor_Pathway_Okanogan_All_Reaches.xlsx",sep=""), sheetName="Smolt_Outmigration", append=TRUE, row.names=FALSE)
  write.xlsx(Spawning_and_Incubation_LF_Okanogan, file=paste(output_path,"Limiting_Factor_Pathway_Okanogan_All_Reaches.xlsx",sep=""), sheetName="Spawning_and_Incubation", append=TRUE, row.names=FALSE)
  write.xlsx(Summer_Rearing_LF_Okanogan, file=paste(output_path,"Limiting_Factor_Pathway_Okanogan_All_Reaches.xlsx",sep=""), sheetName="Summer_Rearing", append=TRUE, row.names=FALSE)
  write.xlsx(Winter_Rearing_LF_Okanogan, file=paste(output_path,"Limiting_Factor_Pathway_Okanogan_All_Reaches.xlsx",sep=""), sheetName="Winter_Rearing", append=TRUE, row.names=FALSE)
  
}


# ---------------------------------------------------------------------------
#
#  Generate Life Stage tables for the Limiting Factor Pathway -  Okanogan - NO LEVEL 3
#   4.June.2021: Greer and I discussed that we should try to pull Level2 -> RTT habitat attributes (for life stage)
#       and compare to original output (filter through Level 3 1s and 2s first)
#
# ---------------------------------------------------------------------------
# --------------------------------------------
#      All Scores generated for all Level 3s
# ----------------------------------------------
# Output: Limiting_Factors_Okanogan_EDT


# ------------------------ pull unique RTT life stages in the Limiting_Factors_Okanogan_EDT ---------------
unique_RTT_life_stages = unique(Limiting_Factors_Okanogan_EDT$`RTT Life Stage`)
unique_RTT_life_stages = unique_RTT_life_stages[order(unique_RTT_life_stages)]

# ----------------- get unique RTT habitat attributes -------
unique_RTT_attributes = unique(Habitat_Attribute_Scores_Okanogan$Habitat_Attribute)
unique_RTT_attributes = unique_RTT_attributes[order(unique_RTT_attributes)]

# ----------- Basic Data frame --------------------
Okanogan_Basic_Reach_Info_LF_initial = Okanogan_Habitat_Quality_Output[,c("ReachName","Assessment.Unit","Steelhead.Reach" )]

# --------------- prep life stages output data ---------
Adult_Migration_LF_Okanogan_no_Level3 = c()
Fry_LF_Okanogan_no_Level3 = c() 
Holding_and_Maturation_LF_Okanogan_no_Level3 = c()
Smolt_Outmigration_LF_Okanogan_no_Level3 = c() 
Spawning_and_Incubation_LF_Okanogan_no_Level3 = c()
Summer_Rearing_LF_Okanogan_no_Level3 = c() 
Winter_Rearing_LF_Okanogan_no_Level3 = c()

for(RTT_life_stage_x in unique_RTT_life_stages){
  
  # ---------------------- create data frame for this RTT life stage -------------
  Okanogan_Basic_Reach_Info_LF_initial_RTT = Okanogan_Basic_Reach_Info_LF_initial
  Okanogan_Basic_Reach_Info_LF_initial_RTT$'RTT Life Stage' = RTT_life_stage_x
  
  # -------------------------------- get habitat attributes for this life stage  ---------------
  habitat_attributes_life_stage_x = Attribute_LifeStage_Crosswalk[which(Attribute_LifeStage_Crosswalk$Species == "Steelhead" &
                                                                          Attribute_LifeStage_Crosswalk$`Life Stage` == RTT_life_stage_x), ]
  
  for(habitat_attribute_x in habitat_attributes_life_stage_x$`Habitat Attribute`){
    
    # ------------------------------ pull reaches with habitat attributes --------------------
    Habitat_Attribute_Scores_Okanogan_LIFE_STAGE = Habitat_Attribute_Scores_Okanogan[which(Habitat_Attribute_Scores_Okanogan$Habitat_Attribute == habitat_attribute_x), ] 
    Habitat_Attribute_Scores_Okanogan_LIFE_STAGE = Habitat_Attribute_Scores_Okanogan_LIFE_STAGE[,c("ReachName","Habitat_Attribute", "Habitat_Attribute_Score")]
    Okanogan_Basic_Reach_Info_LF_initial_RTT_output = merge(Okanogan_Basic_Reach_Info_LF_initial_RTT,Habitat_Attribute_Scores_Okanogan_LIFE_STAGE, by="ReachName", all.x=TRUE )
    
    
    if( RTT_life_stage_x == "Adult Migration" ){
      Adult_Migration_LF_Okanogan_no_Level3  = rbind(Adult_Migration_LF_Okanogan_no_Level3 , Okanogan_Basic_Reach_Info_LF_initial_RTT_output)
    }else if( RTT_life_stage_x == "Fry"  ){
      Fry_LF_Okanogan_no_Level3  = rbind(Fry_LF_Okanogan_no_Level3 , Okanogan_Basic_Reach_Info_LF_initial_RTT_output)
    }else if( RTT_life_stage_x == "Holding and Maturation"  ){
      Holding_and_Maturation_LF_Okanogan_no_Level3  = rbind(Holding_and_Maturation_LF_Okanogan_no_Level3 , Okanogan_Basic_Reach_Info_LF_initial_RTT_output)
    }else if( RTT_life_stage_x == "Smolt Outmigration"  ){
      Smolt_Outmigration_LF_Okanogan_no_Level3  = rbind(Smolt_Outmigration_LF_Okanogan_no_Level3 , Okanogan_Basic_Reach_Info_LF_initial_RTT_output)
    }else if( RTT_life_stage_x == "Spawning and Incubation"  ){
      Spawning_and_Incubation_LF_Okanogan_no_Level3  = rbind(Spawning_and_Incubation_LF_Okanogan_no_Level3 , Okanogan_Basic_Reach_Info_LF_initial_RTT_output)
    }else if( RTT_life_stage_x == "Summer Rearing"  ){
      Summer_Rearing_LF_Okanogan_no_Level3  = rbind(Summer_Rearing_LF_Okanogan_no_Level3 , Okanogan_Basic_Reach_Info_LF_initial_RTT_output)
    }else if( RTT_life_stage_x == "Winter Rearing"  ){
      Winter_Rearing_LF_Okanogan_no_Level3  = rbind(Winter_Rearing_LF_Okanogan_no_Level3 , Okanogan_Basic_Reach_Info_LF_initial_RTT_output)
    }
    
  }
  
}




# ---------------------------------------------------------------------------
#
#  Generate output that mirrors Limiting_Factor_Pathway_Steelhead_OKANOGAN[['Limiting_Factor_Pathway_Restoration']] 
#   4.June.2021: Greer and I discussed that we should try to pull Level2 -> RTT habitat attributes (for life stage)
#       and compare to original output (filter through Level 3 1s and 2s first)
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
#   Function to generate Life Stage Tables (all the habitat attributes and scores)
#
# ---------------------------------------------------------------------------

#Adult_Migration_LF_Okanogan 
#Fry_LF_Okanogan 
#Holding_and_Maturation_LF_Okanogan 
#Smolt_Outmigration_LF_Okanogan 
#Spawning_and_Incubation_LF_Okanogan 
#Summer_Rearing_LF_Okanogan 
#Winter_Rearing_LF_Okanogan 

test_x = TRUE
if(test_x){
  Limiting_Factor_Life_Stage_Table = Holding_and_Maturation_LF_Okanogan_no_Level3
  life_stage_x = "Holding and Maturation" 
}


Generate_Species_Output_Table_Okanogan_Restoration_Protection_NO_LEVEL_3 = function(life_stage_x, Limiting_Factor_Life_Stage_Table){
  
  
  # ----------------- only pull RTT habitat attributes for this life stage -----------
  Attribute_LifeStage_Crosswalk_Life_Stage = Attribute_LifeStage_Crosswalk[which(Attribute_LifeStage_Crosswalk$Species == "Steelhead" & 
                                                                                   Attribute_LifeStage_Crosswalk$`Life Stage` == life_stage_x),]
  
  # --------------------------------------------------
  #   LF Sum Calculate
  # --------------------------------------------------
  
  # ------------------------------------------------
  #  loop through reaches to calculate LF_Sum, LF_Pct, etc.
  # ------------------------------------------------
  
  LF_Scores_Output = c()
  reaches_unique_all = unique(Limiting_Factor_Life_Stage_Table$ReachName)
  
  for(reach_x in reaches_unique_all){
    
    # ------------------ reach data frame ---------------
    Limiting_Factor_Life_Stage_Table_REACH = Limiting_Factor_Life_Stage_Table[which(Limiting_Factor_Life_Stage_Table$ReachName == reach_x), ]
    
    # ------------------- pull reaches with scores ---------------
    Limiting_Factor_Life_Stage_Table_REACH_numeric = Limiting_Factor_Life_Stage_Table_REACH[ which( Limiting_Factor_Life_Stage_Table_REACH$Habitat_Attribute_Score >= 0), ]
    # -------------- pull unique habitat attributes in this Reach ----------------
    RTT_attributes_unique = unique(Limiting_Factor_Life_Stage_Table_REACH_numeric$Habitat_Attribute )
    
    combined_RTT_scores = c()
    for(RTT_attributes_unique_x in RTT_attributes_unique){
      indiv_score = mean(Limiting_Factor_Life_Stage_Table_REACH_numeric$Habitat_Attribute_Score[ Limiting_Factor_Life_Stage_Table_REACH_numeric$Habitat_Attribute ==RTT_attributes_unique_x] )
      combined_RTT_scores = c(combined_RTT_scores,indiv_score)
    }
    
    # ------------------------- LF_Sum ---------------
    LF_Sum_x = sum(combined_RTT_scores)
    
    # ------------------------- LF_Sum ---------------
    LF_Pct_x = LF_Sum_x/ (length(combined_RTT_scores) * 5)
    
    
    # --------------- combine -----------
    LF_output_x = t(as.data.frame(c(reach_x, "Okanogan","Steelhead",life_stage_x, LF_Sum_x, LF_Pct_x)))
    LF_output_x = as.data.frame(LF_output_x)
    colnames(LF_output_x) = c("ReachName","Basin","species","life_stage", "LF_Sum", "LF_Pct")
    
    # ----------------- put together 1s and 3s --------------
    # ------ start the 1s and 3s ---------
    unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes_x = c()
    # ------ pull 1s -----
    x1 = which(Limiting_Factor_Life_Stage_Table_REACH$Habitat_Attribute_Score == 1)
    if(length(x1) > 0){
      x1_attribute = paste(Limiting_Factor_Life_Stage_Table_REACH$Habitat_Attribute[x1], collapse=",")
      LF_output_x$unacceptable_1_indiv_habitat_attributes = x1_attribute
      unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes_x = x1_attribute
    }else{
      LF_output_x$unacceptable_1_indiv_habitat_attributes = ""
    }
    # ------ pull 3s -----
    x3 = which(Limiting_Factor_Life_Stage_Table_REACH$Habitat_Attribute_Score <=3 & Limiting_Factor_Life_Stage_Table_REACH$Habitat_Attribute_Score > 1)
    if(length(x3) > 0){
      x3_attribute = paste(Limiting_Factor_Life_Stage_Table_REACH$Habitat_Attribute[x3], collapse=",")
      LF_output_x$at_risk_2_or_3_indiv_habitat_attributes = x3_attribute
      
      # --------- combine 1s and 3s ---------
      if( length(unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes_x) > 0 ){
        unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes_x = paste(c(unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes_x, x3_attribute), collapse=",")
      }else{
        unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes_x = x3_attribute
      }
    }else{
      LF_output_x$at_risk_2_or_3_indiv_habitat_attributes = ""
      if( nchar(LF_output_x$unacceptable_1_indiv_habitat_attributes) == 0 ){
        unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes_x = ""
      }
    }
    # ------------ combine 1s and 3s ------------
    LF_output_x$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes =unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes_x
    
    # --------------- add to previous ------------
    LF_Scores_Output = rbind(LF_Scores_Output, LF_output_x)
    
  }
  
  #LF_Scores_Output = as.data.frame(LF_Scores_Output)
  #colnames(LF_Scores_Output) = c("ReachName", "LF_Sum", "LF_Pct")
  LF_Scores_Output$LF_Sum= as.numeric( as.character(LF_Scores_Output$LF_Sum))
  LF_Scores_Output$LF_Pct= as.numeric( as.character(LF_Scores_Output$LF_Pct))
  
  # ------------------------------------------------------------------------------------- 
  #                 calculate LF Restoration and Protection Score
  # ------------------------------------------------------------------------------------- 
  
  # ------------------------------------- Restoration ---------------------------------
  LF_Scores_Output = LF_Scores_Output  %>%
    mutate(LF_Score_Restoration = ifelse(LF_Pct  > Restoration_Scoring$Category_Lower[1] & 
                                           LF_Pct  <= Restoration_Scoring$Category_Upper[1] , Restoration_Scoring$Score[1],
                                         ifelse(LF_Pct  > Restoration_Scoring$Category_Lower[2] & 
                                                  LF_Pct  <= Restoration_Scoring$Category_Upper[2] , Restoration_Scoring$Score[2],
                                                ifelse(LF_Pct  > Restoration_Scoring$Category_Lower[3] & 
                                                         LF_Pct  <= Restoration_Scoring$Category_Upper[3] , Restoration_Scoring$Score[3],
                                                       NA))))
  
  # ------------------------------------- Protection ---------------------------------
  LF_Scores_Output = LF_Scores_Output  %>%
    mutate(LF_Score_Protection = ifelse(LF_Pct  > Protection_Scoring$Category_Lower [1] & 
                                          LF_Pct  <= Protection_Scoring$Category_Upper[1] , Protection_Scoring$Score[1],
                                        ifelse(LF_Pct  > Protection_Scoring$Category_Lower[2] & 
                                                 LF_Pct  <= Protection_Scoring$Category_Upper[2] , Protection_Scoring$Score[2],
                                               ifelse(LF_Pct  > Protection_Scoring$Category_Lower[3] & 
                                                        LF_Pct  <= Protection_Scoring$Category_Upper[3] , Protection_Scoring$Score[3],
                                                      NA))))
  
  
  # -------------- re-organize columns ---------
  LF_Scores_Output = LF_Scores_Output[,c(1,2,3,4,5,6,10,11,7,8,9)]
  
  
  return(LF_Scores_Output)
  
}

# -------------------------------------------------------------------------
#    Combine all the life stage output 
# ----------------------------------------------------------------------

# Limiting_Factor_Pathway_Steelhead_OKANOGAN_no_level3
Limiting_Factor_Pathway_Steelhead_OKANOGAN_no_level3_NO_Filters = rbind(
  Generate_Species_Output_Table_Okanogan_Restoration_Protection_NO_LEVEL_3("Adult Migration",  Adult_Migration_LF_Okanogan_no_Level3 ),
  Generate_Species_Output_Table_Okanogan_Restoration_Protection_NO_LEVEL_3("Fry",  Fry_LF_Okanogan_no_Level3 ),
  Generate_Species_Output_Table_Okanogan_Restoration_Protection_NO_LEVEL_3("Holding and Maturation",  Holding_and_Maturation_LF_Okanogan_no_Level3 ),
  Generate_Species_Output_Table_Okanogan_Restoration_Protection_NO_LEVEL_3("Smolt Outmigration",  Smolt_Outmigration_LF_Okanogan_no_Level3 ),
  Generate_Species_Output_Table_Okanogan_Restoration_Protection_NO_LEVEL_3("Spawning and Incubation",  Spawning_and_Incubation_LF_Okanogan_no_Level3 ),
  Generate_Species_Output_Table_Okanogan_Restoration_Protection_NO_LEVEL_3("Summer Rearing",  Summer_Rearing_LF_Okanogan_no_Level3 ),
  Generate_Species_Output_Table_Okanogan_Restoration_Protection_NO_LEVEL_3("Winter Rearing",  Winter_Rearing_LF_Okanogan_no_Level3 )
)

# -------------------------------------------------------------------------
#    Filter out for Protection and Restoration
# ----------------------------------------------------------------------

