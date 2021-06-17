


# ---------------------------------------------------------------------------
#
#      SCRIPT:  Habitat Quality Pathway FILTER for OKANOGAN
#
#      R Project to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------


# ------------------------------------------------------------------------------
#
#        Okanogan Habitat Quality Filter
#
# ------------------------------------------------------------------------------


#  to test
test_x = FALSE
if(test_x){
  species = "Steelhead"
  colnames_HQ_output = colnames(Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']])
  colnames_HQ_habitat_attributes = colnames_HQ_output[7:19]
}

# HQ_Pct above 0.8:  Ninemile 16-5 (not in an AU Protection ), 	 Salmon 16-11 (3 life stages (need to have more than 3)), Tonasket 16-2 (passed through)
Generate_Habitat_Quality_Output_Table_Okanogan = function( species, colnames_HQ_output, colnames_HQ_habitat_attributes ){
  
  
  # ------------------------------------------------------------------------------
  #       Establish species-specific variable names
  # ------------------------------------------------------------------------------
  print(paste("---------------------------------- START HQ Pathway FOR: ",species, sep=""))
  if(species == "Steelhead"){
    # ---------------- species reach ---------------
    species_reach = 'Steelhead.Reach'
    # ---------------- species AU Rank ----------
    AU_rank_name_restoration = 'AU Restoration Rank'
    # ---------------- species AU Rank PROTECTION ----------
    AU_rank_name_protection = 'AU Protection Rank'
    # --------------- Life stage Sum column name ----------
    life_stage_sum_column = 'SH_Life_Stage_Sum'
    # ------------------- output names ----------------------
    restoration_output_name = paste(paste("Steelhead_Habitat_Quality_RESTORATION_Okanogan" , ".xlsx", sep="") )
    protection_output_name = paste(paste("Steelhead_Habitat_Quality_PROTECTION_Okanogan" ,".xlsx", sep=""))
  }else{
    print('Incorrectly entered species name - re-type species name')
    
  }
  
  
  #  ---------------------------------------------------------------------------------
  #           Create AU Ranks data frame
  #  ---------------------------------------------------------------------------------
  Species_AU_Ranks_data = AU_Ranks_Okanogan
  
  #  ---------------------------------------------------------------------------------
  #           Establish Reach Information Data Frame Just for this Output
  #  ---------------------------------------------------------------------------------
  # -------- Reach Information data frame for this species -----------
  Species_Reach_Information_data = Reach_Information_data

  # --------------------- Filter by Okanogan -------------
  Species_Reach_Information_data = Species_Reach_Information_data[which(Species_Reach_Information_data$Basin == "Okanogan"),]
  # --------------- combined all data -------------
  Output_All_Combined  = Species_Reach_Information_data[,c("ReachName","Basin","Assessment.Unit")]
  
  print(paste("Total Initial Reaches (HQ Pathway): ", nrow(Species_Reach_Information_data), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to only have Species-specific reaches   
  #  ---------------------------------------------------------------------------------
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_Reach_Information_data$Species_Reaches = Species_Reach_Information_data[species_reach]
  Species_Reach_Information_data_all = Species_Reach_Information_data
  # ----------------------- filter out for only reaches with this species --------------
  Species_Reach_Information_data = Species_Reach_Information_data %>%  
    filter(Species_Reaches   == 'yes')
  # ----------- add to combined ----------
  Species_Reach_Information_data_merge = Species_Reach_Information_data_all[,c("ReachName", species_reach)]
  Output_All_Combined = merge(Output_All_Combined, Species_Reach_Information_data_merge, by="ReachName", all.x=TRUE)
    
  print(paste("Total reaches after species-reach filter: ", nrow(Species_Reach_Information_data), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to select for AU rank - RESTORATION
  #  ---------------------------------------------------------------------------------
  
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_AU_Ranks_data["Species_AU_Ranks"] = Species_AU_Ranks_data[AU_rank_name_restoration]
  # ----------------------- filter out for only reaches with this species --------------
  Species_AU_Ranks_data_restoration = Species_AU_Ranks_data %>%  
    filter(Species_AU_Ranks    %in%   AU_Rank)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Species_Reach_Information_data_restoration = Species_Reach_Information_data %>%  
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data_restoration$`EDT AU`)
  
  # ----------- add to combined ----------
  AU_ranks_for_combo = Species_AU_Ranks_data[,c("EDT AU","AU Restoration Rank")]
  colnames(AU_ranks_for_combo)[1] = "Assessment.Unit"
  Output_All_Combined = merge(Output_All_Combined, AU_ranks_for_combo, by="Assessment.Unit", all.x=TRUE)
  
  print(paste("Restoration - total AU rank filter: ", nrow(Species_Reach_Information_data_restoration), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #            Filter out to select for AU rank - PROTECTION
  #  ---------------------------------------------------------------------------------
  
  # -------------------- add additional column for this particular species reach presence ---------------
  Species_AU_Ranks_data["Species_AU_Ranks"] = Species_AU_Ranks_data[AU_rank_name_protection]
  # ----------------------- filter out for only reaches with this species --------------
  Species_AU_Ranks_data_protection = Species_AU_Ranks_data %>%  
    filter(Species_AU_Ranks    %in%   AU_Rank)
  # ------------------------ identify after AUs that pass this filter in reach-based table ----------
  Species_Reach_Information_data_protection = Species_Reach_Information_data %>%  
    filter(Assessment.Unit    %in%   Species_AU_Ranks_data_protection$`EDT AU`)
  
  # ----------- add to combined ----------
  AU_ranks_for_combo = Species_AU_Ranks_data[,c("EDT AU","AU Protection Rank")]
  colnames(AU_ranks_for_combo)[1] = "Assessment.Unit"
  Output_All_Combined = merge(Output_All_Combined, AU_ranks_for_combo, by="Assessment.Unit", all.x=TRUE)
  
  
  print(paste("Protection - total after Protection AU rank filter: ", nrow(Species_Reach_Information_data_protection), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #           Reach Confinement (NOTE - only used for Restoration, not for Protection)
  #  ---------------------------------------------------------------------------------
  
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  Confinement_Scores_Restoration = Confinement_Scores %>%  
    filter(Score   ==   Reach_Confinement_SCORE_Criteria)
  # ------------------------ identify AUs that pass this filter in reach-based table ----------
  Species_Reach_Information_data_restoration = Species_Reach_Information_data_restoration %>%  
    filter(ReachName   %in%   Confinement_Scores_Restoration$`ReachName`)
  
  # ----------- add to combined ----------
  Confinement_Scores_for_combo = Confinement_Scores[,c("ReachName","Score")]
  colnames(Confinement_Scores_for_combo) = c("ReachName","Confinement_Score")
  Output_All_Combined = merge(Output_All_Combined, Confinement_Scores_for_combo, by="ReachName", all.x=TRUE)
  
  print(paste("HQ Pathway-RESTORATION - total reaches after reach confinement filter: ", nrow(Species_Reach_Information_data_restoration), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #           Number of Life Stages Filter 
  #  ---------------------------------------------------------------------------------
  
  # -------------------- add additional column for this particular species reach presence ---------------
  Life_Stage_Priorities_AU_and_Reach_data["Life_Stage_Sum_Column"] = Life_Stage_Priorities_AU_and_Reach_data[life_stage_sum_column]
  # ----------------------- filter out for Habitat_Quality_Scores reaches with Habitat Quality Score criteria --------------
  Life_Stage_Priorities_AU_and_Reach_data_FILTERED = Life_Stage_Priorities_AU_and_Reach_data %>%  
    filter(Life_Stage_Sum_Column   >=   Sum_Life_Stage_Criteria)
  
  # ----------- add to combined ----------
  Life_Stage_for_combo = Life_Stage_Priorities_AU_and_Reach_data[,c("ReachName",life_stage_sum_column)]
  Output_All_Combined = merge(Output_All_Combined, Life_Stage_for_combo, by="ReachName", all.x=TRUE)
  
  # ------------------------ identify reaches that pass through the RESTORATION filter ----------
  Habitat_Quality_Pathway_Restoration = Species_Reach_Information_data_restoration %>%  
    filter(ReachName   %in%   Life_Stage_Priorities_AU_and_Reach_data_FILTERED$`ReachName`)
  # ------------------------ identify reaches that pass through the PROTECTION filter ----------
  Habitat_Quality_Pathway_Protection = Species_Reach_Information_data_protection %>%  
    filter(ReachName   %in%   Life_Stage_Priorities_AU_and_Reach_data_FILTERED$`ReachName`)
  
  print(paste("HQ Pathway-RESTORATION - total reaches after life stages number filter: ", nrow(Habitat_Quality_Pathway_Restoration), sep=""))
  
  print(paste("HQ Pathway-PROTECTION - total reaches after life stages number filter: ", nrow(Habitat_Quality_Pathway_Protection), sep=""))
  
  
  #  ---------------------------------------------------------------------------------
  #            RESTORATION = Filter out for AU-level % Habitat Quality (EDT = "% Template")
  #  ---------------------------------------------------------------------------------
  # Filter for <80% habitat function using %HabitatQuality Tab 
  # ------------------- filter out % of Template that is below certain score --------------------
  PRCNT_Habitat_Quality_Okanogan_EDT_FILTER_RESTORATION = PRCNT_Habitat_Quality_Okanogan_EDT[which(PRCNT_Habitat_Quality_Okanogan_EDT$HQ_Score <= PRCNT_of_Template_Restoration_Score), ]
  
  # ------------------------ identify reaches that pass through the RESTORATION filter ----------
  Habitat_Quality_Pathway_Restoration = Habitat_Quality_Pathway_Restoration %>%  
    filter(ReachName   %in%   PRCNT_Habitat_Quality_Okanogan_EDT_FILTER_RESTORATION$`ReachName`)
  
  # ----------- add to combined ----------
  PRCNT_HQ_merge = PRCNT_Habitat_Quality_Okanogan_EDT[,c("ReachName","HQ_Score")]
  Output_All_Combined = merge(Output_All_Combined, PRCNT_HQ_merge, by="ReachName", all.x=TRUE)
  
  print(paste("HQ Pathway-RESTORATION - total reaches after life stages number filter: ", nrow(Habitat_Quality_Pathway_Restoration), sep=""))
  
  #  ---------------------------------------------------------------------------------
  #            PROTECTION = Filter out for AU-level % Habitat Quality (EDT = "% Template")
  #  ---------------------------------------------------------------------------------
  # Filter for <80% habitat function using %HabitatQuality Tab 
  # ------------------- filter out % of Template that is below certain score --------------------
  PRCNT_Habitat_Quality_Okanogan_EDT_FILTER_PROTECTION = PRCNT_Habitat_Quality_Okanogan_EDT[which(PRCNT_Habitat_Quality_Okanogan_EDT$HQ_Score >= PRCNT_of_Template_Protection_Score), ]
  
  # ------------------------ identify reaches that pass through the PROTECTION filter ----------
  Habitat_Quality_Pathway_Protection = Habitat_Quality_Pathway_Protection %>%  
    filter(ReachName   %in%   PRCNT_Habitat_Quality_Okanogan_EDT_FILTER_PROTECTION$`ReachName`)
  
  print(paste("HQ Pathway-PROTECTION - total reaches after life stages number filter: ", nrow(Habitat_Quality_Pathway_Protection), sep=""))
  
  # -------------------------------------------------------
  #     Combine ALL data output with Okanogan_Habitat_Quality_Output (HQ table)
  # ------------------------------------------------------- 
  Habitat_Quality_Scores_Okanogan_merge = Habitat_Quality_Scores_Okanogan[, -which(names(Habitat_Quality_Scores_Okanogan) %in% c("Assessment.Unit"))]
  # merged with all habitat attributes in Habitat_Attribute_Scores_Okanogan instead
  #Output_All_Combined = merge(Output_All_Combined, Habitat_Quality_Scores_Okanogan_merge, by="ReachName", all.x=TRUE)

  # -------------------------------------------------------
  #     Output Scores (if reaches generated)
  # ------------------------------------------------------- 
  
  # ------- IF there are still reaches that got past filters --------
  if( nrow(Habitat_Quality_Pathway_Restoration) > 0 ){
    
    # ----------------------------------------------------------------
    #     Loop through each reach to get HQ Scores AND impaired attributes
    # ----------------------------------------------------------------
    Habitat_Quality_Scores_Okanogan_for_Restoration = c()
    
    for(reaches_x in Habitat_Quality_Pathway_Restoration$ReachName){
      
      # ----------------------------------------------------
      #   Pull Habitat_Quality_Scores_Okanogan_reach_x data for this reach
      # ----------------------------------------------------
      x = which(Habitat_Quality_Scores_Okanogan$ReachName == reaches_x)
      Habitat_Quality_Scores_Okanogan_reach_x = Habitat_Quality_Scores_Okanogan[x,]
      
      # ----------------------------------------------------
      #   Get individual habitat attributes
      # ----------------------------------------------------
      # --------------- Unacceptable (1s) --------------
      x_1 = which(Habitat_Quality_Scores_Okanogan_reach_x[,colnames_HQ_habitat_attributes] == 1)
      unacceptable_1_indiv_habitat_attributes_x = colnames_HQ_habitat_attributes[x_1]
      Habitat_Quality_Scores_Okanogan_reach_x$unacceptable_1_indiv_habitat_attributes = paste(unacceptable_1_indiv_habitat_attributes_x, collapse=",")
      # --------------- At Risk (3s) --------------
      x_3 = which(Habitat_Quality_Scores_Okanogan_reach_x[,colnames_HQ_habitat_attributes]  <= 3  &
                    Habitat_Quality_Scores_Okanogan_reach_x[,colnames_HQ_habitat_attributes] > 1 )
      at_risk_2_or_3_indiv_habitat_attributes_x = colnames_HQ_habitat_attributes[x_3]
      Habitat_Quality_Scores_Okanogan_reach_x$at_risk_2_or_3_indiv_habitat_attributes = paste(at_risk_2_or_3_indiv_habitat_attributes_x, collapse=",")
      # --------------- Unacceptable (1s) or At Risk (3s) --------------
      x_1_3 = which(Habitat_Quality_Scores_Okanogan_reach_x[,colnames_HQ_habitat_attributes]  <= 3  )
      unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes_x = colnames_HQ_habitat_attributes[x_1_3]
      Habitat_Quality_Scores_Okanogan_reach_x$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes = paste(unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes_x, collapse=",")
      
      # ------ only move forward reaches with impaired habitat attributes -------
      if( length(x_1) > 0 | length(x_3) > 0 | length(x_1_3) > 0 ){
        # ------------ combine ------
        Habitat_Quality_Scores_Okanogan_for_Restoration  = rbind(Habitat_Quality_Scores_Okanogan_for_Restoration,Habitat_Quality_Scores_Okanogan_reach_x )
      }
      
     }
    
    
    
    print(paste("--- Restoration - TOTAL reaches after habitat attributes filter: ", length(unique(Habitat_Quality_Scores_Okanogan_for_Restoration$ReachName)), sep=""))
    
    # ----------------------------------------------
    #    prep output for AL Reaches
    # --------------------------------------
    
    Habitat_Attribute_Scores_Okanogan_FILTERED_ALL = Habitat_Attribute_Scores_Okanogan %>%  
      filter(ReachName   %in%   Output_All_Combined$ReachName)
    # --------------- just pull the habitat attributes in RTT list ---------
    #Habitat_Attribute_Scores_Okanogan_FILTERED_ALL = Habitat_Attribute_Scores_Okanogan_FILTERED_ALL %>%  
    #  filter(Habitat_Attribute   %in%   colnames_restoration_x)
    # --------------- only pull 1s (Unacceptable) and 3s (At Risk) ------------
    Habitat_Attribute_Scores_Okanogan_FILTERED_ALL$Habitat_Attribute_Score = as.numeric(as.character(Habitat_Attribute_Scores_Okanogan_FILTERED_ALL$Habitat_Attribute_Score))
 
    # ------------- add to output --------
    all_habitat_attributes = unique(Habitat_Attribute_Scores_Okanogan_FILTERED_ALL$Habitat_Attribute)[order(unique(Habitat_Attribute_Scores_Okanogan_FILTERED_ALL$Habitat_Attribute))]
    for(habitat_attribute_x in all_habitat_attributes){
      
      if(habitat_attribute_x == "Brook Trout"){next} # we don't do Brook Trout
      # ----------- pull data for this habitat attribute ---------
      habitat_attribute_x_data_frame = Habitat_Attribute_Scores_Okanogan_FILTERED_ALL[Habitat_Attribute_Scores_Okanogan_FILTERED_ALL$Habitat_Attribute == habitat_attribute_x,c("ReachName","Habitat_Attribute_Score")]
      # -------------- rename column name ---------
      colnames(habitat_attribute_x_data_frame)[2] = habitat_attribute_x
      # ------------ merge with Output_All_Combined ------
      Output_All_Combined = merge( Output_All_Combined,  habitat_attribute_x_data_frame, by="ReachName", )
    }
    
    # ---------- write all the output ------------
    write.xlsx(Output_All_Combined, file=paste(output_path,"Habitat_Quality_Scores_Filters_Okanogan.xlsx",sep=""),  row.names=FALSE)
    
    #  ---------------------------------------------------------------------------------
    #           Write output data to output file
    #  ---------------------------------------------------------------------------------
    
    # ----------------------- Restoration --------------------
    output_path_x =  paste(output_path,restoration_output_name, sep="")
    write_xlsx(Habitat_Quality_Scores_Okanogan_for_Restoration, output_path_x)
    
  }else{
    
    print(paste("--- No Restoration Reaches generated for species: ", species, sep=""))
    
  }
  
  
  # -------------------------------------------------------
  #     Output Protection Scores (if reaches generated)
  # ------------------------------------------------------- 
  
  if( nrow(Habitat_Quality_Pathway_Protection) > 0 ){
    
    
    # -------------------------------------------------------
    #         Pull Habitat_Quality_Scores_Okanogan row for each Protection reach
    # -------------------------------------------------------
    Habitat_Quality_Scores_Okanogan_for_Protection = c()
    # ------------------ pull habitat attributes in protection reaches ----------
    for(reach_x in Habitat_Quality_Pathway_Protection$ReachName){
      # ----------------------------------------------------
      #   Pull Habitat_Quality_Scores_Okanogan_reach_x data for this reach
      # ----------------------------------------------------
      x = which(Habitat_Quality_Scores_Okanogan$ReachName == reach_x)
      Habitat_Quality_Scores_Okanogan_reach_x = Habitat_Quality_Scores_Okanogan[x,]
      
      Habitat_Quality_Scores_Okanogan_for_Protection = rbind(Habitat_Quality_Scores_Okanogan_for_Protection,
                                                             Habitat_Quality_Scores_Okanogan_reach_x )
      
    }
    
    print(paste("--- Protection - TOTAL reaches after habitat attributes filter: ", nrow(Habitat_Quality_Scores_Okanogan_for_Protection), sep=""))
    
    #  ---------------------------------------------------------------------------------
    #           Write output data to output file
    #  ---------------------------------------------------------------------------------
    
    # ----------------------- Protection --------------------
    output_path_x =  paste(output_path,protection_output_name, sep="")
    write_xlsx(Habitat_Quality_Scores_Okanogan_for_Protection,output_path_x )
    
  }else{
    
    print(paste("--- No Protection Reaches generated for species: ", species, sep=""))
    
  }
  
  # --------------------- Put Restoration and Protection into a list --------------
  Habitat_Quality_Pathway_Output = list( 
    "Habitat_Quality_Pathway_Restoration" = Habitat_Quality_Scores_Okanogan_for_Restoration,
    "Habitat_Quality_Pathway_Protection" = Habitat_Quality_Scores_Okanogan_for_Protection
  )
  
  return(Habitat_Quality_Pathway_Output)
  
}

#  ---------------------------------------------------------------------------------
#          FUNCTION: Pull Unacceptable and At Risk Habitat Attributes
#                     and prepare row to merge with other HQ output
#  ---------------------------------------------------------------------------------

# ------------------ Function to list all the rows below individual habitat criteria -------------------------
# ----- to test -----
test_x = FALSE
if(test_x){
  reach_okanogan_data_frame = habitat_attributes_output_x
  colnames_x = unique(HabitatAttribute_Ratings_Level2_updated$`RTT Habitat Attribute`)
  colnames_x = colnames_x[order(colnames_x)]
}


list_indiv_habitat_attributes_low_FUNCTION_OKANOGAN_UPDATED <- function(reach_okanogan_data_frame, colnames_x){
  
  # ------------------ create the row with all the attributes ---------------
  #  rows have column names that include that habitat attributes AND the scores 
  row_x = as.data.frame(c(  Reach_Information_data[which(Reach_Information_data$ReachName == reach_okanogan_data_frame$Reach[1]),1:6], 
                            rep(NA, length(colnames_x) + 3)  ) )
  colnames(row_x)[7:(length(colnames_x) + 6)] = colnames_x 
  colnames(row_x)[(length(colnames_x) + 7):ncol(row_x) ] = c("unacceptable_1_indiv_habitat_attributes" , "at_risk_2_or_3_indiv_habitat_attributes", 
                                                             "unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes"  )
  
  for(attribute_x in colnames_x){
    
    # ---------- pull habitat attribute ----------------
    habitat_attribute_x_i = which(reach_okanogan_data_frame$`RTT Habitat Attribute` == attribute_x)
    
    # -------- if multiple RTT habitat attributes for a single EDT habitat attribute -----------
    if( length(habitat_attribute_x_i) > 1){
      # ----------------- get lowest Function Condition (habitat attribute) score ------------
      score_x = min(reach_okanogan_data_frame$`Level 2 Functional Condition`[habitat_attribute_x_i])
      # row_x_i = which( reach_okanogan_data_frame$Function_Condition[habitat_attribute_x_i] == score_x )
      
      # ------------- if RTT and EDT attributes are 1:1 (one RTT attribute for one EDT attribute) -----
    }else{
      score_x = min(reach_okanogan_data_frame$`Level 2 Functional Condition`[habitat_attribute_x_i])
    }
    
    # -------------- insert score in correct column --------------
    row_x[,attribute_x] = score_x
    
    # ------------------- insert name in correct column -------------------
    if(score_x == 1){
      # --------------- insert habitat name --------
      row_x$unacceptable_1_indiv_habitat_attributes = paste(row_x$unacceptable_1_indiv_habitat_attributes, attribute_x, sep=",")
      row_x$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes = paste(row_x$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes, attribute_x, sep=",")
      # -------- strip white space from names -------------
      row_x$unacceptable_1_indiv_habitat_attributes = gsub("\\s", "", row_x$unacceptable_1_indiv_habitat_attributes)  
      row_x$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes = gsub("\\s", "", row_x$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes)  
    }
    
    if(score_x > 1 & score_x <= 3){
      # --------------- insert habitat name --------
      row_x$at_risk_2_or_3_indiv_habitat_attributes = paste(row_x$at_risk_2_or_3_indiv_habitat_attributes, attribute_x, sep=",")
      row_x$at_risk_2_or_3_indiv_habitat_attributes = gsub("\\s", "", row_x$at_risk_2_or_3_indiv_habitat_attributes)  
    }
    
  }
  
  # ----------------- remove leading NAs  -------------------
  row_x[,'unacceptable_1_indiv_habitat_attributes'] = gsub('NA,','',row_x[,'unacceptable_1_indiv_habitat_attributes'])
  row_x[,'at_risk_2_or_3_indiv_habitat_attributes'] = gsub('NA,','',row_x[,'at_risk_2_or_3_indiv_habitat_attributes'])
  row_x[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'] = gsub('NA,','',row_x[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'])
  
  # ------------------ change habitat attribute score names so they match MetEntWen output -----------
  # ------- remove blank space ------
  colnames(row_x)[7:(ncol(row_x)-3)] = gsub("\\s", "", colnames(row_x)[7:(ncol(row_x)-3)]) 
  # ---------- add "score" to end ------
  colnames(row_x)[7:(ncol(row_x)-3)] = paste(colnames(row_x)[7:(ncol(row_x)-3)], "_score", sep="") 
  
  # ------- output -----------
  return( row_x )
  
}
#  ---------------------------------------------------------------------------------
#       (ORIGINAL)   FUNCTION: Pull Unacceptable and At Risk Habitat Attributes
#                     and prepare row to merge with other HQ output
#  ---------------------------------------------------------------------------------

# ------------------ Function to list all the rows below individual habitat criteria -------------------------
# ----- to test -----
test_x = FALSE
if(test_x){
  reach_okanogan_data_frame = habitat_attributes_output_x
  colnames_x = unique(HabitatAttribute_Ratings_Level2_updated$`RTT Habitat Attribute`)
  colnames_x = colnames_x[order(colnames_x)]
}


list_indiv_habitat_attributes_low_FUNCTION_OKANOGAN_ORIGINAL <- function(reach_okanogan_data_frame, colnames_x){
  
  # ------------------ create the row with all the attributes ---------------
  #  rows have column names that include that habitat attributes AND the scores 
  row_x = as.data.frame(c(  Reach_Information_data[which(Reach_Information_data$ReachName == reach_okanogan_data_frame$Reach[1]),1:6], 
                            rep(NA, length(colnames_x) + 3)  ) )
  colnames(row_x)[7:(length(colnames_x) + 6)] = colnames_x 
  colnames(row_x)[(length(colnames_x) + 7):ncol(row_x) ] = c("unacceptable_1_indiv_habitat_attributes" , "at_risk_2_or_3_indiv_habitat_attributes", 
                                                             "unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes"  )
  
  for(attribute_x in colnames_x){
    
    # ---------- pull habitat attribute ----------------
    habitat_attribute_x_i = which(reach_okanogan_data_frame$`RTT Habitat Attribute` == attribute_x)
    
    # -------- if multiple RTT habitat attributes for a single EDT habitat attribute -----------
    if( length(habitat_attribute_x_i) > 1){
      # ----------------- get lowest Function Condition (habitat attribute) score ------------
      score_x = min(reach_okanogan_data_frame$`Level 2 Functional Condition`[habitat_attribute_x_i])
      # row_x_i = which( reach_okanogan_data_frame$Function_Condition[habitat_attribute_x_i] == score_x )
      
      # ------------- if RTT and EDT attributes are 1:1 (one RTT attribute for one EDT attribute) -----
    }else{
      score_x = min(reach_okanogan_data_frame$`Level 2 Functional Condition`[habitat_attribute_x_i])
    }
    
    # -------------- insert score in correct column --------------
    row_x[,attribute_x] = score_x
    
    # ------------------- insert name in correct column -------------------
    if(score_x == 1){
      # --------------- insert habitat name --------
      row_x$unacceptable_1_indiv_habitat_attributes = paste(row_x$unacceptable_1_indiv_habitat_attributes, attribute_x, sep=",")
      row_x$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes = paste(row_x$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes, attribute_x, sep=",")
      # -------- strip white space from names -------------
      row_x$unacceptable_1_indiv_habitat_attributes = gsub("\\s", "", row_x$unacceptable_1_indiv_habitat_attributes)  
      row_x$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes = gsub("\\s", "", row_x$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes)  
    }
    
    if(score_x > 1 & score_x <= 3){
      # --------------- insert habitat name --------
      row_x$at_risk_2_or_3_indiv_habitat_attributes = paste(row_x$at_risk_2_or_3_indiv_habitat_attributes, attribute_x, sep=",")
      row_x$at_risk_2_or_3_indiv_habitat_attributes = gsub("\\s", "", row_x$at_risk_2_or_3_indiv_habitat_attributes)  
    }
    
  }
  
  # ----------------- remove leading NAs  -------------------
  row_x[,'unacceptable_1_indiv_habitat_attributes'] = gsub('NA,','',row_x[,'unacceptable_1_indiv_habitat_attributes'])
  row_x[,'at_risk_2_or_3_indiv_habitat_attributes'] = gsub('NA,','',row_x[,'at_risk_2_or_3_indiv_habitat_attributes'])
  row_x[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'] = gsub('NA,','',row_x[,'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'])
  
  # ------------------ change habitat attribute score names so they match MetEntWen output -----------
  # ------- remove blank space ------
  colnames(row_x)[7:(ncol(row_x)-3)] = gsub("\\s", "", colnames(row_x)[7:(ncol(row_x)-3)]) 
  # ---------- add "score" to end ------
  colnames(row_x)[7:(ncol(row_x)-3)] = paste(colnames(row_x)[7:(ncol(row_x)-3)], "_score", sep="") 
  
  # ------- output -----------
  return( row_x )
  
}

#  ---------------------------------------------------------------------------------
#
#   FUNCTION to combine main (Methow, Entiat, and Wenatchee) and Okanogan HQ Output - RESTORATION
#
#  ---------------------------------------------------------------------------------
test_x = FALSE
if(test_x){
  MetEntWen_data_frame = Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']]
  Okanogan_data_frame = Habitat_Quality_Pathway_Steelhead_OKANOGAN[['Habitat_Quality_Pathway_Restoration']]
}

Combine_MetEntWen_and_Okanogan_Habitat_Quality_Output = function(MetEntWen_data_frame,   Okanogan_data_frame,  habitat_quality_scores_colnames_for_combo){
  
  # -------------- update "Riparian_Mean" name ---------------
  x = which(habitat_quality_scores_colnames_for_combo == "Riparian_Mean")
  habitat_quality_scores_colnames_for_combo[x] = "Riparian_Mean_score"
  # ----------- update Okanogan name ---------
  x = which(colnames(Okanogan_data_frame) == "Riparian_score")
  colnames(Okanogan_data_frame)[x] = "Riparian_Mean_score"
  
  # -------------- start with basic info --------------
  df_add_to_MetEntWen = Okanogan_data_frame[,1:6]
  
  # ---------------- loop through all the habitat attributes ----------
  for(colx in habitat_quality_scores_colnames_for_combo){
    
    # ------- if column is in Okanogan data -----
    if( any(colnames(Okanogan_data_frame) == colx) ){
      df_add_to_MetEntWen[,colx] = Okanogan_data_frame[,colx]
      print(colx)
      # ------- if column not in Okanogan data ------
    }else{
      #print(colx)
      df_add_to_MetEntWen[,colx] = NA
    }
  }
  
  # --------------- HQ_Sum is NA (for now) -----------
  df_add_to_MetEntWen$HQ_Sum = NA
  
  # ----------------- add HQ_Pct, HQ Restoration and Protection, and unacceptable/at risk attributes ----
  # ---------- truncate the HQ Score data frame -----------
  PRCNT_Habitat_Quality_Okanogan_EDT_Truncated = PRCNT_Habitat_Quality_Okanogan_EDT[,c("ReachName","HQ_Score")]
  # -------- merge HQ Score -----
  df_add_to_MetEntWen = merge(df_add_to_MetEntWen, PRCNT_Habitat_Quality_Okanogan_EDT_Truncated, by="ReachName")
  colnames(df_add_to_MetEntWen)[length(colnames(df_add_to_MetEntWen))] = "HQ_Pct"
  
  # ------------------------------ HQ Restoration Score ------------------------
  df_add_to_MetEntWen = df_add_to_MetEntWen  %>%
    mutate(HQ_Score_Restoration = ifelse(HQ_Pct  > Restoration_Scoring$Category_Lower[1] & 
                                           HQ_Pct  < Restoration_Scoring$Category_Upper[1] , Restoration_Scoring$Score[1],
                                         ifelse(HQ_Pct  >= Restoration_Scoring$Category_Lower[2] & 
                                                  HQ_Pct  <= Restoration_Scoring$Category_Upper[2] , Restoration_Scoring$Score[2],
                                                ifelse(HQ_Pct  > Restoration_Scoring$Category_Lower[3] & 
                                                         HQ_Pct  <= Restoration_Scoring$Category_Upper[3] , Restoration_Scoring$Score[3],
                                                       NA))))
  # ------------------------------ HQ Protection Score ------------------------
  df_add_to_MetEntWen = df_add_to_MetEntWen  %>%
    mutate(HQ_Score_Protection = ifelse(HQ_Pct  > Protection_Scoring$Category_Lower [1] & 
                                          HQ_Pct  < Protection_Scoring$Category_Upper[1] , Protection_Scoring$Score[1],
                                        ifelse(HQ_Pct  >= Protection_Scoring$Category_Lower[2] & 
                                                 HQ_Pct  <= Protection_Scoring$Category_Upper[2] , Protection_Scoring$Score[2],
                                               ifelse(HQ_Pct  > Protection_Scoring$Category_Lower[3] & 
                                                        HQ_Pct  <= Protection_Scoring$Category_Upper[3] , Protection_Scoring$Score[3],
                                                      NA))))
  
  # --------------------- add At Risk and Unnacceptable habitat attributes -----------------
  df_add_to_MetEntWen[,colnames(Okanogan_data_frame)[(length(colnames(Okanogan_data_frame))-2):length(colnames(Okanogan_data_frame))]] =
    Okanogan_data_frame[,colnames(Okanogan_data_frame)[(length(colnames(Okanogan_data_frame))-2):length(colnames(Okanogan_data_frame))]]
  
  # ---------------- update Riparian column name -----------
  x = which(colnames(df_add_to_MetEntWen) == "Riparian_Mean_score")
  colnames(df_add_to_MetEntWen)[x] = "Riparian_Mean"
  
  # ----------------- combine prepared Okanogan data frame with MetEntWen data frame -----
  MetEntWen_data_frame_updated = rbind(MetEntWen_data_frame,  df_add_to_MetEntWen)
  
  # ------------------ Return ---------------
  return(MetEntWen_data_frame_updated)
}



#  ---------------------------------------------------------------------------------
#
#   FUNCTION to combine main (Methow, Entiat, and Wenatchee) and Okanogan HQ Output - PROTECTION
#
#  ---------------------------------------------------------------------------------


test_x = FALSE
if(test_x){
  MetEntWen_data_frame = Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Protection']]
  Okanogan_data_frame = Habitat_Quality_Pathway_Steelhead_OKANOGAN[['Habitat_Quality_Pathway_Protection']]  
}

Combine_MetEntWen_and_Okanogan_Habitat_Quality_Output_Protection = function(MetEntWen_data_frame,   Okanogan_data_frame,  habitat_quality_scores_colnames_for_combo){
  
  # -------------- update "Riparian_Mean" name ---------------
  x = which(habitat_quality_scores_colnames_for_combo == "Riparian_Mean")
  habitat_quality_scores_colnames_for_combo[x] = "Riparian_Mean_score"
  # ----------- update Okanogan name ---------
  x = which(colnames(Okanogan_data_frame) == "Riparian_score")
  colnames(Okanogan_data_frame)[x] = "Riparian_Mean_score"
  
  # -------------- start with basic info --------------
  df_add_to_MetEntWen = Okanogan_data_frame[,1:6]
  
  # ---------------- loop through all the habitat attributes ----------
  for(colx in habitat_quality_scores_colnames_for_combo){
    # ------- if column is in Okanogan data -----
    if( any(colnames(Okanogan_data_frame) == colx) ){
      df_add_to_MetEntWen[,colx] = Okanogan_data_frame[,colx]
      # ------- if column not in Okanogan data ------
      print(colx)
    }else{
      
      df_add_to_MetEntWen[,colx] = NA
    }
  }
  
  # NOTE - for Habitat Quality Score - it is not "added up", but directly imported from EDT tab "%HabitatQuality" (based on % Template parameter in EDT)
  
  # --------------- HQ_Sum is NA (for now) -----------
  df_add_to_MetEntWen$HQ_Sum = NA
  
  # ----------------- add HQ_Pct, HQ Restoration and Protection, and unacceptable/at risk attributes ----
  # ---------- truncate the HQ Score data frame -----------
  PRCNT_Habitat_Quality_Okanogan_EDT_Truncated = PRCNT_Habitat_Quality_Okanogan_EDT[,c("ReachName","HQ_Score")]
  # -------- merge HQ Score -----
  df_add_to_MetEntWen = merge(df_add_to_MetEntWen, PRCNT_Habitat_Quality_Okanogan_EDT_Truncated, by="ReachName")
  colnames(df_add_to_MetEntWen)[length(colnames(df_add_to_MetEntWen))] = "HQ_Pct"
  
  # ------------------------------ HQ Restoration Score ------------------------
  df_add_to_MetEntWen = df_add_to_MetEntWen  %>%
    mutate(HQ_Score_Restoration = ifelse(HQ_Pct  > Restoration_Scoring$Category_Lower[1] & 
                                           HQ_Pct  < Restoration_Scoring$Category_Upper[1] , Restoration_Scoring$Score[1],
                                         ifelse(HQ_Pct  >= Restoration_Scoring$Category_Lower[2] & 
                                                  HQ_Pct  <= Restoration_Scoring$Category_Upper[2] , Restoration_Scoring$Score[2],
                                                ifelse(HQ_Pct  > Restoration_Scoring$Category_Lower[3] & 
                                                         HQ_Pct  <= Restoration_Scoring$Category_Upper[3] , Restoration_Scoring$Score[3],
                                                       NA))))
  # ------------------------------ HQ Protection Score ------------------------
  df_add_to_MetEntWen = df_add_to_MetEntWen  %>%
    mutate(HQ_Score_Protection = ifelse(HQ_Pct  > Protection_Scoring$Category_Lower [1] & 
                                          HQ_Pct  < Protection_Scoring$Category_Upper[1] , Protection_Scoring$Score[1],
                                        ifelse(HQ_Pct  >= Protection_Scoring$Category_Lower[2] & 
                                                 HQ_Pct  <= Protection_Scoring$Category_Upper[2] , Protection_Scoring$Score[2],
                                               ifelse(HQ_Pct  > Protection_Scoring$Category_Lower[3] & 
                                                        HQ_Pct  <= Protection_Scoring$Category_Upper[3] , Protection_Scoring$Score[3],
                                                      NA))))
  
  # --------------------- add At Risk and Unnacceptable habitat attributes -----------------
  #df_add_to_MetEntWen[,colnames(Okanogan_data_frame)[(length(colnames(Okanogan_data_frame))-2):length(colnames(Okanogan_data_frame))]] =
  #  Okanogan_data_frame[,colnames(Okanogan_data_frame)[(length(colnames(Okanogan_data_frame))-2):length(colnames(Okanogan_data_frame))]]
  
  # ---------------- update Riparian column name -----------
  x = which(colnames(df_add_to_MetEntWen) == "Riparian_Mean_score")
  colnames(df_add_to_MetEntWen)[x] = "Riparian_Mean"
  
  # ----------------- combine prepared Okanogan data frame with MetEntWen data frame -----
  MetEntWen_data_frame_updated = rbind(MetEntWen_data_frame,  df_add_to_MetEntWen)
  
  # ------------------ Return ---------------
  return(MetEntWen_data_frame_updated)
}



combo_habitat_attributes = c( unique(AttributeCrosswalk$`RTT Habitat Attributes`), unique(Habitat_Attribute_Scores_Okanogan$Habitat_Attribute))

for(habitat_attribute_x in combo_habitat_attributes){
  
}