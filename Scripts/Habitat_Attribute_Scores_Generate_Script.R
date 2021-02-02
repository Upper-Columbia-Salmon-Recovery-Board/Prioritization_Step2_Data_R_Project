
# ---------------------------------------------------------------------------
#
#      SCRIPT: Generate Habitat Attribute Scores Table
 #            (for Habitat Quality AND Limiting Factor Analysis)
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
#   Read in List of Data sources for Habitat Attributes
#
# ---------------------------------------------------------------------------


source(paste(script_path, 'Data_Sources_List_for_Habitat_Attributes.R', sep=""))

# ---------------------------------------------------------------------------
#
#   Call Functions for Habitat Attribute Filters
#
# ---------------------------------------------------------------------------



source(paste(script_path, 'FUNCTIONS_for_Habitat_Attribute_Filters.R', sep=""))




# ---------------------------------------------------------------------------
#
#
#        Generate Habitat Attributes Scores (1, 3, 5) for each Habitat Attribute
#
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
#  Loop through each habitat attribute and data source to generate score
#
# ---------------------------------------------------------------------------

reach_names = unique(Reach_Information_data$ReachName)
Habitat_Attribute_Scores = data.frame()
# --------------------------------------
#   Loop through each Reach to generate score
# --------------------------------------
ptm <- proc.time()[3]
for(reach_x in reach_names){
  print(reach_x)

  # --------------------------------------
  #   Loop through each Habitat Attribute
  # --------------------------------------
  for(habitat_attribute_x in names(Habitat_Attributes_List)){
    
    # ------------ data frame to record habitat attributes --------
    habitat_attribute_x_data_frame = data.frame()
    
    # --------------------------------------------------------------------
    #   Loop through each Data Source for this specific habitat attribute
    # --------------------------------------------------------------------
    data_sources_list =  Habitat_Attributes_List[habitat_attribute_x]
    for(location_x in 1:length(data_sources_list[[1]])){
      
      # ------------ Generate metric value  AND score (1,3,5) for each habitat attribute -------------------
      # outputs both metric value and score
      metric_value_x = FUNCTION_Return_Habitat_Data(habitat_attribute_x, location_x, reach_x )
      rownames(metric_value_x) = location_x
      # -------- data frame for this specific reach and habitat attribute --------
      habitat_attribute_x_data_frame = rbind(habitat_attribute_x_data_frame, metric_value_x)
      
    }

    # --------------------------------------------------------------------
    #   Take minimum value for each reach and Habitat Attribute
    # --------------------------------------------------------------------
    habitat_attribute_x_data_frame$score = as.numeric(habitat_attribute_x_data_frame$score)
    lowest_val = as.data.frame( min(habitat_attribute_x_data_frame$score, na.rm=T) )
    
    # --------------------------------------
    # create single row
    # -------------------------------------- 
    na_column = as.data.frame(c(NA))
    
    # --------- single data sources for habitat attribute ---------
    if(nrow(habitat_attribute_x_data_frame) == 1){
      output_single_row = cbind(habitat_attribute_x_data_frame[,c("ReachName", "Basin", "Assessment.Unit", "Habitat_Attribute","data_source","score")] , na_column, na_column, na_column, lowest_val)
      output_single_row[1,3] = paste(habitat_attribute_x_data_frame$data_source[1], "(HabitatAttributeScore1), ") 
      
    # --------- two data sources for habitat attribute ---------
    }else if( nrow(habitat_attribute_x_data_frame) == 2 ){
      output_single_row = cbind(habitat_attribute_x_data_frame[1,c("ReachName", "Basin", "Assessment.Unit", "Habitat_Attribute","data_source","score")] , habitat_attribute_x_data_frame$score[2], na_column, na_column, lowest_val)
      output_single_row[1,3] = paste(paste(habitat_attribute_x_data_frame$data_source[1], "(HabitatAttributeScore1), "), 
                                           paste(habitat_attribute_x_data_frame$data_source[2], "(HabitatAttributeScore2), ") ) 
        
    # --------- three data sources for habitat attribute ---------
    }else if( nrow(habitat_attribute_x_data_frame) == 3 ){
      output_single_row = cbind(habitat_attribute_x_data_frame[1,c("ReachName", "Basin", "Assessment.Unit", "Habitat_Attribute","data_source","score")] , habitat_attribute_x_data_frame$score[2], habitat_attribute_x_data_frame$score[3], na_column, lowest_val)
      output_single_row[1,3] = paste(paste(paste(habitat_attribute_x_data_frame$data_source[1], "(HabitatAttributeScore1), "), 
            paste(habitat_attribute_x_data_frame$data_source[2], "(HabitatAttributeScore2), ") ),
            paste(habitat_attribute_x_data_frame$data_source[3], "(HabitatAttributeScore3)") )
    # --------- four data sources for habitat attribute ---------
    }else if( nrow(habitat_attribute_x_data_frame) == 4 ){
      output_single_row = cbind(habitat_attribute_x_data_frame[1,c("ReachName", "Basin", "Assessment.Unit", "Habitat_Attribute","data_source","score")] , habitat_attribute_x_data_frame$score[2], habitat_attribute_x_data_frame$score[3], habitat_attribute_x_data_frame$score[4], lowest_val)
      output_single_row[1,3] = paste(paste(paste(paste(habitat_attribute_x_data_frame$data_source[1], "(HabitatAttributeScore1), "), 
                                  paste(habitat_attribute_x_data_frame$data_source[2], "(HabitatAttributeScore2), ") ),
                                  paste(habitat_attribute_x_data_frame$data_source[3], "(HabitatAttributeScore3)") ),
                                  paste(habitat_attribute_x_data_frame$data_source[4], "(HabitatAttributeScore4)") )
    }
    
    colnames(output_single_row) = c(  'ReachName', 'Basin', 'Assessment.Unit', 'Habitat_Attribute','Data_Sources',
                                      'HabitatAttributeScore1', 'HabitatAttributeScore2','HabitatAttributeScore3','HabitatAttributeScore4',
                                      'Habitat_Attribute_Score')
    output_single_row$Notes_or_Professional_Judgement = NA # will update this cell if there are notes or professoinal judgement

    # --------------------------------------------------------------------
    #   Insert Professional Judgment where indicated
    # --------------------------------------------------------------------
    
    # ------------ Where indicated, Over-ride the score with Professional Judgment -------------------
    # ---------------- generate professional judgment score -----------------
    prof_judgement_score_notes_x = FUNCTION_Update_REI_value_OR_Profession_Judgment(habitat_attribute_x, reach_x)
    # --------------- only replace professional judgment where new score (1,3,5) is generated --------------
    if(!is.na(prof_judgement_score_notes_x[1])){
      output_single_row$Habitat_Attribute_Score = prof_judgement_score_notes_x[1]
      output_single_row$Data_Sources = paste("Professoinal_Judgement (Final), ",output_single_row$Data_Sources)
      output_single_row$Notes_or_Professional_Judgement = prof_judgement_score_notes_x[2]
    }
    
    # ---------------------------------------
    #    combine metrics with data frame
    # ---------------------------------------
    Habitat_Attribute_Scores = rbind(Habitat_Attribute_Scores, output_single_row)
    
     
  }
  
}
print(paste("Time to complete loop: ", paste(round((proc.time()[3] - ptm)/60, 2), " minutes")    ))

# --------- make score numeric --------------
cols.num = c("HabitatAttributeScore1" , "HabitatAttributeScore2" , "HabitatAttributeScore3",
             "HabitatAttributeScore4",  "Habitat_Attribute_Score")
Habitat_Attribute_Scores[cols.num] <- sapply(Habitat_Attribute_Scores[cols.num],as.numeric)

# ------------ make it as a tibble --------------
Habitat_Attribute_Scores = as.tibble(Habitat_Attribute_Scores)

# --------------- replace infinite values (habitat attributes/reach combos where no data) with NA ---
infinite_x = is.infinite(Habitat_Attribute_Scores$Habitat_Attribute_Score)
Habitat_Attribute_Scores$Habitat_Attribute_Score[infinite_x] = "NA"

# ------------ convert all the NA to text NA so it outputs --------
NA_x = is.na(Habitat_Attribute_Scores$HabitatAttributeScore1)
Habitat_Attribute_Scores$HabitatAttributeScore1[NA_x] = "NA"
NA_x = is.na(Habitat_Attribute_Scores$HabitatAttributeScore2)
Habitat_Attribute_Scores$HabitatAttributeScore2[NA_x] = "NA"
NA_x = is.na(Habitat_Attribute_Scores$HabitatAttributeScore3)
Habitat_Attribute_Scores$HabitatAttributeScore3[NA_x] = "NA"
NA_x = is.na(Habitat_Attribute_Scores$HabitatAttributeScore4)
Habitat_Attribute_Scores$HabitatAttributeScore4[NA_x] = "NA"
NA_x = is.na(Habitat_Attribute_Scores$Notes_or_Professional_Judgement)
Habitat_Attribute_Scores$Notes_or_Professional_Judgement[NA_x] = "NA"

# ------------------ 
output_path_x =  paste(output_path,'Habitat_Attribute_Scores.xlsx', sep="") 
write_xlsx(Habitat_Attribute_Scores, output_path_x)


Habitat_Attribute_Scores %>%
  filter(Habitat_Attribute   == "Coarse Substrate") %>%
  filter(ReachName   == "Beaver Creek Lower 06") %>%
  select(ReachName, Habitat_Attribute_Score)


