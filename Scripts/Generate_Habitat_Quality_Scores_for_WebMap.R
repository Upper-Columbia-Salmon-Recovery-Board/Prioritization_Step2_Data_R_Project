

# -----------------------------------------------------------------------------------------------------------------------------------------------
#
#
#   - - - - - - - - -  Script to Generate Habitat Quality layer for WebMap - - - - - - - - - 
#           NOTE - main thing is to update Habitat_Quality_Scores with Okanogan scores
#  
#
# -----------------------------------------------------------------------------------------------------------------------------------------------


# ----------------- initiated new Habitat_Quality_Scores -----------
Habitat_Quality_Scores_updated = Habitat_Quality_Scores

# ------------------------------ add Okanogan scores ----------------
HQ_columns_to_pull = colnames(Habitat_Quality_Scores_updated)[7:ncol(Habitat_Quality_Scores_updated)]
for(HQ_col_x in HQ_columns_to_pull ){
  HQ_col_Okanogan = which(colnames(Habitat_Quality_Scores_Okanogan) == HQ_col_x)
  for(row_x in 1:nrow(Habitat_Quality_Scores_Okanogan)){
    reach_x = Habitat_Quality_Scores_Okanogan$ReachName[row_x]
    reach_HQ_orig = which(Habitat_Quality_Scores_updated$ReachName == reach_x)
    Habitat_Quality_Scores_updated[reach_HQ_orig,HQ_col_x] = Habitat_Quality_Scores_Okanogan[row_x,HQ_col_x]
  }
}
 

# ------------------ output Habitat Quality Scores for WebMap ----------------
Habitat_Quality_Scores_for_WebMap = Habitat_Quality_Scores_updated[,Habitat_Quality_Scores_columns_to_pull]

# ---------------------- pull column names -----------------
colnames(Habitat_Quality_Scores_for_WebMap) = Habitat_qulaity_Scores_for_WebMap_column_names

# ---------------------------------------------------------------------------
#    FUNCTION to Convert 1, 3, 5 to Unacceptable, At Risk, and Adequate
# ---------------------------------------------------------------------------

FUNCTION_score_numeric_to_word = function(column_data){
  column_data = gsub(1, "Unacceptable", column_data)
  column_data = gsub(2, "At Risk", column_data)
  column_data = gsub(3, "At Risk", column_data)
  column_data = gsub(4, "At Risk", column_data)
  column_data = gsub(5, "Adequate", column_data)
  return(column_data)
}


# ---------------------------------------------------------------------------
#    Loop through and replace 1,3,5 with Adequate/At Risk/Unacceptable
# ---------------------------------------------------------------------------

for(habitat_attribute_x in colnames(Habitat_Quality_Scores_for_WebMap)[2:(length(colnames(Habitat_Quality_Scores_for_WebMap))-2)]){
  Habitat_Quality_Scores_for_WebMap[,habitat_attribute_x] = FUNCTION_score_numeric_to_word(Habitat_Quality_Scores_for_WebMap[,habitat_attribute_x] )  # convert numeric (1,3,5) scores to words (Unacceptable, At Risk, Adequate)
}
    




