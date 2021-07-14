# ---------------------------------------------------------------------------
#
#      SCRIPT: Function to combine Habitat Quality and Habitat Attributes (Limiting Factor)
#               Tables into one for Web Map
#
#      R Project to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------


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
#    Start the Table with Basin Information
# ---------------------------------------------------------------------------

Habitat_Attributes_Ratings_Table = as.data.frame(Reach_Information_data[,c("ReachName","Assessment.Unit", "Basin")])

# ---------------------------------------------------------------------------
#   Loop through each habitat attribute and get score
# ---------------------------------------------------------------------------
# NOTE - each habitat attribute name is unique

colnames_Habitat_Quality_Scores = colnames(Habitat_Quality_Scores)
reach_col = which(colnames_Habitat_Quality_Scores == "ReachName")

for(habitat_attribute_x in Order_of_Habitat_Attribute_Rating_Table_Columns){
  print(" ---------------------- ")
  print(habitat_attribute_x)
  # --------------- Limiting Factor Pathway -------------
  if( any(Habitat_Attribute_Scores_columns_to_pull == habitat_attribute_x) ){
    output_x = Habitat_Attribute_Scores[which(Habitat_Attribute_Scores$Habitat_Attribute == habitat_attribute_x), ]
    output_x = output_x[,c("ReachName", "Habitat_Attribute_Score")]
    output_x[,2] = FUNCTION_score_numeric_to_word(output_x[,2])  # convert numeric (1,3,5) scores to words (Unacceptable, At Risk, Adequate)
    Habitat_Attributes_Ratings_Table = merge(Habitat_Attributes_Ratings_Table, output_x, by="ReachName")  # 
    colnames(Habitat_Attributes_Ratings_Table)[colnames(Habitat_Attributes_Ratings_Table)      # Rename two variable names
                       %in% c("Habitat_Attribute_Score")] <- c(habitat_attribute_x)
    
  # ----------- Habitat Quality Pathway --------------
  }else{
    
    colx = which(colnames_Habitat_Quality_Scores == habitat_attribute_x)
    output_x = as.data.frame(Habitat_Quality_Scores[, c(reach_col,colx)])
    output_x[,2] = FUNCTION_score_numeric_to_word(output_x[,2])  # convert numeric (1,3,5) scores to words (Unacceptable, At Risk, Adequate)
    Habitat_Attributes_Ratings_Table = merge(Habitat_Attributes_Ratings_Table, output_x, by="ReachName")
  }

  print(dim(Habitat_Attributes_Ratings_Table))
  
  
}

# -----------------------------------------------------
#  Add HQ Sum and PCT
# --------------------------------------------
Habitat_Quality_Scores_for_merge_x = Habitat_Quality_Scores[,c("ReachName","HQ_Sum","HQ_Pct" )]
Habitat_Attributes_Ratings_Table = merge(Habitat_Attributes_Ratings_Table,Habitat_Quality_Scores_for_merge_x, by="ReachName", all.x=TRUE )

# -----------------------------------------------------
#   Only include desired basins
# --------------------------------------------

x_include = c()
for(basin_x in basins_to_include ){
  x_include = c(x_include, which(Habitat_Attributes_Ratings_Table$Basin ==basin_x ) )
}
x_include = x_include[order(x_include)]

# ----------- filter out ----------
Habitat_Attributes_Ratings_Table = Habitat_Attributes_Ratings_Table[x_include,]

