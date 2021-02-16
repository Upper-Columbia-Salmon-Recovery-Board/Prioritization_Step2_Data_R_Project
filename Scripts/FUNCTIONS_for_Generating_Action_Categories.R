

# ---------------------------------------------------------------------------
#
#      SCRIPT: Generate Action Categories from priority reaches
#
#      R Project to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------


#Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']]
#habitat_attributes_x_column = Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']]$unacceptable_1_indiv_habitat_attributes
#data_frame_with_habitat_attributes = Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']] 

# ---------------------------------------------------------------------------
#
#     Function to generate list
#
# ---------------------------------------------------------------------------

# ------------------------ Function to generate column of action categories for the habitat attributes ---------
# NOTE: data must have columns with "unacceptable_1_indiv_habitat_attributes", "at_risk_2_or_3_indiv_habitat_attributes", "unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes"
data_frame_with_habitat_attributes = Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']]
data_frame_with_habitat_attributes = Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']]
FUNCTION_to_generate_Action_Categories = function(data_frame_with_habitat_attributes){
  
  # ------------------ generate action categories for Unacceptable (1) habitat attributes ------------
  data_frame_with_habitat_attributes$unacceptable_1_action_categories = apply(as.matrix(data_frame_with_habitat_attributes$unacceptable_1_indiv_habitat_attributes), MARGIN = 1,
                                   FUNCTION_match_habitat_attributes_and_action_categories)
  # -------------- generate action categories for Unacceptable and At Risk (1 OR 3) habitat attributes ------------
  data_frame_with_habitat_attributes$at_risk_2_or_3_action_categories = apply(as.matrix(data_frame_with_habitat_attributes$at_risk_2_or_3_indiv_habitat_attributes), MARGIN = 1,
                                                                                            FUNCTION_match_habitat_attributes_and_action_categories)
  
  # -------------- generate action categories for Unacceptable and At Risk (1 OR 3) habitat attributes ------------
  data_frame_with_habitat_attributes$unacceptable_and_at_risk_1_3_action_categories = apply(as.matrix(data_frame_with_habitat_attributes$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes), MARGIN = 1,
                                                                              FUNCTION_match_habitat_attributes_and_action_categories)
  
  return(data_frame_with_habitat_attributes)
}


row_x = data_frame_with_habitat_attributes$unacceptable_1_indiv_habitat_attributes[9]

# -------------------  function to take list of habitat attributes to generate action categories ------
FUNCTION_match_habitat_attributes_and_action_categories = function(row){
  
  # ------ dividing into multiple -----
  habitat_attributes_row_x2 = unlist(strsplit(row, ","))
  # --------- remove white space ------
  habitat_attributes_row_x2 = gsub(" ", "", habitat_attributes_row_x2, fixed = TRUE)
  
  # --------------- generation action categories -----------
  Action_Categories_Output = c()
  for(habitat_attribute_x in habitat_attributes_row_x2){
    Crosswalk_Habitat_Attributes_and_Actions_habitat_attribute_x = Crosswalk_Habitat_Attributes_and_Actions %>%
      filter(Habitat_Attribute_2 %in% habitat_attribute_x)
    Action_Categories_x = unique(Crosswalk_Habitat_Attributes_and_Actions_habitat_attribute_x$`Action Category`)
    if(length(Action_Categories_Output) == 0 ){
      Action_Categories_Output = paste(Action_Categories_x, collapse = ',')
    }else{
      Action_Categories_Output = paste(Action_Categories_Output, paste(Action_Categories_x, collapse = ','), sep=",")
    }
    
  }
  
  return(Action_Categories_Output)
  
}


Crosswalk_Habitat_Attributes_and_Actions %>%
  filter(Crosswalk_Habitat_Attributes_and_Actions$`Habitat Attribute`  %in% habitat_attribute_x)
