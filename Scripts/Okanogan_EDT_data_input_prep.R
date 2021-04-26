
# ---------------------------------------------------------------------------
#
#      SCRIPT: Prepare Okanogan EDT data for input
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
#    Update RTT Habitat Attribute Names
#
# ---------------------------------------------------------------------------

# ------------- get unique EDT reaches ---------
unique_EDT_attributes = unique(HabitatAttribute_Ratings$`EDT Attribute`)

for(EDT_habitat_attribute_x in unique_EDT_attributes){
  
  # ------------------ get new RTT habitat attribute name for EDT name --------
  x = which(AttributeCrosswalk$`Level 2 Attribute` == EDT_habitat_attribute_x   |
            tolower(AttributeCrosswalk$`Level 2 Attribute`) == tolower(EDT_habitat_attribute_x)  )  # make both lower case
  RTT_habitat_attribute_name_x = AttributeCrosswalk$`RTT Habitat Attributes`[x]
  
  if(length(RTT_habitat_attribute_name_x) > 0){
    # -------------------- update RTT reach name -------
    x = which( HabitatAttribute_Ratings$`EDT Attribute` ==  EDT_habitat_attribute_x )
    HabitatAttribute_Ratings$`RTT Habitat Attribute`[x] = RTT_habitat_attribute_name_x
    print(EDT_habitat_attribute_x)
    
  }else{
    
    print(paste("No RTT crosswalk name for EDT habitat attribute: ", EDT_habitat_attribute_x))
    
  }
  

}


# ---------------------------------------------------------------------------
#
#    Generate "Functional Condition" (which is corollary to HQ Score)
#
# ---------------------------------------------------------------------------

# ----------------------------------------------------
#      Only carry forward habitat attributes that have RTT Habitat Attributes
# ----------------------------------------------------

HabitatAttribute_Ratings_RTT_habitat_attributes = HabitatAttribute_Ratings[ !is.na(HabitatAttribute_Ratings$`RTT Habitat Attribute`), ]

# ----------------------------------------------------
#       Level 2 data
# ----------------------------------------------------

if(any(EDT_leves_use == 2)){
  
  # ---------- identify all the Level 2 ------------------
  level_2_attributes = which(HabitatAttribute_Ratings_RTT_habitat_attributes$`Attribute Level` == 2)
  # ------ get habitat attributes level 2 data frame ------------
  HabitatAttribute_Ratings_level_2 = HabitatAttribute_Ratings_RTT_habitat_attributes[level_2_attributes, ]
  
  # ------------------------ apply filter --------------------
  HabitatAttribute_Ratings_level_2 = HabitatAttribute_Ratings_level_2  %>%
    rowwise() %>%
    mutate(Function_Condition = ifelse(factorWeight  > Criteria_Okanogan_EDT_Scoring_Level_2$Category_lower_limit[1] & 
                                         factorWeight  < Criteria_Okanogan_EDT_Scoring_Level_2$Category_upper_limit[1] , Criteria_Okanogan_EDT_Scoring_Level_2$Score[1],
                                        ifelse(factorWeight  >= Criteria_Okanogan_EDT_Scoring_Level_2$Category_lower_limit[2] & 
                                                 factorWeight  <= Criteria_Okanogan_EDT_Scoring_Level_2$Category_upper_limit[2] , Criteria_Okanogan_EDT_Scoring_Level_2$Score[2],
                                               ifelse(factorWeight  > Criteria_Okanogan_EDT_Scoring_Level_2$Category_lower_limit[3] & 
                                                        factorWeight  <= Criteria_Okanogan_EDT_Scoring_Level_2$Category_upper_limit[3] , Criteria_Okanogan_EDT_Scoring_Level_2$Score[3],
                                                      NA))))
}



# ----------------------------------------------------
#       Level 3 data
# ----------------------------------------------------

if(any(EDT_leves_use == 3)){
  
  # ---------- identify all the Level 2 ------------------
  level_3_attributes = which(HabitatAttribute_Ratings_RTT_habitat_attributes$`Attribute Level` == 3)
  # ------ get habitat attributes level 2 data frame ------------
  HabitatAttribute_Ratings_level_3 = HabitatAttribute_Ratings_RTT_habitat_attributes[level_3_attributes, ]
  
  # ------------------------ apply filter --------------------
  HabitatAttribute_Ratings_level_3 = HabitatAttribute_Ratings_level_2  %>%
    rowwise() %>%
    mutate(Function_Condition = ifelse(factorWeight  > Criteria_Okanogan_EDT_Scoring_Level_3$Category_lower_limit[1] & 
                                         factorWeight  < Criteria_Okanogan_EDT_Scoring_Level_3$Category_upper_limit[1] , Criteria_Okanogan_EDT_Scoring_Level_3$Score[1],
                                       ifelse(factorWeight  >= Criteria_Okanogan_EDT_Scoring_Level_3$Category_lower_limit[2] & 
                                                factorWeight  <= Criteria_Okanogan_EDT_Scoring_Level_3$Category_upper_limit[2] , Criteria_Okanogan_EDT_Scoring_Level_3$Score[2],
                                              ifelse(factorWeight  > Criteria_Okanogan_EDT_Scoring_Level_3$Category_lower_limit[3] & 
                                                       factorWeight  <= Criteria_Okanogan_EDT_Scoring_Level_3$Category_upper_limit[3] , Criteria_Okanogan_EDT_Scoring_Level_3$Score[3],
                                                     NA))))
}


# ---------------------------------------------------------------------------
#
#    Generate RTT Life Stage names from Crosswalk
#
# ---------------------------------------------------------------------------

# NOTE: the Limiting_Factors_Okanogan_EDT already has RTT Life Stage names, but this
#       script to update names is still run in case the RTT Life Stage names or the 
#       crosswalk changes

for(EDT_life_stage_x in unique(LifeStageCrosswalk_EDT$`EDT Life Stage`) ){
  
  # -------------- new RTT life stage name -------------
  RTT_life_stage_x = LifeStageCrosswalk_EDT$`RTT Life Stage`[which(LifeStageCrosswalk_EDT$`EDT Life Stage` == EDT_life_stage_x)]
  
  # ---------------------- identify rows to update RTT life stage name ------------------
  rows_life_stage_update_x = which(Limiting_Factors_Okanogan_EDT$`EDT Life Stage` == EDT_life_stage_x)
  
  # -------------- update RTT life stage name  -------------
  Limiting_Factors_Okanogan_EDT$`RTT Life Stage`[rows_life_stage_update_x] = RTT_life_stage_x
  
}


# ---------------------------------------------------------------------------
#
#    Generate RTT habitat attributes for Limiting_Factors_Okanogan_EDT 
#
# ---------------------------------------------------------------------------

# step 1: crosswalk level 3 to level 2 habitat attributes
# step 2: crosswalk level 2 habitat attributes to RTT habitat attributes 

# ---------------------------------------------------------------------------
#
#    Generate Limiting Factor Score (RTT) 
#
# ---------------------------------------------------------------------------

# ------------------------ Generate Limiting Factor Scores --------------------
Limiting_Factors_Okanogan_EDT = Limiting_Factors_Okanogan_EDT  %>%
  rowwise() %>%
  mutate(RTT_Limiting_Factor_Score = ifelse(`Performance Effect`  > Criteria_Okanogan_EDT_Scoring_Limiting_Factor_Level_3$Category_lower_limit[1] & 
                                              `Performance Effect`   < Criteria_Okanogan_EDT_Scoring_Limiting_Factor_Level_3$Category_upper_limit[1] , Criteria_Okanogan_EDT_Scoring_Limiting_Factor_Level_3$Score[1],
                                     ifelse(`Performance Effect`   >= Criteria_Okanogan_EDT_Scoring_Limiting_Factor_Level_3$Category_lower_limit[2] & 
                                              `Performance Effect`   <= Criteria_Okanogan_EDT_Scoring_Limiting_Factor_Level_3$Category_upper_limit[2] , Criteria_Okanogan_EDT_Scoring_Limiting_Factor_Level_3$Score[2],
                                            ifelse(`Performance Effect`   > Criteria_Okanogan_EDT_Scoring_Limiting_Factor_Level_3$Category_lower_limit[3] & 
                                                     `Performance Effect`   <= Criteria_Okanogan_EDT_Scoring_Limiting_Factor_Level_3$Category_upper_limit[3] , Criteria_Okanogan_EDT_Scoring_Limiting_Factor_Level_3$Score[3],
                                                   NA))))

# --------------------------------------------------------------------------------------------------------------- 
# Generate Level 2 Habitat Attribute name (Level 3 -> Level 2) AND 
#            RTT Habitat Attributes (Level 2 -> RTT habitat attribute) 
# --------------------------------------------------------------------------------------------------------------- 

# ------------- list unique Level 3 limiting factors ----------
unique_level_3 = unique( Limiting_Factors_Okanogan_EDT$Attribute )

for(level_3_habitat_attribute_x  in unique_level_3){
  print(level_3_habitat_attribute_x)
  # ------------------ identify which rows have the level_3_habitat_attribute_x  ------------
  rows_x = which(Limiting_Factors_Okanogan_EDT$Attribute == level_3_habitat_attribute_x)
  # ---------------- identify correct level 2 AND RTT habitat attribute  -----------------
  level_2_x = which(AttributeCrosswalk$`Level 3 Attribute`==level_3_habitat_attribute_x)
  level_2_row_x = AttributeCrosswalk[level_2_x, ]
  # ---------------- add level 2 habitat attribute ------------
  print(level_2_row_x[,c('Level 2 Attribute','RTT Habitat Attributes')])
  print(" -------------------------------- ---------------------------- ------------------")
}



