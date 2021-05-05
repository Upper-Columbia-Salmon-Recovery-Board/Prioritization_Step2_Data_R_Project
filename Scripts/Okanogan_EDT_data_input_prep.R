
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

# ----------------------------------------------------------------------------------
#      Only carry forward habitat attributes that have RTT Habitat Attributes
# ----------------------------------------------------------------------------------

HabitatAttribute_Ratings_RTT_habitat_attributes = HabitatAttribute_Ratings[ !is.na(HabitatAttribute_Ratings$`RTT Habitat Attribute`), ]

# ----------------------------------------------------------------------------------
#   Level 2 data - generate scores (Function_Condition) from factorWeight
# ----------------------------------------------------------------------------------

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



# ----------------------------------------------------------------------------------
#       Level 3 data (NOTE: I don't think I used this crosswalk - I set up initially)
# ----------------------------------------------------------------------------------

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
#
#    Generate "Functional Condition" (which is corollary to HQ Score)
#
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
#    Level 2
#
# ---------------------------------------------------------------------------

# ----------------------------------------------------------------------------------
#      Update RTT Habitat Attributes names (per AttributeCrosswalk)
# ----------------------------------------------------------------------------------

# ------------ create updated data frame -----------------
HabitatAttribute_Ratings_Level2_updated = HabitatAttribute_Ratings_Level2

# -------- get list of unique habitat attributes -----------
unique_EDT_attributes_x = unique(HabitatAttribute_Ratings_Level2_updated$`EDT Attribute`)
# ------------- make them lower case and remove spaces -----------
unique_EDT_attributes_x_no_spaces_lower_case = gsub(" ", "", unique_EDT_attributes_x, fixed = TRUE)
unique_EDT_attributes_x_no_spaces_lower_case = tolower(unique_EDT_attributes_x_no_spaces_lower_case)

# --------------- get AttributeCrosswalk Level 2 Habitat Attributes in Lower case -----------
AttributeCrosswalk_Level_2_Attribute_lowercase_no_space = gsub(" ", "", AttributeCrosswalk$`Level 2 Attribute`, fixed = TRUE)
AttributeCrosswalk_Level_2_Attribute_lowercase_no_space = tolower(AttributeCrosswalk_Level_2_Attribute_lowercase_no_space)

for(EDT_attribute_x in unique_EDT_attributes_x){
  
  # ----------- get location in unique_EDT_attributes_x for reference ------
  i = which(unique_EDT_attributes_x == EDT_attribute_x)
  EDT_attribute_x_lowercase_no_space = unique_EDT_attributes_x_no_spaces_lower_case[i]
  
  # --------------- identify RTT Habitat attribute in crosswalk -------
  RTT_attribute_crosswalk_loc_x = which(AttributeCrosswalk$`Level 2 Attribute` == EDT_attribute_x  )
  RTT_attribute_crosswalk_loc_x_lowercase_no_space = which(AttributeCrosswalk_Level_2_Attribute_lowercase_no_space == EDT_attribute_x_lowercase_no_space )
  RTT_attribute_crosswalk_loc_x = unique(c(RTT_attribute_crosswalk_loc_x,  RTT_attribute_crosswalk_loc_x_lowercase_no_space)) # get the unique index references
  # -------------- get RTT Habitat Attribute in the Crosswalk -----------
  RTT_attribute_updated_x = AttributeCrosswalk$`RTT Habitat Attributes`[RTT_attribute_crosswalk_loc_x] # generate new RTT attribute name
  
  # ----------- if there is an RTT habitat attribute for this EDT habitat attribute ------
  
  if( length(RTT_attribute_updated_x) > 0){
    
    # ----------- get location in HabitatAttribute_Ratings_Level2_updated -------
    EDT_attribute_loc_x = which(HabitatAttribute_Ratings_Level2_updated$`EDT Attribute` == EDT_attribute_x )
    # ----------------------- update the RTT habitat attribute names  --------------
    HabitatAttribute_Ratings_Level2_updated$`RTT Habitat Attribute`[EDT_attribute_loc_x] = RTT_attribute_updated_x
    
  # -------------- if there is NO RTT habitat attribute for this EDT attribute - give RTT attribute an NA -------------
  }else{
    # ----------- get location in HabitatAttribute_Ratings_Level2_updated -------
    EDT_attribute_loc_x = which(HabitatAttribute_Ratings_Level2_updated$`EDT Attribute` == EDT_attribute_x )
    HabitatAttribute_Ratings_Level2_updated$`RTT Habitat Attribute`[EDT_attribute_loc_x] = NA
  }
  
}
# compare list to AttributeCrosswalk -  to make sure all NAs are correct:    unique( HabitatAttribute_Ratings_Level2_updated$`EDT Attribute` [ which( is.na(HabitatAttribute_Ratings_Level2_updated$`RTT Habitat Attribute`) )])

# ----------------------------------------------------------------------------------
#      Only carry forward habitat attributes that have RTT Habitat Attributes
# ----------------------------------------------------------------------------------

HabitatAttribute_Ratings_Level2_updated = HabitatAttribute_Ratings_Level2_updated[ !is.na(HabitatAttribute_Ratings_Level2_updated$`RTT Habitat Attribute`), ]

# ---------------------------------------------------------------------------
#
#    Generate "Functional Condition" (which is corollary to HQ Score) (updates current value)
#
# ---------------------------------------------------------------------------

# ------------------------ Generate Limiting Factor Scores --------------------
HabitatAttribute_Ratings_Level2_updated = HabitatAttribute_Ratings_Level2_updated  %>%
  rowwise() %>%
  mutate(`Level 2 Functional Condition`  = ifelse(factorWeight  > Criteria_Okanogan_EDT_Scoring_Level_2$Category_lower_limit[1] & 
                                                    factorWeight   < Criteria_Okanogan_EDT_Scoring_Level_2$Category_upper_limit[1] , Criteria_Okanogan_EDT_Scoring_Level_2$Score[1],
                                            ifelse(factorWeight   >= Criteria_Okanogan_EDT_Scoring_Level_2$Category_lower_limit[2] & 
                                                     factorWeight   <= Criteria_Okanogan_EDT_Scoring_Level_2$Category_upper_limit[2] , Criteria_Okanogan_EDT_Scoring_Level_2$Score[2],
                                                   ifelse(factorWeight   > Criteria_Okanogan_EDT_Scoring_Level_2$Category_lower_limit[3] & 
                                                            factorWeight  <= Criteria_Okanogan_EDT_Scoring_Level_2$Category_upper_limit[3] , Criteria_Okanogan_EDT_Scoring_Level_2$Score[3],
                                                          NA))))

# ---------------------------------------------------------------------------
#
#    Level 3
#
# ---------------------------------------------------------------------------

# ----------------------------------------------------------------------------------
#      Update RTT Habitat Attributes names (per AttributeCrosswalk)
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
#    Generate "Functional Condition" (which is corollary to HQ Score) (updates current value)
#
# ---------------------------------------------------------------------------

# ------------------------ Generate Limiting Factor Scores --------------------
HabitatAttribute_Ratings_Level3 = HabitatAttribute_Ratings_Level3  %>%
  rowwise() %>%
  mutate(`Level 3 Functional Condition`  = ifelse(factorWeight  > Criteria_Okanogan_EDT_Scoring_Level_3$Category_lower_limit[1] & 
                                                    factorWeight   < Criteria_Okanogan_EDT_Scoring_Level_3$Category_upper_limit[1] , Criteria_Okanogan_EDT_Scoring_Level_3$Score[1],
                                                  ifelse(factorWeight   >= Criteria_Okanogan_EDT_Scoring_Level_3$Category_lower_limit[2] & 
                                                           factorWeight   <= Criteria_Okanogan_EDT_Scoring_Level_3$Category_upper_limit[2] , Criteria_Okanogan_EDT_Scoring_Level_3$Score[2],
                                                         ifelse(factorWeight   > Criteria_Okanogan_EDT_Scoring_Level_3$Category_lower_limit[3] & 
                                                                  factorWeight  <= Criteria_Okanogan_EDT_Scoring_Level_3$Category_upper_limit[3] , Criteria_Okanogan_EDT_Scoring_Level_3$Score[3],
                                                                NA))))




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
# -------------- start Level 2 habitat attribute column -------------
Limiting_Factors_Okanogan_EDT$"EDT_Attribute_Level_2" = NA
# -------------- start Level 3 with no Level 2 Crosswalk -------------
Limiting_Factors_Okanogan_EDT$"EDT_Attribute_Level_3_with_no_Level_2_Crosswalk" = NA
# -------------- start Level 2 habitat attribute WITH RTT attribute in crosswalk column -------------
Limiting_Factors_Okanogan_EDT$"EDT_Attribute_Level_2_with_RTT_attribute_crosswalk" = NA
# -------------- start RTT attribute column -------------
Limiting_Factors_Okanogan_EDT$"RTT_Habitat_Attributes" = NA


# -------------- start RTT habitat attribute column -------------
# Limiting_Factors_Okanogan_EDT$"RTT Habitat Attribute" = NA

for(level_3_habitat_attribute_x  in  unique_level_3){
  print("------------------------------------------------------------")
  print(level_3_habitat_attribute_x)
  
  # ------------------------------------------------------------------------------
  #    Identify rows with this Level 3 habitat attribute
  # ------------------------------------------------------------------------------ 
  
  # ------------------ identify which rows have the level_3_habitat_attribute_x  ------------
  rows_x = which(Limiting_Factors_Okanogan_EDT$Attribute == level_3_habitat_attribute_x)
  
  # ------------------------------------------------------------------------------
  #    Level 3 => Level 2
  # ------------------------------------------------------------------------------ 
  # ---------------- identify correct level 2  -----------------
  level_2_x = which(Level2_Level3_EDT_Crosswalk$`Level 3 Attribute`==level_3_habitat_attribute_x)
  level_2_row_x = Level2_Level3_EDT_Crosswalk[level_2_x, ]
  # ----------------- add all Level 2 to row -----------
  # ------- get just level 2 habitat attribute ----
  level_2_attribute_x = as.data.frame(level_2_row_x[,c('Level 2 Attribute')])
  # -------------- collapse into one string (to put into one cell each row) ------
  level_2_attribute_x_single_string =  paste(level_2_attribute_x[,1], collapse=',' )

  
  #print("NROW(level_2_row_x)")
  #print(nrow(level_2_row_x))
  #print(level_2_row_x[,c('Level 2 Attribute')])
  #print(" - - - ")
  #print(level_2_attribute_x)
  
  # ------------------------------------------------------------------------------
  #       If there is a Level 2 habitat attribute
  # ------------------------------------------------------------------------------ 

  # ------------- identify RTT habitat attributes  ----------
  if(nrow(level_2_row_x)>0){
    RTT_row_x = c()
    for(level_2_x in level_2_row_x$`Level 2 Attribute`){
      RTT_x = which(AttributeCrosswalk$`Level 2 Attribute`==level_2_x)
      if( !is.na(AttributeCrosswalk$`RTT Habitat Attributes`[RTT_x ]) ){
        RTT_row_x = rbind(RTT_row_x, AttributeCrosswalk[RTT_x, ])
      }
    }

    # ------------------ all rows with this level 3 habitat attribute, put all level 2 habitat attriubtes ------
    Limiting_Factors_Okanogan_EDT$"EDT_Attribute_Level_2"[rows_x] = level_2_attribute_x_single_string
    
    # -------------------------------------------------
    #    Level 2 habitat attribute
    # -------------------------------------------------
    # --------------- get level 2 into a data frame -------------
    level_2_for_RTT_attribute_x = as.data.frame(RTT_row_x[,c('Level 2 Attribute')])
    # -------------- collapse into one string (to put into one cell each row) ------
    level_2_for_RTT_attribute_x_single_string =  paste(level_2_for_RTT_attribute_x[,1], collapse=',' )
    
    # -------------------------------------------------
    #    Get RTT Habitat Attributes
    # -------------------------------------------------
    # --------------- get RTT habitat attribute into a data frame -------------
    RTT_attribute_x = as.data.frame(RTT_row_x[,c("RTT Habitat Attributes")])
    # -------------- collapse into one string (to put into one cell each row) ------
    RTT_attribute_x_single_string =  paste( unique(RTT_attribute_x[,1]), collapse=',' )
    # ------------- remove white space (blank space) ----------
    RTT_attribute_x_single_string = gsub(" ", "", RTT_attribute_x_single_string, fixed = TRUE)
    
    
    EDT_no_Level2 = NA  # make blank because there is a level 2
    
  # ------------------------------------------------------------------------------
  #     If there is no Level 2 habitat attribute crosswalk
  # ------------------------------------------------------------------------------ 

  }else{
    EDT_no_Level2 = level_3_habitat_attribute_x
    Limiting_Factors_Okanogan_EDT$"EDT_Attribute_Level_2"[rows_x] = NA
    level_2_for_RTT_attribute_x_single_string = NA  # blank for Level 2 crosswalk
    RTT_attribute_x_single_string = NA  # blank for RTT habitat attribute crosswalk
  }
  #print(" ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~")
  #print(RTT_row_x)
  
  # --------------------------------------------
  #      write all Level 2 habitat attributes with an RTT habitat attribute (in crosswalk) 
  # --------------------------------------------

  # ------------------ all rows with this level 3 habitat attribute, put all level 2 habitat attributes ------
  Limiting_Factors_Okanogan_EDT$"EDT_Attribute_Level_2_with_RTT_attribute_crosswalk"[rows_x] = level_2_for_RTT_attribute_x_single_string
  
  # --------------------------------------------
  #      write all Level 3 habitat attributes with NO Level 2 crosswalk
  # --------------------------------------------
  if( !is.na(EDT_no_Level2) > 0){
    # --------------- get level 2 into a data frame -------------
    level_3_with_no_level_2 = as.character(EDT_no_Level2)
    # ------------------ all rows with this level 3 habitat attribute, put all level 2 habitat attributes ------
    Limiting_Factors_Okanogan_EDT$"EDT_Attribute_Level_3_with_no_Level_2_Crosswalk"[rows_x] = level_3_with_no_level_2
  }
  
  # --------------------------------------------
  #      write all RTT habitat attributes 
  # --------------------------------------------

  # ------------------ all rows with this level 3 habitat attribute, put all level 2 habitat attributes ------
  Limiting_Factors_Okanogan_EDT$"RTT_Habitat_Attributes"[rows_x] = RTT_attribute_x_single_string
  
}




# --------------------------------------------------------------------------------------------------------------- 
# 
#    Add Okanogan Barriers data to the existing Barriers data (for Okanogan-Wenatchee-Methow)
# 
# --------------------------------------------------------------------------------------------------------------- 

# ----------------------------------------------------------------------------- 
#     Remove the "type" of barrier in the reach name
# ----------------------------------------------------------------------------- 
# ------- strip the decimal 
Barriers_Okanogan_EDT$Reach_no_ref = gsub("\\s*\\.[^\\)]+\\)","",as.character(Barriers_Okanogan_EDT$Reach))

# ----------------------------------------------------------------------------- 
#     Score Barriers
# ----------------------------------------------------------------------------- 

# ------------------------ Generate Barriers Scores --------------------
Barriers_Okanogan_EDT = Barriers_Okanogan_EDT  %>%
  rowwise() %>%
  mutate(Barriers_Score = ifelse(`Change in NEQ`  > Criteria_Okanogan_EDT_Scoring_Barrires$Category_lower_limit[1] & 
                                              `Change in NEQ`   < Criteria_Okanogan_EDT_Scoring_Barrires$Category_upper_limit[1] , Criteria_Okanogan_EDT_Scoring_Barrires$Score[1],
                                            ifelse(`Change in NEQ`   >= Criteria_Okanogan_EDT_Scoring_Barrires$Category_lower_limit[2] & 
                                                     `Change in NEQ`   <= Criteria_Okanogan_EDT_Scoring_Barrires$Category_upper_limit[2] , Criteria_Okanogan_EDT_Scoring_Barrires$Score[2],
                                                   ifelse(`Change in NEQ`   > Criteria_Okanogan_EDT_Scoring_Barrires$Category_lower_limit[3] & 
                                                            `Change in NEQ`   <= Criteria_Okanogan_EDT_Scoring_Barrires$Category_upper_limit[3] , Criteria_Okanogan_EDT_Scoring_Barrires$Score[3],
                                                          NA))))


# ------------------------ Only pull barriers within the individual habitat attribute score --------------------
Barriers_Okanogan_EDT_updated = Barriers_Okanogan_EDT[which(Barriers_Okanogan_EDT$Barriers_Score <= Individual_Habitat_Attribute_Score), ]

# ----------------------------------------------------------------------------- 
#     Add to existing barrier data
# ----------------------------------------------------------------------------- 

# ------- prepare to merge with Wenatchee-Entiat-Methow Barrier data ----------
Barriers_Okanogan_EDT_updated_for_merge = as.data.frame( rep("Okanogan", length.out=nrow(Barriers_Okanogan_EDT_updated))   )  
colnames(Barriers_Okanogan_EDT_updated_for_merge) = "Basin"
Barriers_Okanogan_EDT_updated_for_merge$`Assessment.Unit` = Barriers_Okanogan_EDT_updated$`Assessment Unit`
Barriers_Okanogan_EDT_updated_for_merge$ReachName = Barriers_Okanogan_EDT_updated$Reach_no_ref
Barriers_Okanogan_EDT_updated_for_merge$`Action Category` = rep(Barriers_Pathways_Data$`Action Category`[1], length.out=nrow(Barriers_Okanogan_EDT_updated))
Barriers_Okanogan_EDT_updated_for_merge$Habitat_Attributes =  rep(Barriers_Pathways_Data$Habitat_Attributes[1], length.out=nrow(Barriers_Okanogan_EDT_updated))
Barriers_Okanogan_EDT_updated_for_merge$Pathways =  rep(Barriers_Pathways_Data$Pathways[1], length.out=nrow(Barriers_Okanogan_EDT_updated))

# -------------- merge the data ------------
Barriers_Pathways_Data = rbind( Barriers_Pathways_Data, Barriers_Okanogan_EDT_updated_for_merge)



