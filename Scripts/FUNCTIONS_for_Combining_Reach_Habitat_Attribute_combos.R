
# ---------------------------------------------------------------------------
#
#      SCRIPT: Combine Action Categories across SPECIES and PATHWAYS
#                ORGANZIED by Reach and Habitat Attribute/Action Category (multiple columsn per reach)
#
#      R Project to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------
#
#      Function to generate Action Categories from Habitat Attributes
#
# ------------------------------------------------------------------------------------------


# -------------------  function to take list of habitat attributes to generate action categories ------
FUNCTION_match_INDIVIDUAL_habitat_attributes_and_action_categories = function(habitat_attribute_x){

  # --------- remove white space ------
  habitat_attributes_x2 = gsub(" ", "", habitat_attribute_x, fixed = TRUE)
    
  # ------------- match the habitat attribute to the action category
  Crosswalk_Habitat_Attributes_and_Actions_habitat_attribute_x = Crosswalk_Habitat_Attributes_and_Actions %>%
    filter(Habitat_Attribute_2 %in% habitat_attributes_x2)
  
  # ----------- just get unique Action Categories ----------
  Action_Categories_x = unique(Crosswalk_Habitat_Attributes_and_Actions_habitat_attribute_x$`Action Category`)

  return(Action_Categories_x)
  
}




# ----------------- Function to generate whether species - life stage - habitat attribute is a core metric ---------

# species_x2 = species_x
# species_x2 = "Spring Chinook,Steelhead"
# life_stage_x = "Fry"
# habitat_attribute_x = "Flow-SummerBaseFlow"

FUNCTION_match_INDIVIDUAL_core_metrics_from_habitat_attributes_SPECIES = function(species_x2, life_stage_x, habitat_attribute_x){
  
  # --------- remove white space ------
  habitat_attributes_x2 = gsub(" ", "", habitat_attribute_x, fixed = TRUE)
  
  species_x2 = unique( unlist(strsplit(species_x2, ",")) )
  
  # -------------------------------------------------------------
  #    IF ONE species (not individual rows per species)
  # -------------------------------------------------------------
  if( length(species_x2) == 1 ){
    
    # ------------- match the habitat attribute to the action category
    Attribute_LifeStage_Crosswalk_INDIVIDUAL= Attribute_LifeStage_Crosswalk %>%
      filter(Habitat_Attribute_2 %in%  habitat_attributes_x2) %>%
      filter( `Life Stage` %in% life_stage_x) %>%
      filter( Species %in% species_x2 )
    
    # --------------- assign core metric ----------
    core_metric_output = Attribute_LifeStage_Crosswalk_INDIVIDUAL$`Life Stage Core Metric?`

    # ----------- Yes/No if a Core Metric ----------
    if( length(core_metric_output) == 0  ){
      core_metric_x = "no"
    }else{
      if( !is.na(core_metric_output) ){
        core_metric_x = "yes"
      }else{
        core_metric_x = "no"
      }
    }

    
  # -------------------------------------------------------------
  #    IF multiple species (not individual rows per species)
  # -------------------------------------------------------------
  }else if( length(species_x2) > 1 ){
    
    core_metric_output = c()
    for(species_xi in unlist(strsplit(species_x2, ","))  ){

      # ------------- match the habitat attribute to the action category
      Attribute_LifeStage_Crosswalk_INDIVIDUAL= Attribute_LifeStage_Crosswalk %>%
        filter(Habitat_Attribute_2 %in%  habitat_attributes_x2) %>%
        filter( `Life Stage` %in% life_stage_x) %>%
        filter( Species %in% species_xi )
      
      if( !is.null(Attribute_LifeStage_Crosswalk_INDIVIDUAL$`Life Stage Core Metric?`) ){
        core_metric_output = paste(core_metric_output,Attribute_LifeStage_Crosswalk_INDIVIDUAL$`Life Stage Core Metric?`, sep=",")
      }
      
    }
    core_metric_output = substr(core_metric_output,2,nchar(core_metric_output))   # remove the leading comma
    if( length(core_metric_output) == 0 ){
      core_metric_x = "no"
    }else{
      if(grepl( "x", core_metric_output, fixed = TRUE)){
        core_metric_x = "yes"
      }else{
        core_metric_x = "no"
      }
    }

  }else{
    print("species entered incorrectly")
  }

  return(core_metric_x)
  
}




# ------------------------------------------------------------------------------------------
#
#      Function to combine across Reach - Habitat Attribute - Life Stage
#
# ------------------------------------------------------------------------------------------


# To Test
test = TRUE
if(test){
  HQ_spring_chinook = Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']]
  HQ_steelhead = Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']]
  HQ_bull_trout = Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']]
  LF_spring_chinook = Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']]
  LF_steelhead = Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']]
  LF_bull_trout = Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Restoration']]
  columns_info = c( "ReachName","Basin","Assessment.Unit" ) # columns to automatically add to beginning (left side) of output
  exclude_bull_trout = "no"
}


FUNCTION_combine_by_Reach_AND_Habitat_Attribute_Life_Stage = function(HQ_spring_chinook, HQ_steelhead, HQ_bull_trout,  LF_spring_chinook, LF_steelhead, LF_bull_trout, columns_info, exclude_bull_trout){
  
  # ------------------------------------------------------------
  #       Get Unique Reaches
  # ------------------------------------------------------------
  unique_reaches = unique( c(HQ_spring_chinook$ReachName, HQ_steelhead$ReachName, HQ_bull_trout$ReachName, 
                     LF_spring_chinook$ReachName, LF_steelhead$ReachName, LF_bull_trout$ReachName) )
  # ------------------------------------------------------------
  #      Loop through each Reach and combine data
  # ------------------------------------------------------------
  Reach_Habitat_Attribute_combined_output = c()
  
  for(reach_x in unique_reaches){
    print(reach_x)
    # --------------------- generate HQ and LF index ----------
    if( any(HQ_spring_chinook$ReachName == reach_x)){ HQ_spring_chinook_index = which(HQ_spring_chinook$ReachName == reach_x) }else{HQ_spring_chinook_index = NA}
    if( any(HQ_steelhead$ReachName == reach_x)){ HQ_steelhead_index = which(HQ_steelhead$ReachName == reach_x) }else{HQ_steelhead_index = NA}
    if( any(HQ_bull_trout$ReachName == reach_x)){ HQ_bull_trout_index = which(HQ_bull_trout$ReachName == reach_x) }else{HQ_bull_trout_index = NA}
    if( any(LF_spring_chinook$ReachName == reach_x)){ LF_spring_chinook_index = which(LF_spring_chinook$ReachName == reach_x) }else{LF_spring_chinook_index = NA}
    if( any(LF_steelhead$ReachName == reach_x)){ LF_steelhead_index = which(LF_steelhead$ReachName == reach_x) }else{LF_steelhead_index = NA}
    if( any(LF_bull_trout$ReachName == reach_x)){ LF_bull_trout_index = which(LF_bull_trout$ReachName == reach_x) }else{LF_bull_trout_index = NA}
    
    # ------------------------------------------------------------
    #     List All the Habitat Attributes
    # ------------------------------------------------------------
    habitat_attributes_all = c(
      
      if(!is.na(HQ_spring_chinook_index)){HQ_spring_chinook$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes[HQ_spring_chinook_index]},
      if(!is.na(HQ_steelhead_index)){HQ_steelhead$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes[HQ_steelhead_index]  },
      if(!is.na(HQ_bull_trout_index)){HQ_bull_trout$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes[HQ_bull_trout_index]  },
      if(!is.na(LF_spring_chinook_index[1])){  paste(LF_spring_chinook$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes[LF_spring_chinook_index], collapse=",")  },
      if(!is.na(LF_steelhead_index[1])){ paste(LF_spring_chinook$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes[LF_spring_chinook_index], collapse=",")[1] },
      if(!is.na(LF_bull_trout_index[1])){paste(LF_bull_trout$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes[LF_bull_trout_index], collapse=",")[1] }
      
    ) 
    habitat_attributes_all = gsub(" ", "", habitat_attributes_all, fixed = TRUE)
    habitat_attributes_all = unique( unlist(strsplit(paste(habitat_attributes_all, collapse=","), ",")) )
                                     
    # ------------------------------------------------------------
    #     List All UNACCEPTABLE Habitat Attributes
    # ------------------------------------------------------------
    unique_unacceptable_attributes = c(
      
      if(!is.na(HQ_spring_chinook_index)){HQ_spring_chinook$unacceptable_1_indiv_habitat_attributes[HQ_spring_chinook_index]},
      if(!is.na(HQ_steelhead_index)){HQ_steelhead$unacceptable_1_indiv_habitat_attributes[HQ_steelhead_index]  },
      if(!is.na(HQ_bull_trout_index)){HQ_bull_trout$unacceptable_1_indiv_habitat_attributes[HQ_bull_trout_index]  },
      if(!is.na(LF_spring_chinook_index[1])){  paste(LF_spring_chinook$unacceptable_1_indiv_habitat_attributes[LF_spring_chinook_index], collapse=",")  },
      if(!is.na(LF_steelhead_index[1])){ paste(LF_steelhead$unacceptable_1_indiv_habitat_attributes[LF_steelhead_index], collapse=",")[1] },
      if(!is.na(LF_bull_trout_index[1])){paste(LF_bull_trout$unacceptable_1_indiv_habitat_attributes[LF_bull_trout_index], collapse=",")[1] }
      
    ) 
    unique_unacceptable_attributes = gsub(" ", "", unique_unacceptable_attributes, fixed = TRUE)
    unique_unacceptable_attributes = unique( unlist(strsplit(paste(unique_unacceptable_attributes, collapse=","), ",")) )
    unique_unacceptable_attributes = paste(unique_unacceptable_attributes, collapse=",")
    
    # ------------------------------------------------------------
    #     List All AT RISK Habitat Attributes
    # ------------------------------------------------------------
    unique_at_risk_attributes = c(
      
      if(!is.na(HQ_spring_chinook_index)){HQ_spring_chinook$at_risk_2_or_3_indiv_habitat_attributes[HQ_spring_chinook_index]},
      if(!is.na(HQ_steelhead_index)){HQ_steelhead$at_risk_2_or_3_indiv_habitat_attributes[HQ_steelhead_index]  },
      if(!is.na(HQ_bull_trout_index)){HQ_bull_trout$at_risk_2_or_3_indiv_habitat_attributes[HQ_bull_trout_index]  },
      if(!is.na(LF_spring_chinook_index[1])){  paste(LF_spring_chinook$at_risk_2_or_3_indiv_habitat_attributes[LF_spring_chinook_index], collapse=",")  },
      if(!is.na(LF_steelhead_index[1])){ paste(LF_steelhead$at_risk_2_or_3_indiv_habitat_attributes[LF_steelhead_index], collapse=",")[1] },
      if(!is.na(LF_bull_trout_index[1])){paste(LF_bull_trout$at_risk_2_or_3_indiv_habitat_attributes[LF_bull_trout_index], collapse=",")[1] }
      
    ) 
    unique_at_risk_attributes = gsub(" ", "", unique_at_risk_attributes, fixed = TRUE)
    unique_at_risk_attributes = unique( unlist(strsplit(paste(unique_at_risk_attributes, collapse=","), ",")) )
    unique_at_risk_attributes = paste(unique_at_risk_attributes, collapse=",")
    
    
    # ------------------------------------------------------------
    #     List for Each species
    # ------------------------------------------------------------
    life_stages_spring_chinook = c(
      if(!is.na(HQ_spring_chinook_index)){ "multiple"  },
      if(!is.na(LF_spring_chinook_index[1])){  paste(LF_spring_chinook$life_stage[LF_spring_chinook_index], collapse=",")  }
    ) 
    life_stages_spring_chinook = unique( unlist(strsplit(paste(life_stages_spring_chinook, collapse=","), ",")) )
    
    life_stages_steelhead = c(
      if(!is.na(HQ_steelhead_index)){  "multiple"  },
      if(!is.na(LF_steelhead_index[1])){  paste(LF_steelhead$life_stage[LF_steelhead_index], collapse=",") }
    ) 
    life_stages_steelhead = unique( unlist(strsplit(paste(life_stages_steelhead, collapse=","), ",")) )
    
    life_stages_bull_trout = c(
      if(!is.na(HQ_bull_trout_index)){  "multiple"  },
      if(!is.na(LF_bull_trout_index[1])){   paste(LF_bull_trout$life_stage[LF_bull_trout_index], collapse=",")   }
    ) 
    life_stages_bull_trout = unique( unlist(strsplit(paste(life_stages_bull_trout, collapse=","), ",")) )
    
    # -------- get life stages all ------------
    life_stages_all = unique(c( life_stages_spring_chinook, life_stages_steelhead, life_stages_bull_trout) )
    
    
    # ------------------------------------------------------------
    #    Loop through Each Habitat Attribute 
    # ------------------------------------------------------------
    for(habitat_attribute_x in habitat_attributes_all){
      
      # ------------------------------------------------------------
      #    For Each Life Stage -make a new row
      # ------------------------------------------------------------
      
      for(life_stage_x in life_stages_all){
        #print(" ------------------ ")
        #print(habitat_attribute_x)
        #print(life_stage_x)
        # ------------------------------------------------------------
        #     Add Reach Information Data 
        # ------------------------------------------------------------
        HQ_and_LF_combo_x = as.data.frame(Reach_Information_data[which(Reach_Information_data$ReachName == reach_x), columns_info])
        
        # ------------------------------------------------------------
        #     Add Habitat Attribute
        # ------------------------------------------------------------
        HQ_and_LF_combo_x$Habitat_Attribute = habitat_attribute_x
        
        # ------------------------------------------------------------
        #     Add Life Stage
        # ------------------------------------------------------------
        HQ_and_LF_combo_x$Life_Stage = life_stage_x 
        
        # ------------------------------------------------------------
        #    Action Categories
        # ------------------------------------------------------------
        action_category_x = FUNCTION_match_INDIVIDUAL_habitat_attributes_and_action_categories(habitat_attribute_x)
        number_of_actions_x = length(action_category_x)
        action_category_x = paste(action_category_x, collapse=",")
        # ------ add to row ---------
        HQ_and_LF_combo_x$Action_Categories = action_category_x
        HQ_and_LF_combo_x$Number_of_Actions = number_of_actions_x
        
        # ------------------------------------------------------------
        #   List the species
        # ------------------------------------------------------------
        species_x = c()
        # ------------- do yes no for pathways --------
        if(  !is.na(HQ_spring_chinook_index) | !is.na(LF_spring_chinook_index[1]) ){ 
          species_x = paste(species_x, "Spring Chinook", sep=",") }
        if(  !is.na(HQ_steelhead_index) | !is.na(LF_steelhead_index[1]) ){ 
          species_x = paste(species_x, "Steelhead", sep=",") }
        if(  !is.na(HQ_bull_trout_index) | !is.na(LF_bull_trout_index[1]) ){ 
          species_x = paste(species_x, "Bull Trout", sep=",") }
        species_x = substr(species_x,2,nchar(species_x))   # remove the leading comma
        # ------- add to output --------------
        HQ_and_LF_combo_x$Species = species_x
        
        # ------------------------------------------------------------
        #  Unacceptable Habitat Attributes (Yes/No)
        # ------------------------------------------------------------ 
        
        if(   length(grep(habitat_attribute_x, unique_at_risk_attributes)) > 0  ){
          HQ_and_LF_combo_x$Unacceptable_Habitat_Attributes_Presence = "yes"
        }else{
          HQ_and_LF_combo_x$Unacceptable_Habitat_Attributes_Presence = "no"
        }
        
        # ------------------------------------------------------------
        #  At Risk Habitat Attributes (Yes/No)
        # ------------------------------------------------------------ 
        if( length(grep(habitat_attribute_x, unique_unacceptable_attributes)) > 0   ){
          HQ_and_LF_combo_x$At_Risk_Habitat_Attributes_Presence = "yes"
        }else{
          HQ_and_LF_combo_x$At_Risk_Habitat_Attributes_Presence = "no"
        }
        
        # ------------------------------------------------------------
        #  Metric a Core metric
        # ------------------------------------------------------------ 
        HQ_and_LF_combo_x$Core_Metric = FUNCTION_match_INDIVIDUAL_core_metrics_from_habitat_attributes_SPECIES(species_x, life_stage_x, habitat_attribute_x )
        
        
        # ------------------------------------------------------------
        #  Reach Rank (FOR NOW just putting a "1")
        # ------------------------------------------------------------ 
        HQ_and_LF_combo_x$Reach_Rank = 1
        
        # ------------------------------------------------------------
        # Combine with output data frame
        # ------------------------------------------------------------ 
        
        Reach_Habitat_Attribute_combined_output = rbind(Reach_Habitat_Attribute_combined_output, HQ_and_LF_combo_x)
        
      }
      
       
    }
  }
  
  # ------------------------------------------------------------
  #    Update Habitat Attribute_Names
  # ------------------------------------------------------------
  unique_habitat_attributes = unique(Attribute_LifeStage_Crosswalk$Habitat_Attribute_2)
  for(habitat_attribute_x in unique_habitat_attributes){
    
    # ----------- new name (name with spaces in it so it is more readable) -------
    new_name_x = which(Attribute_LifeStage_Crosswalk$Habitat_Attribute_2 == habitat_attribute_x )
    new_name = Attribute_LifeStage_Crosswalk$`Habitat Attribute`[new_name_x[1]]
    # ------ identify all the places the name exists ---------
    rows_habitat_attribute_x = which(Reach_Habitat_Attribute_combined_output$Habitat_Attribute == habitat_attribute_x)
    # ------------ updated wit new name ------------
    Reach_Habitat_Attribute_combined_output$Habitat_Attribute[rows_habitat_attribute_x] = new_name
    
  }
  
  # ------------------------------------------------------------
  #    Remove Bull Trout Rows (if exclude_bull_trout is "yes") 
  # ------------------------------------------------------------
  
  if(exclude_bull_trout == "yes"){
    
    # ----------- which rows are Bull Trout ------------
    bull_trout_rows_x = which(Reach_Habitat_Attribute_combined_output$Species == "Bull Trout")
    # ------------- remove those rows from output ---------------
    Reach_Habitat_Attribute_combined_output = Reach_Habitat_Attribute_combined_output[ , -bull_trout_rows_x]
    
  }
  
  
  return(Reach_Habitat_Attribute_combined_output)
}





# ------------------------------------------------------------------------------------------
#
#      Function to combine across Reach - Habitat Attribute - Life Stage - Species
#         NOTE: added Species so 
#
# ------------------------------------------------------------------------------------------


# To Test
test_x = "yes"
if(test_x == "yes"){
  HQ_spring_chinook = Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']]
  HQ_steelhead = Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']]
  HQ_bull_trout = Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']]
  LF_spring_chinook = Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']]
  LF_steelhead = Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']]
  LF_bull_trout = Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Restoration']]
  columns_info = c( "ReachName","Basin","Assessment.Unit" ) # columns to automatically add to beginning (left side) of output
}


FUNCTION_combine_by_Reach_AND_Habitat_Attribute_Life_Stage_Species = function(HQ_spring_chinook, HQ_steelhead, HQ_bull_trout,  LF_spring_chinook, LF_steelhead, LF_bull_trout, columns_info, exclude_bull_trout, HQ_life_stages){
  
  # ------------------------------------------------------------
  #       Get Unique Reaches
  # ------------------------------------------------------------
  unique_reaches = unique( c(HQ_spring_chinook$ReachName, HQ_steelhead$ReachName, HQ_bull_trout$ReachName, 
                             LF_spring_chinook$ReachName, LF_steelhead$ReachName, LF_bull_trout$ReachName) )
  # ------------------------------------------------------------
  #      Loop through each Reach and combine data
  # ------------------------------------------------------------
  Reach_Habitat_Attribute_combined_output = c()
  
  for(reach_x in unique_reaches){
    
    
    # --------------------- generate HQ and LF index ----------
    if( any(HQ_spring_chinook$ReachName == reach_x)){ HQ_spring_chinook_index = which(HQ_spring_chinook$ReachName == reach_x) }else{HQ_spring_chinook_index = NA }
    if( any(HQ_steelhead$ReachName == reach_x)){ HQ_steelhead_index = which(HQ_steelhead$ReachName == reach_x) }else{HQ_steelhead_index = NA}
    if( any(HQ_bull_trout$ReachName == reach_x)){ HQ_bull_trout_index = which(HQ_bull_trout$ReachName == reach_x) }else{HQ_bull_trout_index = NA}
    if( any(LF_spring_chinook$ReachName == reach_x)){ LF_spring_chinook_index = which(LF_spring_chinook$ReachName == reach_x) }else{LF_spring_chinook_index = NA}
    if( any(LF_steelhead$ReachName == reach_x)){ LF_steelhead_index = which(LF_steelhead$ReachName == reach_x) }else{LF_steelhead_index = NA}
    if( any(LF_bull_trout$ReachName == reach_x)){ LF_bull_trout_index = which(LF_bull_trout$ReachName == reach_x) }else{LF_bull_trout_index = NA}
    
    # ------------------------------------------------------------
    #     List Habitat Attributes for each species and life stage
    # ------------------------------------------------------------

    if(!is.na(HQ_spring_chinook_index)){ habitat_attributes_spring_chinook_HQ = HQ_spring_chinook$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes[HQ_spring_chinook_index]}else{habitat_attributes_spring_chinook_HQ = NA}
    if(!is.na(HQ_steelhead_index)){ habitat_attributes_steelhead_HQ = HQ_steelhead$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes[HQ_steelhead_index]  }else{habitat_attributes_steelhead_HQ = NA}
    if(!is.na(HQ_bull_trout_index)){ habitat_attributes_bull_trout_HQ = HQ_bull_trout$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes[HQ_bull_trout_index]  }else{habitat_attributes_bull_trout_HQ = NA}
    if(!is.na(LF_spring_chinook_index[1])){  habitat_attributes_spring_chinook_LF = paste(LF_spring_chinook$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes[LF_spring_chinook_index], collapse=",")  }else{habitat_attributes_spring_chinook_LF = NA}
    if(!is.na(LF_steelhead_index[1])){  habitat_attributes_steelhead_LF = paste(LF_steelhead$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes[LF_steelhead_index], collapse=",")[1] }else{habitat_attributes_steelhead_LF = NA}
    if(!is.na(LF_bull_trout_index[1])){ habitat_attributes_bull_trout_LF = paste(LF_bull_trout$unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes[LF_bull_trout_index], collapse=",")[1] }else{habitat_attributes_bull_trout_LF = NA}
    
    # ------------------------------------------------------------
    #     List All UNACCEPTABLE Habitat Attributes for each species and life stage
    # ------------------------------------------------------------

    # gsub function removes commas
    if(!is.na(HQ_spring_chinook_index)){unique_unacceptable_attributes_spring_chinook_HQ = gsub(" ", "", HQ_spring_chinook$unacceptable_1_indiv_habitat_attributes[HQ_spring_chinook_index], fixed = TRUE) }else{unique_unacceptable_attributes_spring_chinook_HQ = NA}
    if(!is.na(HQ_steelhead_index)){unique_unacceptable_attributes_steelhead_HQ = gsub(" ", "", HQ_steelhead$unacceptable_1_indiv_habitat_attributes[HQ_steelhead_index], fixed = TRUE) }else{unique_unacceptable_attributes_steelhead_HQ = NA}
    if(!is.na(HQ_bull_trout_index)){unique_unacceptable_attributes_bull_trout_HQ = gsub(" ", "", HQ_bull_trout$unacceptable_1_indiv_habitat_attributes[HQ_bull_trout_index], fixed = TRUE) }else{unique_unacceptable_attributes_bull_trout_HQ = NA}
    if(!is.na(LF_spring_chinook_index[1])){  unique_unacceptable_attributes_spring_chinook_LF = gsub(" ", "", paste(LF_spring_chinook$unacceptable_1_indiv_habitat_attributes[LF_spring_chinook_index], collapse=",")[1], fixed = TRUE) }else{unique_unacceptable_attributes_spring_chinook_LF = NA}
    if(!is.na(LF_steelhead_index[1])){ unique_unacceptable_attributes_steelhead_LF = gsub(" ", "", paste(LF_steelhead$unacceptable_1_indiv_habitat_attributes[LF_steelhead_index], collapse=",")[1], fixed = TRUE) }else{unique_unacceptable_attributes_steelhead_LF = NA}
    if(!is.na(LF_bull_trout_index[1])){ unique_unacceptable_attributes_bull_trout_LF = gsub(" ", "", paste(LF_bull_trout$unacceptable_1_indiv_habitat_attributes[LF_bull_trout_index], collapse=",")[1], fixed = TRUE) }else{unique_unacceptable_attributes_bull_trout_LF = NA}
    
    # ------------------------------------------------------------
    #     List All AT RISK Habitat Attributes for each species and life stage
    # ------------------------------------------------------------
    
    # gsub function removes commas
    if(!is.na(HQ_spring_chinook_index)){unique_at_risk_attributes_spring_chinook_HQ = gsub(" ", "", HQ_spring_chinook$at_risk_2_or_3_indiv_habitat_attributes[HQ_spring_chinook_index], fixed = TRUE) }else{unique_at_risk_attributes_spring_chinook_HQ = NA}
    if(!is.na(HQ_steelhead_index)){unique_at_risk_attributes_steelhead_HQ = gsub(" ", "", HQ_steelhead$at_risk_2_or_3_indiv_habitat_attributes[HQ_steelhead_index], fixed = TRUE) }else{unique_at_risk_attributes_steelhead_HQ = NA}
    if(!is.na(HQ_bull_trout_index)){unique_at_risk_attributes_bull_trout_HQ = gsub(" ", "", HQ_bull_trout$at_risk_2_or_3_indiv_habitat_attributes[HQ_bull_trout_index], fixed = TRUE) }else{unique_at_risk_attributes_bull_trout_HQ = NA}
    if(!is.na(LF_spring_chinook_index[1])){  unique_at_risk_attributes_spring_chinook_LF = gsub(" ", "", paste(LF_spring_chinook$at_risk_2_or_3_indiv_habitat_attributes[LF_spring_chinook_index], collapse=",")[1], fixed = TRUE) }else{unique_at_risk_attributes_spring_chinook_LF = NA}
    if(!is.na(LF_steelhead_index[1])){ unique_at_risk_attributes_steelhead_LF = gsub(" ", "", paste(LF_steelhead$at_risk_2_or_3_indiv_habitat_attributes[LF_steelhead_index], collapse=",")[1], fixed = TRUE) }else{unique_at_risk_attributes_steelhead_LF = NA}
    if(!is.na(LF_bull_trout_index[1])){ unique_at_risk_attributes_bull_trout_LF = gsub(" ", "", paste(LF_bull_trout$at_risk_2_or_3_indiv_habitat_attributes[LF_bull_trout_index], collapse=",")[1], fixed = TRUE) }else{unique_at_risk_attributes_bull_trout_LF = NA}
    
    # ------------------------------------------------------------
    #     List for Each species - IF just generating life stage "multiple" for HQ
    # ------------------------------------------------------------
 
    if(!is.na(HQ_spring_chinook_index)){ life_stages_spring_chinook_HQ = "multiple"  }else{ life_stages_spring_chinook_HQ = c()}
    if(!is.na(LF_spring_chinook_index[1])){  life_stages_spring_chinook_LF = paste(LF_spring_chinook$life_stage[LF_spring_chinook_index], collapse=",")  }else{ life_stages_spring_chinook_LF = c()}
  
    if(!is.na(HQ_steelhead_index)){ life_stages_steelhead_HQ = "multiple" }else{ life_stages_steelhead_HQ = c()}
    if(!is.na(LF_steelhead_index[1])){  life_stages_steelhead_LF =paste(LF_steelhead$life_stage[LF_steelhead_index], collapse=",") }else{ life_stages_steelhead_LF = c()}

    if(!is.na(HQ_bull_trout_index)){ life_stages_bull_trout_HQ = "multiple"  }else{ life_stages_bull_trout_HQ = c()}
    if(!is.na(LF_bull_trout_index[1])){  life_stages_bull_trout_LF = paste(LF_bull_trout$life_stage[LF_bull_trout_index], collapse=",")   }else{ life_stages_bull_trout_LF = c()  }
    
    # ------------------------------------------------------------
    #    IF add life stages for Habitat Quality
    # ------------------------------------------------------------
    
    if(HQ_life_stages == "yes"){
      # ------------- Spring Chinook -----------
      if(!is.na(HQ_spring_chinook_index)){    life_stages_spring_chinook_HQ = FUNCTION_generate_life_stage_list_for_species_reach("Spring Chinook", reach_x)   }else{ life_stages_spring_chinook_HQ = c()}
      if(!is.na(LF_spring_chinook_index[1])){  life_stages_spring_chinook_LF = paste(LF_spring_chinook$life_stage[LF_spring_chinook_index], collapse=",")  }else{ life_stages_spring_chinook_LF = c()}
      # ----------- Steelhead -----------------
      if(!is.na(HQ_steelhead_index)){    life_stages_steelhead_HQ = FUNCTION_generate_life_stage_list_for_species_reach("Steelhead", reach_x)        }else{ life_stages_steelhead_HQ = c()}
      if(!is.na(LF_steelhead_index[1])){ life_stages_steelhead_LF =paste(LF_steelhead$life_stage[LF_steelhead_index], collapse=",") }else{ life_stages_steelhead_LF = c()}
      # --------------- Bull Trout -------------
      if(!is.na(HQ_bull_trout_index)){   life_stages_bull_trout_HQ = FUNCTION_generate_life_stage_list_for_species_reach("Bull Trout", reach_x)     }else{ life_stages_bull_trout_HQ = c()}
      if(!is.na(LF_bull_trout_index[1])){   life_stages_bull_trout_LF = paste(LF_bull_trout$life_stage[LF_bull_trout_index], collapse=",")  }else{ life_stages_bull_trout_LF = c()  }
    }
    # ------------------------------------------------------------
    #     List All Species in this Reach
    # ------------------------------------------------------------
    species_list = c()
    if(  !is.na(HQ_spring_chinook_index) | !is.na(LF_spring_chinook_index[1]) ){ 
      species_list = paste(species_list, "Spring Chinook", sep=",") }
    if(  !is.na(HQ_steelhead_index) | !is.na(LF_steelhead_index[1]) ){ 
      species_list = paste(species_list, "Steelhead", sep=",") }
    if(  !is.na(HQ_bull_trout_index) | !is.na(LF_bull_trout_index[1]) ){ 
      species_list = paste(species_list, "Bull Trout", sep=",") }
    species_list = substr(species_list,2,nchar(species_list))   # remove the leading comma
    species_list = unique( unlist(strsplit(paste(species_list, collapse=","), ",")) )
    
    # ------------------------------------------------------------
    #    For Each Life Stage - make a new row
    # ------------------------------------------------------------
    for(species_x in species_list){
      
      if(species_x == "Spring Chinook"){
        if(!is.null(life_stages_spring_chinook_HQ)){ life_stages_HQ =   unique(unlist(strsplit( life_stages_spring_chinook_HQ, "," ))) }else{life_stages_HQ = NA}
        if(!is.null(life_stages_spring_chinook_LF)){ life_stages_LF =   unique(unlist(strsplit( life_stages_spring_chinook_LF, "," ))) }else{life_stages_LF = NA}
        }
      if(species_x == "Steelhead"){ 
        if(!is.null(life_stages_steelhead_HQ)){ life_stages_HQ =   unique(unlist(strsplit( life_stages_steelhead_HQ, "," ))) }else{life_stages_HQ = NA}
        if(!is.null(life_stages_steelhead_LF)){ life_stages_LF =   unique(unlist(strsplit( life_stages_steelhead_LF, "," )))  }else{life_stages_LF = NA}
      }
      if(species_x == "Bull Trout"){ 
        if(!is.null(life_stages_bull_trout_HQ)){ life_stages_HQ =   unique(unlist(strsplit( life_stages_bull_trout_HQ, "," ))) }else{life_stages_HQ = NA}
        if(!is.null(life_stages_bull_trout_LF)){ life_stages_LF =   unique(unlist(strsplit( life_stages_bull_trout_LF, "," )))  }else{life_stages_LF = NA}
      }
      
      # ------------------------------------------------------------
      #    Habitat Quality Pathways
      # ------------------------------------------------------------
      for(life_stage_x in life_stages_HQ){
        
        # ------------------------------------------------------------
        #  Match Impaired Habitat Attributes to Life Stage Habitat Attributes
        # ------------------------------------------------------------
        
        # ---------=-------------------------
        #  Pull Life Stage habitat attributes
        # ---------=-------------------------
        life_stage_habitat_attributes = Attribute_LifeStage_Crosswalk %>%
          filter( `Life Stage` %in% life_stage_x) %>%
          filter( Species %in% species_x ) %>%
          dplyr::select(`Habitat Attribute`)
        life_stage_habitat_attributes = as.data.frame(life_stage_habitat_attributes)
        life_stage_habitat_attributes = life_stage_habitat_attributes$`Habitat Attribute`
        # ------------- remove the blank space ---------------
        life_stage_habitat_attributes = gsub(" ","",life_stage_habitat_attributes)
        
        # ----------------------------------
        #  Get Life Stages in HQ Pathway
        # ----------------------------------
        if( species_x == "Spring Chinook"  ){
          if(!is.na(habitat_attributes_spring_chinook_HQ)){
            habitat_attributes_species = unique( unlist(strsplit(habitat_attributes_spring_chinook_HQ, ","))) }else{habitat_attributes_species = NA}
        }
        if( species_x == "Steelhead"){
          if(!is.na(habitat_attributes_steelhead_HQ)){
            habitat_attributes_species = unique( unlist(strsplit(habitat_attributes_steelhead_HQ, ","))) }else{habitat_attributes_species = NA}
        }
        if( species_x == "Bull Trout"){
          if(!is.na(habitat_attributes_bull_trout_HQ)){
            habitat_attributes_species = unique( unlist(strsplit(habitat_attributes_bull_trout_HQ, ","))) }else{habitat_attributes_species = NA}
        }
        
        # ----------------------------------
        #  Overlap between life stage and HQ
        # ----------------------------------
        habitat_attributes_all = intersect(life_stage_habitat_attributes, habitat_attributes_species)
        
        # ----------------------------------
        #  Unacceptable and At Risk attributes
        # ----------------------------------
        
        if(species_x == "Spring Chinook"){ 
          unique_unacceptable_attributes = unique_unacceptable_attributes_spring_chinook_HQ
          unique_at_risk_attributes = unique_at_risk_attributes_spring_chinook_HQ
        }
        if(species_x == "Steelhead"){ 
          unique_unacceptable_attributes = unique_unacceptable_attributes_steelhead_HQ
          unique_at_risk_attributes = unique_at_risk_attributes_steelhead_HQ
        }
        if(species_x == "Bull Trout"){ 
          unique_unacceptable_attributes = unique_unacceptable_attributes_bull_trout_HQ
          unique_at_risk_attributes = unique_at_risk_attributes_bull_trout_HQ
        }
        
        
        # ------------------------------------------------------------
        #    Loop through Each Habitat Attribute 
        # ------------------------------------------------------------
        if( length(habitat_attributes_all) > 0 ){
          for(habitat_attribute_x in habitat_attributes_all){
            
            # ------------------------------------------------------------
            #     Add Reach Information Data 
            # ------------------------------------------------------------
            HQ_and_LF_combo_x = as.data.frame(Reach_Information_data[which(Reach_Information_data$ReachName == reach_x), columns_info])
            
            # ------------------------------------------------------------
            #     Add Habitat Attribute
            # ------------------------------------------------------------
            HQ_and_LF_combo_x$Habitat_Attribute = habitat_attribute_x
            
            # ------------------------------------------------------------
            #   List the species
            # ------------------------------------------------------------
            HQ_and_LF_combo_x$Species = species_x 
            
            # ------------------------------------------------------------
            #     Add Life Stage
            # ------------------------------------------------------------
            HQ_and_LF_combo_x$Life_Stage = life_stage_x 
            
            # ------------------------------------------------------------
            #     Add Pathway 
            # ------------------------------------------------------------
            HQ_and_LF_combo_x$Action = "Restore Reach Function" 
            
            # ------------------------------------------------------------
            #    Action Categories
            # ------------------------------------------------------------
            action_category_x = FUNCTION_match_INDIVIDUAL_habitat_attributes_and_action_categories(habitat_attribute_x)
            number_of_actions_x = length(action_category_x)
            action_category_x = paste(action_category_x, collapse=",")
            # ------ add to row ---------
            HQ_and_LF_combo_x$Action_Categories = action_category_x
            HQ_and_LF_combo_x$Number_of_Actions = number_of_actions_x
            
            
            # ------------------------------------------------------------
            #  Unacceptable Habitat Attributes (Yes/No)
            # ------------------------------------------------------------ 
            
            if(   length( grep(habitat_attribute_x, unique_at_risk_attributes) ) > 0  ){
              HQ_and_LF_combo_x$Unacceptable_Habitat_Attributes_Presence = "yes"
            }else{
              HQ_and_LF_combo_x$Unacceptable_Habitat_Attributes_Presence = "no"
            }
            
            # ------------------------------------------------------------
            #  At Risk Habitat Attributes (Yes/No)
            # ------------------------------------------------------------ 
            if( length(grep(habitat_attribute_x, unique_unacceptable_attributes)) > 0   ){
              HQ_and_LF_combo_x$At_Risk_Habitat_Attributes_Presence = "yes"
            }else{
              HQ_and_LF_combo_x$At_Risk_Habitat_Attributes_Presence = "no"
            }
            
            # ------------------------------------------------------------
            #  Metric a Core metric
            # ------------------------------------------------------------ 
            HQ_and_LF_combo_x$Core_Metric = FUNCTION_match_INDIVIDUAL_core_metrics_from_habitat_attributes_SPECIES(species_x, life_stage_x, habitat_attribute_x )
            
            # ------------------------------------------------------------
            #  Reach Rank (FOR NOW just putting a "1")
            # ------------------------------------------------------------ 
            HQ_and_LF_combo_x$Reach_Rank = 1
            
            # ------------------------------------------------------------
            # Combine with output data frame
            # ------------------------------------------------------------ 
            
            Reach_Habitat_Attribute_combined_output = rbind(Reach_Habitat_Attribute_combined_output, HQ_and_LF_combo_x)
          }
        }

      }
      
      # ------------------------------------------------------------
      #    Limiting Factor
      # ------------------------------------------------------------
      for(life_stage_x in life_stages_LF){
        
        # ------------------------------------------------------------
        #  Match Impaired Habitat Attributes to Life Stage Habitat Attributes
        # ------------------------------------------------------------
        
        # ---------=-------------------------
        #  Pull Life Stage habitat attributes
        # ---------=-------------------------
        life_stage_habitat_attributes = Attribute_LifeStage_Crosswalk %>%
          filter( `Life Stage` %in% life_stage_x) %>%
          filter( Species %in% species_x ) %>%
          dplyr::select(`Habitat Attribute`)
        life_stage_habitat_attributes = as.data.frame(life_stage_habitat_attributes)
        life_stage_habitat_attributes = life_stage_habitat_attributes$`Habitat Attribute`
        # ------------- remove the blank space ---------------
        life_stage_habitat_attributes = gsub(" ","",life_stage_habitat_attributes)
        
        # ----------------------------------
        #  Get Life Stages in LF Pathway
        # ----------------------------------
        if( species_x == "Spring Chinook"  ){
          if(!is.na(habitat_attributes_spring_chinook_LF)){
            habitat_attributes_species = unique( unlist(strsplit(habitat_attributes_spring_chinook_LF, ","))) }else{habitat_attributes_species = NA}
        }
        if( species_x == "Steelhead"){
          if(!is.na(habitat_attributes_steelhead_LF)){
            habitat_attributes_species = unique( unlist(strsplit(habitat_attributes_steelhead_LF, ","))) }else{habitat_attributes_species = NA}
        }
        if( species_x == "Bull Trout"){
          if(!is.na(habitat_attributes_bull_trout_LF)){
            habitat_attributes_species = unique( unlist(strsplit(habitat_attributes_bull_trout_LF, ","))) }else{habitat_attributes_species = NA}
        }
        habitat_attributes_species  = gsub(" ","",habitat_attributes_species)
        
        # ----------------------------------
        #  Overlap between life stage and HQ
        # ----------------------------------
        habitat_attributes_all = intersect(life_stage_habitat_attributes, habitat_attributes_species)
        
        # ----------------------------------
        #  Unacceptable and At Risk attributes
        # ----------------------------------
        
        if(species_x == "Spring Chinook"){ 
          unique_unacceptable_attributes = unique_unacceptable_attributes_spring_chinook_LF
          unique_at_risk_attributes = unique_at_risk_attributes_spring_chinook_LF
        }
        if(species_x == "Steelhead"){ 
          unique_unacceptable_attributes = unique_unacceptable_attributes_steelhead_LF
          unique_at_risk_attributes = unique_at_risk_attributes_steelhead_LF
        }
        if(species_x == "Bull Trout"){ 
          unique_unacceptable_attributes = unique_unacceptable_attributes_bull_trout_LF
          unique_at_risk_attributes = unique_at_risk_attributes_bull_trout_LF
        }
        
        # ------------------------------------------------------------
        #    Loop through Each Habitat Attribute 
        # ------------------------------------------------------------
        if( length(habitat_attributes_all) > 0 ){
          for(habitat_attribute_x in habitat_attributes_all){
            
            # ------------------------------------------------------------
            #     Add Reach Information Data 
            # ------------------------------------------------------------
            HQ_and_LF_combo_x = as.data.frame(Reach_Information_data[which(Reach_Information_data$ReachName == reach_x), columns_info])
            
            # ------------------------------------------------------------
            #     Add Habitat Attribute
            # ------------------------------------------------------------
            HQ_and_LF_combo_x$Habitat_Attribute = habitat_attribute_x
            
            # ------------------------------------------------------------
            #   List the species
            # ------------------------------------------------------------
            HQ_and_LF_combo_x$Species = species_x
            
            # ------------------------------------------------------------
            #     Add Life Stage
            # ------------------------------------------------------------
            HQ_and_LF_combo_x$Life_Stage = life_stage_x 
            
            # ------------------------------------------------------------
            #     Add Pathway 
            # ------------------------------------------------------------
            HQ_and_LF_combo_x$Action = "Address Limiting Factors" 
            
            # ------------------------------------------------------------
            #    Action Categories
            # ------------------------------------------------------------
            action_category_x = FUNCTION_match_INDIVIDUAL_habitat_attributes_and_action_categories(habitat_attribute_x)
            number_of_actions_x = length(action_category_x)
            action_category_x = paste(action_category_x, collapse=",")
            # ------ add to row ---------
            HQ_and_LF_combo_x$Action_Categories = action_category_x
            HQ_and_LF_combo_x$Number_of_Actions = number_of_actions_x
            
            
            # ------------------------------------------------------------
            #  Unacceptable Habitat Attributes (Yes/No)
            # ------------------------------------------------------------ 
            
            if(   length(grep(habitat_attribute_x, unique_at_risk_attributes)) > 0  ){
              HQ_and_LF_combo_x$Unacceptable_Habitat_Attributes_Presence = "yes"
            }else{
              HQ_and_LF_combo_x$Unacceptable_Habitat_Attributes_Presence = "no"
            }
            
            # ------------------------------------------------------------
            #  At Risk Habitat Attributes (Yes/No)
            # ------------------------------------------------------------ 
            if( length(grep(habitat_attribute_x, unique_unacceptable_attributes)) > 0   ){
              HQ_and_LF_combo_x$At_Risk_Habitat_Attributes_Presence = "yes"
            }else{
              HQ_and_LF_combo_x$At_Risk_Habitat_Attributes_Presence = "no"
            }
            
            # ------------------------------------------------------------
            #  Metric a Core metric
            # ------------------------------------------------------------ 
            HQ_and_LF_combo_x$Core_Metric = FUNCTION_match_INDIVIDUAL_core_metrics_from_habitat_attributes_SPECIES(species_x, life_stage_x, habitat_attribute_x )
            
            # ------------------------------------------------------------
            #  Reach Rank (FOR NOW just putting a "1")
            # ------------------------------------------------------------ 
            HQ_and_LF_combo_x$Reach_Rank = 1
            
            # ------------------------------------------------------------
            # Combine with output data frame
            # ------------------------------------------------------------ 
            
            Reach_Habitat_Attribute_combined_output = rbind(Reach_Habitat_Attribute_combined_output, HQ_and_LF_combo_x)
          }
        }
      }
    }
  
  } # end of reaches loops
  

  # ------------------------------------------------------------
  #    Add Fish Barrier 
  # ------------------------------------------------------------
  for(row_x in 1:nrow(Barriers_Pathways_Data)){
    
    # --------------- generate items 
    reach_x = Barriers_Pathways_Data$ReachName[row_x]
    habitat_attribute_x = Barriers_Pathways_Data$Habitat_Attributes[row_x]
    action_x = Barriers_Pathways_Data$`Action Category`[row_x]
    number_of_actions_x = 1
    
    # ------------------------------------------------------------
    #     Add Reach Information Data 
    # ------------------------------------------------------------
    HQ_and_LF_combo_x = as.data.frame(Reach_Information_data[which(Reach_Information_data$ReachName == reach_x), columns_info])
    
    # ------------------------------------------------------------
    #     Add Habitat Attribute
    # ------------------------------------------------------------
    HQ_and_LF_combo_x$Habitat_Attribute = habitat_attribute_x
    
    # ------------------- use this info for all rows for this reach/habitat attribute (barriers) ----------
    HQ_and_LF_combo_x_ORIG = HQ_and_LF_combo_x
    
    # ------------------------------------------------------------
    #     Identify life stages in reach (based on reach presence)
    # ------------------------------------------------------------
    spring_chinook_presence = FUNCTION_generate_life_stage_list_for_species_reach("Spring Chinook", reach_x)
    steelhead_presence = FUNCTION_generate_life_stage_list_for_species_reach("Steelhead", reach_x)
    bull_trout_presence = FUNCTION_generate_life_stage_list_for_species_reach("Bull Trout", reach_x)
    
    # --------------- get list of species --------------
    species_list = c()
    if(!is.null(spring_chinook_presence)){species_list = paste(species_list, "Spring Chinook", sep=",")}
    if(!is.null(steelhead_presence)){species_list = paste(species_list, "Steelhead", sep=",")}
    if(!is.null(bull_trout_presence)){species_list = paste(species_list, "Bull Trout",sep= ",")}
    species_list = unlist( strsplit(substr(species_list,2,nchar(species_list)), "," )  )
                           
    # -------------------- LOOP through each species ----------
    for(species_x in species_list){
      
      # ------------- start new row --------------
      HQ_and_LF_combo_x = HQ_and_LF_combo_x_ORIG
      
      # ------------------------------------------------------------
      #   List the species
      # ------------------------------------------------------------
      HQ_and_LF_combo_x$Species = species_x
      
      # ------------ save row for each life stage ------
      HQ_and_LF_combo_x_Species = HQ_and_LF_combo_x
      
      # ------------------------------------------------------------
      #   Get list of life stages for this species
      # ------------------------------------------------------------
      if(species_x == "Spring Chinook"){  life_stages_all = unlist(strsplit(spring_chinook_presence, ","))   }
      if(species_x == "Steelhead"){  life_stages_all = unlist(strsplit(steelhead_presence, ","))   }
      if(species_x == "Bull Trout"){  life_stages_all = unlist(strsplit(bull_trout_presence, ","))   }

      for(life_stage_x in life_stages_all){
        
        # ------------- start new row --------------
        HQ_and_LF_combo_x = HQ_and_LF_combo_x_Species
        
        # ------------------------------------------------------------
        #     Add Life Stage
        # ------------------------------------------------------------
        HQ_and_LF_combo_x$Life_Stage = life_stage_x 
        
        # ------------------------------------------------------------
        #     Add Action pathway
        # ------------------------------------------------------------
        HQ_and_LF_combo_x$Action = "Restore Fish Passage"
        
        # ------------------------------------------------------------
        #    Action Categories
        # ------------------------------------------------------------
        # ------ add to row ---------
        HQ_and_LF_combo_x$Action_Categories = action_category_x
        HQ_and_LF_combo_x$Number_of_Actions = number_of_actions_x
        
        # ------------------------------------------------------------
        #  Unacceptable and At Risk Habitat Attributes (Yes/No)
        # ------------------------------------------------------------ 
        HQ_and_LF_combo_x$Unacceptable_Habitat_Attributes_Presence = "no"
        HQ_and_LF_combo_x$At_Risk_Habitat_Attributes_Presence = "no"
        
        # ------------------------------------------------------------
        #  Metric a Core metric
        # ------------------------------------------------------------ 
        HQ_and_LF_combo_x$Core_Metric = "no"
        
        # ------------------------------------------------------------
        #  Reach Rank (FOR NOW just putting a "1")
        # ------------------------------------------------------------ 
        HQ_and_LF_combo_x$Reach_Rank = 1
        
        # ------------------------------------------------------------
        # Combine with output data frame
        # ------------------------------------------------------------ 
        Reach_Habitat_Attribute_combined_output = rbind(Reach_Habitat_Attribute_combined_output, HQ_and_LF_combo_x)
      }
      
    }

  }
  
  
  # ------------------------------------------------------------
  #    Remove Duplicates (since can have same life stage and habitat attribute via HQ and LF pathways)
  # ------------------------------------------------------------
  reachname_habitat_attribute_life_stage_species_combo = with(Reach_Habitat_Attribute_combined_output, paste0(ReachName, Habitat_Attribute, Life_Stage, Species))
  duplicated_combos = which( duplicated(reachname_habitat_attribute_life_stage_species_combo) )
  Reach_Habitat_Attribute_combined_output = Reach_Habitat_Attribute_combined_output[-duplicated_combos, ]
  
  # ------------------------------------------------------------
  #    Update Habitat Attribute_Names
  # ------------------------------------------------------------
  unique_habitat_attributes = unique(Attribute_LifeStage_Crosswalk$Habitat_Attribute_2)
  for(habitat_attribute_x in unique_habitat_attributes){
    
    # ----------- new name (name with spaces in it so it is more readable) -------
    new_name_x = which(Attribute_LifeStage_Crosswalk$Habitat_Attribute_2 == habitat_attribute_x )
    new_name = Attribute_LifeStage_Crosswalk$`Habitat Attribute`[new_name_x[1]]
    # ------ identify all the places the name exists ---------
    rows_habitat_attribute_x = which(Reach_Habitat_Attribute_combined_output$Habitat_Attribute == habitat_attribute_x)
    # ------------ updated wit new name ------------
    Reach_Habitat_Attribute_combined_output$Habitat_Attribute[rows_habitat_attribute_x] = new_name
    
  }
  
  # ------------------------------------------------------------
  #    Remove Bull Trout Rows (if exclude_bull_trout is "yes") 
  # ------------------------------------------------------------
 
  if(exclude_bull_trout == "yes"){
    
    # ----------- which rows are Bull Trout ------------
    bull_trout_rows_x = which(Reach_Habitat_Attribute_combined_output$Species == "Bull Trout")
    # ------------- remove those rows from output ---------------
    Reach_Habitat_Attribute_combined_output = Reach_Habitat_Attribute_combined_output[ -bull_trout_rows_x , ]
    
  }
  
  # ------------------------------------------------------------
  #   Update References for WebMap: PRCT (%), AND (&), and the "/" as a space
  # ------------------------------------------------------------
  # -------------------- Fines/Embeddedness ------------------
  rename_rows = which(Reach_Habitat_Attribute_combined_output$Habitat_Attribute == "% Fines/Embeddedness")
  Reach_Habitat_Attribute_combined_output$Habitat_Attribute[rename_rows] = "PRCNT Fines and Embeddedness"
  # -------------------- Pool Quantity and Quality ------------------
  rename_rows = which(Reach_Habitat_Attribute_combined_output$Habitat_Attribute == "Pool Quantity & Quality" )
  Reach_Habitat_Attribute_combined_output$Habitat_Attribute[rename_rows] = "Pool Quantity and Quality" 
  # -------------------- Entrainment and Stranding ------------------
  rename_rows = which(Reach_Habitat_Attribute_combined_output$Habitat_Attribute == "Entrainment/Stranding" )
  Reach_Habitat_Attribute_combined_output$Habitat_Attribute[rename_rows] = "Entrainment and Stranding"

  
  return(Reach_Habitat_Attribute_combined_output)
}






# ------------------------------------------------------------------------------------------
#
#      Generate life stages for HQ pathway for specific reach (based on life stage presence data)
#
# ------------------------------------------------------------------------------------------

FUNCTION_generate_life_stage_list_for_species_reach = function(species_x, reach_x){
  
  # ----------------------- filter by reach ------
  Life_Stage_Priorities_AU_and_Reach_data_REACH_X = Life_Stage_Priorities_AU_and_Reach_data[ which(Life_Stage_Priorities_AU_and_Reach_data$ReachName == reach_x), ] 
  
  life_stage_list_x = c()
  
  # --------- Spring Chinook --------
  if(species_x == "Spring Chinook"){
    
    for(life_stage_i in names(spring_chinook_life_stages_presence)){
      # ------- generate name of column for this life stage --------
      life_stage_i2 = spring_chinook_life_stages_presence[[life_stage_i]]
      # -------------- pull the value -------
      life_stage_presence_0_1 = Life_Stage_Priorities_AU_and_Reach_data_REACH_X[[life_stage_i2]]
      # -------- add life stage name -------
      if(life_stage_presence_0_1 == 1){ life_stage_list_x = paste(life_stage_list_x,life_stage_i, sep="," ) }
    }
  }
  
  
  # --------- Steelhead --------
  if(species_x == "Steelhead"){
    
    for(life_stage_i in names(steelhead_life_stages_presence)){
      # ------- generate name of column for this life stage --------
      life_stage_i2 = steelhead_life_stages_presence[[life_stage_i]]
      # -------------- pull the value -------
      life_stage_presence_0_1 = Life_Stage_Priorities_AU_and_Reach_data_REACH_X[[life_stage_i2]]
      # -------- add life stage name -------
      if(life_stage_presence_0_1 == 1){ life_stage_list_x = paste(life_stage_list_x,life_stage_i, sep="," ) }
    }
  }
  
  # --------- Bull Trout --------
  if(species_x == "Bull Trout"){
    
    for(life_stage_i in names(bull_trout_life_stages_presence)){
      # ------- generate name of column for this life stage --------
      life_stage_i2 = bull_trout_life_stages_presence[[life_stage_i]]
      # -------------- pull the value -------
      life_stage_presence_0_1 = Life_Stage_Priorities_AU_and_Reach_data_REACH_X[[life_stage_i2]]
      # -------- add life stage name -------
      if(life_stage_presence_0_1 == 1){ life_stage_list_x = paste(life_stage_list_x,life_stage_i, sep="," ) }
    }
  }
  
  # --------- remove the leading comma ------
  if(!is.null(life_stage_list_x)){
    life_stage_list_x  = substr(life_stage_list_x,2,nchar(life_stage_list_x))
  }
  
  return(life_stage_list_x)
  
  
}

# ------------------------------------------------------------------------------------------
#
#      Function to add Barriers Prioritization
#         NOTE: added Species so 
#
# ------------------------------------------------------------------------------------------













# ------------------------------------------------------------------------------------------
#
#      Function to remove Bull Trout
#         NOTE: added Species so 
#
# ------------------------------------------------------------------------------------------




    

  


