
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
# HQ_spring_chinook = Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']]
# HQ_steelhead = Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']]
# HQ_bull_trout = Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']]
# LF_spring_chinook = Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']]
# LF_steelhead = Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']]
# LF_bull_trout = Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Restoration']]
# columns_info = c( "ReachName","Basin","Assessment.Unit" ) # columns to automatically add to beginning (left side) of output

FUNCTION_combine_by_Reach_AND_Habitat_Attribute_Life_Stage = function(HQ_spring_chinook, HQ_steelhead, HQ_bull_trout,  LF_spring_chinook, LF_steelhead, LF_bull_trout, columns_info){
  
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
      if(!is.na(LF_steelhead_index[1])){ paste(LF_spring_chinook$unacceptable_1_indiv_habitat_attributes[LF_spring_chinook_index], collapse=",")[1] },
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
      if(!is.na(LF_steelhead_index[1])){ paste(LF_spring_chinook$at_risk_2_or_3_indiv_habitat_attributes[LF_spring_chinook_index], collapse=",")[1] },
      if(!is.na(LF_bull_trout_index[1])){paste(LF_bull_trout$at_risk_2_or_3_indiv_habitat_attributes[LF_bull_trout_index], collapse=",")[1] }
      
    ) 
    unique_at_risk_attributes = gsub(" ", "", unique_at_risk_attributes, fixed = TRUE)
    unique_at_risk_attributes = unique( unlist(strsplit(paste(unique_at_risk_attributes, collapse=","), ",")) )
    unique_at_risk_attributes = paste(unique_at_risk_attributes, collapse=",")
    
    # ------------------------------------------------------------
    #     List All the Life Stages
    # ------------------------------------------------------------
    life_stages_all = c(
      
      if(!is.na(HQ_spring_chinook_index)){ "multiple"  },
      if(!is.na(HQ_steelhead_index)){  "multiple"  },
      if(!is.na(HQ_bull_trout_index)){  "multiple"  },
      if(!is.na(LF_spring_chinook_index[1])){  paste(LF_spring_chinook$life_stage[LF_spring_chinook_index], collapse=",")  },
      if(!is.na(LF_steelhead_index[1])){  paste(LF_spring_chinook$life_stage[LF_spring_chinook_index], collapse=",") },
      if(!is.na(LF_bull_trout_index[1])){   paste(LF_bull_trout$life_stage[LF_bull_trout_index], collapse=",")   }
      
    ) 
    life_stages_all = unique( unlist(strsplit(paste(life_stages_all, collapse=","), ",")) )

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
  return(Reach_Habitat_Attribute_combined_output)
}





# ------------------------------------------------------------------------------------------
#
#      Function to combine across Reach - Habitat Attribute - Life Stage - Species
#         NOTE: added Species so 
#
# ------------------------------------------------------------------------------------------


# To Test
# HQ_spring_chinook = Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']]
# HQ_steelhead = Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']]
# HQ_bull_trout = Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']]
# LF_spring_chinook = Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']]
# LF_steelhead = Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']]
# LF_bull_trout = Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Restoration']]
# columns_info = c( "ReachName","Basin","Assessment.Unit" ) # columns to automatically add to beginning (left side) of output

FUNCTION_combine_by_Reach_AND_Habitat_Attribute_Life_Stage_Species = function(HQ_spring_chinook, HQ_steelhead, HQ_bull_trout,  LF_spring_chinook, LF_steelhead, LF_bull_trout, columns_info){
  
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
      if(!is.na(LF_steelhead_index[1])){ paste(LF_spring_chinook$unacceptable_1_indiv_habitat_attributes[LF_spring_chinook_index], collapse=",")[1] },
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
      if(!is.na(LF_steelhead_index[1])){ paste(LF_spring_chinook$at_risk_2_or_3_indiv_habitat_attributes[LF_spring_chinook_index], collapse=",")[1] },
      if(!is.na(LF_bull_trout_index[1])){paste(LF_bull_trout$at_risk_2_or_3_indiv_habitat_attributes[LF_bull_trout_index], collapse=",")[1] }
      
    ) 
    unique_at_risk_attributes = gsub(" ", "", unique_at_risk_attributes, fixed = TRUE)
    unique_at_risk_attributes = unique( unlist(strsplit(paste(unique_at_risk_attributes, collapse=","), ",")) )
    unique_at_risk_attributes = paste(unique_at_risk_attributes, collapse=",")
    
    # ------------------------------------------------------------
    #     List All the Life Stages
    # ------------------------------------------------------------
    life_stages_all = c(
      
      if(!is.na(HQ_spring_chinook_index)){ "multiple"  },
      if(!is.na(HQ_steelhead_index)){  "multiple"  },
      if(!is.na(HQ_bull_trout_index)){  "multiple"  },
      if(!is.na(LF_spring_chinook_index[1])){  paste(LF_spring_chinook$life_stage[LF_spring_chinook_index], collapse=",")  },
      if(!is.na(LF_steelhead_index[1])){  paste(LF_spring_chinook$life_stage[LF_spring_chinook_index], collapse=",") },
      if(!is.na(LF_bull_trout_index[1])){   paste(LF_bull_trout$life_stage[LF_bull_trout_index], collapse=",")   }
      
    ) 
    life_stages_all = unique( unlist(strsplit(paste(life_stages_all, collapse=","), ",")) )
    
    
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
    #    Loop through Each Habitat Attribute 
    # ------------------------------------------------------------
    for(habitat_attribute_x in habitat_attributes_all){
      
      # ------------------------------------------------------------
      #    For Each Life Stage -make a new row
      # ------------------------------------------------------------
      
      for(life_stage_x in life_stages_all){

        for(species_x in species_list){
          
          
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
          #   List the species
          # ------------------------------------------------------------
          HQ_and_LF_combo_x$Species = species_x 
          
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
  return(Reach_Habitat_Attribute_combined_output)
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




    

  


