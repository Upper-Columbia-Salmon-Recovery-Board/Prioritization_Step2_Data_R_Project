
# ---------------------------------------------------------------------------
#
#      SCRIPT: Combine Action Categories across SPECIES and PATHWAYS
#           ORGANZIED by Reach (one column per reach)
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
#    Function to summarize Habitat Quality Output by species (and across all)
#
# ---------------------------------------------------------------------------

test_x = FALSE
if(test_x){
  score_1_or_3 = "two and three"
  restoration_or_protection = 'restoration'
}


FUNCTION_combine_Habitat_Quality_Action_Categories_PER_REACH = function(score_1_or_3,  restoration_or_protection){
  
  # --------------- establish which rating criteria (1 = unacceptable, 3 = unacceptable and at risk) ------
  if(score_1_or_3 == "one"){
    attributes_column = 'unacceptable_1_indiv_habitat_attributes'
    actions_column = "unacceptable_1_action_categories"
  }else if(score_1_or_3 == "two and three"){
    attributes_column = 'at_risk_2_or_3_indiv_habitat_attributes'
    actions_column = "at_risk_2_or_3_action_categories"
  }else if(score_1_or_3 == "one thru three"){
    attributes_column = 'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'
    actions_column = "unacceptable_and_at_risk_1_3_action_categories"
    
  }
  
  # --------------- establish if restoration or protection ----------
  if(restoration_or_protection == "restoration"){
    HQ_spring_chinook = Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Restoration']] 
    HQ_steelhead = Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Restoration']]
    HQ_bull_trout = Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Restoration']]
    
  }else if(restoration_or_protection == "protection"){
    HQ_spring_chinook = Habitat_Quality_Pathway_Spring_Chinook[['Habitat_Quality_Pathway_Protection']] 
    HQ_steelhead = Habitat_Quality_Pathway_Steelhead[['Habitat_Quality_Pathway_Protection']]
    HQ_bull_trout = Habitat_Quality_Pathway_Bull_Trout[['Habitat_Quality_Pathway_Protection']]
    
  }
  
  # ------------------------------------------------------------
  #       Initiate data frame
  # ------------------------------------------------------------
  if(exclude_bull_trout == "no"){
    reaches_unique = unique( c(HQ_spring_chinook$ReachName, HQ_steelhead$ReachName, HQ_bull_trout$ReachName ) )
  }else if(exclude_bull_trout == "yes"){
    reaches_unique = unique( c(HQ_spring_chinook$ReachName, HQ_steelhead$ReachName) )
  }
  
  reaches_unique = reaches_unique[order(reaches_unique)]  # just alphabetize 
  
  # ------------ get Reach Information ------
  Reach_Information_HQ_Actions = Reach_Information_data %>%  
    filter(ReachName   %in% reaches_unique)

  # ------------------------------------------------------------
  #       Loop Through reaches to get information
  # ------------------------------------------------------------
  Pathway_Output_x = c()
  for(reachname_x in reaches_unique){
   
    # ------------------ basic info about the reach ------------
    x_row = which(Reach_Information_HQ_Actions$ReachName == reachname_x)
    output_info_row_x = Reach_Information_HQ_Actions[x_row, c('ReachName','Basin',"Assessment.Unit",  "Spring.Chinook.Reach","Steelhead.Reach" ,"Bull.Trout.Reach" )]
    
    # --------------------- initiate cells for each reach ---------------
    pathways_x  = c()
    habitat_attributes_x = c()
    action_categories_x = c()
    spring_chinook_habitat_attributes = c()
    steelhead_habitat_attributes = c()
    bull_trout_habitat_attributes = c()
    all_three_species_habitat_attributes = c()
    spring_chinook_actions_x = c()
    steelhead_actions_x = c()
    bull_trout_actions_x = c()
    all_three_species_actions = c()
    
    # ---------------------- Spring_Chinook ----------
    if( any(HQ_spring_chinook$ReachName == reachname_x) ){
      # ------------- pull that reach --------------
      HQ_row_x = HQ_spring_chinook[which(HQ_spring_chinook$ReachName == reachname_x),]
      
      # -------- IF there are habitat attributes for that score (unacceptable, at risk, unacceptable-at risk) -------
      if(  !is.na(HQ_row_x[,attributes_column]) ){
        # ---------------- pathway -------------------
        pathways_x = paste(pathways_x, "HQ_spring_chinook", sep=",")
        # ----------------- habitat attributes ------------------
        habitat_attributes_x = paste(habitat_attributes_x, HQ_row_x[,attributes_column], sep=",")
        # --------------- habitat attributes related to Spring Chinook actions --------------
        spring_chinook_habitat_attributes = HQ_row_x[,attributes_column]
        # ----------------- action categories ------------------
        action_categories_x = paste(action_categories_x, HQ_row_x[,actions_column], sep=",")
        # ------------------- specifically add to spring chinook actions --------------
        spring_chinook_actions_x = HQ_row_x[,actions_column]
      }
    }
    
    # ---------------------- Steelhead ----------
    if( any(HQ_steelhead$ReachName == reachname_x) ){
      # ------------- pull that reach --------------
      HQ_row_x = HQ_steelhead[which(HQ_steelhead$ReachName == reachname_x),]
      
      # -------- IF there are habitat attributes for that score (unnaceptable, at risk, unacceptable-at risk) -------
      if(  !is.na(HQ_row_x[,attributes_column]) ){
        # ---------------- pathway -------------------
        pathways_x = paste(pathways_x, "HQ_steelhead", sep=",")
        # ----------------- habitat attributes ------------------
        habitat_attributes_x = paste(habitat_attributes_x, HQ_row_x[,attributes_column], sep=",")
        # --------------- habitat attributes related to Steelhead actions --------------
        steelhead_habitat_attributes = HQ_row_x[,attributes_column]
        # ----------------- action categories ------------------
        action_categories_x = paste(action_categories_x, HQ_row_x[,actions_column], sep=",")
        # ------------------- specifically add to steelhead actions --------------
        steelhead_actions_x = HQ_row_x[,actions_column]
      }
    }
   
    
    # ---------------------- Bull_Trout ----------
    if( exclude_bull_trout == "no" ){
      
      if( any(HQ_bull_trout$ReachName == reachname_x) ){
        # ------------- pull that reach --------------
        HQ_row_x = HQ_bull_trout[which(HQ_bull_trout$ReachName == reachname_x),]
        
        # -------- IF there are habitat attributes for that score (unacceptable, at risk, unacceptable-at risk) -------
        if(  !is.na(HQ_row_x[,attributes_column]) ){
          # ---------------- pathway -------------------
          pathways_x = paste(pathways_x, "HQ_bull_trout", sep=",")
          # ----------------- habitat attributes ------------------
          habitat_attributes_x = paste(habitat_attributes_x, HQ_row_x[,attributes_column], sep=",")
          # --------------- habitat attributes related to Bull Trout actions --------------
          bull_trout_habitat_attributes = HQ_row_x[,attributes_column]
          # ----------------- action categories ------------------
          action_categories_x = paste(action_categories_x, HQ_row_x[,actions_column], sep=",")
          # ------------------- specifically add to bull trout actions --------------
          bull_trout_actions_x = HQ_row_x[,actions_column]
        }
      }
    }
    
    
    # --------------------------------------------------------------------------
    #    All Species
    # --------------------------------------------------------------------------
    
    # ------------------------------------- Pathways -------------------------------
    # --------- if no pathways -------
    if( is.null(pathways_x) ){
      pathways_x = NA
      number_of_pathways_x = 0
    # ---------- if a pathway present (HQ or LF) -----
    }else{
      # ---------------- remove leading comma -------
      pathways_x = substr(pathways_x,2,nchar(pathways_x))
      # ---------- number of pathways ---------------
      number_of_pathways_x =  length( unlist(strsplit(pathways_x, ",")) )
    }
    
    # ------------------------------------- Habitat Attributes -------------------------------
    # ------- if no habitat attributes --------
    if( is.null(habitat_attributes_x) ){
      habitat_attributes_x = NA
      number_of_habitat_attributes_x = 0
    # ------------ if impaired habitat attributes present -----
    }else{
      # ---------------- remove leading comma -------
      habitat_attributes_x = substr(habitat_attributes_x,2,nchar(habitat_attributes_x)) # remove leading comma
      # ------- number of habitat attributes -------------
      habitat_attributes_x = unique(unlist(strsplit(habitat_attributes_x, ",")))   # get list of habitat attributes with NO redundancies
      number_of_habitat_attributes_x =  length( habitat_attributes_x )
      habitat_attributes_x = paste(habitat_attributes_x, collapse=",")
    }
    
    # ------------------------------------- Action Categories -------------------------------
    # ------- if no action categories in this reach -----
    if( is.null(action_categories_x) ){
      action_categories_x = NA
      number_of_action_categories_x = 0
    # ------------- if actions present in this reach -------
    }else{
      # ---------------- remove leading comma -------
      action_categories_x = substr(action_categories_x,2,nchar(action_categories_x))
      # -------------- number of action categories -----------
      action_categories_x = unique( unlist(strsplit(action_categories_x, ",")) )  # get list of action categories with NO redundancies
      number_of_action_categories_x =  length( action_categories_x )
      action_categories_x = paste(action_categories_x, collapse=",")
    }

    # ------- start number to count all species -----
    species_count_x = 0

    # -----------------------------------------
    #     for spring chinook
    # -----------------------------------------
    # ------- number of habitat attributes -------------
    if( is.null(spring_chinook_habitat_attributes) ){
      spring_chinook_habitat_attributes = c()   # get list of habitat attributes with NO redundancies
      number_of_spring_chinook_habitat_attributes =  0
      spring_chinook_presence = "no"
    }else{
      spring_chinook_habitat_attributes = unique(unlist(strsplit(spring_chinook_habitat_attributes, ",")))   # get list of habitat attributes with NO redundancies
      number_of_spring_chinook_habitat_attributes =  length( spring_chinook_habitat_attributes )
      spring_chinook_habitat_attributes = paste(spring_chinook_habitat_attributes, collapse=",")
      spring_chinook_presence = "yes"
      species_count_x = species_count_x + 1
    }

    # -------------- number of action categories -----------
    if(class(spring_chinook_actions_x) == "list"){spring_chinook_actions_x = unlist(spring_chinook_actions_x)}
    
    if( is.null(spring_chinook_actions_x) ){
      spring_chinook_actions_x = NA   # get list of habitat attributes with NO redundancies
      number_of_spring_chinook_actions_x =  0
    }else{
      if(class(spring_chinook_actions_x) == "list"){ spring_chinook_actions_x = unlist(spring_chinook_actions_x)}
      spring_chinook_actions_x = unique( unlist(strsplit(spring_chinook_actions_x, ",")) )  # get list of action categories with NO redundancies
      number_of_spring_chinook_actions_x =  length( spring_chinook_actions_x )
      spring_chinook_actions_x = paste(spring_chinook_actions_x, collapse=",")
    }
  
    # -----------------------------------------
    #     for steelhead
    # -----------------------------------------
    # ------- number of habitat attributes -------------
    if( is.null(steelhead_habitat_attributes) ){
      steelhead_habitat_attributes = c()   # get list of habitat attributes with NO redundancies
      number_of_steelhead_habitat_attributes =  0
    }else{
      steelhead_habitat_attributes = unique(unlist(strsplit(steelhead_habitat_attributes, ",")))   # get list of habitat attributes with NO redundancies
      number_of_steelhead_habitat_attributes =  length( steelhead_habitat_attributes )
      steelhead_habitat_attributes = paste(steelhead_habitat_attributes, collapse=",")
      species_count_x = species_count_x + 1
    }
    # -------------- number of action categories -----------
    if(class(steelhead_actions_x) == "list"){steelhead_actions_x = unlist(steelhead_actions_x)}
    
    if( is.null(steelhead_actions_x) ){
      steelhead_actions_x = NA   # get list of habitat attributes with NO redundancies
      number_of_steelhead_actions_x =  0
    }else{
      if(class(steelhead_actions_x) == "list"){ steelhead_actions_x = unlist(steelhead_actions_x)}
      steelhead_actions_x = unique( unlist(strsplit(steelhead_actions_x, ",")) )  # get list of action categories with NO redundancies
      number_of_steelhead_actions_x =  length( steelhead_actions_x )
      steelhead_actions_x = paste(steelhead_actions_x, collapse=",")
    }
    
    # -----------------------------------------
    #     for bull trout
    # -----------------------------------------
    # ------- number of habitat attributes -------------
    if(is.null(bull_trout_habitat_attributes)){
      bull_trout_habitat_attributes = c()   # get list of habitat attributes with NO redundancies
      number_of_bull_trout_habitat_attributes =  0
    }else{
      bull_trout_habitat_attributes = unique(unlist(strsplit(bull_trout_habitat_attributes, ",")))   # get list of habitat attributes with NO redundancies
      number_of_bull_trout_habitat_attributes =  length( bull_trout_habitat_attributes )
      bull_trout_habitat_attributes = paste(bull_trout_habitat_attributes, collapse=",")
      species_count_x = species_count_x + 1
    }
    
    # -------------- number of action categories -----------
    if(class(bull_trout_actions_x) == "list"){bull_trout_actions_x = unlist(bull_trout_actions_x)}
    
    if(is.null(bull_trout_actions_x)){
      bull_trout_actions_x = NA   # get list of habitat attributes with NO redundancies
      number_of_bull_trout_actions_x =  0
    }else{
      if(class(bull_trout_actions_x) == "list"){ bull_trout_actions_x = unlist(bull_trout_actions_x)}
      bull_trout_actions_x = unique( unlist(strsplit(bull_trout_actions_x, ",")) )  # get list of action categories with NO redundancies
      number_of_bull_trout_actions_x =  length( bull_trout_actions_x )
      bull_trout_actions_x = paste(bull_trout_actions_x, collapse=",")
    }
    
    # -------------- if all species are present - yes or no -------------
    if(species_count_x == 3){ all_species_present = "yes"
    }else{ all_species_present = "no" }
    
    # -----------------------------------------
    #   Add life stage ( Life_Stage_Priorities_AU_and_Reach_data has 0 and 1 for life stage presence )
    # ----------------------------------------- 
    life_stage_list = c()
    if( !is.null(spring_chinook_habitat_attributes) ){
      life_stages_present = FUNCTION_life_stage_presence("spring_chinook", reachname_x)
      life_stage_list = paste(life_stage_list, life_stages_present, sep="," )
      life_stage_spring_chinook_output = life_stages_present
    }else{
      life_stage_spring_chinook_output = "NA"
      spring_chinook_habitat_attributes = "NA"
    }
    if( !is.null(steelhead_habitat_attributes)  ){
      life_stages_present = FUNCTION_life_stage_presence("steelhead", reachname_x)
      life_stage_list = paste(life_stage_list, life_stages_present, sep="," )
      life_stage_steelhead_output = life_stages_present
    }else{
      life_stage_steelhead_output = "NA"
      steelhead_habitat_attributes = "NA"
    }
    if( !is.null(bull_trout_habitat_attributes)  & exclude_bull_trout != "yes" ){
      life_stages_present = FUNCTION_life_stage_presence("bull_trout", reachname_x)
      life_stage_list = paste(life_stage_list, life_stages_present, sep="," )
      life_stage_bull_trout_output = life_stages_present
    }else{
      life_stage_bull_trout_output = "NA"
      bull_trout_habitat_attributes = "NA"
    }
    # ---------------- remove leading comma -------
    if( is.null(life_stage_list) ){
      life_stage_list = "NA"
    }else{
      life_stage_list = substr(life_stage_list,2,nchar(life_stage_list))
    }
    
    # -----------------------------------------
    #     combine and output
    # -----------------------------------------
    output_info_row_x = output_info_row_x[,c("ReachName")]
    output_row_x = as.data.frame( as.data.frame(c(output_info_row_x, spring_chinook_presence,  all_species_present,
                                      pathways_x, number_of_pathways_x,
                                      habitat_attributes_x, number_of_habitat_attributes_x, 
                                      spring_chinook_habitat_attributes, number_of_spring_chinook_habitat_attributes,
                                      steelhead_habitat_attributes, number_of_steelhead_habitat_attributes,
                                      bull_trout_habitat_attributes, number_of_bull_trout_habitat_attributes,
                                      action_categories_x, number_of_action_categories_x,
                                      spring_chinook_actions_x, number_of_spring_chinook_actions_x,
                                      steelhead_actions_x, number_of_steelhead_actions_x,
                                      bull_trout_actions_x, number_of_bull_trout_actions_x, 
                                      life_stage_list, 
                                      life_stage_spring_chinook_output, life_stage_steelhead_output, life_stage_bull_trout_output )    ) )
    if(nrow(output_row_x)>1){output_row_x = t(output_row_x)} # if output needs to be transposed (prepped as rows, not columns)
    colnames(output_row_x) = c("ReachName", "Spring_Chinook_Actions_Present_Yes_No", "SprCh_STLD_BullTr_All_Present_Yes_No",
                               "Pathways","Number_of_Pathways", 
                               "Impaired_Habitat_Attributes_All_Species","Number_Impaired_Habitat_Attributes_All_Species",
                               "Impaired_Habitat_Attributes_SpringChinook","Number_Impaired_Habitat_Attributes_SpringChinook",
                               "Impaired_Habitat_Attributes_Steelhead","Number_Impaired_Habitat_Attributes_Steelhead",
                               "Impaired_Habitat_Attributes_BullTrout","Number_Impaired_Habitat_Attributes_BullTrout",
                               "Action_Categories_All_Species", "Number_Action_Categories_All_Species",
                               "Action_Categories_SpringChinook", "Number_Action_Categories_SpringChinook",
                               "Action_Categories_Steelhead", "Number_Action_Categories_Steelhead",
                               "Action_Categories_BullTrout", "Number_Action_Categories_BullTrout", 
                               "Life_Stage_All",
                               "Life_Stage_Spring_Chinook", "Life_Stage_Steelhead", "Life_Stage_Bull_Trout")
    Pathway_Output_x = rbind(Pathway_Output_x, output_row_x)
  }
  
  # ----------------------- combine Reach Information and Pathway processed info for output ----------
  Output_DF = merge(Reach_Information_HQ_Actions, Pathway_Output_x, by="ReachName")
  rownames(Output_DF) = seq(1, nrow(Output_DF))
  
  # ---------------- remove rows with no pathways --------
  if( length(which(Output_DF$Number_of_Pathways == 0)) > 0  ){
    Output_DF = Output_DF[-which(Output_DF$Number_of_Pathways == 0) , ]
  }
  
  return(Output_DF)
  
}


# ---------------------------------------------------------------------------
#
#    Function to Life Stage Presence
#
# ---------------------------------------------------------------------------

FUNCTION_life_stage_presence= function(species_x, reachname_x){
  
  # ------------- pull life stage names -------
  names_x = paste(species_x,"_life_stages",sep="")
  life_stage_presence_x = c("ReachName", as.vector( unlist(life_stages_prescence[[names_x]]) )   )
  # -------- life stage presence row ------------   life_stage_presence_x
  life_stage_presence = Life_Stage_Priorities_AU_and_Reach_data[ life_stage_presence_x    ]
  life_stage_presence = life_stage_presence[ which(life_stage_presence$ReachName == reachname_x) ,  ]
  # ------------ merge the layers -----------
  life_stages_presence_reach = colnames(life_stage_presence)[ which( life_stage_presence == 1   )  ]
  if(species_x == "spring_chinook"){
    life_stages_output = paste( substr(life_stages_presence_reach, 6, nchar(life_stages_presence_reach) ), collapse=",")
  }else{
    life_stages_output = paste( substr(life_stages_presence_reach, 4, nchar(life_stages_presence_reach) ), collapse=",")
  }
  return(life_stages_output)
}

# ---------------------------------------------------------------------------
#
#    Function to summarize Limiting Factor Output by species (and across all)
#
# ---------------------------------------------------------------------------
test_x = FALSE
if(test_x){
  score_1_or_3 = "two and three"
  restoration_or_protection = 'restoration'
}


FUNCTION_combine_Limiting_Factor_Action_Categories_PER_REACH = function(score_1_or_3,  restoration_or_protection){
  
  # --------------- establish which rating criteria (1 = unacceptable, 3 = unacceptable and at risk) ------
  if(score_1_or_3 == "one"){
    attributes_column = 'unacceptable_1_indiv_habitat_attributes'
    actions_column = "unacceptable_1_action_categories"
  }else if(score_1_or_3 == "two and three"){
    attributes_column = 'at_risk_2_or_3_indiv_habitat_attributes'
    actions_column = "at_risk_2_or_3_action_categories"
  }else if(score_1_or_3 == "one thru three"){
    attributes_column = 'unacceptable_AND_at_risk_1_to_3_indiv_habitat_attributes'
    actions_column = "unacceptable_and_at_risk_1_3_action_categories"
    
  }
  
  # --------------- establish if restoration or protection ----------
  if(restoration_or_protection == "restoration"){
    LF_spring_chinook = Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Restoration']] 
    LF_steelhead = Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Restoration']]
    LF_bull_trout = Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Restoration']]
    
  }else if(restoration_or_protection == "protection"){
    LF_spring_chinook = Limiting_Factor_Pathway_Spring_Chinook[['Limiting_Factor_Pathway_Protection']] 
    LF_steelhead = Limiting_Factor_Pathway_Steelhead[['Limiting_Factor_Pathway_Protection']]
    LF_bull_trout = Limiting_Factor_Pathway_Bull_Trout[['Limiting_Factor_Pathway_Protection']]
    
  }
  
  # ------------------------------------------------------------
  #       Initiate data frame
  # ------------------------------------------------------------
  if(exclude_bull_trout == "no"){
    reaches_unique = unique( c(LF_spring_chinook$ReachName, LF_steelhead$ReachName, LF_bull_trout$ReachName) )
  }else if(exclude_bull_trout == "yes"){
    reaches_unique = unique( c(LF_spring_chinook$ReachName, LF_steelhead$ReachName) )
  }
  
  
  reaches_unique = reaches_unique[order(reaches_unique)]  # just alphabetize 
  
  # ------------ get Reach Information ------
  Reach_Information_LF_Actions = Reach_Information_data %>%  
    filter(ReachName   %in% reaches_unique)

  # ------------------------------------------------------------
  #       Loop Through reaches to get information
  # ------------------------------------------------------------
  Pathway_Output_x = c()
  # to test:  reachname_x = "Chiwawa River Lower 02"
  for( reachname_x in reaches_unique ){
    
    # --------------------- initiate cells for each reach ---------------
    pathways_x  = c()
    habitat_attributes_x = c()
    action_categories_x = c()
    life_stages_x = c()
    spring_chinook_habitat_attributes = c()
    steelhead_habitat_attributes = c()
    bull_trout_habitat_attributes = c()
    all_three_species_habitat_attributes = c()
    spring_chinook_actions_x = c()
    steelhead_actions_x = c()
    bull_trout_life_stages_x = c()
    spring_chinook_life_stages_x = c()
    steelhead_life_stages_x = c()
    bull_trout_actions_x = c()
    all_three_species_actions = c()
    
    # --------------- just get important reach information  ----------
    x = which(Reach_Information_LF_Actions$ReachName == reachname_x)[1]
    output_row_info_x = Reach_Information_LF_Actions[x, c('ReachName','Basin',"Assessment.Unit",  "Spring.Chinook.Reach","Steelhead.Reach" ,"Bull.Trout.Reach" )]
    
    
    # ---------------------- Spring_Chinook ----------
    if( any(LF_spring_chinook$ReachName == reachname_x) ){
      # ------------- pull that reach --------------
      LF_row_x = LF_spring_chinook[which(LF_spring_chinook$ReachName == reachname_x),]
      
      # -------- IF there are habitat attributes for that score (unacceptable, at risk, unacceptable-at risk) -------
      if(  !is.na(LF_row_x[,attributes_column][[1]]) & nchar(LF_row_x[,attributes_column][[1]]) > 0   ){
        # ---------------- pathway -------------------
        pathways_x = paste(pathways_x, "LF_spring_chinook", sep=",")
        
        # ---------- loop through each life stage in this reach --------
        for(life_stage_row_x in 1:nrow(LF_row_x)){
          # ----------------- habitat attributes ------------------
          habitat_attributes_x = paste(habitat_attributes_x, LF_row_x[life_stage_row_x,attributes_column][[1]], sep=",")
          # ----------------- action categories ------------------
          action_categories_x = paste(action_categories_x, LF_row_x[life_stage_row_x,actions_column][[1]], sep=",")
          # ----------------- life stages ---------------------------
          life_stages_x = paste(life_stages_x, LF_row_x[life_stage_row_x,"life_stage"][[1]], sep=",")
          
          # --------------- habitat attributes related to Spring Chinook actions --------------
          spring_chinook_habitat_attributes =  paste(spring_chinook_habitat_attributes,LF_row_x[life_stage_row_x,attributes_column][[1]], sep=",")
          # ------------------- specifically add to spring chinook actions --------------
          spring_chinook_actions_x =    paste(spring_chinook_actions_x, LF_row_x[life_stage_row_x,actions_column][[1]] , sep=",")
          # ------------------- life stages specifically for spring chinook --------------
          spring_chinook_life_stages_x =   paste(spring_chinook_life_stages_x, LF_row_x[life_stage_row_x,"life_stage"][[1]] , sep=",")
        }

      }
    }
    
    # ---------------------- Steelhead ----------
    if( any(LF_steelhead$ReachName == reachname_x) ){
      # ------------- pull that reach --------------
      LF_row_x = LF_steelhead[which(LF_steelhead$ReachName == reachname_x),]
      
      # -------- IF there are habitat attributes for that score (unacceptable, at risk, unacceptable-at risk) -------
      if(  !is.na(LF_row_x[,attributes_column][[1]]) & nchar(LF_row_x[,attributes_column][[1]]) > 0   ){
        # ---------------- pathway -------------------
        pathways_x = paste(pathways_x, "LF_steelhead", sep=",")
        
        # ---------- loop through each life stage in this reach --------
        for(life_stage_row_x in 1:nrow(LF_row_x)){
          # ----------------- habitat attributes ------------------
          habitat_attributes_x = paste(habitat_attributes_x, LF_row_x[life_stage_row_x,attributes_column][[1]], sep=",")
          # ----------------- action categories ------------------
          action_categories_x = paste(action_categories_x, LF_row_x[life_stage_row_x,actions_column][[1]], sep=",")
          # ----------------- life stages ---------------------------
          life_stages_x = paste(life_stages_x, LF_row_x[life_stage_row_x,"life_stage"][[1]], sep=",")
          
          # --------------- habitat attributes related to Steelhead actions --------------
          steelhead_habitat_attributes = paste(steelhead_habitat_attributes, LF_row_x[life_stage_row_x,attributes_column][[1]] , sep=",")
          # ------------------- specifically add to steelhead actions --------------
          steelhead_actions_x =  paste(steelhead_actions_x, LF_row_x[life_stage_row_x,actions_column][[1]] , sep=",")
          # ------------------- life stages specifically for spring chinook --------------
          steelhead_life_stages_x = paste(steelhead_life_stages_x, LF_row_x[life_stage_row_x,"life_stage"][[1]] , sep=",")
        }
      }
    }
    
    
    # ---------------------- Bull_Trout ----------
    if( exclude_bull_trout == "no" ){
      
      if( any(LF_bull_trout$ReachName == reachname_x) ){
        # ------------- pull that reach --------------
        LF_row_x = LF_bull_trout[which(LF_bull_trout$ReachName == reachname_x),]
        
        # -------- IF there are habitat attributes for that score (unacceptable, at risk, unacceptable-at risk) -------
        if(  !is.na(LF_row_x[,attributes_column][[1]]) & nchar(LF_row_x[,attributes_column][[1]]) > 0   ){
          # ---------------- pathway -------------------
          pathways_x = paste(pathways_x, "LF_bull_trout", sep=",")
          
          # ---------- loop through each life stage in this reach --------
          for(life_stage_row_x in 1:nrow(LF_row_x)){
            # ----------------- habitat attributes ------------------
            habitat_attributes_x = paste(habitat_attributes_x, LF_row_x[life_stage_row_x,attributes_column][[1]], sep=",")
            # ----------------- action categories ------------------
            action_categories_x = paste(action_categories_x, LF_row_x[life_stage_row_x,actions_column][[1]], sep=",")
            # ----------------- life stages ---------------------------
            life_stages_x = paste(life_stages_x, LF_row_x[life_stage_row_x,"life_stage"][[1]], sep=",")
            
            # --------------- habitat attributes related to Bull Trout actions --------------
            bull_trout_habitat_attributes =  paste(bull_trout_habitat_attributes, LF_row_x[life_stage_row_x,attributes_column][[1]] , sep=",")
            # ------------------- specifically add to bull trout actions --------------
            bull_trout_actions_x = paste(bull_trout_actions_x, LF_row_x[life_stage_row_x,actions_column][[1]], sep=",")
            # ------------------- life stages specifically for spring chinook --------------
            bull_trout_life_stages_x =  paste(bull_trout_life_stages_x, LF_row_x[life_stage_row_x,"life_stage"][[1]], sep=",")
          }
        }
      }
    }
    # --------------------------------------------------------------------------
    #    All Species
    # --------------------------------------------------------------------------
    
    # ------------------------------------- Pathways -------------------------------
    
    if(is.null(pathways_x)){
      pathways_x = NA
      number_of_pathways_x = 0
    }else{
      # ---------------- remove leading comma -------
      pathways_x = substr(pathways_x,2,nchar(pathways_x))
      # ---------- number of pathways ---------------
      number_of_pathways_x =  length( unlist(strsplit(pathways_x, ",")) )
    }
    
    # ------------------------------------- Habitat Attributes -------------------------------
    if(is.null(habitat_attributes_x)){
      habitat_attributes_x = NA
      number_of_habitat_attributes_x = 0
    }else{
      # ---------------- remove leading comma -------
      habitat_attributes_x = substr(habitat_attributes_x,2,nchar(habitat_attributes_x))
      # ------- number of habitat attributes -------------
      habitat_attributes_x = unique(unlist(strsplit(habitat_attributes_x, ",")))   # get list of habitat attributes with NO redundancies
      number_of_habitat_attributes_x =  length( habitat_attributes_x )
      habitat_attributes_x = paste(habitat_attributes_x, collapse=",")
    }
    
    # ------------------------------------- Action Categories -------------------------------
    if(is.null(action_categories_x)){
      action_categories_x = NA
      number_of_action_categories_x = 0
    }else{
      # ---------------- remove leading comma -------
      action_categories_x = substr(action_categories_x,2,nchar(action_categories_x))
      # -------------- number of action categories -----------
      action_categories_x = unique( unlist(strsplit(action_categories_x, ",")) )  # get list of action categories with NO redundancies
      number_of_action_categories_x =  length( action_categories_x )
      action_categories_x = paste(action_categories_x, collapse=",")
    }
    
    # ------------------------------------- Life Stages -------------------------------
    #print(paste("ALL LIFE STAGES: ", life_stages_x) )
    if(is.null(life_stages_x)){
      life_stages_x = NA
      number_of_life_stages_x = 0
    }else{
      # ---------------- remove leading NA or comma -------
      if(substr(life_stages_x,0,1) == ","){
        life_stages_x = substr(life_stages_x,2,nchar(life_stages_x))
      }else if(substr(life_stages_x,0,3) == "NA,"){
        life_stages_x = substr(life_stages_x,4,nchar(life_stages_x))
      }
     
      # -------------- number of action categories -----------
      life_stages_x = unique( unlist(strsplit(life_stages_x, ",")) )  # get list of action categories with NO redundancies
      number_of_life_stages_x =  length( life_stages_x )
      life_stages_x = paste(life_stages_x, collapse=",")
    }
    
    # ------- start number to count all species -----
    species_count_x = 0
    
    # -----------------------------------------
    #     for spring chinook
    # -----------------------------------------
    # ------- number of habitat attributes -------------
    if(is.null(spring_chinook_habitat_attributes)){
      spring_chinook_habitat_attributes = NA   # get list of habitat attributes with NO redundancies
      number_of_spring_chinook_habitat_attributes =  0
      spring_chinook_presence = "no"
    }else{
      # ---------------- remove leading comma -------
      spring_chinook_habitat_attributes = substr(spring_chinook_habitat_attributes,2,nchar(spring_chinook_habitat_attributes))
      # ----------------- get list w/ no redundancies and count them ------------
      spring_chinook_habitat_attributes = unique(unlist(strsplit(spring_chinook_habitat_attributes, ",")))   # get list of habitat attributes with NO redundancies
      number_of_spring_chinook_habitat_attributes =  length( spring_chinook_habitat_attributes )
      spring_chinook_habitat_attributes = paste(spring_chinook_habitat_attributes, collapse=",")
      spring_chinook_presence = "yes"
      species_count_x = species_count_x + 1
    }
    
    # -------------- number of action categories -----------
    if(is.null(spring_chinook_actions_x)){
      spring_chinook_actions_x = NA   # get list of habitat attributes with NO redundancies
      number_of_spring_chinook_actions_x =  0
    }else{
      # ---------------- remove leading comma -------
      spring_chinook_actions_x = substr(spring_chinook_actions_x,2,nchar(spring_chinook_actions_x))
      # ----------------- get list w/ no redundancies and count them ------------
      if(class(spring_chinook_actions_x) == "list" ){ spring_chinook_actions_x = unlist(spring_chinook_actions_x)}
      spring_chinook_actions_x = unique( unlist(strsplit(spring_chinook_actions_x, ",")) )  # get list of action categories with NO redundancies
      number_of_spring_chinook_actions_x =  length( spring_chinook_actions_x )
      spring_chinook_actions_x = paste(spring_chinook_actions_x, collapse=",")
    }
    
    # -------------- number of life stages -----------
    if(is.null(spring_chinook_life_stages_x)){
      spring_chinook_life_stages_x = NA   # get list of habitat attributes with NO redundancies
      number_of_spring_chinook_life_stages_x =  0
    }else{
      # ---------------- remove leading comma -------
      spring_chinook_life_stages_x = substr(spring_chinook_life_stages_x,2,nchar(spring_chinook_life_stages_x))
      # ----------------- get list w/ no redundancies and count them ------------
      spring_chinook_life_stages_x = unique(unlist(strsplit(spring_chinook_life_stages_x, ",")))   # get list of habitat attributes with NO redundancies
      number_of_spring_chinook_life_stages_x =  length( spring_chinook_life_stages_x )
      spring_chinook_life_stages_x = paste(spring_chinook_life_stages_x, collapse=",")
    }
    
    # -----------------------------------------
    #     for steelhead
    # -----------------------------------------
    # ------- number of habitat attributes -------------
    if(is.null(steelhead_habitat_attributes)){
      steelhead_habitat_attributes = NA   # get list of habitat attributes with NO redundancies
      number_of_steelhead_habitat_attributes =  0
    }else{
      # ---------------- remove leading comma -------
      steelhead_habitat_attributes = substr(steelhead_habitat_attributes,2,nchar(steelhead_habitat_attributes))
      # ----------------- get list w/ no redundancies and count them ------------
      steelhead_habitat_attributes = unique(unlist(strsplit(steelhead_habitat_attributes, ",")))   # get list of habitat attributes with NO redundancies
      number_of_steelhead_habitat_attributes =  length( steelhead_habitat_attributes )
      steelhead_habitat_attributes = paste(steelhead_habitat_attributes, collapse=",")
      species_count_x = species_count_x + 1
    }
    # -------------- number of action categories -----------
    if(is.null(steelhead_actions_x)){
      steelhead_actions_x = NA   # get list of habitat attributes with NO redundancies
      number_of_steelhead_actions_x =  0
    }else{
      # ---------------- remove leading comma -------
      steelhead_actions_x = substr(steelhead_actions_x,2,nchar(steelhead_actions_x))
      # ----------------- get list w/ no redundancies and count them ------------
      
      if(class(steelhead_actions_x) == "list" ){ steelhead_actions_x = unlist(steelhead_actions_x)}
      steelhead_actions_x = unique( unlist(strsplit(steelhead_actions_x, ",")) )  # get list of action categories with NO redundancies
      number_of_steelhead_actions_x =  length( steelhead_actions_x )
      steelhead_actions_x = paste(steelhead_actions_x, collapse=",")
    }
    # -------------- number of life stages -----------
    if(is.null(steelhead_life_stages_x)){
      steelhead_life_stages_x = NA   # get list of habitat attributes with NO redundancies
      number_of_steelhead_life_stages_x =  0
    }else{
      # ---------------- remove leading comma -------
      steelhead_life_stages_x = substr(steelhead_life_stages_x,2,nchar(steelhead_life_stages_x))
      # ----------------- get list w/ no redundancies and count them ------------
      steelhead_life_stages_x = unique(unlist(strsplit(steelhead_life_stages_x, ",")))   # get list of habitat attributes with NO redundancies
      number_of_steelhead_life_stages_x =  length( steelhead_life_stages_x )
      steelhead_life_stages_x = paste(steelhead_life_stages_x, collapse=",")
    }
    
    # -----------------------------------------
    #     for bull trout
    # -----------------------------------------
    # ------- number of habitat attributes -------------
    if(is.null(bull_trout_habitat_attributes)){
      bull_trout_habitat_attributes = NA   # get list of habitat attributes with NO redundancies
      number_of_bull_trout_habitat_attributes =  0
    }else{
      # ---------------- remove leading comma -------
      bull_trout_habitat_attributes = substr(bull_trout_habitat_attributes,2,nchar(bull_trout_habitat_attributes))
      # ----------------- get list w/ no redundancies and count them ------------
      bull_trout_habitat_attributes = unique(unlist(strsplit(bull_trout_habitat_attributes, ",")))   # get list of habitat attributes with NO redundancies
      number_of_bull_trout_habitat_attributes =  length( bull_trout_habitat_attributes )
      bull_trout_habitat_attributes = paste(bull_trout_habitat_attributes, collapse=",")
      species_count_x = species_count_x + 1
    }
    
    # -------------- number of action categories -----------
    if(is.null(bull_trout_actions_x)){
      bull_trout_actions_x = NA   # get list of habitat attributes with NO redundancies
      number_of_bull_trout_actions_x =  0
    }else{
      # ---------------- remove leading comma -------
      bull_trout_actions_x = substr(bull_trout_actions_x,2,nchar(bull_trout_actions_x))
      # ----------------- get list w/ no redundancies and count them ------------
      if(class(bull_trout_actions_x) == "list" ){ bull_trout_actions_x = unlist(bull_trout_actions_x)}
      bull_trout_actions_x = unique( unlist(strsplit(bull_trout_actions_x, ",")) )  # get list of action categories with NO redundancies
      number_of_bull_trout_actions_x =  length( bull_trout_actions_x )
      bull_trout_actions_x = paste(bull_trout_actions_x, collapse=",")
    }
    # -------------- number of life stages -----------
    if(is.null(bull_trout_life_stages_x)){
      bull_trout_life_stages_x = NA   # get list of habitat attributes with NO redundancies
      number_of_bull_trout_life_stages_x =  0
    }else{
      # ---------------- remove leading comma -------
      bull_trout_life_stages_x = substr(bull_trout_life_stages_x,2,nchar(bull_trout_life_stages_x))
      # ----------------- get list w/ no redundancies and count them ------------
      bull_trout_life_stages_x = unique(unlist(strsplit(bull_trout_life_stages_x, ",")))   # get list of habitat attributes with NO redundancies
      number_of_bull_trout_life_stages_x =  length( bull_trout_life_stages_x )
      bull_trout_life_stages_x = paste(bull_trout_life_stages_x, collapse=",")
    }
    
    # -------------- if all species are present - yes or no -------------
    if(species_count_x == 3){ all_species_present = "yes"
    }else{ all_species_present = "no" }
    
    # -----------------------------------------
    #     combine and output
    # -----------------------------------------
    
    output_row_x =  as.data.frame(c(output_row_info_x,spring_chinook_presence,  all_species_present,
                                      pathways_x, number_of_pathways_x,
                                    
                                      habitat_attributes_x,number_of_habitat_attributes_x, 
                                      spring_chinook_habitat_attributes, number_of_spring_chinook_habitat_attributes,
                                      steelhead_habitat_attributes, number_of_steelhead_habitat_attributes,
                                      bull_trout_habitat_attributes, number_of_bull_trout_habitat_attributes,
                                      
                                      action_categories_x, number_of_action_categories_x,
                                      spring_chinook_actions_x, number_of_spring_chinook_actions_x,
                                      steelhead_actions_x, number_of_steelhead_actions_x,
                                      bull_trout_actions_x, number_of_bull_trout_actions_x,
                                      
                                      life_stages_x,number_of_life_stages_x,
                                      spring_chinook_life_stages_x, number_of_spring_chinook_life_stages_x,
                                      steelhead_life_stages_x, number_of_steelhead_life_stages_x,
                                      bull_trout_life_stages_x, number_of_bull_trout_life_stages_x
                                      )  )  
    colnames(output_row_x) = c("ReachName", "Basin","Assessment.Unit","Spring.Chinook.Reach","Steelhead.Reach","Bull.Trout.Reach", "Spring_Chinook_Actions_Present_Yes_No", "SprCh_STLD_BullTr_All_Present_Yes_No",
                               "Pathways","Number_of_Pathways", 
                               
                               "Impaired_Habitat_Attributes_All_Species","Number_Impaired_Habitat_Attributes_All_Species",
                               "Impaired_Habitat_Attributes_SpringChinook","Number_Impaired_Habitat_Attributes_SpringChinook",
                               "Impaired_Habitat_Attributes_Steelhead","Number_Impaired_Habitat_Attributes_Steelhead",
                               "Impaired_Habitat_Attributes_BullTrout","Number_Impaired_Habitat_Attributes_BullTrout",
                               
                               "Action_Categories_All_Species", "Number_Action_Categories_All_Species",
                               "Action_Categories_SpringChinook", "Number_Action_Categories_SpringChinook",
                               "Action_Categories_Steelhead", "Number_Action_Categories_Steelhead",
                               "Action_Categories_BullTrout", "Number_Action_Categories_BullTrout",
                               
                               "Life_Stages","Number_of_Life_Stages",
                               "Life_Stages_SpringChinook", "Number_Life_Stages_SpringChinook",
                               "Life_Stages_Steelhead", "Number_Life_Stages_Steelhead",
                               "Life_Stages_BullTrout", "Number_Life_Stages_BullTrout"
                               )
    Pathway_Output_x = rbind(Pathway_Output_x, output_row_x)

  }
  
  # ---------------- remove rows with no pathways --------
  if( length(which(Pathway_Output_x$Number_of_Pathways == 0)) > 0  ){
    Pathway_Output_x = Pathway_Output_x[-which(Pathway_Output_x$Number_of_Pathways == 0) , ]
  }
  
  return(Pathway_Output_x)
  
}


# ------------------------------------------------------------------------------------------
#
#      Function to combine Habitat Quality and Limiting Factor Outputs
#
# ------------------------------------------------------------------------------------------

test_x = FALSE
if(test_x){
  HQ_pathway_df = Habitat_Quality_Restoration_Unacceptable
  LF_pathway_df = Limiting_Factor_Restoration_Unacceptable
  columns_info = c( "ReachName","Basin","Assessment.Unit" )
}


FUNCTION_combine_across_pathways = function(HQ_pathway_df, LF_pathway_df){
  
  # ------------------------------------------------------------
  #       Get Unique Reaches
  # ------------------------------------------------------------
  reaches_unique = unique(c(HQ_pathway_df$ReachName, LF_pathway_df$ReachName ))
  
  # ------------------------------------------------------------
  #      Loop through each Reach and combine data
  # ------------------------------------------------------------
  HQ_and_LF_combined_output = c()
  for(reach_x in reaches_unique){
    
    # --------------------- generate HQ and LF index ----------
    HQ_reach_index = which(HQ_pathway_df$ReachName == reach_x)
    LF_reach_index = which(LF_pathway_df$ReachName == reach_x)
    
    # ------------------------------------------------------------
    #     Add Reach Information Data 
    # ------------------------------------------------------------
    if( length(HQ_reach_index ) > 0 ){
      HQ_and_LF_combo_x = as.data.frame(HQ_pathway_df[HQ_reach_index, columns_info])
    }else{
      HQ_and_LF_combo_x = as.data.frame(LF_pathway_df[LF_reach_index, columns_info])
    }
    colnames(HQ_and_LF_combo_x) = "ReachName"
    
    # ------------------------------------------------------------
    #     Combine Text - HQ and LF
    # ------------------------------------------------------------
    for(combo_x in columns_to_combine_text){
      combo_x_output = c()
      if( length(HQ_reach_index ) > 0 ){
        combo_x_output = paste(combo_x_output, HQ_pathway_df[HQ_reach_index, combo_x], sep=",")     }
      if( length(LF_reach_index ) > 0 ){
        combo_x_output = paste(combo_x_output, LF_pathway_df[LF_reach_index, combo_x], sep=",")    }
      # ------------- create data frame with column ------
      combo_x_output = substr(combo_x_output,2,nchar(combo_x_output))   # remove the leading comma
      combo_x_output = gsub(" ", "", combo_x_output, fixed = TRUE) # remove blank spaces (since HQ pathway habitat attributes have no blank spaces)
      combo_x_output = gsub(",NA", "", combo_x_output, fixed = TRUE) # remove NAs
      combo_x_output = paste(unique( unlist(strsplit(combo_x_output, ",")) ), collapse=",")  # just get unique 
      combo_x_output = as.data.frame(combo_x_output)
      colnames(combo_x_output) = combo_x
      # ------------ add to previous -------------------
      HQ_and_LF_combo_x = cbind(HQ_and_LF_combo_x,combo_x_output )
    }
    
    # ------------------------------------------------------------
    #     Combine Text - LF only
    # ------------------------------------------------------------
    
    # ----------- if Limiting Factor pathway  - life stage presence --------
    if( length(LF_reach_index ) > 0 ){
      for(combo_x in columns_to_combine_text_LF_only){
        combo_x_output = LF_pathway_df[LF_reach_index, combo_x]
        # ------------- create data frame with column ------
        combo_x_output = as.data.frame(combo_x_output)
        colnames(combo_x_output) = combo_x
        # ------------ add to previous -------------------
        HQ_and_LF_combo_x = cbind(HQ_and_LF_combo_x,combo_x_output )
      }
      
      # ------------- if only an HQ Pathway reach - put NA -----
    }else{
      for(combo_x in columns_to_combine_text_LF_only){
        combo_x_output = NA
        # ------------- create data frame with column ------
        combo_x_output = as.data.frame(combo_x_output)
        colnames(combo_x_output) = combo_x
        # ------------ add to previous -------------------
        HQ_and_LF_combo_x = cbind(HQ_and_LF_combo_x,combo_x_output )
      }
    }

    # ------------------------------------------------------------
    #     Combine Yes/No text (if one pathway yes, then yes, both have to be no to produce no)
    # ------------------------------------------------------------
    for(combo_x in columns_to_combine_yes_no){
      combo_x_output = c()
      if( length(HQ_reach_index ) > 0 ){
        combo_x_output = paste(combo_x_output, HQ_pathway_df[HQ_reach_index, combo_x], sep=",")     }
      if( length(LF_reach_index ) > 0 ){
        combo_x_output = paste(combo_x_output, LF_pathway_df[LF_reach_index, combo_x], sep=",")    }
      # ------------- create data frame with column ------
      combo_x_output = substr(combo_x_output,2,nchar(combo_x_output))  # remove the leading comma
      combo_x_output_TF = grepl( "yes", combo_x_output, fixed = TRUE)
      if(combo_x_output_TF){ combo_x_output = "yes" }else{ combo_x_output = "no"  }
      combo_x_output = as.data.frame(combo_x_output)
      colnames(combo_x_output) = combo_x
      # ------------ add to previous -------------------
      HQ_and_LF_combo_x = cbind(HQ_and_LF_combo_x,combo_x_output )
    }
    
    # ------------------------------------------------------------
    #     Count Unique occurrences
    # ------------------------------------------------------------
    for(combo_x in columns_to_combine_count_unique){
      combo_x_output = c()
      if( length(HQ_reach_index ) > 0 ){
        combo_x_output = paste(combo_x_output, HQ_pathway_df[HQ_reach_index, combo_x], sep=",")     }
      if( length(LF_reach_index ) > 0 ){
        combo_x_output = paste(combo_x_output, LF_pathway_df[LF_reach_index, combo_x], sep=",")    }
      # ------------- create data frame with column ------
      combo_x_output = substr(combo_x_output,2,nchar(combo_x_output))   # remove the leading comma
      combo_x_output = gsub(" ", "", combo_x_output, fixed = TRUE) # remove blank spaces (since HQ pathway habitat attributes have no blank spaces)
      if(combo_x_output == "NA"){
        combo_x_output = 0
      }else{
        combo_x_output = length( unique( unlist(strsplit(combo_x_output, ",")) ) )
      }
      combo_x_output = as.data.frame(combo_x_output)
      colname_x = paste("Number",combo_x, sep="_")
      colnames(combo_x_output) = colname_x
      # ------------ add to previous -------------------
      HQ_and_LF_combo_x = cbind(HQ_and_LF_combo_x,combo_x_output )
    }
    
  
    # ------------------------------------------------------------
    #     Numeric - Add - HQ and LF
    # ------------------------------------------------------------
    for(combo_x in columns_to_combine_numeric){
      combo_x_output = c()
      if( length(HQ_reach_index ) > 0 ){
        combo_x_output = paste(combo_x_output, HQ_pathway_df[HQ_reach_index, combo_x], sep=",")     }
      if( length(LF_reach_index ) > 0 ){
        combo_x_output = paste(combo_x_output, LF_pathway_df[LF_reach_index, combo_x], sep=",")    }
      # ------------- create data frame with column ------
      combo_x_output = substr(combo_x_output,2,nchar(combo_x_output))   # remove the leading comma
      combo_x_output = unlist(strsplit(combo_x_output, ",")) 
      if( length(combo_x_output) == 1 ){
        combo_x_output = as.numeric(combo_x_output)
      }else{
        combo_x_output = as.numeric(combo_x_output[1]) + as.numeric(combo_x_output[2]) 
      }
      combo_x_output = as.data.frame(combo_x_output)
      colnames(combo_x_output) = combo_x
      # ------------ add to previous -------------------
      HQ_and_LF_combo_x = cbind(HQ_and_LF_combo_x,combo_x_output )
    }
    
    
    # ------------------------------------------------------------
    #     Number of Life Stages
    # ------------------------------------------------------------
    for(combo_x in columns_to_combine_numeric_LF_only){
      # ----------------- if a Limiting Factor pathway ------
      if(length(LF_reach_index ) > 0){
        combo_x_output = as.numeric(LF_pathway_df[LF_reach_index, combo_x] )
        
      # ------- if NO limiting factor so put NA when HQ pathway -----
      }else{
        combo_x_output = 0
      }
      combo_x_output = as.data.frame(combo_x_output)
      colnames(combo_x_output) = combo_x
      HQ_and_LF_combo_x = cbind(HQ_and_LF_combo_x,combo_x_output )
    }

    
    
    HQ_and_LF_combined_output = rbind(HQ_and_LF_combined_output, HQ_and_LF_combo_x)
  }
  
  return(HQ_and_LF_combined_output)
  
  
}



# ------------------------------------------------------------------------------------------
#
#      Function to combine across Unacceptable and At Risk
#
# ------------------------------------------------------------------------------------------

test_x = FALSE
if(test_x){
  HQ_LF_Unacceptable = Restoration_Unacceptable
  HQ_LF_At_Risk = Restoration_At_Risk
  HQ_LF_Both = Restoration_Unacceptable_and_At_Risk
  HQ_Both = Habitat_Quality_Restoration_Unacceptable_and_At_Risk
  columns_info = c( "ReachName" ) # columns to automatically add to beginning (left side) of output
  HQ_add_life_stage == "no"
}


FUNCTION_combine_across_Unacceptable_and_AtRisk = function(HQ_LF_Unacceptable, HQ_LF_At_Risk, HQ_LF_Both, HQ_Both, columns_info, exclude_bull_trout, HQ_add_life_stage){
  
  # ------------------------------------------------------------
  #       Get Unique Reaches
  # ------------------------------------------------------------

  # ------------------------------------------------------------
  #      Loop through each Reach and combine data
  # ------------------------------------------------------------
  Unacceptable_AtRisk_combined_output = c()
  
  for(reach_x in HQ_LF_Both$ReachName){
    
    # ------ get row -----
    rowx = which(HQ_LF_Both$ReachName==reach_x)
    
    # --------------------- generate HQ and LF index ----------
    HQ_LF_Both_index = which(HQ_LF_Both$ReachName == reach_x)
    if(any(HQ_LF_Unacceptable$ReachName == reach_x)){ HQ_LF_Unacceptable_index = which(HQ_LF_Unacceptable$ReachName == reach_x) }else{HQ_LF_Unacceptable_index = NA}
    if(any(HQ_LF_At_Risk$ReachName == reach_x)){ HQ_LF_At_Risk_index = which(HQ_LF_At_Risk$ReachName == reach_x) }else{HQ_LF_At_Risk_index = NA}
    HQ_Both_index  = which(HQ_Both$ReachName == reach_x) # data frame with just HQ pathway 

    # ----------- IF excluding bull trout and only bull trout pathway - SKIP this ---------
    if(exclude_bull_trout == "yes" & (HQ_LF_Both[HQ_LF_Both_index,"Pathways"] == "HQ_bull_trout" |   HQ_LF_Both[HQ_LF_Both_index,"Pathways"] == "LF_bull_trout" )  ){
      next
    }
    
    # ------------------------------------------------------------
    #     Add Reach Information Data 
    # ------------------------------------------------------------
    HQ_and_LF_combo_x = as.data.frame(HQ_LF_Both[HQ_LF_Both_index, columns_info])
    colnames(HQ_and_LF_combo_x) = "ReachName"
    HQ_and_LF_combo_x = merge(HQ_and_LF_combo_x, Reach_Information_data[,c("ReachName", "Basin", "Assessment.Unit")], by="ReachName", all.x=TRUE )
    
    # ------------------------------------------------------------
    #    Pathways (list and yes/no) 
    # ------------------------------------------------------------
    
    # ------------- get pathways --------
    pathway_output = HQ_LF_Both[HQ_LF_Both_index, "Pathways"]
    # ----- if excluding bull trout -------
    if(exclude_bull_trout == "yes"){
      pathway_output = gsub("HQ_bull_trout","",pathway_output)
      pathway_output = gsub("LF_bull_trout","",pathway_output)
      if(substr(pathway_output, nchar(pathway_output), nchar(pathway_output)) == ","){
        pathway_output = substr(pathway_output,1,nchar(pathway_output)-1)
      }
      if(substr(pathway_output, 1, 1) == ","){
        pathway_output = substr(pathway_output,2,nchar(pathway_output))
      }
      pathway_output = gsub(",,",",",pathway_output)
    }
    HQ_and_LF_combo_x$Pathways = pathway_output
    HQ_and_LF_combo_x$Number_of_Pathways = HQ_LF_Both[HQ_LF_Both_index, "Number_of_Pathways"]
    
    # ------------------------------------------------------------------------------------------------ 
    #    Generate General Action List (Restore Reach Function or Address Limiting Factors)
    # ------------------------------------------------------------------------------------------------ 
    
    # ---------------- Generate General "Actions" list across all species ------------------
    general_actions = c()
    if( length(grep("HQ",HQ_and_LF_combo_x$Pathways)) > 0){
      general_actions = paste( general_actions , "Restore Reach Function", sep=", ")
    }
    if( length(grep("LF",HQ_and_LF_combo_x$Pathways)) > 0){
      general_actions = paste( general_actions , "Address Limiting Factors", sep=", ")
    }
    general_actions = substr(general_actions,3,nchar(general_actions)) # remove leading comma and space
    HQ_and_LF_combo_x$Actions = general_actions
    
    pathways_x =  unique( unlist(strsplit(HQ_and_LF_combo_x$Pathways, ",")) )
    
    # ----------------- Generate General Actions lists for individual species ---------
    HQ_and_LF_combo_x$Actions_Spring_Chinook = "NA"
    HQ_and_LF_combo_x$Actions_Steelhead = "NA"
    HQ_and_LF_combo_x$Actions_Bull_Trout = "NA"
     
    # -------- add Restore Reach Function OR Address Limiting Factors -------
    if(  grepl( "HQ_spring_chinook", HQ_and_LF_combo_x$Pathways, fixed = TRUE)  ){ HQ_and_LF_combo_x$Actions_Spring_Chinook =  paste(HQ_and_LF_combo_x$Actions_Spring_Chinook, "Restore Reach Function", sep=", ")  }
    if(  grepl( "HQ_steelhead", HQ_and_LF_combo_x$Pathways, fixed = TRUE)  ){ HQ_and_LF_combo_x$Actions_Steelhead = paste(HQ_and_LF_combo_x$Actions_Steelhead, "Restore Reach Function", sep=", ")   }
    if(  grepl( "HQ_bull_trout", HQ_and_LF_combo_x$Pathways, fixed = TRUE)  ){ HQ_and_LF_combo_x$Actions_Bull_Trout = paste(HQ_and_LF_combo_x$Actions_Bull_Trout, "Restore Reach Function", sep=", ")   }
    if(  grepl( "LF_spring_chinook", HQ_and_LF_combo_x$Pathways, fixed = TRUE)  ){HQ_and_LF_combo_x$Actions_Spring_Chinook = paste(HQ_and_LF_combo_x$Actions_Spring_Chinook, "Address Limiting Factors", sep=", ") }
    if(  grepl( "LF_steelhead", HQ_and_LF_combo_x$Pathways, fixed = TRUE)  ){ HQ_and_LF_combo_x$Actions_Steelhead = paste(HQ_and_LF_combo_x$Actions_Steelhead, "Address Limiting Factors", sep=", ") }
    if(  grepl( "LF_bull_trout", HQ_and_LF_combo_x$Pathways, fixed = TRUE)  ){ HQ_and_LF_combo_x$Actions_Bull_Trout = paste(HQ_and_LF_combo_x$Actions_Bull_Trout, "Address Limiting Factors", sep=", ") } 
    
    # -------- remove NAs if one of the pathways pulled ------------
    if( grepl('NA, ', HQ_and_LF_combo_x$Actions_Spring_Chinook, fixed = TRUE)  ){ HQ_and_LF_combo_x$Actions_Spring_Chinook =  substring( HQ_and_LF_combo_x$Actions_Spring_Chinook, 5,nchar( HQ_and_LF_combo_x$Actions_Spring_Chinook) ) }
    if( grepl('NA, ', HQ_and_LF_combo_x$Actions_Steelhead)  ){ HQ_and_LF_combo_x$Actions_Steelhead =  substring( HQ_and_LF_combo_x$Actions_Steelhead, 5,nchar( HQ_and_LF_combo_x$Actions_Steelhead) ) }
    if( grepl('NA, ', HQ_and_LF_combo_x$Actions_Bull_Trout, fixed = TRUE)  ){ HQ_and_LF_combo_x$Actions_Bull_Trout =  substring( HQ_and_LF_combo_x$Actions_Bull_Trout, 5,nchar( HQ_and_LF_combo_x$Actions_Bull_Trout) ) }

    # ------------- do yes no for pathways --------
    if(  grepl( "HQ_spring_chinook", HQ_and_LF_combo_x$Pathways, fixed = TRUE)  ){ HQ_spring_chinook_yesno = "yes" }else{ HQ_spring_chinook_yesno = "no" }
    if(  grepl( "HQ_steelhead", HQ_and_LF_combo_x$Pathways, fixed = TRUE)  ){ HQ_steelhead_yesno = "yes" }else{ HQ_steelhead_yesno = "no" }
    if(  grepl( "HQ_bull_trout", HQ_and_LF_combo_x$Pathways, fixed = TRUE)  ){ HQ_bull_trout_yesno = "yes" }else{ HQ_bull_trout_yesno = "no" }
    if(  grepl( "LF_spring_chinook", HQ_and_LF_combo_x$Pathways, fixed = TRUE)  ){ LF_spring_chinook_yesno = "yes" }else{ LF_spring_chinook_yesno = "no" }
    if(  grepl( "LF_steelhead", HQ_and_LF_combo_x$Pathways, fixed = TRUE)  ){ LF_steelhead_yesno = "yes" }else{ LF_steelhead_yesno = "no" }
    if(  grepl( "LF_bull_trout", HQ_and_LF_combo_x$Pathways, fixed = TRUE)  ){ LF_bull_trout_yesno = "yes" }else{ LF_bull_trout_yesno = "no" }
    # ----- if excluding bull trout --------
    if(exclude_bull_trout == "yes"){
      HQ_bull_trout_yesno = "no"
      LF_bull_trout_yesno = "no"
    }
    
    pathway_yes_no_df = t(  as.data.frame( c(HQ_spring_chinook_yesno, HQ_steelhead_yesno, HQ_bull_trout_yesno,
                                             LF_spring_chinook_yesno, LF_steelhead_yesno, LF_bull_trout_yesno)  ) )
    colnames(pathway_yes_no_df) = c(  "HabitatQuality_Spring_Chinook_Pathway",	"HabitatQuality_Steelhead_Pathway",	"HabitatQuality_BullTrout_Pathway",
                                      "LimitingFactor_Spring_Chinook_Pathway",	"LimitingFactor_Steelhead_Pathway",	"LimitingFactor_BullTrout_Pathway" )
    row.names(pathway_yes_no_df) = row.names(HQ_and_LF_combo_x)
    HQ_and_LF_combo_x = cbind(HQ_and_LF_combo_x, pathway_yes_no_df)
    
    # ----------------- add Barriers if present -------------
    Pathway_Barrier_Prioritization = "no" # run other function to add this post-hoc
    HQ_and_LF_combo_x$Barrier_Prioritization_Pathway = Pathway_Barrier_Prioritization
    
    # ------------------------------------------------------------
    #   List the species
    # ------------------------------------------------------------
    species_x = c()
    # ------------- do yes no for pathways --------
    if(  grepl( "HQ_spring_chinook", HQ_and_LF_combo_x$Pathways, fixed = TRUE) |  grepl( "LF_spring_chinook", HQ_and_LF_combo_x$Pathways, fixed = TRUE) ){ 
      species_x = paste(species_x, "Spring_Chinook", sep=",") }
    if(  grepl( "HQ_steelhead", HQ_and_LF_combo_x$Pathways, fixed = TRUE) |  grepl( "LF_steelhead", HQ_and_LF_combo_x$Pathways, fixed = TRUE) ){ 
      species_x = paste(species_x, "Steelhead", sep=",") }
    # ---- only write if bull trout not excluded ------
    if(exclude_bull_trout == "no"){
      if(  grepl( "HQ_bull_trout", HQ_and_LF_combo_x$Pathways, fixed = TRUE) |  grepl( "LF_bull_trout", HQ_and_LF_combo_x$Pathways, fixed = TRUE) ){ 
        species_x = paste(species_x, "Bull_Trout", sep=",") }
    }
    
    species_x = substr(species_x,2,nchar(species_x))   # remove the leading comma
    # ------- add to output --------------
    HQ_and_LF_combo_x$Species = species_x
    
    # ------------------------------------------------------------
    #  Benefit all species and/or benefits Spring Chinook
    # ------------------------------------------------------------
    
    # ------------- Benefit all species ----------
    HQ_and_LF_combo_x$SprCh_STLD_BullTr_All_Benefit = HQ_LF_Both[HQ_LF_Both_index, "SprCh_STLD_BullTr_All_Present_Yes_No"]
    # ------------- Benefit Spring Chinook ----------
    HQ_and_LF_combo_x$Spring_Chinook_Benefit = HQ_LF_Both[HQ_LF_Both_index, "Spring_Chinook_Actions_Present_Yes_No"]
    
    # ------------------------------------------------------------
    #   Generate Species-specific columns of habitat attributes and actions - for 
    # ------------------------------------------------------------
    
    # ---------- Spring Chinook ----------------
    if( is.na(HQ_LF_Unacceptable_index) ){ HQ_and_LF_combo_x$Spring_Chinook_Habitat_Attributes_Unacceptable = NA }else{ HQ_and_LF_combo_x$Spring_Chinook_Habitat_Attributes_Unacceptable = HQ_LF_Unacceptable$Impaired_Habitat_Attributes_SpringChinook[HQ_LF_Unacceptable_index] }
    if( is.na(HQ_LF_At_Risk_index) ){ HQ_and_LF_combo_x$Spring_Chinook_Habitat_Attributes_At_Risk = NA }else{ HQ_and_LF_combo_x$Spring_Chinook_Habitat_Attributes_At_Risk = HQ_LF_At_Risk$Impaired_Habitat_Attributes_SpringChinook[HQ_LF_At_Risk_index] }
    HQ_and_LF_combo_x$Spring_Chinook_Habitat_Attributes = HQ_LF_Both$Impaired_Habitat_Attributes_SpringChinook[HQ_LF_Both_index]
    if( is.na(HQ_LF_Unacceptable_index) ){  HQ_and_LF_combo_x$Spring_Chinook_Unacceptable_Actions = NA }else{ HQ_and_LF_combo_x$Spring_Chinook_Unacceptable_Actions = HQ_LF_Unacceptable$Action_Categories_SpringChinook[HQ_LF_Unacceptable_index] }
    if( is.na(HQ_LF_At_Risk_index) ){ HQ_and_LF_combo_x$Spring_Chinook_At_Risk_Actions = NA }else{ HQ_and_LF_combo_x$Spring_Chinook_At_Risk_Actions = HQ_LF_At_Risk$Action_Categories_SpringChinook[HQ_LF_At_Risk_index]}
    HQ_and_LF_combo_x$Spring_Chinook_Actions = HQ_LF_Both$Action_Categories_SpringChinook[HQ_LF_Both_index]
    
    # ---------- Steelhead ----------------
    if( is.na(HQ_LF_Unacceptable_index) ){ HQ_and_LF_combo_x$Steelhead_Habitat_Attributes_Unacceptable = NA }else{ HQ_and_LF_combo_x$Steelhead_Habitat_Attributes_Unacceptable = HQ_LF_Unacceptable$Impaired_Habitat_Attributes_Steelhead[HQ_LF_Unacceptable_index] }
    if( is.na(HQ_LF_At_Risk_index) ){ HQ_and_LF_combo_x$Steelhead_Habitat_Attributes_At_Risk  = NA }else{  HQ_and_LF_combo_x$Steelhead_Habitat_Attributes_At_Risk = HQ_LF_At_Risk$Impaired_Habitat_Attributes_Steelhead[HQ_LF_At_Risk_index] }
    HQ_and_LF_combo_x$Steelhead_Habitat_Attributes = HQ_LF_Both$Impaired_Habitat_Attributes_Steelhead[HQ_LF_Both_index]
    if( is.na(HQ_LF_Unacceptable_index) ){ HQ_and_LF_combo_x$Steelhead_Unacceptable_Actions = NA }else{ HQ_and_LF_combo_x$Steelhead_Unacceptable_Actions = HQ_LF_Unacceptable$Action_Categories_Steelhead[HQ_LF_Unacceptable_index] }
    if( is.na(HQ_LF_At_Risk_index) ){ HQ_and_LF_combo_x$Steelhead_At_Risk_Actions = NA }else{ HQ_and_LF_combo_x$Steelhead_At_Risk_Actions = HQ_LF_At_Risk$Action_Categories_Steelhead[HQ_LF_At_Risk_index] }
    HQ_and_LF_combo_x$Steelhead_Actions = HQ_LF_Both$Action_Categories_Steelhead[HQ_LF_Both_index]
    
    # ---------- Bull Trout ----------------
    if(exclude_bull_trout == "no"){
      if( is.na(HQ_LF_Unacceptable_index) ){ HQ_and_LF_combo_x$Bull_Trout_Habitat_Attributes_Unacceptable = NA }else{  HQ_and_LF_combo_x$Bull_Trout_Habitat_Attributes_Unacceptable = HQ_LF_Unacceptable$Impaired_Habitat_Attributes_BullTrout[HQ_LF_Unacceptable_index] }
      if( is.na(HQ_LF_At_Risk_index) ){ HQ_and_LF_combo_x$Bull_Trout_Habitat_Attributes_At_Risk  = NA }else{  HQ_and_LF_combo_x$Bull_Trout_Habitat_Attributes_At_Risk = HQ_LF_At_Risk$Impaired_Habitat_Attributes_BullTrout[HQ_LF_At_Risk_index] }
      HQ_and_LF_combo_x$Bull_Trout_Habitat_Attributes = HQ_LF_Both$Impaired_Habitat_Attributes_BullTrout[HQ_LF_Both_index]
      if( is.na(HQ_LF_Unacceptable_index) ){ HQ_and_LF_combo_x$Bull_Trout_Unacceptable_Actions = NA }else{  HQ_and_LF_combo_x$Bull_Trout_Unacceptable_Actions = HQ_LF_Unacceptable$Action_Categories_BullTrout[HQ_LF_Unacceptable_index] }
      if( is.na(HQ_LF_At_Risk_index) ){ HQ_and_LF_combo_x$Bull_Trout_At_Risk_Actions  = NA }else{  HQ_and_LF_combo_x$Bull_Trout_At_Risk_Actions = HQ_LF_At_Risk$Action_Categories_BullTrout[HQ_LF_At_Risk_index] }
      HQ_and_LF_combo_x$Bull_Trout_Actions = HQ_LF_Both$Action_Categories_BullTrout[HQ_LF_Both_index]
    }else{
      HQ_and_LF_combo_x$Bull_Trout_Habitat_Attributes_Unacceptable = NA
      HQ_and_LF_combo_x$Bull_Trout_Habitat_Attributes_At_Risk = NA
      HQ_and_LF_combo_x$Bull_Trout_Habitat_Attributes = NA
      HQ_and_LF_combo_x$Bull_Trout_Unacceptable_Actions = NA
      HQ_and_LF_combo_x$Bull_Trout_At_Risk_Actions = NA
      HQ_and_LF_combo_x$Bull_Trout_Actions = NA # bull_trout_actions_x
    }
    
    # ------------------------------------------------------------
    #   List all life stages
    # ------------------------------------------------------------
    HQ_and_LF_combo_x$Life_Stages = HQ_LF_Both[HQ_LF_Both_index, c("Life_Stages" )  ]
    HQ_and_LF_combo_x$Life_Stages_SpringChinook = HQ_LF_Both[HQ_LF_Both_index, c( "Life_Stages_SpringChinook"  )  ]
    HQ_and_LF_combo_x$Life_Stages_Steelhead = HQ_LF_Both[HQ_LF_Both_index, c( "Life_Stages_Steelhead"  )  ]
    HQ_and_LF_combo_x$Life_Stages_BullTrout = HQ_LF_Both[HQ_LF_Both_index, c( "Life_Stages_BullTrout" )  ]
    
    # ------------------------------------------------------------
    #   List all actions
    # ------------------------------------------------------------
    HQ_and_LF_combo_x$Action_Categories_All_Species = HQ_LF_Both$Action_Categories_All_Species[HQ_LF_Both_index] 
    
    # ------------------------------------------------------------
    #  Number of all actions
    # ------------------------------------------------------------
    HQ_and_LF_combo_x$Number_Action_Categories_All_Species = HQ_LF_Both$Number_Action_Categories_All_Species[HQ_LF_Both_index] 
    
    # ------------------------------------------------------------
    #  All Habitat Attributes
    # ------------------------------------------------------------
    HQ_and_LF_combo_x$Impaired_Habitat_Attributes_All_Species = HQ_LF_Both$Impaired_Habitat_Attributes_All_Species[HQ_LF_Both_index] 
    
    # ------------------------------------------------------------
    #  Number of all Habitat Attributes
    # ------------------------------------------------------------
    HQ_and_LF_combo_x$Number_Impaired_Habitat_Attributes_All_Species = HQ_LF_Both$Number_Impaired_Habitat_Attributes_All_Species[HQ_LF_Both_index] 
    
    # ------------------------------------------------------------
    #  Unacceptable Habitat Attributes (Yes/No)
    # ------------------------------------------------------------ 
    if( is.na(HQ_LF_Unacceptable_index)  ){
      HQ_and_LF_combo_x$Unacceptable_Habitat_Attributes_Presence = "no"
    }else if( reach_x == HQ_LF_Unacceptable$ReachName[HQ_LF_Unacceptable_index]   ){
      HQ_and_LF_combo_x$Unacceptable_Habitat_Attributes_Presence = "yes"
    }else{
      HQ_and_LF_combo_x$Unacceptable_Habitat_Attributes_Presence = "no"
    }
    
    # ------------------------------------------------------------
    #  Unacceptable Habitat Attributes
    # ------------------------------------------------------------
    if( is.na(HQ_LF_Unacceptable_index)  ){
      HQ_and_LF_combo_x$Unacceptable_Impaired_Habitat_Attributes_All_Species = NA
    }else if( reach_x == HQ_LF_Unacceptable$ReachName[HQ_LF_Unacceptable_index]   ){
      HQ_and_LF_combo_x$Unacceptable_Impaired_Habitat_Attributes_All_Species = HQ_LF_Unacceptable$Impaired_Habitat_Attributes_All_Species[HQ_LF_Unacceptable_index] 
    }else{
      HQ_and_LF_combo_x$Unacceptable_Impaired_Habitat_Attributes_All_Species = NA
    }
    
    # ------------------------------------------------------------
    #  Number of Unacceptable Habitat Attributes
    # ------------------------------------------------------------
    if( is.na(HQ_LF_Unacceptable_index)  ){
      HQ_and_LF_combo_x$Unacceptable_Number_Impaired_Habitat_Attributes_All_Species = NA
    }else if( reach_x == HQ_LF_Unacceptable$ReachName[HQ_LF_Unacceptable_index]   ){
      HQ_and_LF_combo_x$Unacceptable_Number_Impaired_Habitat_Attributes_All_Species = HQ_LF_Unacceptable$Number_Impaired_Habitat_Attributes_All_Species[HQ_LF_Unacceptable_index] 
    }else{
      HQ_and_LF_combo_x$Unacceptable_Number_Impaired_Habitat_Attributes_All_Species = NA
    }
    
    # ------------------------------------------------------------
    #  At Risk Habitat Attributes (Yes/No)
    # ------------------------------------------------------------ 
    if( is.na(HQ_LF_At_Risk_index) ){
      HQ_and_LF_combo_x$At_Risk_Habitat_Attributes_Presence = "no"
    }else if( reach_x == HQ_LF_At_Risk$ReachName[HQ_LF_At_Risk_index]   ){
      HQ_and_LF_combo_x$At_Risk_Habitat_Attributes_Presence = "yes"
    }else{
      HQ_and_LF_combo_x$At_Risk_Habitat_Attributes_Presence = "no"
    }
    
    # ------------------------------------------------------------
    #  At Risk Habitat Attributes
    # ------------------------------------------------------------
    if( is.na(HQ_LF_At_Risk_index) ){
      HQ_and_LF_combo_x$At_Risk_Impaired_Habitat_Attributes_All_Species = NA
    }else if( reach_x == HQ_LF_At_Risk$ReachName[HQ_LF_At_Risk_index]   ){
      HQ_and_LF_combo_x$At_Risk_Impaired_Habitat_Attributes_All_Species = HQ_LF_At_Risk$Impaired_Habitat_Attributes_All_Species[HQ_LF_At_Risk_index] 
    }else{
      HQ_and_LF_combo_x$At_Risk_Impaired_Habitat_Attributes_All_Species = NA
    }
    
    # ------------------------------------------------------------
    #  Number of At Risk Habitat Attributes
    # ------------------------------------------------------------
    if( is.na(HQ_LF_At_Risk_index) ){
      HQ_and_LF_combo_x$At_Risk_Number_Impaired_Habitat_Attributes_All_Species = NA
    }else if( reach_x == HQ_LF_At_Risk$ReachName[HQ_LF_At_Risk_index]   ){
      HQ_and_LF_combo_x$At_Risk_Number_Impaired_Habitat_Attributes_All_Species = HQ_LF_At_Risk$Number_Impaired_Habitat_Attributes_All_Species[HQ_LF_At_Risk_index] 
    }else{
      HQ_and_LF_combo_x$At_Risk_Number_Impaired_Habitat_Attributes_All_Species = NA
    }
    
    
    # ------------------------------------------------------------
    #  IF excluding Bull Trout, remove the references and life stages
    # ------------------------------------------------------------
    
    if(exclude_bull_trout == "yes" & 
       ( HQ_and_LF_combo_x$HabitatQuality_BullTrout_Pathway  == "yes" |  HQ_and_LF_combo_x$LimitingFactor_BullTrout_Pathway == "yes"  )    ){
      
      # --------------------- if the only pathway is HQ or LF Bull Trout - remove the row ------------------
      if(
        HQ_and_LF_combo_x$HabitatQuality_Spring_Chinook_Pathway == "no" &
        HQ_and_LF_combo_x$HabitatQuality_Steelhead_Pathway == "no" &
        HQ_and_LF_combo_x$LimitingFactor_Spring_Chinook_Pathway  == "no" &
        HQ_and_LF_combo_x$LimitingFactor_Steelhead_Pathway == "no" &
        HQ_and_LF_combo_x$Barrier_Prioritization_Pathway == "no" 
      ){
        do_not_write_row_since_only_bull_trout = "yes"
      }else{
        do_not_write_row_since_only_bull_trout = "no"
      }
      
    }else{
      do_not_write_row_since_only_bull_trout = "no"
    }
    
    # ------------------------------------------------------------
    #   add HQ life stages presence (if HQ_add_life_stage is yes)
    # ------------------------------------------------------------
    if(HQ_add_life_stage  == "yes"){
      
      # ---- pull the reach's row in HQ Pathway data frame ---------
      HQ_Both_row = HQ_Both[HQ_Both_index, ]
      # --------------------- Pathways------------------
      pathways_x_list = unlist(strsplit(HQ_and_LF_combo_x$Pathways, ","))
      pathway_list_letters = substr(pathways_x_list, 1, 2)
      # ------------- unique species ---------
      species_list = substr(pathways_x_list, 4, nchar(pathways_x_list)) # unique species
      # ------------- if Okanogan - just do for Steelhead --------------
      if( HQ_LF_Both$Basin[rowx] ==  "Okanogan"){species_list = "steelhead"}
      # ------------ IF Habitat Quality Pathways presence -------------------
      if(  any(pathway_list_letters == "HQ")  ){
        
        # ---------- loop through each species in HQ pathway ---------
        life_stages_all_output = c()
        for(species_x_HQ in species_list){
          life_stages_all = FUNCTION_output_life_stages_based_on_species_and_life_stage_presence(species_x_HQ, reach_x, HQ_Both_row)
          life_stages_all_output = paste(life_stages_all_output,life_stages_all, sep="," )
        }
        life_stages_all_output = substr(life_stages_all_output, 2, nchar(life_stages_all_output))
        life_stages_all_output = unique(unlist(strsplit(life_stages_all_output, ",")))
        life_stages_all_output = paste(life_stages_all_output, collapse=",")
      }
      
      # --------------- update life stages ----------------
      
      # ------ if life stages list is NA ------
      if(is.na(HQ_and_LF_combo_x$Life_Stages_Presence)){
        HQ_and_LF_combo_x$Life_Stages_Presence = life_stages_all_output
        
        # ------- if life stages exist already -----
      }else{
        life_stages_updated = paste(HQ_and_LF_combo_x$Life_Stages_Presence,life_stages_all_output, sep="," )
        life_stages_updated = unique(unlist(strsplit(life_stages_updated, ",")))
        life_stages_updated = paste(life_stages_updated, collapse=",")
        HQ_and_LF_combo_x$Life_Stages_Presence =  life_stages_updated
      }
      
    }
    
    
    # ------------------------------------------------------------
    #  Add to Output
    # ------------------------------------------------------------
    # if do_not_write_row_since_only_bull_trout = "yes" - row is ONLY from bull trout
    if(do_not_write_row_since_only_bull_trout == "no"){
      Unacceptable_AtRisk_combined_output = rbind(Unacceptable_AtRisk_combined_output, HQ_and_LF_combo_x)
    }
    
  }
    
  return(Unacceptable_AtRisk_combined_output)
  
}





# ------------------------------------------------------------------------------------------
#
#      Generate life stages based on A) presence of impaired habitat attributes in HQ pathway and B) life stage presence in reach
#
# ------------------------------------------------------------------------------------------

FUNCTION_output_life_stages_based_on_species_and_life_stage_presence = function(species_x_HQ, reach_x, HQ_Both_row){
  
  # ------------------- generate life stage list for this species (from reach presence data ) ------------------
  life_stages_species_presence_list = FUNCTION_generate_life_stage_list_for_species_reach_FLAT_TABLE(species_x_HQ, reach_x)
  life_stages_species_presence_list = unlist(strsplit(life_stages_species_presence_list, ","))
  
  # ------------ convert species name AND generate habitat attributes from impaired list -----------
  if(species_x_HQ == "spring_chinook"){
    species_x_HQ2 = "Spring Chinook"
    habitat_attributes_species = HQ_Both_row$Impaired_Habitat_Attributes_SpringChinook
    }
  if(species_x_HQ == "steelhead"){
    species_x_HQ2 = "Steelhead"
    habitat_attributes_species = HQ_Both_row$Impaired_Habitat_Attributes_Steelhead
  }
  if(species_x_HQ == "bull_trout"){
    species_x_HQ2 = "Bull Trout"
    habitat_attributes_species = HQ_Both_row$Impaired_Habitat_Attributes_BullTrout
  }
  habitat_attributes_species = unlist(strsplit(habitat_attributes_species, ","))

    
  # ------------------------------------------------------------
  #    Habitat Quality Pathways
  # ------------------------------------------------------------
  life_stages_list_output = c()
  for(life_stage_x_HQ in life_stages_species_presence_list){
    
    # ------------------------------------------------------------
    #  Match Impaired Habitat Attributes to Life Stage Habitat Attributes
    # ------------------------------------------------------------
    
    # ---------=-------------------------
    #  Pull Life Stage habitat attributes
    # ---------=-------------------------
    life_stage_habitat_attributes = Attribute_LifeStage_Crosswalk %>%
      filter( `Life Stage` %in% life_stage_x_HQ) %>%
      filter( Species %in% species_x_HQ2 ) %>%
      dplyr::select(`Habitat Attribute`)
    life_stage_habitat_attributes = as.data.frame(life_stage_habitat_attributes)
    life_stage_habitat_attributes = life_stage_habitat_attributes$`Habitat Attribute`
    # ------------- remove the blank space ---------------
    life_stage_habitat_attributes = gsub(" ","",life_stage_habitat_attributes)
    
    # -----------------------------------
    #  Pull Life Stage habitat attributes
    # -----------------------------------
    
    # ----------------------------------
    #  Overlap between life stage presence w/ habitat attributes for that life stage (life_stage_habitat_attributes) 
    #         and HQ pathway impaired list (habitat_attributes_species)
    # ----------------------------------
    habitat_attributes_all = intersect(life_stage_habitat_attributes, habitat_attributes_species)
    
    # ---------- include life stage if intersection betweeh HQ patwhway impaired attributes AND life stage presence -----
    if(length(habitat_attributes_all)>0){
      life_stages_list_output = paste(life_stages_list_output, life_stage_x_HQ, sep=",")
    }
      
  
  }
  life_stages_list_output = substr(life_stages_list_output, 2, nchar(life_stages_list_output))
  return(life_stages_list_output)
}




# ------------------------------------------------------------------------------------------
#
#      Generate Priority life stages AND species for Fish Barriers
#
# ------------------------------------------------------------------------------------------

FUNCTION_output_life_stages_based_on_species_and_life_stage_presence_FISH_BARRIERs = function(reach_x, exclude_bull_trout){
  
  # --------------- Prep Output -----------
  species_x_output = c()
  life_stage_output = c()
  
  # ------------------- generate life stage list for this species (from reach presence data ) ------------------
  Life_Stage_Priorities_AU_and_Reach_data_REACH_X = Life_Stage_Priorities_AU_and_Reach_data[ which(Life_Stage_Priorities_AU_and_Reach_data$ReachName == reach_x), ] 
  
  # --------------- spring chinook -----------
  spring_chinook_col_names = unlist( spring_chinook_life_stages_presence[names(spring_chinook_life_stages_presence)] )
  life_stage_names = names(spring_chinook_col_names)
  life_stage_presence_numbers = Life_Stage_Priorities_AU_and_Reach_data_REACH_X[ spring_chinook_col_names ]
  if( any(life_stage_presence_numbers == 1) ){
    species_x_output = paste(species_x_output, "Spring Chinook", sep=",")
    columns_presence = which(life_stage_presence_numbers == 1)
    life_stages_x = life_stage_names[columns_presence]
    life_stages_x = paste(life_stages_x, collapse=",")
    life_stage_output = paste(life_stage_output,life_stages_x, sep="," )
  }
  
  # --------------- steelhead -----------
  steelhead_col_names = unlist( steelhead_life_stages_presence[names(steelhead_life_stages_presence)] )
  life_stage_names = names(steelhead_col_names)
  life_stage_presence_numbers = Life_Stage_Priorities_AU_and_Reach_data_REACH_X[ steelhead_col_names ]
  if(any(life_stage_presence_numbers == 1)){
    species_x_output = paste(species_x_output, "Steelhead", sep=",")
    columns_presence = which(life_stage_presence_numbers == 1)
    life_stages_x = life_stage_names[columns_presence]
    life_stages_x = paste(life_stages_x, collapse=",")
    life_stage_output = paste(life_stage_output,life_stages_x, sep="," )
  }
  
  # --------------- bull trout -----------
  if(exclude_bull_trout == "no"){
    
    bull_trout_col_names = unlist( bull_trout_life_stages_presence[names(bull_trout_life_stages_presence)] )
    life_stage_names = names(bull_trout_col_names)
    life_stage_presence_numbers = Life_Stage_Priorities_AU_and_Reach_data_REACH_X[ bull_trout_col_names ]
    if(any(life_stage_presence_numbers == 1)){
      species_x_output = paste(species_x_output, "Bull Trout", sep=",")
      columns_presence = which(life_stage_presence_numbers == 1)
      life_stages_x = life_stage_names[columns_presence]
      life_stages_x = paste(life_stages_x, collapse=",")
      life_stage_output = paste(life_stage_output,life_stages_x, sep="," )
    }
    
  }

  
  species_x_output = substr(species_x_output, 2, nchar(species_x_output))
  life_stage_output = substr(life_stage_output, 2, nchar(life_stage_output))
  life_stage_output = paste(unique(unlist(strsplit(life_stage_output, ","))), collapse=",")
  
  return(c(species_x_output, life_stage_output))
}



# datax = FUNCTION_combine_across_Unacceptable_and_AtRisk(HQ_LF_Unacceptable, HQ_LF_At_Risk, HQ_LF_Both)
# ------------------------------------------------------------------------------------------
#
#      Generate life stages for HQ pathway for specific reach (based on life stage presence data)
#
# ------------------------------------------------------------------------------------------

FUNCTION_generate_life_stage_list_for_species_reach_FLAT_TABLE = function(species_x, reach_x){
  
  # ----------------------- filter by reach ------
  Life_Stage_Priorities_AU_and_Reach_data_REACH_X = Life_Stage_Priorities_AU_and_Reach_data[ which(Life_Stage_Priorities_AU_and_Reach_data$ReachName == reach_x), ] 
  colnames_alternate = gsub(" ","", names(Life_Stage_Priorities_AU_and_Reach_data_REACH_X))
  
  life_stage_list_x = c()
  
  # --------- Spring Chinook --------
  if(species_x == "spring_chinook"){
    
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
  if(species_x == "steelhead"){
    
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
  if(species_x == "bull_trout"){
    
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

# ------------------------------------------------------------
#
#   Function to add Barrier Prioritization info to Output
#
# ------------------------------------------------------------

# HQ_LF_Combined = Restoration_Prioritization_Output
test_x = FALSE
if(test_x){
  HQ_LF_Combined = Restoration_Prioritization_Output
}

FUNCTION_Add_Barrier_Data = function(HQ_LF_Combined,  Barriers_Pathways_Data,  exclude_bull_trout){
  
  # ------------------- get the reaches ------------------
  for(reach_x in unique(Barriers_Pathways_Data$ReachName) ){
    
    HQ_LF_index = which(HQ_LF_Combined$ReachName == reach_x)
    barrier_index = which(Barriers_Pathways_Data$ReachName == reach_x)
    barrier_index = barrier_index[1] # if more than one barrier - just pulling the action category and habitat attribute names
    
    # ---------------- AU ranks ------------
    AU_rank_spring_chinook = AU_Ranks_data$SPCHNTier_Restoration[which(AU_Ranks_data$`Assessment Unit` == Reach_Information_data$Assessment.Unit[Reach_Information_data$ReachName == reach_x])]
    if( is.na(AU_rank_spring_chinook) ){ AU_rank_spring_chinook = 0 }
    AU_rank_steelhead = AU_Ranks_data$STLTier_Restoration[which(AU_Ranks_data$`Assessment Unit` == Reach_Information_data$Assessment.Unit[Reach_Information_data$ReachName == reach_x])]
    if( is.na(AU_rank_steelhead) ){ AU_rank_steelhead = 0 }
    AU_rank_bull_trout = AU_Ranks_data$BTTier_Restoration[which(AU_Ranks_data$`Assessment Unit` == Reach_Information_data$Assessment.Unit[Reach_Information_data$ReachName == reach_x])]
    if( is.na(AU_rank_bull_trout) ){ AU_rank_bull_trout = 0 }
    
    # ------------------------------------------------------------
    #   IF the barriers reach is in the existing list of prioritized reaches
    # ------------------------------------------------------------
    
    if( length(HQ_LF_index) > 0 ){
        
        # ---------------------------------------------------------------------------
        #    Update Pathways, Action Categories, and Habitat Attributes
        # ---------------------------------------------------------------------------
        
        # ---------------- make barrier pathway a yes --------------
        HQ_LF_Combined$Barrier_Prioritization_Pathway[HQ_LF_index] = "yes"
        
        # ------------- add pathway info -----------
        HQ_LF_Combined$Pathways[HQ_LF_index] = paste(HQ_LF_Combined$Pathways[HQ_LF_index] ,"Barriers_pathway",sep=",")
        HQ_LF_Combined$Number_of_Pathways[HQ_LF_index] = HQ_LF_Combined$Number_of_Pathways[HQ_LF_index] + 1
        
        # ----------------- add Action Categories  ------------
        HQ_LF_Combined$Action_Categories_All_Species[HQ_LF_index] = paste( HQ_LF_Combined$Action_Categories_All_Species[HQ_LF_index], Barriers_Pathways_Data$`Action Category`[barrier_index], sep=",")
        HQ_LF_Combined$Number_Action_Categories_All_Species[HQ_LF_index] = HQ_LF_Combined$Number_Action_Categories_All_Species[HQ_LF_index] + 1
        
        # -------------- add Habitat Attributes ------------
        #HQ_LF_Combined$Impaired_Habitat_Attributes_All_Species[HQ_LF_index] = paste( HQ_LF_Combined$Impaired_Habitat_Attributes_All_Species[HQ_LF_index], Barriers_Pathways_Data$Habitat_Attributes[barrier_index], sep=",")    
        #HQ_LF_Combined$Number_Impaired_Habitat_Attributes_All_Species[HQ_LF_index] = HQ_LF_Combined$Number_Action_Categories_All_Species[HQ_LF_index] + 1
  
        # -------------- add General Action ------------
        HQ_LF_Combined$Actions[HQ_LF_index] = paste( HQ_LF_Combined$Actions[HQ_LF_index], "Restore Fish Passage", sep=", ") 
        
        # ------------------------ if Spring Chinook reach ---------------------
        if( Reach_Information_data$Spring.Chinook.Reach[which(Reach_Information_data$ReachName == reach_x)] == "yes"   |
            AU_rank_spring_chinook == 1 ){
          # -------------------------- Action Pathway --------------
          HQ_LF_Combined$Actions_Spring_Chinook[HQ_LF_index] = paste(HQ_LF_Combined$Actions_Spring_Chinook[HQ_LF_index] ,"Restore Fish Passage",sep=",")
          if( substr( HQ_LF_Combined$Actions_Spring_Chinook[HQ_LF_index], 1, 3  ) == "NA,"  ){  HQ_LF_Combined$Actions_Spring_Chinook[HQ_LF_index] = substr( HQ_LF_Combined$Actions_Spring_Chinook[HQ_LF_index], 4, nchar(HQ_LF_Combined$Actions_Spring_Chinook[HQ_LF_index])  )   }
          # ------------------------- Action Category --------------------
          HQ_LF_Combined$Spring_Chinook_Actions[HQ_LF_index] = paste(HQ_LF_Combined$Spring_Chinook_Actions[HQ_LF_index] ,Crosswalk_Habitat_Attributes_and_Actions$`Action Category`[Crosswalk_Habitat_Attributes_and_Actions$Pathway == "Barriers"],sep=",")
          if( substr( HQ_LF_Combined$Spring_Chinook_Actions[HQ_LF_index], 1, 3  ) == "NA,"  ){  HQ_LF_Combined$Spring_Chinook_Actions[HQ_LF_index] = substr( HQ_LF_Combined$Spring_Chinook_Actions[HQ_LF_index], 4, nchar(HQ_LF_Combined$Spring_Chinook_Actions[HQ_LF_index])  )   }  # spring_chinook_actions_x
          # ------------------------- Unacceptable Actions --------------------
          HQ_LF_Combined$Spring_Chinook_Unacceptable_Actions[HQ_LF_index] = paste(HQ_LF_Combined$Spring_Chinook_Unacceptable_Actions[HQ_LF_index] ,Crosswalk_Habitat_Attributes_and_Actions$`Action Category`[Crosswalk_Habitat_Attributes_and_Actions$Pathway == "Barriers"],sep=",")
          if( substr( HQ_LF_Combined$Spring_Chinook_Unacceptable_Actions[HQ_LF_index], 1, 3  ) == "NA,"  ){  HQ_LF_Combined$Spring_Chinook_Unacceptable_Actions[HQ_LF_index] = substr( HQ_LF_Combined$Spring_Chinook_Unacceptable_Actions[HQ_LF_index], 4, nchar(HQ_LF_Combined$Spring_Chinook_Unacceptable_Actions[HQ_LF_index])  )   }
          # ---------------------- Spring Chinook benefit ------------
          HQ_LF_Combined$Spring_Chinook_Benefit[HQ_LF_index] = "yes" 
          # ---------------------- Add Spring Chinook to species ------------
          if( grepl( "Spring_Chinook", HQ_LF_Combined$Species[HQ_LF_index]) == FALSE ){ HQ_LF_Combined$Species[HQ_LF_index] = paste(HQ_LF_Combined$Species[HQ_LF_index], "Spring_Chinook", sep=",")  }
          
        }
        # ------------------------ if Steelhead reach ---------------------
        if( Reach_Information_data$Steelhead.Reach[which(Reach_Information_data$ReachName == reach_x)] == "yes" |
           AU_rank_steelhead == 1 ){
          # -------------------------- Action Pathway --------------
          HQ_LF_Combined$Actions_Steelhead[HQ_LF_index] = paste(HQ_LF_Combined$Actions_Steelhead[HQ_LF_index] ,"Restore Fish Passage",sep=",")
          if( substr( HQ_LF_Combined$Actions_Steelhead[HQ_LF_index], 1, 3  ) == "NA,"  ){  HQ_LF_Combined$Actions_Steelhead[HQ_LF_index] = substr( HQ_LF_Combined$Actions_Steelhead[HQ_LF_index], 4, nchar(HQ_LF_Combined$Actions_Steelhead[HQ_LF_index])  )   }
          # ------------------------- Action Category --------------------
          HQ_LF_Combined$Steelhead_Actions[HQ_LF_index] = paste(HQ_LF_Combined$Steelhead_Actions[HQ_LF_index] ,Crosswalk_Habitat_Attributes_and_Actions$`Action Category`[Crosswalk_Habitat_Attributes_and_Actions$Pathway == "Barriers"],sep=",")
          if( substr( HQ_LF_Combined$Steelhead_Actions[HQ_LF_index], 1, 3  ) == "NA,"  ){  HQ_LF_Combined$Steelhead_Actions[HQ_LF_index] = substr( HQ_LF_Combined$Steelhead_Actions[HQ_LF_index], 4, nchar(HQ_LF_Combined$Steelhead_Actions[HQ_LF_index])  )   }
          # ------------------------- Unacceptable Actions --------------------
          HQ_LF_Combined$Steelhead_Unacceptable_Actions[HQ_LF_index] = paste(HQ_LF_Combined$Steelhead_Unacceptable_Actions[HQ_LF_index] ,Crosswalk_Habitat_Attributes_and_Actions$`Action Category`[Crosswalk_Habitat_Attributes_and_Actions$Pathway == "Barriers"],sep=",")
          if( substr( HQ_LF_Combined$Steelhead_Unacceptable_Actions[HQ_LF_index], 1, 3  ) == "NA,"  ){  HQ_LF_Combined$Steelhead_Unacceptable_Actions[HQ_LF_index] = substr( HQ_LF_Combined$Steelhead_Unacceptable_Actions[HQ_LF_index], 4, nchar(HQ_LF_Combined$Steelhead_Unacceptable_Actions[HQ_LF_index])  )   }
          # ---------------------- Add Steelhead to species ------------
          if( grepl( "Steelhead", HQ_LF_Combined$Species[HQ_LF_index]) == FALSE  ){ HQ_LF_Combined$Species[HQ_LF_index] = paste(HQ_LF_Combined$Species[HQ_LF_index], "Steelhead", sep=",")  }
              
        }
        # ------------------------ if Bull Trout reach ---------------------
        if( Reach_Information_data$Bull.Trout.Reach[which(Reach_Information_data$ReachName == reach_x)] == "yes" |
           AU_rank_bull_trout == 1 ){
          # -------------------------- Action Pathway --------------
          HQ_LF_Combined$Actions_Bull_Trout[HQ_LF_index] = paste(HQ_LF_Combined$Actions_Bull_Trout[HQ_LF_index] ,"Restore Fish Passage",sep=",")
          if( substr( HQ_LF_Combined$Actions_Bull_Trout[HQ_LF_index], 1, 3  ) == "NA,"  ){  HQ_LF_Combined$Actions_Bull_Trout[HQ_LF_index] = substr( HQ_LF_Combined$Actions_Bull_Trout[HQ_LF_index], 4, nchar(HQ_LF_Combined$Actions_Bull_Trout[HQ_LF_index])  )   }
          # ------------------------- Action Category --------------------
          HQ_LF_Combined$Bull_Trout_Actions[HQ_LF_index] = paste(HQ_LF_Combined$Bull_Trout_Actions[HQ_LF_index] ,Crosswalk_Habitat_Attributes_and_Actions$`Action Category`[Crosswalk_Habitat_Attributes_and_Actions$Pathway == "Barriers"],sep=",")
          if( substr( HQ_LF_Combined$Bull_Trout_Actions[HQ_LF_index], 1, 3  ) == "NA,"  ){  HQ_LF_Combined$Bull_Trout_Actions[HQ_LF_index] = substr( HQ_LF_Combined$Bull_Trout_Actions[HQ_LF_index], 4, nchar(HQ_LF_Combined$Bull_Trout_Actions[HQ_LF_index])  )   }
          # ------------------------- Unacceptable Actions --------------------
          HQ_LF_Combined$Bull_Trout_Unacceptable_Actions[HQ_LF_index] = paste(HQ_LF_Combined$Bull_Trout_Unacceptable_Actions[HQ_LF_index] ,Crosswalk_Habitat_Attributes_and_Actions$`Action Category`[Crosswalk_Habitat_Attributes_and_Actions$Pathway == "Barriers"],sep=",")
          if( substr( HQ_LF_Combined$Bull_Trout_Unacceptable_Actions[HQ_LF_index], 1, 3  ) == "NA,"  ){  HQ_LF_Combined$Bull_Trout_Unacceptable_Actions[HQ_LF_index] = substr( HQ_LF_Combined$Bull_Trout_Unacceptable_Actions[HQ_LF_index], 4, nchar(HQ_LF_Combined$Bull_Trout_Unacceptable_Actions[HQ_LF_index])  )   }
          # ---------------------- Add Bull Trout to species ------------
          if( grepl( "Bull_Trout", HQ_LF_Combined$Species[HQ_LF_index]) == FALSE  ){ HQ_LF_Combined$Species[HQ_LF_index] = paste(HQ_LF_Combined$Species[HQ_LF_index], "Bull_Trout", sep=",")  }
          
        }
        
        # -------------------- prep the ranks (so set NA to 100) -------------
        if( is.na(AU_Ranks_data$SPCHNTier_Restoration[which(AU_Ranks_data$`Assessment Unit` == Reach_Information_data$Assessment.Unit[which(Reach_Information_data$ReachName == reach_x)])]) ){AU_rank_spring_chinook= 5}else{AU_rank_spring_chinook = AU_Ranks_data$SPCHNTier_Restoration[which(AU_Ranks_data$`Assessment Unit` == Reach_Information_data$Assessment.Unit[which(Reach_Information_data$ReachName == reach_x)])]}
        if( is.na(AU_Ranks_data$STLTier_Restoration[which(AU_Ranks_data$`Assessment Unit` == Reach_Information_data$Assessment.Unit[which(Reach_Information_data$ReachName == reach_x)])]) ){AU_rank_steelhead = 5}else{AU_rank_steelhead = AU_Ranks_data$STLTier_Restoration[which(AU_Ranks_data$`Assessment Unit` == Reach_Information_data$Assessment.Unit[which(Reach_Information_data$ReachName == reach_x)])]}
        if( is.na(AU_Ranks_data$BTTier_Restoration[which(AU_Ranks_data$`Assessment Unit` == Reach_Information_data$Assessment.Unit[which(Reach_Information_data$ReachName == reach_x)])]) ){AU_rank_bull_trout = 5}else{AU_rank_bull_trout = AU_Ranks_data$BTTier_Restoration[which(AU_Ranks_data$`Assessment Unit` == Reach_Information_data$Assessment.Unit[which(Reach_Information_data$ReachName == reach_x)])]}
        
        # --------------- remove leading NA if present -------------
        if( grepl('NA, ', HQ_LF_Combined$Actions[HQ_LF_index], fixed = TRUE)  ){ HQ_LF_Combined$Actions[HQ_LF_index] =  substring( HQ_LF_Combined$Actions[HQ_LF_index], 5,nchar( HQ_LF_Combined$Actions[HQ_LF_index]) ) }
        if( grepl('NA, ', HQ_LF_Combined$Actions_Spring_Chinook[HQ_LF_index], fixed = TRUE)  ){ HQ_LF_Combined$Actions_Spring_Chinook[HQ_LF_index] =  substring( HQ_LF_Combined$Actions_Spring_Chinook[HQ_LF_index], 5,nchar( HQ_LF_Combined$Actions_Spring_Chinook[HQ_LF_index]) ) }
        if( grepl('NA, ', HQ_LF_Combined$Actions_Steelhead[HQ_LF_index], fixed = TRUE)  ){ HQ_LF_Combined$Actions_Steelhead[HQ_LF_index] =  substring( HQ_LF_Combined$Actions_Steelhead[HQ_LF_index], 5,nchar( HQ_LF_Combined$Actions_Steelhead[HQ_LF_index]) ) }
        if( grepl('NA, ', HQ_LF_Combined$Actions_Bull_Trout[HQ_LF_index], fixed = TRUE)  ){ HQ_LF_Combined$Actions_Bull_Trout[HQ_LF_index] =  substring( HQ_LF_Combined$Actions_Bull_Trout[HQ_LF_index], 5,nchar( HQ_LF_Combined$Actions_Bull_Trout[HQ_LF_index]) ) }
        
        
    # ------------------------------------------------------------
    #   IF the barriers reach DOES NOT exist in prioritized reach list
    # ------------------------------------------------------------
    
    }else{
      
      barrier_index = which(Barriers_Pathways_Data$ReachName == reach_x)
      barrier_index = barrier_index[1] # if more than one barrier - just pulling the action category and habitat attribute names
      
      # ------------- add reach information --------------------
      HQ_and_LF_combo_x = as.data.frame( Barriers_Pathways_Data[barrier_index, columns_info]  )
      
      # ------------- add pathway info -----------
      HQ_and_LF_combo_x$Pathways = "Barriers_pathway"
      HQ_and_LF_combo_x$Number_of_Pathways = 1
      # --------- add General Actions column --------
      HQ_and_LF_combo_x$Actions = "Restore Fish Passage"
      # ------ for individual species (based on reach having species presence) ------
      HQ_and_LF_combo_x$Actions_Spring_Chinook = "NA"  
      HQ_and_LF_combo_x$Actions_Steelhead =  "NA"  
      HQ_and_LF_combo_x$Actions_Bull_Trout = "NA"  
      
      # ------------ yes or no pathways --------
      HQ_and_LF_combo_x$HabitatQuality_Spring_Chinook_Pathway = "no"
      HQ_and_LF_combo_x$HabitatQuality_Steelhead_Pathway= "no"
      HQ_and_LF_combo_x$HabitatQuality_BullTrout_Pathway= "no"
      HQ_and_LF_combo_x$LimitingFactor_Spring_Chinook_Pathway= "no"	
      HQ_and_LF_combo_x$LimitingFactor_Steelhead_Pathway= "no"	
      HQ_and_LF_combo_x$LimitingFactor_BullTrout_Pathway= "no"
      
      # ----------- add "no" to barrier -----
      HQ_and_LF_combo_x$Barrier_Prioritization_Pathway = "yes"
      
      # ------------- misc other ----------
      species_and_life_stages = FUNCTION_output_life_stages_based_on_species_and_life_stage_presence_FISH_BARRIERs(reach_x, exclude_bull_trout)
      HQ_and_LF_combo_x$Species = "NA"  # species_and_life_stages[1]
      HQ_and_LF_combo_x$SprCh_STLD_BullTr_All_Benefit = NA
      HQ_and_LF_combo_x$Spring_Chinook_Benefit = NA
      HQ_and_LF_combo_x$Spring_Chinook_Habitat_Attributes_Unacceptable = NA            
      HQ_and_LF_combo_x$Spring_Chinook_Habitat_Attributes_At_Risk = NA
      HQ_and_LF_combo_x$Spring_Chinook_Habitat_Attributes = NA   
      HQ_and_LF_combo_x$Spring_Chinook_Unacceptable_Actions = NA                      
      HQ_and_LF_combo_x$Spring_Chinook_At_Risk_Actions = NA
      HQ_and_LF_combo_x$Spring_Chinook_Actions = NA   
      HQ_and_LF_combo_x$Steelhead_Habitat_Attributes_Unacceptable = NA
      HQ_and_LF_combo_x$Steelhead_Habitat_Attributes_At_Risk = NA
      HQ_and_LF_combo_x$Steelhead_Habitat_Attributes = NA   
      HQ_and_LF_combo_x$Steelhead_Unacceptable_Actions = NA                           
      HQ_and_LF_combo_x$Steelhead_At_Risk_Actions = NA
      HQ_and_LF_combo_x$Steelhead_Actions = NA 
      HQ_and_LF_combo_x$Bull_Trout_Habitat_Attributes_Unacceptable = NA                
      HQ_and_LF_combo_x$Bull_Trout_Habitat_Attributes_At_Risk = NA
      HQ_and_LF_combo_x$Bull_Trout_Habitat_Attributes = NA 
      HQ_and_LF_combo_x$Bull_Trout_Unacceptable_Actions = NA                           
      HQ_and_LF_combo_x$Bull_Trout_At_Risk_Actions = NA
      HQ_and_LF_combo_x$Bull_Trout_Actions = NA
      HQ_and_LF_combo_x$Life_Stages = NA
      HQ_and_LF_combo_x$Life_Stages_SpringChinook = NA
      HQ_and_LF_combo_x$Life_Stages_Steelhead = NA
      HQ_and_LF_combo_x$Life_Stages_BullTrout = NA
      #HQ_and_LF_combo_x$Life_Stages_Presence = NA
      
      # ----------------- add Action Categories  ------------
      HQ_and_LF_combo_x$Action_Categories_All_Species = Barriers_Pathways_Data$`Action Category`[barrier_index]
      HQ_and_LF_combo_x$Number_Action_Categories_All_Species = 1
      
      # -------------- add Habitat Attributes ------------
      HQ_and_LF_combo_x$Impaired_Habitat_Attributes_All_Species = "NA" # Barriers_Pathways_Data$Habitat_Attributes[barrier_index]
      HQ_and_LF_combo_x$Number_Impaired_Habitat_Attributes_All_Species = "NA"  # 1
      
      # ------ more misc -------
      HQ_and_LF_combo_x$Unacceptable_Habitat_Attributes_Presence = NA
      HQ_and_LF_combo_x$Unacceptable_Impaired_Habitat_Attributes_All_Species = NA
      HQ_and_LF_combo_x$Unacceptable_Number_Impaired_Habitat_Attributes_All_Species = NA
      
      HQ_and_LF_combo_x$At_Risk_Habitat_Attributes_Presence = NA
      HQ_and_LF_combo_x$At_Risk_Impaired_Habitat_Attributes_All_Species = NA
      HQ_and_LF_combo_x$At_Risk_Number_Impaired_Habitat_Attributes_All_Species = NA
      
      # ------------------------ if Spring Chinook reach ---------------------
      if( Reach_Information_data$Spring.Chinook.Reach[which(Reach_Information_data$ReachName == reach_x)] == "yes"   |
          AU_rank_spring_chinook == 1 ){
        # -------------------------- Action Pathway --------------
        HQ_and_LF_combo_x$Actions_Spring_Chinook = paste(HQ_and_LF_combo_x$Actions_Spring_Chinook ,"Restore Fish Passage",sep=",")
        if( substr( HQ_and_LF_combo_x$Actions_Spring_Chinook, 1, 3  ) == "NA,"  ){  HQ_and_LF_combo_x$Actions_Spring_Chinook = substr( HQ_and_LF_combo_x$Actions_Spring_Chinook, 4, nchar(HQ_and_LF_combo_x$Actions_Spring_Chinook)  )   }
        # ------------------------- Action Category --------------------
        HQ_and_LF_combo_x$Spring_Chinook_Actions = paste(HQ_and_LF_combo_x$Spring_Chinook_Actions ,Crosswalk_Habitat_Attributes_and_Actions$`Action Category`[Crosswalk_Habitat_Attributes_and_Actions$Pathway == "Barriers"],sep=",")
        if( substr( HQ_and_LF_combo_x$Spring_Chinook_Actions, 1, 3  ) == "NA,"  ){  HQ_and_LF_combo_x$Spring_Chinook_Actions = substr( HQ_and_LF_combo_x$Spring_Chinook_Actions, 4, nchar(HQ_and_LF_combo_x$Spring_Chinook_Actions)  )   }
        # ------------------------- Unacceptable Actions --------------------
        HQ_and_LF_combo_x$Spring_Chinook_Unacceptable_Actions = paste(HQ_and_LF_combo_x$Spring_Chinook_Unacceptable_Actions ,Crosswalk_Habitat_Attributes_and_Actions$`Action Category`[Crosswalk_Habitat_Attributes_and_Actions$Pathway == "Barriers"],sep=",")
        if( substr( HQ_and_LF_combo_x$Spring_Chinook_Unacceptable_Actions, 1, 3  ) == "NA,"  ){  HQ_and_LF_combo_x$Spring_Chinook_Unacceptable_Actions = substr( HQ_and_LF_combo_x$Spring_Chinook_Unacceptable_Actions, 4, nchar(HQ_and_LF_combo_x$Spring_Chinook_Unacceptable_Actions)  )   }
        # ---------------------- Spring Chinook benefit ------------
        HQ_and_LF_combo_x$Spring_Chinook_Benefit = "yes" 
        # ------------------------ add species name ------------
        HQ_and_LF_combo_x$Species = "Spring_Chinook"
      }
      # ------------------------ if Steelhead reach ---------------------
      if( Reach_Information_data$Steelhead.Reach[which(Reach_Information_data$ReachName == reach_x)] == "yes" |
         AU_rank_steelhead == 1 ){
        # -------------------------- Action Pathway --------------
        HQ_and_LF_combo_x$Actions_Steelhead = paste(HQ_and_LF_combo_x$Actions_Steelhead ,"Restore Fish Passage",sep=",")
        if( substr( HQ_and_LF_combo_x$Actions_Steelhead, 1, 3  ) == "NA,"  ){  HQ_and_LF_combo_x$Actions_Steelhead = substr( HQ_and_LF_combo_x$Actions_Steelhead, 4, nchar(HQ_and_LF_combo_x$Actions_Steelhead)  )   }
        # ------------------------- Action Category --------------------
        HQ_and_LF_combo_x$Steelhead_Actions = paste(HQ_and_LF_combo_x$Steelhead_Actions ,Crosswalk_Habitat_Attributes_and_Actions$`Action Category`[Crosswalk_Habitat_Attributes_and_Actions$Pathway == "Barriers"],sep=",")
        if( substr( HQ_and_LF_combo_x$Steelhead_Actions, 1, 3  ) == "NA,"  ){  HQ_and_LF_combo_x$Steelhead_Actions = substr( HQ_and_LF_combo_x$Steelhead_Actions, 4, nchar(HQ_and_LF_combo_x$Steelhead_Actions)  )   }
        # ------------------------- Unacceptable Actions --------------------
        HQ_and_LF_combo_x$Steelhead_Unacceptable_Actions = paste(HQ_and_LF_combo_x$Steelhead_Unacceptable_Actions ,Crosswalk_Habitat_Attributes_and_Actions$`Action Category`[Crosswalk_Habitat_Attributes_and_Actions$Pathway == "Barriers"],sep=",")
        if( substr( HQ_and_LF_combo_x$Steelhead_Unacceptable_Actions, 1, 3  ) == "NA,"  ){  HQ_and_LF_combo_x$Steelhead_Unacceptable_Actions = substr( HQ_and_LF_combo_x$Steelhead_Unacceptable_Actions, 4, nchar(HQ_and_LF_combo_x$Steelhead_Unacceptable_Actions)  )   }
        # ------------------------ add species name ------------
        HQ_and_LF_combo_x$Species = paste(HQ_and_LF_combo_x$Species,"Steelhead",sep=",")
        if( substr( HQ_and_LF_combo_x$Species, 1, 3  ) == "NA,"  ){  HQ_and_LF_combo_x$Species = substr( HQ_and_LF_combo_x$Species, 4, nchar( HQ_and_LF_combo_x$Species )  )   }
      }
      # ------------------------ if Bull Trout reach ---------------------
      if( Reach_Information_data$Bull.Trout.Reach[which(Reach_Information_data$ReachName == reach_x)] == "yes" |
         AU_rank_bull_trout == 1 ){
        # -------------------------- Action Pathway --------------
        HQ_and_LF_combo_x$Actions_Bull_Trout = paste(HQ_and_LF_combo_x$Actions_Bull_Trout ,"Restore Fish Passage",sep=",")
        if( substr( HQ_and_LF_combo_x$Actions_Bull_Trout, 1, 3  ) == "NA,"  ){  HQ_and_LF_combo_x$Actions_Bull_Trout = substr( HQ_and_LF_combo_x$Actions_Bull_Trout, 4, nchar(HQ_and_LF_combo_x$Actions_Bull_Trout)  )   }
        # ------------------------- Action Category --------------------
        HQ_and_LF_combo_x$Bull_Trout_Actions = paste(HQ_and_LF_combo_x$Bull_Trout_Actions ,Crosswalk_Habitat_Attributes_and_Actions$`Action Category`[Crosswalk_Habitat_Attributes_and_Actions$Pathway == "Barriers"],sep=",")
        if( substr( HQ_and_LF_combo_x$Bull_Trout_Actions, 1, 3  ) == "NA,"  ){  HQ_and_LF_combo_x$Bull_Trout_Actions = substr( HQ_and_LF_combo_x$Bull_Trout_Actions, 4, nchar(HQ_and_LF_combo_x$Bull_Trout_Actions)  )   }
        # ------------------------- Unacceptable Actions --------------------
        HQ_and_LF_combo_x$Bull_Trout_Unacceptable_Actions = paste(HQ_and_LF_combo_x$Bull_Trout_Unacceptable_Actions ,Crosswalk_Habitat_Attributes_and_Actions$`Action Category`[Crosswalk_Habitat_Attributes_and_Actions$Pathway == "Barriers"],sep=",")
        if( substr( HQ_and_LF_combo_x$Bull_Trout_Unacceptable_Actions, 1, 3  ) == "NA,"  ){  HQ_and_LF_combo_x$Bull_Trout_Unacceptable_Actions = substr( HQ_and_LF_combo_x$Bull_Trout_Unacceptable_Actions, 4, nchar(HQ_and_LF_combo_x$Bull_Trout_Unacceptable_Actions)  )   }
        # ------------------------ add species name ------------
        HQ_and_LF_combo_x$Species = paste(HQ_and_LF_combo_x$Species,"Bull_Trout",sep=",")
        if( substr( HQ_and_LF_combo_x$Species, 1, 3  ) == "NA,"  ){  HQ_and_LF_combo_x$Species = substr( HQ_and_LF_combo_x$Species, 4, nchar( HQ_and_LF_combo_x$Species)  )   }
      }
      
      # ------ Add another row to output ------
      colnames(HQ_and_LF_combo_x) = colnames(HQ_LF_Combined)
      HQ_LF_Combined = rbind(HQ_LF_Combined, HQ_and_LF_combo_x)
    }
    
  }
  
  # ----------------------------------------- convert lists that are blank to NA --------------
  HQ_LF_Combined$Species[ which( is.na(HQ_LF_Combined$Species) ) ] = "NA"
  HQ_LF_Combined$SprCh_STLD_BullTr_All_Benefit[ which( is.na(HQ_LF_Combined$SprCh_STLD_BullTr_All_Benefit) ) ] = "NA"
  HQ_LF_Combined$Spring_Chinook_Benefit[ which( is.na(HQ_LF_Combined$Spring_Chinook_Benefit) ) ] = "NA"
  HQ_LF_Combined$Life_Stages[ which( is.na(HQ_LF_Combined$Life_Stages) ) ] = "NA"
  
  HQ_LF_Combined$Unacceptable_Habitat_Attributes_Presence[ which( is.na(HQ_LF_Combined$Unacceptable_Habitat_Attributes_Presence) ) ] = "NA"
  HQ_LF_Combined$Unacceptable_Impaired_Habitat_Attributes_All_Species[ which( is.na(HQ_LF_Combined$Unacceptable_Impaired_Habitat_Attributes_All_Species) ) ] = "NA"
  
  HQ_LF_Combined$At_Risk_Habitat_Attributes_Presence[ which( is.na(HQ_LF_Combined$At_Risk_Habitat_Attributes_Presence) ) ] = "NA"
  HQ_LF_Combined$At_Risk_Impaired_Habitat_Attributes_All_Species[ which( is.na(HQ_LF_Combined$At_Risk_Impaired_Habitat_Attributes_All_Species) ) ] = "NA"
  
  # ------------------- convert NAs in numeric to 0 -------------
  HQ_LF_Combined$Unacceptable_Number_Impaired_Habitat_Attributes_All_Species[ which( is.na(HQ_LF_Combined$Unacceptable_Number_Impaired_Habitat_Attributes_All_Species) ) ] = 0
  
  HQ_LF_Combined$At_Risk_Number_Impaired_Habitat_Attributes_All_Species[ which( is.na(HQ_LF_Combined$At_Risk_Number_Impaired_Habitat_Attributes_All_Species) ) ] = 0
  
  # ---------------------------------------
  #  Add species to reach pulled for fish passage (and not in current species reach layer)
  # ---------------------------------------
  x_i = 0
  for(reach_x in reach_to_add_species){
    x_i = x_i + 1
    reach_to_add_species_x = which(HQ_LF_Combined$ReachName == reach_x)
    # ------------------ add species name ----------
    HQ_LF_Combined$Species[reach_to_add_species_x] = species_to_add_to_reach[x_i]
    # ------------------ add barrier to action category ----------
    if(species_to_add_to_reach == "Steelhead"){
      HQ_LF_Combined$steelhead_actions_x[reach_to_add_species_x] = paste(HQ_LF_Combined$steelhead_actions_x[reach_to_add_species_x], Crosswalk_Habitat_Attributes_and_Actions$`Action Category`[Crosswalk_Habitat_Attributes_and_Actions$Pathway == "Barriers"], sep=",")
      HQ_LF_Combined$Action_Categories_All_Species[reach_to_add_species_x] = paste(HQ_LF_Combined$Action_Categories_All_Species[reach_to_add_species_x], Crosswalk_Habitat_Attributes_and_Actions$`Action Category`[Crosswalk_Habitat_Attributes_and_Actions$Pathway == "Barriers"], sep=",")
      HQ_LF_Combined$Actions_Steelhead[reach_to_add_species_x] = paste(HQ_LF_Combined$Actions_Steelhead[reach_to_add_species_x], "Restore Fish Passage", sep=",")
      
      # ------------- remove leading NA or comma ----------------
      if( grepl("NA,", HQ_LF_Combined$steelhead_actions_x[reach_to_add_species_x] )  ){  HQ_LF_Combined$steelhead_actions_x[reach_to_add_species_x] = substr(HQ_LF_Combined$steelhead_actions_x[reach_to_add_species_x], 4,nchar(HQ_LF_Combined$steelhead_actions_x[reach_to_add_species_x]))  }
      if( grepl("NA,", HQ_LF_Combined$Action_Categories_All_Species[reach_to_add_species_x] )  ){  HQ_LF_Combined$Action_Categories_All_Species[reach_to_add_species_x] = substr(HQ_LF_Combined$Action_Categories_All_Species[reach_to_add_species_x], 4,nchar(HQ_LF_Combined$Action_Categories_All_Species[reach_to_add_species_x]))  }
      if( grepl("NA,", HQ_LF_Combined$Actions_Steelhead[reach_to_add_species_x] )  ){  HQ_LF_Combined$Actions_Steelhead[reach_to_add_species_x] = substr(HQ_LF_Combined$Actions_Steelhead[reach_to_add_species_x], 4,nchar(HQ_LF_Combined$Actions_Steelhead[reach_to_add_species_x]))  }
      
      
    }
    
    
  }
  
  return(HQ_LF_Combined)

}
  


# ------------------------------------------------------------
#
#   Function to add Barrier Prioritization info to Flat Tables for Web Map
#
# ------------------------------------------------------------

# HQ_LF_Combined = Reach_Habitat_Attribute_Life_Stage__Species_Restoration_Output

FUNCTION_Add_Barrier_Data_to_WebMap_Flat_Tables = function(HQ_LF_Combined, Barriers_Pathways_Data){
  
  
  for(reach_x in unique(Barriers_Pathways_Data$ReachName) ){
    
    HQ_LF_index = which(HQ_LF_Combined$ReachName == reach_x)
    barrier_index = which(Barriers_Pathways_Data$ReachName == reach_x)
    barrier_index = barrier_index[1] # if more than one barrier - just pulling the action category and habitat attribute names
    
    # ------------------------------------------------------------
    #   IF the barriers reach is in the existing list of prioritized reaches
    # ------------------------------------------------------------
    
    if( length(HQ_LF_index) > 0){
      
      
      # ---------------------------------------------------------------------------
      #    Update Pathways, Action Categories, and Habitat Attributes
      # ---------------------------------------------------------------------------
      
      # ---------------- make barrier pathway a yes --------------
      HQ_LF_Combined$Barrier_Prioritization_Pathway[HQ_LF_index] = "yes"
      
      # ------------- add pathway info -----------
      HQ_LF_Combined$Pathways[HQ_LF_index] = paste(HQ_LF_Combined$Pathways[HQ_LF_index] ,"Barriers_pathway",sep=",")
      HQ_LF_Combined$Number_of_Pathways[HQ_LF_index] = HQ_LF_Combined$Number_of_Pathways[HQ_LF_index] + 1
      
      # ----------------- add Action Categories  ------------
      HQ_LF_Combined$Action_Categories_All_Species[HQ_LF_index] = paste( HQ_LF_Combined$Action_Categories_All_Species[HQ_LF_index], Barriers_Pathways_Data$`Action Category`[barrier_index], sep=",")
      HQ_LF_Combined$Number_Action_Categories_All_Species[HQ_LF_index] = HQ_LF_Combined$Number_Action_Categories_All_Species[HQ_LF_index] + 1
      
      # -------------- add Habitat Attributes ------------
      HQ_LF_Combined$Impaired_Habitat_Attributes_All_Species[HQ_LF_index] = paste( HQ_LF_Combined$Impaired_Habitat_Attributes_All_Species[HQ_LF_index], Barriers_Pathways_Data$Habitat_Attributes[barrier_index], sep=",")    
      HQ_LF_Combined$Number_Impaired_Habitat_Attributes_All_Species[HQ_LF_index] = HQ_LF_Combined$Number_Action_Categories_All_Species[HQ_LF_index] + 1
      
      
      # ------------------------------------------------------------
      #   IF the barriers reach DOES NOT exist in prioritized reach list
      # ------------------------------------------------------------
      
    }else{
      
      barrier_index = which(Barriers_Pathways_Data$ReachName == reach_x)
      barrier_index = barrier_index[1] # if more than one barrier - just pulling the action category and habitat attribute names
      
      # ------------- add reach information --------------------
      HQ_and_LF_combo_x = as.data.frame( Barriers_Pathways_Data[barrier_index, columns_info]  )
      
      # ------------- add pathway info -----------
      HQ_and_LF_combo_x$Pathways = "Barriers_pathway"
      HQ_and_LF_combo_x$Number_of_Pathways = 1
      
      # ------------ yes or no pathways --------
      HQ_and_LF_combo_x$HabitatQuality_Spring_Chinook_Pathway = "no"
      HQ_and_LF_combo_x$HabitatQuality_Steelhead_Pathway= "no"
      HQ_and_LF_combo_x$HabitatQuality_BullTrout_Pathway= "no"
      HQ_and_LF_combo_x$LimitingFactor_Spring_Chinook_Pathway= "no"	
      HQ_and_LF_combo_x$LimitingFactor_Steelhead_Pathway= "no"	
      HQ_and_LF_combo_x$LimitingFactor_BullTrout_Pathway= "no"
      
      # ----------- add "no" to barrier -----
      HQ_and_LF_combo_x$Barrier_Prioritization_Pathway = "yes"
      
      # ------------- misc other ----------
      HQ_and_LF_combo_x$Species = NA
      HQ_and_LF_combo_x$SprCh_STLD_BullTr_All_Benefit = NA
      HQ_and_LF_combo_x$Spring_Chinook_Benefit = NA
      HQ_and_LF_combo_x$Spring_Chinook_Habitat_Attributes = NA                    
      HQ_and_LF_combo_x$Spring_Chinook_Actions = NA                                       
      HQ_and_LF_combo_x$Steelhead_Habitat_Attributes = NA                               
      HQ_and_LF_combo_x$Steelhead_Actions = NA 
      HQ_and_LF_combo_x$Bull_Trout_Habitat_Attributes = NA                             
      HQ_and_LF_combo_x$Bull_Trout_Actions = NA
      HQ_and_LF_combo_x$Life_Stages = NA
      HQ_and_LF_combo_x$Life_Stages_Presence = NA
      
      # ----------------- add Action Categories  ------------
      HQ_and_LF_combo_x$Action_Categories_All_Species = Barriers_Pathways_Data$`Action Category`[barrier_index]
      HQ_and_LF_combo_x$Number_Action_Categories_All_Species = 1
      
      # -------------- add Habitat Attributes ------------
      HQ_and_LF_combo_x$Impaired_Habitat_Attributes_All_Species = Barriers_Pathways_Data$Habitat_Attributes[barrier_index]
      HQ_and_LF_combo_x$Number_Impaired_Habitat_Attributes_All_Species = 1
      
      # ------ more misc -------
      HQ_and_LF_combo_x$Unacceptable_Habitat_Attributes_Presence = NA
      HQ_and_LF_combo_x$Unacceptable_Impaired_Habitat_Attributes_All_Species = NA
      HQ_and_LF_combo_x$Unacceptable_Number_Impaired_Habitat_Attributes_All_Species = NA
      
      HQ_and_LF_combo_x$At_Risk_Habitat_Attributes_Presence = NA
      HQ_and_LF_combo_x$At_Risk_Impaired_Habitat_Attributes_All_Species = NA
      HQ_and_LF_combo_x$At_Risk_Number_Impaired_Habitat_Attributes_All_Species = NA
      
      # ------ Add another row to output ------
      colnames(HQ_and_LF_combo_x) = colnames(HQ_LF_Combined)
      HQ_LF_Combined = rbind(HQ_LF_Combined, HQ_and_LF_combo_x)
      
      
    }
    
  }
  
  # ----------------------------------------- convert lists that are blank to NA --------------
  HQ_LF_Combined$Species[ which( is.na(HQ_LF_Combined$Species) ) ] = "NA"
  HQ_LF_Combined$SprCh_STLD_BullTr_All_Benefit[ which( is.na(HQ_LF_Combined$SprCh_STLD_BullTr_All_Benefit) ) ] = "NA"
  HQ_LF_Combined$Spring_Chinook_Benefit[ which( is.na(HQ_LF_Combined$Spring_Chinook_Benefit) ) ] = "NA"
  HQ_LF_Combined$Life_Stages[ which( is.na(HQ_LF_Combined$Life_Stages) ) ] = "NA"
  
  HQ_LF_Combined$Unacceptable_Habitat_Attributes_Presence[ which( is.na(HQ_LF_Combined$Unacceptable_Habitat_Attributes_Presence) ) ] = "NA"
  HQ_LF_Combined$Unacceptable_Impaired_Habitat_Attributes_All_Species[ which( is.na(HQ_LF_Combined$Unacceptable_Impaired_Habitat_Attributes_All_Species) ) ] = "NA"
  
  HQ_LF_Combined$At_Risk_Habitat_Attributes_Presence[ which( is.na(HQ_LF_Combined$At_Risk_Habitat_Attributes_Presence) ) ] = "NA"
  HQ_LF_Combined$At_Risk_Impaired_Habitat_Attributes_All_Species[ which( is.na(HQ_LF_Combined$At_Risk_Impaired_Habitat_Attributes_All_Species) ) ] = "NA"
  
  
  
  # ------------------- convert NAs in numeric to 0 -------------
  HQ_LF_Combined$Unacceptable_Number_Impaired_Habitat_Attributes_All_Species[ which( is.na(HQ_LF_Combined$Unacceptable_Number_Impaired_Habitat_Attributes_All_Species) ) ] = 0
  
  HQ_LF_Combined$At_Risk_Number_Impaired_Habitat_Attributes_All_Species[ which( is.na(HQ_LF_Combined$At_Risk_Number_Impaired_Habitat_Attributes_All_Species) ) ] = 0
  
  
  return(HQ_LF_Combined)
  
}


# ------------------------------------------------------------
#
#   Function to A) remove all Bull Trout, B) remove underscore in Species Name, 
#              C) update habitat attribute names to remove & and %
#
# ------------------------------------------------------------

test_x = FALSE
if(test_x){
  data_frame_x = Restoration_Prioritization_Output_for_WebMap
  colnames_outward_facing_WebMap_ORDER = colnames_outward_facing_WebMap_ORDER
  colnames_outward_facing_WebMap_UPDATED = colnames_outward_facing_WebMap_UPDATED
}
FUNCTION_prepare_outward_facing_table = function(data_frame_x, colnames_outward_facing_WebMap_ORDER, colnames_outward_facing_WebMap_UPDATED, exclude_bull_trout){
  
  # -------------------- remove Bull Trout rows and instances ----------
  if(exclude_bull_trout == "yes" & any( data_frame_x$Species == "Bull_Trout" ) ){
    
    # --------------- identify rows that JUST have Bull Trout and remove -----------
    rows_only_Bull_Trout = which( data_frame_x$Species == "Bull_Trout"  )
    data_frame_x = data_frame_x[ -rows_only_Bull_Trout   ,   ]
    
    # --------------- remove instances of Bull Trout
    
  }


  
  # -------------- correct column order correct ------------
  data_frame_x = data_frame_x[,colnames_outward_facing_WebMap_ORDER]
  
  # ---------------- remove Under Score ( _ ) from species names
  data_frame_x$Species = gsub("_", " ", data_frame_x$Species )
  
  # ---------------- if "NA" in a life stage - take out  ----
  data_frame_x$Life_Stages = gsub("NA", "", data_frame_x$Life_Stages )
  
  # ------------- if "Restore Reach Function" (HQ pathway) - add "multiple (HQ Pathway)" ------
  HQ_pathway = which(grepl("Restore Reach Function", data_frame_x$Actions))
  data_frame_x$Life_Stages[HQ_pathway] = paste(data_frame_x$Life_Stages[HQ_pathway], "multiple (HQ pathway)", sep=", ")
  
  # --------------- if "Restore Fish Passage" (Barrier Pathway) - add "multiple (Barrier pathway)" ----
  barrier_pathway = which(grepl("Restore Fish Passage", data_frame_x$Actions))
  data_frame_x$Life_Stages[barrier_pathway] = paste(data_frame_x$Life_Stages[barrier_pathway], "multiple (barrier pathway)", sep=", ")
  
  # ------ remove leading comma from life stage name --------
  leading_comma = which(substr(data_frame_x$Life_Stages, 0, 2) == ", ")
  data_frame_x$Life_Stages[leading_comma] = substr(data_frame_x$Life_Stages[leading_comma], 3, nchar(data_frame_x$Life_Stages[leading_comma]))
  
  # ---------------- round river mile to decimal place ----
  if( any(colnames(data_frame_x) == "RM_Start" ) ){
    data_frame_x$RM_Start = round(data_frame_x$RM_Start, 2)
    data_frame_x$RM_End = round(data_frame_x$RM_End, 2)
  }

  
  # ------------------------------------------------------------
  #   Update References for WebMap: PRCT (%), AND (&), and the "/" as a space
  # ------------------------------------------------------------
  # -------------------- Fines/Embeddedness ------------------
  data_frame_x$Impaired_Habitat_Attributes_All_Species  = gsub("%Fines/Embeddedness", "PRCNT Fines and Embeddedness", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  # -------------------- Pool Quantity and Quality ------------------
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub("PoolQuantity&Quality", "Pool Quantity and Quality", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  # -------------------- Entrainment and Stranding ------------------
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub("Entrainment/Stranding", "Entrainment and Stranding", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  # -------------------- Floodplain Connectivity ------------------
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub("FloodplainConnectivity", "Off-Channel- Floodplain", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  # -------------------- Off-Channel/Side-Channels------------------
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub("Off-Channel/Side-Channels", "Off-Channel- Side-Channels", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  # -------------------- Off-Channel/Side-Channels------------------
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub(",,,", ",", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub(",,", ",", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  
  # ---------------- update the column names as necessary ------------
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub("CoarseSubstrate", "Coarse Substrate", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub("Cover-Wood", "Cover- Wood", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub("Off-Channel-Floodplain", "Off-Channel- Floodplain", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub("Off-Channel-Side-Channels", "Off-Channel- Side-Channels", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub("Cover-Undercut Banks", "Cover- Undercut Banks", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub("Cover-UndercutBanks", "Cover- Undercut Banks", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub("Temperature-AdultSpawning", "Temperature- Adult Spawning", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub("Temperature-Rearing", "Temperature- Rearing", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub("Flow-SummerBaseFlow", "Flow- Summer Base Flow", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub("Cover-Boulders", "Cover- Boulders", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub("BrookTrout", "Brook Trout", data_frame_x$Impaired_Habitat_Attributes_All_Species )

  for(i in 1:length(colnames_outward_facing_WebMap_UPDATED) ){
    
    # ---------------- change column name to be more readable ----
    colnames(data_frame_x)[i] <- colnames_outward_facing_WebMap_UPDATED[i]
    
  }
  
  
  # ------------------------------------------------------------
  #   Update Action Category Names (add spaces)
  # ------------------------------------------------------------
  unique_action_categories = unique(Crosswalk_Habitat_Attributes_and_Actions$Action_Category_2)
  for(i in 1:length(unique_action_categories)){
    
    # ----------- old (no space) action category name -------
    old_action_category_x = unique_action_categories[i]
    # ------------ updated action category name ---------
    updated_action_category_x = Crosswalk_Habitat_Attributes_and_Actions$`Action Category`[which(Crosswalk_Habitat_Attributes_and_Actions$Action_Category_2 ==old_action_category_x )][1]
    # ------ update names -------------
    data_frame_x$`Action Categories` = gsub(old_action_category_x, updated_action_category_x, data_frame_x$`Action Categories`)
  }
  

  return(data_frame_x)
  
}




# ------------------------------------------------------------
#
#   Function to  update habitat attribute names to remove & and % for Restoratoin results
#
# ------------------------------------------------------------

test_x = FALSE
if(test_x){
  data_frame_x = Restoration_Prioritization_Output_for_WebMap
}
FUNCTION_update_habitat_attributes_Restoration = function(data_frame_x){
  
  # ------------------------------------------------------------
  #   Update References for WebMap: PRCT (%), AND (&), and the "/" as a space
  # ------------------------------------------------------------
  
  # ------------------------------------------------------------
  #   Update Unacceptable Limiting Factors
  # ------------------------------------------------------------
  
  # -------------------- Fines/Embeddedness ------------------
  data_frame_x$`Unacceptable Limiting Factors`  = gsub("%Fines/Embeddedness", "PRCNT Fines and Embeddedness", data_frame_x$`Unacceptable Limiting Factors` )
  # -------------------- Pool Quantity and Quality ------------------
  data_frame_x$`Unacceptable Limiting Factors` = gsub("PoolQuantity&Quality", "Pool Quantity and Quality", data_frame_x$`Unacceptable Limiting Factors` )
  # -------------------- Entrainment and Stranding ------------------
  data_frame_x$`Unacceptable Limiting Factors` = gsub("Entrainment/Stranding", "Entrainment and Stranding", data_frame_x$`Unacceptable Limiting Factors` )
  # -------------------- Floodplain Connectivity ------------------
  data_frame_x$`Unacceptable Limiting Factors` = gsub("FloodplainConnectivity", "Off-Channel- Floodplain", data_frame_x$`Unacceptable Limiting Factors` )
  # -------------------- Off-Channel/Side-Channels ------------------
  data_frame_x$`Unacceptable Limiting Factors` = gsub("Off-Channel/Side-Channels", "Off-Channel- Side-Channels", data_frame_x$`Unacceptable Limiting Factors` )
  # -------------------- Off-Channel/Side-Channels ------------------
  data_frame_x$`Unacceptable Limiting Factors` = gsub(",,,", ",", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub(",,", ",", data_frame_x$`Unacceptable Limiting Factors` )
  
  
  # ---------------- update the column names as necessary ------------
  data_frame_x$`Unacceptable Limiting Factors` = gsub("CoarseSubstrate", "Coarse Substrate", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub("Cover-Wood", "Cover- Wood", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub("Off-Channel-Floodplain", "Off-Channel- Floodplain", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub("Off-Channel-Side-Channels", "Off-Channel- Side-Channels", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub("Cover-Undercut Banks", "Cover- Undercut Banks", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub("Cover-UndercutBanks", "Cover- Undercut Banks", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub("Temperature-AdultSpawning", "Temperature- Adult Spawning", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub("Temperature-Rearing", "Temperature- Rearing", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub("Flow-SummerBaseFlow", "Flow- Summer Base Flow", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub("Cover-Boulders", "Cover- Boulders", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub("BrookTrout", "Brook Trout", data_frame_x$`Unacceptable Limiting Factors` )
  
  data_frame_x$`Unacceptable Limiting Factors` = gsub("Temperature-AdultHolding", "Temperature- Adult Holding", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub("Pools-DeepPools", "Pools- Deep Pools", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub("Food-FoodWebResources", "Food- Food Web Resources", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub("BankStability_score", "Bank Stability", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub("ChannelStability_score", "Channel Stability", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub("Stability_Mean", "Stability", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub("_score", "", data_frame_x$`Unacceptable Limiting Factors` )
  data_frame_x$`Unacceptable Limiting Factors` = gsub("Riparian_Mean", "Riparian Mean", data_frame_x$`Unacceptable Limiting Factors` )
  
  # ----------------- remove leading columns ----------
  data_frame_x$`Unacceptable Limiting Factors` = gsub('^\\,|\\.$', '', data_frame_x$`Unacceptable Limiting Factors`)
  
  # ------------------------------------------------------------
  #   Update At-Risk Limiting Factors
  # ------------------------------------------------------------

  # -------------------- Fines/Embeddedness ------------------
  data_frame_x$`At-Risk Limiting Factors`  = gsub("%Fines/Embeddedness", "PRCNT Fines and Embeddedness", data_frame_x$`At-Risk Limiting Factors` )
  # -------------------- Pool Quantity and Quality ------------------
  data_frame_x$`At-Risk Limiting Factors` = gsub("PoolQuantity&Quality", "Pool Quantity and Quality", data_frame_x$`At-Risk Limiting Factors` )
  # -------------------- Entrainment and Stranding ------------------
  data_frame_x$`At-Risk Limiting Factors` = gsub("Entrainment/Stranding", "Entrainment and Stranding", data_frame_x$`At-Risk Limiting Factors` )
  # -------------------- Floodplain Connectivity ------------------
  data_frame_x$`At-Risk Limiting Factors` = gsub("FloodplainConnectivity", "Off-Channel- Floodplain", data_frame_x$`At-Risk Limiting Factors` )
  # -------------------- Off-Channel/Side-Channels ------------------
  data_frame_x$`At-Risk Limiting Factors` = gsub("Off-Channel/Side-Channels", "Off-Channel- Side-Channels", data_frame_x$`At-Risk Limiting Factors` )
  # --------------- remove empty spaces -----------------
  data_frame_x$`At-Risk Limiting Factors` = gsub(",,,", ",", data_frame_x$`At-Risk Limiting Factors` )
  data_frame_x$`At-Risk Limiting Factors` = gsub(",,", ",", data_frame_x$`At-Risk Limiting Factors` )
  
  
  # ---------------- update the column names as necessary ------------
  data_frame_x$`At-Risk Limiting Factors` = gsub("CoarseSubstrate", "Coarse Substrate", data_frame_x$`At-Risk Limiting Factors` )
  data_frame_x$`At-Risk Limiting Factors` = gsub("Cover-Wood", "Cover- Wood", data_frame_x$`At-Risk Limiting Factors` )
  data_frame_x$`At-Risk Limiting Factors` = gsub("Off-Channel-Floodplain", "Off-Channel- Floodplain", data_frame_x$`At-Risk Limiting Factors` )
  data_frame_x$`At-Risk Limiting Factors` = gsub("Off-Channel-Side-Channels", "Off-Channel- Side-Channels", data_frame_x$`At-Risk Limiting Factors` )
  data_frame_x$`At-Risk Limiting Factors` = gsub("Cover-Undercut Banks", "Cover- Undercut Banks", data_frame_x$`At-Risk Limiting Factors` )
  data_frame_x$`At-Risk Limiting Factors` = gsub("Cover-UndercutBanks", "Cover- Undercut Banks", data_frame_x$`At-Risk Limiting Factors` )
  data_frame_x$`At-Risk Limiting Factors` = gsub("Temperature-AdultSpawning", "Temperature- Adult Spawning", data_frame_x$`At-Risk Limiting Factors` )
  
  data_frame_x$`At-Risk Limiting Factors` = gsub("Temperature-AdultHolding", "Temperature- Adult Holding", data_frame_x$`At-Risk Limiting Factors` )
  data_frame_x$`At-Risk Limiting Factors` = gsub("Pools-DeepPools", "Pools- Deep Pools", data_frame_x$`At-Risk Limiting Factors` )
  data_frame_x$`At-Risk Limiting Factors` = gsub("Food-FoodWebResources", "Food- Food Web Resources", data_frame_x$`At-Risk Limiting Factors` )
  data_frame_x$`At-Risk Limiting Factors` = gsub("BankStability_score", "Bank Stability", data_frame_x$`At-Risk Limiting Factors` )
  data_frame_x$`At-Risk Limiting Factors` = gsub("ChannelStability_score", "Channel Stability", data_frame_x$`At-Risk Limiting Factors` )
  data_frame_x$`At-Risk Limiting Factors` = gsub("Stability_Mean", "Stability", data_frame_x$`At-Risk Limiting Factors` )
  data_frame_x$`At-Risk Limiting Factors` = gsub("_score", "", data_frame_x$`At-Risk Limiting Factors` )
  data_frame_x$`At-Risk Limiting Factors` = gsub("Riparian_Mean", "Riparian Mean", data_frame_x$`At-Risk Limiting Factors` )
  
  data_frame_x$`At-Risk Limiting Factors` = gsub("Temperature-Rearing", "Temperature- Rearing", data_frame_x$`At-Risk Limiting Factors` )
  data_frame_x$`At-Risk Limiting Factors` = gsub("Flow-SummerBaseFlow", "Flow- Summer Base Flow", data_frame_x$`At-Risk Limiting Factors` )
  data_frame_x$`At-Risk Limiting Factors` = gsub("Cover-Boulders", "Cover- Boulders", data_frame_x$`At-Risk Limiting Factors` )
  data_frame_x$`At-Risk Limiting Factors` = gsub("BrookTrout", "Brook Trout", data_frame_x$`At-Risk Limiting Factors` )

  # ----------------- remove leading columns ----------
  data_frame_x$`At-Risk Limiting Factors` = gsub('^\\,|\\.$', '', data_frame_x$`At-Risk Limiting Factors`)
  
  # --------------------------- remove leading column in Actoin Categories ---------------------------
  data_frame_x$`Action Categories`= gsub('^\\,|\\.$', '', data_frame_x$`Action Categories`)
  
  
  return(data_frame_x)
  
}


# ------------------------------------------------------------
#
#   Function to update Protection Results
#
# ------------------------------------------------------------
FUNCTION_update_Protection_results= function(data_frame_x){
  

  # ------------------------------------------------------------
  #   Update Unacceptable Limiting Factors
  # ------------------------------------------------------------
  
  # -------------------- HQ Pathway -> "Maintain Reach Function" ------------------
  data_frame_x$`Priority Actions` = gsub("HQ_spring_chinook", "Maintain Reach Function", data_frame_x$`Priority Actions` )
  data_frame_x$`Priority Actions` = gsub("HQ_steelhead", "Maintain Reach Function", data_frame_x$`Priority Actions` )
  data_frame_x$`Priority Actions` = gsub("HQ_bull_trout", "Maintain Reach Function", data_frame_x$`Priority Actions` )
  
  # -------------------- HQ Pathway -> "Maintain Reach Function" ------------------
  data_frame_x$`Priority Actions` = gsub("LF_spring_chinook", "Prevent Limiting Factors", data_frame_x$`Priority Actions` )
  data_frame_x$`Priority Actions` = gsub("LF_steelhead", "Prevent Limiting Factors", data_frame_x$`Priority Actions` )
  data_frame_x$`Priority Actions` = gsub("LF_bull_trout", "Prevent Limiting Factors", data_frame_x$`Priority Actions` )
  
  
  # ----------------------- remove duplicate Priority Actions -------
  for(row_x in 1:nrow(data_frame_x)){
    # ------------ pull single cell of priority actoins -----
    priority_action_x = data_frame_x$`Priority Actions`[row_x]
    # ----------- remove duplicates ------
    priority_action_x <- unlist(strsplit(priority_action_x, split=","))
    priority_action_x = paste(unique(priority_action_x), collapse = ',')
    data_frame_x$`Priority Actions`[row_x] = priority_action_x
  }

  return(data_frame_x)
  
}

# ------------------------------------------------------------
#
#   Function to add Reach Information
#
# ------------------------------------------------------------


# data_frame_x = Restoration_Prioritization_Output_for_WebMap
FUNCTION_add_reach_information = function(data_frame_x, colnames_reach_info){
  
  # ------ ADD Reach name -----------
  colnames_reach_info = c("ReachName",colnames_reach_info)
  
  # ---------------------- reaches data ------------
  reaches_path = "C:/Users/Ryan/Documents/GitHub/Prioritization_Step2_Data_R_Project/Data/Reaches/Reaches.shp"
  reaches <- sf::st_read(reaches_path) # this shapefile does not show up properly
  reaches <- sf::st_transform(reaches, 4326)
  reaches = as.data.frame(reaches)
  reaches = reaches[,colnames_reach_info]

  
  # -------------------- add the info ---------
  data_frame_x = merge(data_frame_x, reaches, by="ReachName")
  
  return(data_frame_x)
  
}
  
  


# ---------------------------------------------------------------------
#    Script to compare life stage priorities
# ---------------------------------------------------------------------

compare_x = FALSE

if(compare_x){
  
  life_stage_priorities_comparison = c()
  for(reach_x in Restoration_Prioritization_Output_for_WebMap$`Reach Name`){
    
    x_webmap = which(Restoration_Prioritization_Output_for_WebMap$`Reach Name` == reach_x)
    
    x_orig = which(Life_Stage_Priorities_AU_and_Reach_data$ReachName == reach_x)
    # -------------- print priority life stages that made it through the filter --------
    #output_x = t(as.data.frame(c(Restoration_Prioritization_Output_for_WebMap$`Priority Life Stages`[x], 
    #                             Life_Stage_Priorities_AU_and_Reach_data$)))
  }
}



# ---------------------------------------------------------------------------
#    
#   Misc updates for Prioritization and Restoratoin to Prepare for WebMap
#     Add Reach Rank and Organize data
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#    Prepare Restoration
# ---------------------------------------------------------------------------

FUNCTION_Add_Reach_Rank_and_Misc_Updates_for_WebMap_Restoration = function(Restoration_Prioritization_Output_for_WebMap){
  
  # ------------ make the data frame updated -----------
  Restoration_Prioritization_Output_for_WebMap_Updated = Restoration_Prioritization_Output_for_WebMap
  
  columns_to_add_Restoration_Output = c()
  for(reach_x in Restoration_Prioritization_Output_for_WebMap_Updated$ReachName){
    
    # ---------------------- Add Basin ------------------
    basin_x = Reach_Information_data$Basin[which(Reach_Information_data$ReachName == reach_x)]
    
    # ------------------- Add Reach Rank ---------------
    reach_rank_x = Reach_Rankings_Output_Restoration$AU_level_Reach_Rank[which(Reach_Rankings_Output_Restoration$ReachName == reach_x)]
    if(length(reach_rank_x) == 0){ reach_rank_x = NA}
     
    # -------------- Unacceptable Limiting Factors ----------------
    unacceptable_limiting_factors_x = Restoration_Prioritization_Output$Unacceptable_Impaired_Habitat_Attributes_All_Species[which(Restoration_Prioritization_Output$ReachName == reach_x)]
    
    # ------------ Add At-Risk Limiting Factors  ----------------
    at_risk_limiting_factors_x = Restoration_Prioritization_Output$At_Risk_Impaired_Habitat_Attributes_All_Species[which(Restoration_Prioritization_Output$ReachName == reach_x)]
    
    # --------------- combine all the output -------------
    output_x = t( as.data.frame( c(basin_x, reach_rank_x,unacceptable_limiting_factors_x, at_risk_limiting_factors_x )  )  )
    colnames(output_x) = c("Basin","Reach Rank", "Unacceptable Limiting Factors", "At-Risk Limiting Factors")
    columns_to_add_Restoration_Output = rbind(columns_to_add_Restoration_Output,  output_x )
    
  }
  rownames(columns_to_add_Restoration_Output) = rownames(Restoration_Prioritization_Output_for_WebMap_Updated)
  Restoration_Prioritization_Output_for_WebMap_Updated = cbind(Restoration_Prioritization_Output_for_WebMap_Updated, columns_to_add_Restoration_Output)
  
  # ---------------- Update Column Names --------
  if(any(colnames(Restoration_Prioritization_Output_for_WebMap_Updated) == "ReachName")){
    colx = which(colnames( Restoration_Prioritization_Output_for_WebMap_Updated ) == "ReachName")
    colnames( Restoration_Prioritization_Output_for_WebMap_Updated )[colx] = "Reach Name"
  }
  if(any(colnames(Restoration_Prioritization_Output_for_WebMap_Updated) == "Assessment.Unit")){
    colx = which(colnames( Restoration_Prioritization_Output_for_WebMap_Updated ) == "Assessment.Unit")
    colnames( Restoration_Prioritization_Output_for_WebMap_Updated )[colx] = "Assessment Unit"
  }
  if(any(colnames(Restoration_Prioritization_Output_for_WebMap_Updated) == "Actions")){
    colx = which(colnames( Restoration_Prioritization_Output_for_WebMap_Updated ) == "Actions")
    colnames( Restoration_Prioritization_Output_for_WebMap_Updated )[colx] = "Priority Actions"
  }
  if(any(colnames(Restoration_Prioritization_Output_for_WebMap_Updated) == "Life_Stages")){
    colx = which(colnames( Restoration_Prioritization_Output_for_WebMap_Updated ) == "Life_Stages")
    colnames( Restoration_Prioritization_Output_for_WebMap_Updated )[colx] = "Priority Life Stages"
  }
  if(any(colnames(Restoration_Prioritization_Output_for_WebMap_Updated) == "Species")){
    colx = which(colnames( Restoration_Prioritization_Output_for_WebMap_Updated ) == "Species")
    colnames( Restoration_Prioritization_Output_for_WebMap_Updated )[colx] = "Priority Species"
  }
  if(any(colnames(Restoration_Prioritization_Output_for_WebMap_Updated) == "Action_Categories_All_Species")){
    colx = which(colnames( Restoration_Prioritization_Output_for_WebMap_Updated ) == "Action_Categories_All_Species")
    colnames( Restoration_Prioritization_Output_for_WebMap_Updated )[colx] = "Action Categories"
  }
  
  # --------------------------- re-order reaches to be in correct order ---------------------------------
  # NOTE: these are also the correct column names (as of 24.June.2021)
  Restoration_Prioritization_Output_for_WebMap_Updated_column_order = c("Reach Name","Basin", "Assessment Unit" ,"Priority Actions" ,"Reach Rank",            
                                                                        "Priority Species" , "Priority Life Stages" ,                 
                                                                        "Unacceptable Limiting Factors" ,"At-Risk Limiting Factors", "Action Categories" )
  Restoration_Prioritization_Output_for_WebMap_Updated = Restoration_Prioritization_Output_for_WebMap_Updated[,Restoration_Prioritization_Output_for_WebMap_Updated_column_order]
  
  # ----------------------- update habit at attribute names ------
  Restoration_Prioritization_Output_for_WebMap_Updated = FUNCTION_update_habitat_attributes_Restoration(Restoration_Prioritization_Output_for_WebMap_Updated)
  
  #---------------- convert reach rank to integer -----------
  Restoration_Prioritization_Output_for_WebMap_Updated$`Reach Rank` = as.character(Restoration_Prioritization_Output_for_WebMap_Updated$`Reach Rank`)
  
  # ------------------------------------------------------------------------------------ 
  #                     update action category names and species names
  # ------------------------------------------------------------------------------------ 
  
  Crosswalk_Habitat_Attributes_and_Actions_Unique = Crosswalk_Habitat_Attributes_and_Actions[which(  duplicated(Crosswalk_Habitat_Attributes_and_Actions$Action_Category_2) == FALSE ), ]
  Crosswalk_Habitat_Attributes_and_Actions_Unique = Crosswalk_Habitat_Attributes_and_Actions_Unique[order(Crosswalk_Habitat_Attributes_and_Actions_Unique$Action_Category_2), ]
  
  for(row_x in 1:nrow(Restoration_Prioritization_Output_for_WebMap_Updated) ){
    
    # ----------------------------------------------------
    #     Update action category names (so spaces between woords)
    # ----------------------------------------------------
    
    action_x = Restoration_Prioritization_Output_for_WebMap_Updated$'Action Categories'[row_x]
    
    # ------------- loop thorugh all actions and update names --------
    actions_new_x = NA
    for(row_action_name_x in 1:nrow(Crosswalk_Habitat_Attributes_and_Actions_Unique) ){
      if( grepl(Crosswalk_Habitat_Attributes_and_Actions_Unique$Action_Category_2[row_action_name_x], action_x, fixed=TRUE  ) ){
        actions_new_x = paste(actions_new_x, Crosswalk_Habitat_Attributes_and_Actions_Unique$`Action Category`[row_action_name_x], sep=", ")
      }
    }
    # -------- remove leading NA ---------
    if( grepl("NA, ", actions_new_x, fixed=TRUE  ) ){ actions_new_x = substring(actions_new_x, 5, nchar(actions_new_x)) }
    # --------------- update action categories --------
    Restoration_Prioritization_Output_for_WebMap_Updated$'Action Categories'[row_x] = actions_new_x
    
    # ----------------------------------------------------
    #     Update species names (so no undersscore between Spring and Chinook)
    # ----------------------------------------------------
    
    species_x = Restoration_Prioritization_Output_for_WebMap_Updated$`Priority Species`[row_x]
    species_new_x = "no priority species"
    
    if( grepl("Spring_Chinook", species_x, fixed=TRUE  ) |  grepl("Spring Chinook", species_x, fixed=TRUE  )  ){ species_new_x = paste(species_new_x, "Spring Chinook", sep=", ") }
    if( grepl("Steelhead", species_x, fixed=TRUE  ) ){ species_new_x = paste(species_new_x, "Steelhead", sep=", ") }
    if( grepl("Bull_Trout", species_x, fixed=TRUE  )  | grepl("Bull Trout", species_x, fixed=TRUE  ) ){ species_new_x = paste(species_new_x, "Bull Trout", sep=", ") }
    
    # ------------------ remove "no priority species" is species present
    if( grepl("no priority species, ", species_new_x, fixed=TRUE  ) ){ species_new_x = substring(species_new_x, 22, nchar(species_new_x)) }
    
    # --------------- update action categories --------
    Restoration_Prioritization_Output_for_WebMap_Updated$`Priority Species`[row_x]= species_new_x
    
  }
  
  # ------------------------------------------------------------------------------------ 
  #              Add species to reach pulled for fish passage (and not in current species reach layer)
  # ------------------------------------------------------------------------------------
  x_i = 0
  for(reach_x in reach_to_add_species){
    x_i = x_i + 1
    reach_to_add_species_x = which(Restoration_Prioritization_Output_for_WebMap_Updated$`Reach Name` == reach_x)
    Restoration_Prioritization_Output_for_WebMap_Updated$`Priority Species`[reach_to_add_species_x] = species_to_add_to_reach[x_i]
  }

  # ------------------------------------------------------------------------------------ 
  #              Update "Off-Channel- Floodplain" to "Floodplain Connectivity"
  # ------------------------------------------------------------------------------------
  Restoration_Prioritization_Output_for_WebMap_Updated$`Unacceptable Limiting Factors` = gsub("Off-Channel- Floodplain", "Floodplain Connectivity", Restoration_Prioritization_Output_for_WebMap_Updated$`Unacceptable Limiting Factors`)
  Restoration_Prioritization_Output_for_WebMap_Updated$`At-Risk Limiting Factors` = gsub("Off-Channel- Floodplain", "Floodplain Connectivity", Restoration_Prioritization_Output_for_WebMap_Updated$`At-Risk Limiting Factors`)
  
  return(Restoration_Prioritization_Output_for_WebMap_Updated)
}


# ---------------------------------------------------------------------------
#    Prepare Restoration - SINGLE SPECIES
# ---------------------------------------------------------------------------
test_x = FALSE
if(test_x){
  indiv_restoration_output = Restoration_Prioritization_Output_Steelhead
}

FUNCTION_Add_Reach_Rank_and_Misc_Updates_for_WebMap_Restoration_INDIV_SPECIES = function(indiv_restoration_output){
  
  # ------------ make the data frame updated -----------
  indiv_restoration_output_Updated = indiv_restoration_output
  
  # ---------- add Basin column ------------
  indiv_restoration_output_Updated$Basin = NA
  indiv_restoration_output_Updated$ReachRank = NA
  
  columns_to_add_Restoration_Output = c()
  for(reach_x in indiv_restoration_output_Updated$ReachName){
    
    # ---------------- pull the row ---------------
    reach_index_x = which(indiv_restoration_output_Updated$ReachName == reach_x)
    
    # ---------------------- Add Basin ------------------
    indiv_restoration_output_Updated$Basin[reach_index_x] = Reach_Information_data$Basin[which(Reach_Information_data$ReachName == reach_x)]
    
    # ------------------- Add Reach Rank ---------------
    reach_rank_x = Reach_Rankings_Output_Restoration$AU_level_Reach_Rank[which(Reach_Rankings_Output_Restoration$ReachName == reach_x)]
    if(length(reach_rank_x) == 0){ reach_rank_x = NA}
    indiv_restoration_output_Updated$ReachRank[reach_index_x] = reach_rank_x
    
  }
  
  # ----------- identify which columns ------------
  # ----------- priority action ------
  action_column_name_index_x = which( grepl("Actions_", colnames(indiv_restoration_output_Updated), fixed=TRUE ) )
  action_column_name_x = colnames(indiv_restoration_output_Updated)[action_column_name_index_x]
  # ----------- priority life stage ------
  life_stage_column_name_index_x = which( grepl("Life_Stages_", colnames(indiv_restoration_output_Updated), fixed=TRUE ) )
  life_stage_column_name_x = colnames(indiv_restoration_output_Updated)[life_stage_column_name_index_x]
  # ----------- at risk habitat attributes ------
  at_risk_column_name_index_x = which( grepl("_Habitat_Attributes_At_Risk", colnames(indiv_restoration_output_Updated), fixed=TRUE ) )
  at_risk_column_name_x = colnames(indiv_restoration_output_Updated)[at_risk_column_name_index_x]
  # ----------- unacceptable habitat attributes ------
  unacceptable_column_name_index_x = which( grepl("_Habitat_Attributes_Unacceptable", colnames(indiv_restoration_output_Updated), fixed=TRUE ) )
  unacceptable_column_name_x = colnames(indiv_restoration_output_Updated)[unacceptable_column_name_index_x]
  # ----------- unacceptable habitat attributes ------
  actions_column_name_index_x = which( grepl("_Actions", colnames(indiv_restoration_output_Updated), fixed=TRUE ) )
  actions_column_name_x = colnames(indiv_restoration_output_Updated)[actions_column_name_index_x]

  # ---------------- Update Column Names --------
  if(any(colnames(indiv_restoration_output_Updated) == "ReachName")){
    colx = which(colnames( indiv_restoration_output_Updated ) == "ReachName")
    colnames( indiv_restoration_output_Updated )[colx] = "Reach Name"
  }
  if(any(colnames(indiv_restoration_output_Updated) == "Assessment.Unit")){
    colx = which(colnames( indiv_restoration_output_Updated ) == "Assessment.Unit")
    colnames( indiv_restoration_output_Updated )[colx] = "Assessment Unit"
  }
  if(any(colnames(indiv_restoration_output_Updated) == "ReachRank")){
    colx = which(colnames( indiv_restoration_output_Updated ) == "ReachRank")
    colnames( indiv_restoration_output_Updated )[colx] = "Reach Rank"
  }
  if(any(colnames(indiv_restoration_output_Updated) == action_column_name_x)){
    colx = which(colnames( indiv_restoration_output_Updated ) == action_column_name_x)
    colnames( indiv_restoration_output_Updated )[colx] = "Priority Actions"
  }
  if(any(colnames(indiv_restoration_output_Updated) == life_stage_column_name_x)){
    colx = which(colnames( indiv_restoration_output_Updated ) == life_stage_column_name_x)
    colnames( indiv_restoration_output_Updated )[colx] = "Priority Life Stages"
  }
  if(any(colnames(indiv_restoration_output_Updated) == at_risk_column_name_x)){
    colx = which(colnames( indiv_restoration_output_Updated ) == at_risk_column_name_x)
    colnames( indiv_restoration_output_Updated )[colx] = "At-Risk Limiting Factors"
  }
  if(any(colnames(indiv_restoration_output_Updated) == unacceptable_column_name_x)){
    colx = which(colnames( indiv_restoration_output_Updated ) == unacceptable_column_name_x)
    colnames( indiv_restoration_output_Updated )[colx] = "Unacceptable Limiting Factors"
  }
  if(any(colnames(indiv_restoration_output_Updated) == actions_column_name_x)){
    colx = which(colnames( indiv_restoration_output_Updated ) == actions_column_name_x)
    colnames( indiv_restoration_output_Updated )[colx] = "Action Categories"
  }
  
  
  # -------------------- Floodplain Connectivity ------------------
  indiv_restoration_output_Updated$"Unacceptable Limiting Factors" = gsub("FloodplainConnectivity", "Off-Channel- Floodplain", indiv_restoration_output_Updated$"Unacceptable Limiting Factors" )
  indiv_restoration_output_Updated$"At-Risk Limiting Factors" = gsub("FloodplainConnectivity", "Off-Channel- Floodplain", indiv_restoration_output_Updated$"At-Risk Limiting Factors" )
  # -------------------- Off-Channel- Side-Channels ------------------
  indiv_restoration_output_Updated$"Unacceptable Limiting Factors" = gsub("Off-Channel/Side-Channels", "Off-Channel- Side-Channels", indiv_restoration_output_Updated$"Unacceptable Limiting Factors" )
  indiv_restoration_output_Updated$"At-Risk Limiting Factors" = gsub("Off-Channel/Side-Channels", "Off-Channel- Side-Channels", indiv_restoration_output_Updated$"At-Risk Limiting Factors" )
  # -------------------- Remove empty spaces ------------------
  indiv_restoration_output_Updated$"Unacceptable Limiting Factors" = gsub(",,,", ",", indiv_restoration_output_Updated$"Unacceptable Limiting Factors" )
  indiv_restoration_output_Updated$"At-Risk Limiting Factors" = gsub(",,", ",", indiv_restoration_output_Updated$"At-Risk Limiting Factors" )
  
  
  # --------------------------- re-order reaches to be in correct order ---------------------------------
  # NOTE: these are also the correct column names (as of 24.June.2021)
  Indiv_Restoration_Output_for_WebMap_Updated_column_order = c("Reach Name","Basin", "Assessment Unit" ,"Priority Actions" ,"Reach Rank",            
                                                                         "Priority Life Stages" ,                 
                                                                        "Unacceptable Limiting Factors" ,"At-Risk Limiting Factors", "Action Categories" )
  indiv_restoration_output_Updated = indiv_restoration_output_Updated[,Indiv_Restoration_Output_for_WebMap_Updated_column_order]
  
  # ----------------------- update habit at attribute names ------
  indiv_restoration_output_Updated = FUNCTION_update_habitat_attributes_Restoration(indiv_restoration_output_Updated)
  
  #---------------- convert reach rank to integer -----------
  indiv_restoration_output_Updated$`Reach Rank` = as.character(indiv_restoration_output_Updated$`Reach Rank`)
  
  # --------------------- update action category names ---------------------
  Crosswalk_Habitat_Attributes_and_Actions_Unique = Crosswalk_Habitat_Attributes_and_Actions[which(  duplicated(Crosswalk_Habitat_Attributes_and_Actions$Action_Category_2) == FALSE ), ]
  Crosswalk_Habitat_Attributes_and_Actions_Unique = Crosswalk_Habitat_Attributes_and_Actions_Unique[order(Crosswalk_Habitat_Attributes_and_Actions_Unique$Action_Category_2), ]
  
  for(row_x in 1:nrow(indiv_restoration_output_Updated) ){
    action_x = indiv_restoration_output_Updated$'Action Categories'[row_x]
    
    # ------------- loop thorugh all actions and update names --------
    actions_new_x = NA
    for(row_action_name_x in 1:nrow(Crosswalk_Habitat_Attributes_and_Actions_Unique) ){
      if( grepl(Crosswalk_Habitat_Attributes_and_Actions_Unique$Action_Category_2[row_action_name_x], action_x, fixed=TRUE  ) ){
        actions_new_x = paste(actions_new_x, Crosswalk_Habitat_Attributes_and_Actions_Unique$`Action Category`[row_action_name_x], sep=", ")
      }
    }
    # -------- remove leading NA ---------
    if( grepl("NA, ", actions_new_x, fixed=TRUE  ) ){ actions_new_x = substring(actions_new_x, 5, nchar(actions_new_x)) }
    # --------------- update action categories --------
    indiv_restoration_output_Updated$'Action Categories'[row_x] = actions_new_x
    
  }
  
  # ------------------------------------------------------------------------------------ 
  #              Update "Off-Channel- Floodplain" to "Floodplain Connectivity"
  # ------------------------------------------------------------------------------------
  indiv_restoration_output_Updated$`Unacceptable Limiting Factors` = gsub("Off-Channel- Floodplain", "Floodplain Connectivity", indiv_restoration_output_Updated$`Unacceptable Limiting Factors`)
  indiv_restoration_output_Updated$`At-Risk Limiting Factors` = gsub("Off-Channel- Floodplain", "Floodplain Connectivity", indiv_restoration_output_Updated$`At-Risk Limiting Factors`)
  
  
  return(indiv_restoration_output_Updated)
}


# ---------------------------------------------------------------------------
#    Prepare Protection
# ---------------------------------------------------------------------------

FUNCTION_Add_Reach_Rank_and_Misc_Updates_for_WebMap_Protection = function(Protection_Prioritization_Output){

  Protection_Prioritization_Output_Updated = Protection_Prioritization_Output
  
  Protection_Prioritization_Output_Updated$Reach_Rank = NA
  for(row_i in 1:nrow( Protection_Prioritization_Output_Updated) ){
    
    # ------------------- Pull Reach Rank ---------------
    reach_rank_x = Reach_Rankings_Output_Protection$AU_level_Reach_Rank[ which( Reach_Rankings_Output_Protection$ReachName == Protection_Prioritization_Output_Updated$ReachName[row_i] )]
    
    # --------------- Add to Ouptut -------------
    if(length(reach_rank_x) > 0 ){
      Protection_Prioritization_Output_Updated$Reach_Rank[row_i] = reach_rank_x
    }
    
  }
  
  # ------------------- order columns and give them correct names --------------
  Protection_Prioritization_Output_Updated_column_order =         c("ReachName","Basin", "Assessment.Unit",      "Pathway",    "Reach_Rank",  "Life_Stages","Action")
  
  Protection_Prioritization_Output_Updated_column_UPDATED_names = c("Reach Name", "Basin", "Assessment Unit", "Priority Actions","Reach Rank", "Priority Life Stages", "Action Categories")
  
  # ---------------- re-order columns for WebMap ------
  Protection_Prioritization_Output_Updated_for_WebMap = Protection_Prioritization_Output_Updated[,Protection_Prioritization_Output_Updated_column_order]
  # --------------- correct names for output ------------
  colnames(Protection_Prioritization_Output_Updated_for_WebMap) = Protection_Prioritization_Output_Updated_column_UPDATED_names
  
  # ----------------------- update habit at attribute names ------
  Protection_Prioritization_Output_Updated_for_WebMap = FUNCTION_update_Protection_results(Protection_Prioritization_Output_Updated_for_WebMap)
  
  #---------------- convert reach rank to integer -----------
  Protection_Prioritization_Output_Updated_for_WebMap$`Reach Rank` = as.character(Protection_Prioritization_Output_Updated_for_WebMap$`Reach Rank`)
  
  return(Protection_Prioritization_Output_Updated_for_WebMap)
}


# ---------------------------------------------------------------------------
#    Prepare Protection - single species
# ---------------------------------------------------------------------------

test_x = FALSE
if(test_x){
  Protection_Prioritization_Output_indiv_species = Protection_Prioritization_Output_Spring_Chinook
  species_x = "Spring Chinook"
}

FUNCTION_Add_Reach_Rank_and_Misc_Updates_for_WebMap_Protection_INDIV_SPECIES = function(Protection_Prioritization_Output_indiv_species, species_x){
  
  Protection_Prioritization_Output_indiv_species$Reach_Rank = NA
  for(row_i in 1:nrow( Protection_Prioritization_Output_indiv_species) ){
    
    # ------------------- Pull Reach Rank ---------------
    reach_rank_x = Reach_Rankings_Output_Protection$AU_level_Reach_Rank[ which( Reach_Rankings_Output_Protection$ReachName == Protection_Prioritization_Output_indiv_species$ReachName[row_i] )]
    
    # --------------- Add to Output -------------
    if(length(reach_rank_x) > 0 ){
      Protection_Prioritization_Output_indiv_species$Reach_Rank[row_i] = reach_rank_x
    }
    
  }
  
  # ------------------- order columns and give them correct names --------------
  if(species_x == "Spring Chinook"){
    Protection_Prioritization_Output_Updated_column_order = c("ReachName", "Assessment.Unit", "Basin",      "Spring_Chinook_Pathways",    "Reach_Rank",  "Spring_Chinook_Life_Stages","Action")
  }else if(species_x == "Steelhead"){
    Protection_Prioritization_Output_Updated_column_order = c("ReachName", "Assessment.Unit", "Basin",      "Steelhead_Pathways",    "Reach_Rank",  "Steelhead_Life_Stages","Action")
  }else if(species_x == "Bull Trout"){
    Protection_Prioritization_Output_Updated_column_order = c("ReachName", "Assessment.Unit", "Basin",      "Bull_Trout_Pathways",    "Reach_Rank",  "Bull_Trout_Life_Stages","Action")
  }
    
  Protection_Prioritization_Output_Updated_column_UPDATED_names = c("Reach Name","Assessment Unit", "Basin",  "Priority Actions","Reach Rank", "Priority Life Stages", "Action Categories")
  
  # ---------------- re-order columns for WebMap ------
  Protection_Prioritization_Output_Updated_for_WebMap = Protection_Prioritization_Output_indiv_species[,Protection_Prioritization_Output_Updated_column_order]
  # --------------- correct names for output ------------
  colnames(Protection_Prioritization_Output_Updated_for_WebMap) = Protection_Prioritization_Output_Updated_column_UPDATED_names
  
  # ----------------------- update habit at attribute names ------
  Protection_Prioritization_Output_Updated_for_WebMap = FUNCTION_update_Protection_results(Protection_Prioritization_Output_Updated_for_WebMap)
  
  #---------------- convert reach rank to integer -----------
  Protection_Prioritization_Output_Updated_for_WebMap$`Reach Rank` = as.character(Protection_Prioritization_Output_Updated_for_WebMap$`Reach Rank`)
  
  return(Protection_Prioritization_Output_Updated_for_WebMap)
}
