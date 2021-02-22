
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

# score_1_or_3 = "one"
# restoration_or_protection = 'restoration'

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
  reaches_unique = unique( c(HQ_spring_chinook$ReachName, HQ_steelhead$ReachName, HQ_bull_trout$ReachName ) )
  reaches_unique = reaches_unique[order(reaches_unique)]  # just alphabetize 
  
  # ------------ get Reach Information ------
  Reach_Information_HQ_Actions = Reach_Information_data %>%  
    filter(ReachName   %in% reaches_unique)
  # --------------- just get important reach information  ----------
  Reach_Information_HQ_Actions = Reach_Information_HQ_Actions[, c('ReachName','Basin',"Assessment.Unit",  "Spring.Chinook.Reach","Steelhead.Reach" ,"Bull.Trout.Reach" )]
  
  # ------------------------------------------------------------
  #       Loop Through reaches to get information
  # ------------------------------------------------------------
  Pathway_Output_x = c()
  for(reachname_x in reaches_unique){
    
    # --------------------- initiate cells for each reach ---------------
    pathways_x  = c()
    habitat_attributes_x = c()
    action_categories_x = c()
    spring_chinook_habitat_attributes = c()
    steelhead_habitat_attributes = c()
    bull_trout_habitat_attributes = c()
    all_three_species_habitat_attributes = c()
    spring_chinook_actions = c()
    steelhead_actions = c()
    bull_trout_actions = c()
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
        spring_chinook_actions = HQ_row_x[,actions_column]
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
        steelhead_actions = HQ_row_x[,actions_column]
      }
    }
   
    
    # ---------------------- Bull_Trout ----------
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
        bull_trout_actions = HQ_row_x[,actions_column]
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
      spring_chinook_habitat_attributes = unique(unlist(strsplit(spring_chinook_habitat_attributes, ",")))   # get list of habitat attributes with NO redundancies
      number_of_spring_chinook_habitat_attributes =  length( spring_chinook_habitat_attributes )
      spring_chinook_habitat_attributes = paste(spring_chinook_habitat_attributes, collapse=",")
      spring_chinook_presence = "yes"
      species_count_x = species_count_x + 1
    }

    # -------------- number of action categories -----------
    if(is.null(spring_chinook_actions)){
      spring_chinook_actions = NA   # get list of habitat attributes with NO redundancies
      number_of_spring_chinook_actions =  0
    }else{
      spring_chinook_actions = unique( unlist(strsplit(spring_chinook_actions, ",")) )  # get list of action categories with NO redundancies
      number_of_spring_chinook_actions =  length( spring_chinook_actions )
      spring_chinook_actions = paste(spring_chinook_actions, collapse=",")
    }
    
    # -----------------------------------------
    #     for steelhead
    # -----------------------------------------
    # ------- number of habitat attributes -------------
    if(is.null(steelhead_habitat_attributes)){
      steelhead_habitat_attributes = NA   # get list of habitat attributes with NO redundancies
      number_of_steelhead_habitat_attributes =  0
    }else{
      steelhead_habitat_attributes = unique(unlist(strsplit(steelhead_habitat_attributes, ",")))   # get list of habitat attributes with NO redundancies
      number_of_steelhead_habitat_attributes =  length( steelhead_habitat_attributes )
      steelhead_habitat_attributes = paste(steelhead_habitat_attributes, collapse=",")
      species_count_x = species_count_x + 1
    }
    # -------------- number of action categories -----------
    if(is.null(steelhead_actions)){
      steelhead_actions = NA   # get list of habitat attributes with NO redundancies
      number_of_steelhead_actions =  0
    }else{
      steelhead_actions = unique( unlist(strsplit(steelhead_actions, ",")) )  # get list of action categories with NO redundancies
      number_of_steelhead_actions =  length( steelhead_actions )
      steelhead_actions = paste(steelhead_actions, collapse=",")
    }
    
    # -----------------------------------------
    #     for bull trout
    # -----------------------------------------
    # ------- number of habitat attributes -------------
    if(is.null(bull_trout_habitat_attributes)){
      bull_trout_habitat_attributes = NA   # get list of habitat attributes with NO redundancies
      number_of_bull_trout_habitat_attributes =  0
    }else{
      bull_trout_habitat_attributes = unique(unlist(strsplit(bull_trout_habitat_attributes, ",")))   # get list of habitat attributes with NO redundancies
      number_of_bull_trout_habitat_attributes =  length( bull_trout_habitat_attributes )
      bull_trout_habitat_attributes = paste(bull_trout_habitat_attributes, collapse=",")
      species_count_x = species_count_x + 1
    }
    
    # -------------- number of action categories -----------
    if(is.null(bull_trout_actions)){
      bull_trout_actions = NA   # get list of habitat attributes with NO redundancies
      number_of_bull_trout_actions =  0
    }else{
      bull_trout_actions = unique( unlist(strsplit(bull_trout_actions, ",")) )  # get list of action categories with NO redundancies
      number_of_bull_trout_actions =  length( bull_trout_actions )
      bull_trout_actions = paste(bull_trout_actions, collapse=",")
    }
    
    # -------------- if all species are present - yes or no -------------
    if(species_count_x == 3){ all_species_present = "yes"
    }else{ all_species_present = "no" }
    
    # -----------------------------------------
    #   Add life stage ( Life_Stage_Priorities_AU_and_Reach_data has 0 and 1 for life stage presence )
    # ----------------------------------------- 
    life_stage_list = c()
    if(!is.null(spring_chinook_habitat_attributes)){
      life_stages_present = FUNCTION_life_stage_presence("spring_chinook", reachname_x)
      life_stage_list = paste(life_stage_list, life_stages_present, sep="," )
      life_stage_spring_chinook_output = life_stages_present
    }else{
      life_stage_spring_chinook_output = "NA"
    }
    if(!is.null(steelhead_habitat_attributes)){
      life_stages_present = FUNCTION_life_stage_presence("steelhead", reachname_x)
      life_stage_list = paste(life_stage_list, life_stages_present, sep="," )
      life_stage_steelhead_output = life_stages_present
    }else{
      life_stage_steelhead_output = "NA"
    }
    if(!is.null(bull_trout_habitat_attributes) & exclude_bull_trout != "yes"){
      life_stages_present = FUNCTION_life_stage_presence("bull_trout", reachname_x)
      life_stage_list = paste(life_stage_list, life_stages_present, sep="," )
      life_stage_bull_trout_output = life_stages_present
    }else{
      life_stage_bull_trout_output = "NA"
    }
    # ---------------- remove leading comma -------
    life_stage_list = substr(life_stage_list,2,nchar(life_stage_list))
    
    # -----------------------------------------
    #     combine and output
    # -----------------------------------------
    
    output_row_x = t( as.data.frame(c(spring_chinook_presence,  all_species_present,
                                      pathways_x, number_of_pathways_x,
                                      habitat_attributes_x,number_of_habitat_attributes_x, 
                                      spring_chinook_habitat_attributes, number_of_spring_chinook_habitat_attributes,
                                      steelhead_habitat_attributes, number_of_steelhead_habitat_attributes,
                                      bull_trout_habitat_attributes, number_of_bull_trout_habitat_attributes,
                                      action_categories_x, number_of_action_categories_x,
                                      spring_chinook_actions, number_of_spring_chinook_actions,
                                      steelhead_actions, number_of_steelhead_actions,
                                      bull_trout_actions, number_of_bull_trout_actions, 
                                      life_stage_list, life_stage_spring_chinook_output, life_stage_steelhead_output, life_stage_bull_trout_output )  )  )
    colnames(output_row_x) = c("Spring_Chinook_Actions_Present_Yes_No", "SprCh_STLD_BullTr_All_Present_Yes_No",
                               "Pathways","Number_of_Pathways", 
                               "Impaired_Habitat_Attributes_All_Species","Number_Impaired_Habitat_Attributes_All_Species",
                               "Impaired_Habitat_Attributes_SpringChinook","Number_Impaired_Habitat_Attributes_SpringChinook",
                               "Impaired_Habitat_Attributes_Steelhead","Number_Impaired_Habitat_Attributes_Steelhead",
                               "Impaired_Habitat_Attributes_BullTrout","Number_Impaired_Habitat_Attributes_BullTrout",
                               "Action_Categories_All_Species", "Number_Action_Categories_All_Species",
                               "Action_Categories_SpringChinook", "Number_Action_Categories_SpringChinook",
                               "Action_Categories_Steelhead", "Number_Action_Categories_Steelhead",
                               "Action_Categories_BullTrout", "Number_Action_Categories_BullTrout", "Life_Stage_All",
                               "Life_Stage_Spring_Chinook", "Life_Stage_Steelhead", "Life_Stage_Bull_Trout")
    Pathway_Output_x = rbind(Pathway_Output_x, output_row_x)
  }
  
  # ----------------------- combine Reach Information and Pathway processed info for output ----------
  Output_DF = cbind(Reach_Information_HQ_Actions, Pathway_Output_x)
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

# score_1_or_3 = "one"
# restoration_or_protection = 'restoration'

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
  reaches_unique = unique( c(LF_spring_chinook$ReachName, LF_steelhead$ReachName, LF_bull_trout$ReachName) )
  reaches_unique = reaches_unique[order(reaches_unique)]  # just alphabetize 
  
  # ------------ get Reach Information ------
  Reach_Information_LF_Actions = Reach_Information_data %>%  
    filter(ReachName   %in% reaches_unique)
  # --------------- just get important reach information  ----------
  Reach_Information_LF_Actions = Reach_Information_LF_Actions[, c('ReachName','Basin',"Assessment.Unit",  "Spring.Chinook.Reach","Steelhead.Reach" ,"Bull.Trout.Reach" )]
  
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
    spring_chinook_actions = c()
    steelhead_actions = c()
    bull_trout_life_stages = c()
    spring_chinook_life_stages = c()
    steelhead_life_stages = c()
    bull_trout_actions = c()
    all_three_species_actions = c()
    
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
          spring_chinook_actions =    paste(spring_chinook_actions, LF_row_x[life_stage_row_x,actions_column][[1]] , sep=",")
          # ------------------- life stages specifically for spring chinook --------------
          spring_chinook_life_stages =   paste(spring_chinook_life_stages, LF_row_x[life_stage_row_x,"life_stage"][[1]] , sep=",")
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
          steelhead_actions =  paste(steelhead_actions, LF_row_x[life_stage_row_x,actions_column][[1]] , sep=",")
          # ------------------- life stages specifically for spring chinook --------------
          steelhead_life_stages = paste(steelhead_life_stages, LF_row_x[life_stage_row_x,"life_stage"][[1]] , sep=",")
        }
      }
    }
    
    
    # ---------------------- Bull_Trout ----------
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
          bull_trout_actions = paste(bull_trout_actions, LF_row_x[life_stage_row_x,actions_column][[1]], sep=",")
          # ------------------- life stages specifically for spring chinook --------------
          bull_trout_life_stages =  paste(bull_trout_life_stages, LF_row_x[life_stage_row_x,"life_stage"][[1]], sep=",")
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
    if(is.null(life_stages_x)){
      life_stages_x = NA
      number_of_life_stages_x = 0
    }else{
      # ---------------- remove leading comma -------
      life_stages_x = substr(life_stages_x,2,nchar(life_stages_x))
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
    if(is.null(spring_chinook_actions)){
      spring_chinook_actions = NA   # get list of habitat attributes with NO redundancies
      number_of_spring_chinook_actions =  0
    }else{
      # ---------------- remove leading comma -------
      spring_chinook_actions = substr(spring_chinook_actions,2,nchar(spring_chinook_actions))
      # ----------------- get list w/ no redundancies and count them ------------
      spring_chinook_actions = unique( unlist(strsplit(spring_chinook_actions, ",")) )  # get list of action categories with NO redundancies
      number_of_spring_chinook_actions =  length( spring_chinook_actions )
      spring_chinook_actions = paste(spring_chinook_actions, collapse=",")
    }
    
    # -------------- number of life stages -----------
    if(is.null(spring_chinook_life_stages)){
      spring_chinook_life_stages = NA   # get list of habitat attributes with NO redundancies
      number_of_spring_chinook_life_stages =  0
    }else{
      # ---------------- remove leading comma -------
      spring_chinook_life_stages = substr(spring_chinook_life_stages,2,nchar(spring_chinook_life_stages))
      # ----------------- get list w/ no redundancies and count them ------------
      spring_chinook_life_stages = unique(unlist(strsplit(spring_chinook_life_stages, ",")))   # get list of habitat attributes with NO redundancies
      number_of_spring_chinook_life_stages =  length( spring_chinook_life_stages )
      spring_chinook_life_stages = paste(spring_chinook_life_stages, collapse=",")
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
    if(is.null(steelhead_actions)){
      steelhead_actions = NA   # get list of habitat attributes with NO redundancies
      number_of_steelhead_actions =  0
    }else{
      # ---------------- remove leading comma -------
      steelhead_actions = substr(steelhead_actions,2,nchar(steelhead_actions))
      # ----------------- get list w/ no redundancies and count them ------------
      steelhead_actions = unique( unlist(strsplit(steelhead_actions, ",")) )  # get list of action categories with NO redundancies
      number_of_steelhead_actions =  length( steelhead_actions )
      steelhead_actions = paste(steelhead_actions, collapse=",")
    }
    # -------------- number of life stages -----------
    if(is.null(steelhead_life_stages)){
      steelhead_life_stages = NA   # get list of habitat attributes with NO redundancies
      number_of_steelhead_life_stages =  0
    }else{
      # ---------------- remove leading comma -------
      steelhead_life_stages = substr(steelhead_life_stages,2,nchar(steelhead_life_stages))
      # ----------------- get list w/ no redundancies and count them ------------
      steelhead_life_stages = unique(unlist(strsplit(steelhead_life_stages, ",")))   # get list of habitat attributes with NO redundancies
      number_of_steelhead_life_stages =  length( steelhead_life_stages )
      steelhead_life_stages = paste(steelhead_life_stages, collapse=",")
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
    if(is.null(bull_trout_actions)){
      bull_trout_actions = NA   # get list of habitat attributes with NO redundancies
      number_of_bull_trout_actions =  0
    }else{
      # ---------------- remove leading comma -------
      bull_trout_actions = substr(bull_trout_actions,2,nchar(bull_trout_actions))
      # ----------------- get list w/ no redundancies and count them ------------
      bull_trout_actions = unique( unlist(strsplit(bull_trout_actions, ",")) )  # get list of action categories with NO redundancies
      number_of_bull_trout_actions =  length( bull_trout_actions )
      bull_trout_actions = paste(bull_trout_actions, collapse=",")
    }
    # -------------- number of life stages -----------
    if(is.null(bull_trout_life_stages)){
      bull_trout_life_stages = NA   # get list of habitat attributes with NO redundancies
      number_of_bull_trout_life_stages =  0
    }else{
      # ---------------- remove leading comma -------
      bull_trout_life_stages = substr(bull_trout_life_stages,2,nchar(bull_trout_life_stages))
      # ----------------- get list w/ no redundancies and count them ------------
      bull_trout_life_stages = unique(unlist(strsplit(bull_trout_life_stages, ",")))   # get list of habitat attributes with NO redundancies
      number_of_bull_trout_life_stages =  length( bull_trout_life_stages )
      bull_trout_life_stages = paste(bull_trout_life_stages, collapse=",")
    }
    
    # -------------- if all species are present - yes or no -------------
    if(species_count_x == 3){ all_species_present = "yes"
    }else{ all_species_present = "no" }
    
    # -----------------------------------------
    #     combine and output
    # -----------------------------------------
    
    output_row_x = t( as.data.frame(c(spring_chinook_presence,  all_species_present,
                                      pathways_x, number_of_pathways_x,
                                    
                                      habitat_attributes_x,number_of_habitat_attributes_x, 
                                      spring_chinook_habitat_attributes, number_of_spring_chinook_habitat_attributes,
                                      steelhead_habitat_attributes, number_of_steelhead_habitat_attributes,
                                      bull_trout_habitat_attributes, number_of_bull_trout_habitat_attributes,
                                      
                                      action_categories_x, number_of_action_categories_x,
                                      spring_chinook_actions, number_of_spring_chinook_actions,
                                      steelhead_actions, number_of_steelhead_actions,
                                      bull_trout_actions, number_of_bull_trout_actions,
                                      
                                      life_stages_x,number_of_life_stages_x,
                                      spring_chinook_life_stages, number_of_spring_chinook_life_stages,
                                      steelhead_life_stages, number_of_steelhead_life_stages,
                                      bull_trout_life_stages, number_of_bull_trout_life_stages
                                      )  )  )
    colnames(output_row_x) = c("Spring_Chinook_Actions_Present_Yes_No", "SprCh_STLD_BullTr_All_Present_Yes_No",
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
  
  # ----------------------- combine Reach Informatoin and Pathway processed info for output ----------
  Output_DF = cbind(Reach_Information_LF_Actions, Pathway_Output_x)
  rownames(Output_DF) = seq(1, nrow(Output_DF))
  
  # ---------------- remove rows with no pathways --------
  if( length(which(Output_DF$Number_of_Pathways == 0)) > 0  ){
    Output_DF = Output_DF[-which(Output_DF$Number_of_Pathways == 0) , ]
  }
  
  return(Output_DF)
  
}


# ------------------------------------------------------------------------------------------
#
#      Function to combine Habitat Quality and Limiting Factor Outputs
#
# ------------------------------------------------------------------------------------------


# HQ_pathway_df = Habitat_Quality_Restoration_Unacceptable
# LF_pathway_df = Limiting_Factor_Restoration_Unacceptable

columns_info = c( "ReachName" )

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
    if( length(HQ_reach_index ) > 0){
      HQ_and_LF_combo_x = as.data.frame(HQ_pathway_df[HQ_reach_index, columns_info])
    }else{
      HQ_and_LF_combo_x = as.data.frame(LF_pathway_df[LF_reach_index, columns_info])
    }
    
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
      combo_x_output = paste(unique( unlist(strsplit(combo_x_output, ",")) ), collapse=",")  # just get unique 
      combo_x_output = as.data.frame(combo_x_output)
      colnames(combo_x_output) = combo_x
      # ------------ add to previous -------------------
      HQ_and_LF_combo_x = cbind(HQ_and_LF_combo_x,combo_x_output )
    }
    
    # ------------------------------------------------------------
    #     Combine Text - LF only
    # ------------------------------------------------------------
    
    if(length(LF_reach_index ) > 0){
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

# To Test
# HQ_LF_Unacceptable = Restoration_Unacceptable
# HQ_LF_At_Risk = Restoration_At_Risk
# HQ_LF_Both = Restoration_Unacceptable_and_At_Risk
# HQ_Both = Habitat_Quality_Restoration_Unacceptable_and_At_Risk
#  columns_info = c( "ReachName","Basin","Assessment.Unit" ) # columns to automatically add to beginning (left side) of output

FUNCTION_combine_across_Unacceptable_and_AtRisk = function(HQ_LF_Unacceptable, HQ_LF_At_Risk, HQ_LF_Both, HQ_both, columns_info, exclude_bull_trout, HQ_add_life_stage){
  
  # ------------------------------------------------------------
  #       Get Unique Reaches
  # ------------------------------------------------------------

  # ------------------------------------------------------------
  #      Loop through each Reach and combine data
  # ------------------------------------------------------------
  Unacceptable_AtRisk_combined_output = c()
  
  for(reach_x in HQ_LF_Both$ReachName){
    
    # --------------------- generate HQ and LF index ----------
    HQ_LF_Both_index = which(HQ_LF_Both$ReachName == reach_x)
    if(any(HQ_LF_Unacceptable$ReachName == reach_x)){ HQ_LF_Unacceptable_index = which(HQ_LF_Unacceptable$ReachName == reach_x) }
    if(any(HQ_LF_At_Risk$ReachName == reach_x)){ HQ_LF_At_Risk_index = which(HQ_LF_At_Risk$ReachName == reach_x) }
    HQ_Both_index  = which(HQ_Both$ReachName == reach_x) # data frame with just HQ pathway 

    # ----------- IF excluding bull trout and only bull trout pathway - SKIP this ---------
    if(exclude_bull_trout == "yes" & (HQ_LF_Both[HQ_LF_Both_index,"Pathways"] == "HQ_bull_trout" |   HQ_LF_Both[HQ_LF_Both_index,"Pathways"] == "LF_bull_trout" )  ){
      next
    }
    
    # ------------------------------------------------------------
    #     Add Reach Information Data 
    # ------------------------------------------------------------
    HQ_and_LF_combo_x = as.data.frame(HQ_LF_Both[HQ_LF_Both_index, columns_info])
    
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
    
    pathways_x =  unique( unlist(strsplit(HQ_and_LF_combo_x$Pathways, ",")) )
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
    #   Generate Species-specific columns of habitat attributes and actions
    # ------------------------------------------------------------
    
    # ---------- Spring Chinook ----------------
    HQ_and_LF_combo_x$Spring_Chinook_Habitat_Attributes = HQ_LF_Both$Impaired_Habitat_Attributes_SpringChinook[HQ_LF_Both_index]
    HQ_and_LF_combo_x$Spring_Chinook_Actions = HQ_LF_Both$Action_Categories_SpringChinook[HQ_LF_Both_index]
    
    # ---------- Steelhead ----------------
    HQ_and_LF_combo_x$Steelhead_Habitat_Attributes = HQ_LF_Both$Impaired_Habitat_Attributes_Steelhead[HQ_LF_Both_index]
    HQ_and_LF_combo_x$Steelhead_Actions = HQ_LF_Both$Action_Categories_Steelhead[HQ_LF_Both_index]
    
    # ---------- Bull Trout ----------------
    if(exclude_bull_trout == "no"){
      HQ_and_LF_combo_x$Bull_Trout_Habitat_Attributes = HQ_LF_Both$Impaired_Habitat_Attributes_BullTrout[HQ_LF_Both_index]
      HQ_and_LF_combo_x$Bull_Trout_Actions = HQ_LF_Both$Action_Categories_BullTrout[HQ_LF_Both_index]
    }else{
      HQ_and_LF_combo_x$Bull_Trout_Habitat_Attributes = NA
      HQ_and_LF_combo_x$Bull_Trout_Actions = NA
    }
    
    # ------------------------------------------------------------
    #   List all life stages
    # ------------------------------------------------------------
    HQ_and_LF_combo_x$Life_Stages = HQ_LF_Both[HQ_LF_Both_index, "Life_Stages"]
    
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
    if( reach_x == HQ_LF_Unacceptable$ReachName[HQ_LF_Unacceptable_index]   ){
      HQ_and_LF_combo_x$Unacceptable_Habitat_Attributes_Presence = "yes"
    }else{
      HQ_and_LF_combo_x$Unacceptable_Habitat_Attributes_Presence = "no"
    }
    
    # ------------------------------------------------------------
    #  Unacceptable Habitat Attributes
    # ------------------------------------------------------------
    if( reach_x == HQ_LF_Unacceptable$ReachName[HQ_LF_Unacceptable_index]   ){
      HQ_and_LF_combo_x$Unacceptable_Impaired_Habitat_Attributes_All_Species = HQ_LF_Unacceptable$Impaired_Habitat_Attributes_All_Species[HQ_LF_Unacceptable_index] 
    }else{
      HQ_and_LF_combo_x$Unacceptable_Impaired_Habitat_Attributes_All_Species = NA
    }
    
    # ------------------------------------------------------------
    #  Number of Unacceptable Habitat Attributes
    # ------------------------------------------------------------
    if( reach_x == HQ_LF_Unacceptable$ReachName[HQ_LF_Unacceptable_index]   ){
      HQ_and_LF_combo_x$Unacceptable_Number_Impaired_Habitat_Attributes_All_Species = HQ_LF_Unacceptable$Number_Impaired_Habitat_Attributes_All_Species[HQ_LF_Unacceptable_index] 
    }else{
      HQ_and_LF_combo_x$Unacceptable_Number_Impaired_Habitat_Attributes_All_Species = NA
    }
    
    # ------------------------------------------------------------
    #  At Risk Habitat Attributes (Yes/No)
    # ------------------------------------------------------------ 
    if( reach_x == HQ_LF_At_Risk$ReachName[HQ_LF_At_Risk_index]   ){
      HQ_and_LF_combo_x$At_Risk_Habitat_Attributes_Presence = "yes"
    }else{
      HQ_and_LF_combo_x$At_Risk_Habitat_Attributes_Presence = "no"
    }
    
    # ------------------------------------------------------------
    #  At Risk Habitat Attributes
    # ------------------------------------------------------------
    if( reach_x == HQ_LF_At_Risk$ReachName[HQ_LF_At_Risk_index]   ){
      HQ_and_LF_combo_x$At_Risk_Impaired_Habitat_Attributes_All_Species = HQ_LF_At_Risk$Impaired_Habitat_Attributes_All_Species[HQ_LF_At_Risk_index] 
    }else{
      HQ_and_LF_combo_x$At_Risk_Impaired_Habitat_Attributes_All_Species = NA
    }
    
    # ------------------------------------------------------------
    #  Number of At Risk Habitat Attributes
    # ------------------------------------------------------------
    if( reach_x == HQ_LF_At_Risk$ReachName[HQ_LF_At_Risk_index]   ){
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
    #   add HQ life stages (if HQ_add_life_stage is yes)
    # ------------------------------------------------------------
    if(HQ_add_life_stage  == "yes"){
      # ---- pull the reach's row in HQ Pathway data frame ---------
      HQ_Both_row = HQ_Both[HQ_Both_index, ]
      # --------------------- Pathways------------------
      pathways_x_list = unlist(strsplit(HQ_and_LF_combo_x$Pathways, ","))
      pathway_list_letters = substr(pathways_x_list, 1, 2)
      # ------------- unique species ---------
      species_list = substr(pathways_x_list, 4, nchar(pathways_x_list)) # unique species
      
      # ------------ IF Habitat Quality Pathways presence -------------------
      if( any(pathway_list_letters == "HQ") ){
        
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
      if(is.na(HQ_and_LF_combo_x$Life_Stages)){
        HQ_and_LF_combo_x$Life_Stages = life_stages_all_output
        
        # ------- if life stages exist already -----
      }else{
        life_stages_updated = paste(HQ_and_LF_combo_x$Life_Stages,life_stages_all_output, sep="," )
        life_stages_updated = unique(unlist(strsplit(life_stages_updated, ",")))
        life_stages_updated = paste(life_stages_updated, collapse=",")
        HQ_and_LF_combo_x$Life_Stages =  life_stages_updated
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
#      Generate life stages AND species for Fish Barriers
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
  if(any(life_stage_presence_numbers == 1)){
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
  
  # --------- Bull Trout -------- grep
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

FUNCTION_Add_Barrier_Data = function(HQ_LF_Combined,  Barriers_Pathways_Data,  exclude_bull_trout){
  
  # ------------------- get the reaches ------------------
  for(reach_x in unique(Barriers_Pathways_Data$ReachName) ){
    
    HQ_LF_index = which(HQ_LF_Combined$ReachName == reach_x)
    barrier_index = which(Barriers_Pathways_Data$ReachName == reach_x)
    
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
      species_and_life_stages = FUNCTION_output_life_stages_based_on_species_and_life_stage_presence_FISH_BARRIERs(reach_x, exclude_bull_trout)
      HQ_and_LF_combo_x$Species = species_and_life_stages[1]
      HQ_and_LF_combo_x$SprCh_STLD_BullTr_All_Benefit = NA
      HQ_and_LF_combo_x$Spring_Chinook_Benefit = NA
      HQ_and_LF_combo_x$Spring_Chinook_Habitat_Attributes = NA                    
      HQ_and_LF_combo_x$Spring_Chinook_Actions = NA                                       
      HQ_and_LF_combo_x$Steelhead_Habitat_Attributes = NA                               
      HQ_and_LF_combo_x$Steelhead_Actions = NA 
      HQ_and_LF_combo_x$Bull_Trout_Habitat_Attributes = NA                             
      HQ_and_LF_combo_x$Bull_Trout_Actions = NA
      HQ_and_LF_combo_x$Life_Stages = species_and_life_stages[2]
      
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
#   Function to add Barrier Prioritization info to Flat Tables for Web Map
#
# ------------------------------------------------------------

# HQ_LF_Combined = Reach_Habitat_Attribute_Life_Stage__Species_Restoration_Output

FUNCTION_Add_Barrier_Data_to_WebMap_Flat_Tables = function(HQ_LF_Combined, Barriers_Pathways_Data){
  
  
  for(reach_x in unique(Barriers_Pathways_Data$ReachName) ){
    
    HQ_LF_index = which(HQ_LF_Combined$ReachName == reach_x)
    barrier_index = which(Barriers_Pathways_Data$ReachName == reach_x)
    
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
#   Function to A) remove all Bull Trout, B) make Species Name,
#
# ------------------------------------------------------------

# data_frame_x = Restoration_Prioritization_Output_for_WebMap
# colnames_outward_facing_WebMap_ORDER = c("ReachName","RM_Start", "RM_End","Assessment.Unit","Species","Life_Stages","Impaired_Habitat_Attributes_All_Species","Action_Categories_All_Species" )
# colnames_outward_facing_WebMap_UPDATED = c("Reach Name","River Mile - Start", "River Mile - End","Assessment Unit","Species","Priority Life Stages","Limiting Factor","Action Categories" )
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
  
  # ---------------- if "NA" in a life stage - change it to "multiple ----
  data_frame_x$Life_Stages = gsub("NA", "multiple", data_frame_x$Life_Stages )
  
  # ---------------- round river mile to decimal place ----
  data_frame_x$RM_Start = round(data_frame_x$RM_Start, 2)
  data_frame_x$RM_End = round(data_frame_x$RM_End, 2)
  
  # ------------------------------------------------------------
  #   Update References for WebMap: PRCT (%), AND (&), and the "/" as a space
  # ------------------------------------------------------------
  # -------------------- Fines/Embeddedness ------------------
  data_frame_x$Impaired_Habitat_Attributes_All_Species  = gsub("%Fines/Embeddedness", "PRCNT Fines and Embeddedness", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  # -------------------- Pool Quantity and Quality ------------------
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub("PoolQuantity&Quality", "Pool Quantity and Quality", data_frame_x$Impaired_Habitat_Attributes_All_Species )
  # -------------------- Entrainment and Stranding ------------------
  data_frame_x$Impaired_Habitat_Attributes_All_Species = gsub("Entrainment/Stranding", "Entrainment and Stranding", data_frame_x$Impaired_Habitat_Attributes_All_Species )
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
#   Function to update Reach Information
#
# ------------------------------------------------------------



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
  
  
