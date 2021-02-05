




# ---------------------------------------------------------------------------
#
#      SCRIPT: Combine Action Categories across SPECIES and PATHWAYS
#
#      R Project to generate Priority Action Categories Based on Habitat Quality 
#          and Limiting Factor Analysis from Step 2 of RTT Prioritization Process
#
#          Author: Ryan Niemeyer, Upper Columbia Salmon Recovery Board
#          For more information, see https://www.ucsrb.org/prioritization/
#
# ---------------------------------------------------------------------------


score_1_or_3 = 1
restoration_or_protection = 'restoration'

FUNCTION_combine_Habitat_Quality_Action_Categories_PER_REACH = function(score_1_or_3,  restoration_or_protection){
  
  # --------------- establish which rating crieria (1 = unacceptable, 3 = unacceptable and at risk) ------
  if(score_1_or_3 == 1){
    attributes_column = 'unacceptable_1_indiv_habitat_attributes'
    actions_column = "unacceptable_1_action_categories"
  }else if(score_1_or_3 == 3){
    attributes_column = 'unacceptable_and_at_risk_1_3_indiv_habitat_attributes'
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
  reaches_unique = unique(c(HQ_spring_chinook$ReachName, HQ_steelhead$ReachName, HQ_bull_trout$ReachName ))
  
  # ------------ get Reach Information ------
  Reach_Information_HQ_Actions = Reach_Information_data %>%  
    filter(ReachName   %in% reaches_unique)
  # --------------- just get important reach information  ----------
  Reach_Information_HQ_Actions = Reach_Information_HQ_Actions[, c('ReachName','Basin',"Assessment.Unit",  "Spring.Chinook.Reach","Steelhead.Reach" ,"Bull.Trout.Reach" )]
  
  # ------------------------------------------------------------
  #       Loop Through reaches to get information
  # ------------------------------------------------------------
  Pathway_Output_x = c()
  for(reachname_x in Reach_Information_HQ_Actions$ReachName){
    
    # --------------------- initiate cells for each reach ---------------
    pathways_x  = c()
    habitat_attributes_x = c()
    action_categories_x = c()
    
    # ---------------------- Spring_Chinook ----------
    if( any(HQ_spring_chinook$ReachName == reachname_x) ){
      
      # ------------- pull that reach --------------
      HQ_row_x = HQ_spring_chinook[which(HQ_spring_chinook$ReachName == reachname_x),]
      # ---------------- pathway -------------------
      pathways_x = paste(pathways_x, "HQ_spring_chinook", sep=",")
      # ----------------- habitat attributes ------------------
      habitat_attributes_x = paste(habitat_attributes_x, HQ_row_x[,attributes_column], sep=",")
      # ----------------- action categories ------------------
      action_categories_x = paste(action_categories_x, HQ_row_x[,actions_column], sep=",")
      
    }
    
    # ---------------------- Steelhead ----------
    if( any(HQ_steelhead$ReachName == reachname_x) ){
      
      # ------------- pull that reach --------------
      HQ_row_x = HQ_steelhead[which(HQ_steelhead$ReachName == reachname_x),]
      # ---------------- pathway -------------------
      pathways_x = paste(pathways_x, "HQ_steelhead", sep=",")
      # ----------------- habitat attributes ------------------
      habitat_attributes_x = paste(habitat_attributes_x, HQ_row_x[,attributes_column], sep=",")
      # ----------------- action categories ------------------
      action_categories_x = paste(action_categories_x, HQ_row_x[,actions_column], sep=",")
      
    }
   
    
    # ---------------------- Bull_Trout ----------
    if( any(HQ_bull_trout$ReachName == reachname_x) ){
      
      # ------------- pull that reach --------------
      HQ_row_x = HQ_bull_trout[which(HQ_bull_trout$ReachName == reachname_x),]
      # ---------------- pathway -------------------
      pathways_x = paste(pathways_x, "HQ_bull_trout", sep=",")
      # ----------------- habitat attributes ------------------
      habitat_attributes_x = paste(habitat_attributes_x, HQ_row_x[,attributes_column], sep=",")
      # ----------------- action categories ------------------
      action_categories_x = paste(action_categories_x, HQ_row_x[,actions_column], sep=",")
      
    }
    
    # -------------------------- remove leading comma -----------------
    pathways_x = substr(pathways_x,2,nchar(pathways_x))
    habitat_attributes_x = substr(habitat_attributes_x,2,nchar(habitat_attributes_x))
    action_categories_x = substr(action_categories_x,2,nchar(action_categories_x))
    
    # ---------- prep the entire row ---------------
    number_of_pathways_x =  length( unlist(strsplit(pathways_x, ",")) )
    number_of_habitat_attributes_x =  length( unlist(strsplit(habitat_attributes_x, ",")) )
    number_of_action_categories_x =  length( unlist(strsplit(action_categories_x, ",")) )
    
    output_row_x = t( as.data.frame(c(pathways_x, number_of_pathways_x,
                     habitat_attributes_x,number_of_habitat_attributes_x,
                     action_categories_x, number_of_action_categories_x )) )
    colnames(output_row_x) = c("Pathways","Number_of_Pathways", "Impaired_Habitat_Attributes", "Number_Impaired_Habitat_Attributes",
                               "Action_Categories", "Number_Action_Categories")
    Pathway_Output_x = rbind(Pathway_Output_x, output_row_x)
  }
  
  # ----------------------- combine Reach Informatoin and Pathway processed info for output ----------
  Output_DF = cbind(Reach_Information_HQ_Actions, Pathway_Output_x)
  rownames(Output_DF) = seq(1, nrow(Output_DF))
  
  return(Output_DF)
  
}





FUNCTION_combine_Limiting_Factor_Action_Categories_PER_REACH = function(score_1_or_3,  restoration_or_protection){
  
  # --------------- establish which rating crieria (1 = unacceptable, 3 = unacceptable and at risk) ------
  if(score_1_or_3 == 1){
    attributes_column = 'unacceptable_1_indiv_habitat_attributes'
    actions_column = "unacceptable_1_action_categories"
  }else if(score_1_or_3 == 3){
    attributes_column = 'unacceptable_and_at_risk_1_3_indiv_habitat_attributes'
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
  reaches_unique = unique(c(LF_spring_chinook$ReachName, LF_steelhead$ReachName, LF_bull_trout$ReachName ))
  
  # ------------ get Reach Information ------
  Reach_Information_LF_Actions = Reach_Information_data %>%  
    filter(ReachName   %in% reaches_unique)
  # --------------- just get important reach information  ----------
  Reach_Information_LF_Actions = Reach_Information_LF_Actions[, c('ReachName','Basin',"Assessment.Unit",  "Spring.Chinook.Reach","Steelhead.Reach" ,"Bull.Trout.Reach" )]
  
  # ------------------------------------------------------------
  #       Loop Through reaches to get information
  # ------------------------------------------------------------
  Pathway_Output_x = c()
  for(reachname_x in Reach_Information_LF_Actions$ReachName){
    
    # --------------------- initiate cells for each reach ---------------
    pathways_x  = c()
    habitat_attributes_x = c()
    action_categories_x = c()
    
    # ---------------------- Spring_Chinook ----------
    if( any(LF_spring_chinook$ReachName == reachname_x) ){
      
      # ------------- pull that reach --------------
      LF_row_x = LF_spring_chinook[which(LF_spring_chinook$ReachName == reachname_x),]
      # ---------------- pathway -------------------
      pathways_x = paste(pathways_x, "LF_spring_chinook", sep=",")
      # ----------------- habitat attributes ------------------
      habitat_attributes_x = paste(habitat_attributes_x, LF_row_x[,attributes_column], sep=",")
      # ----------------- action categories ------------------
      action_categories_x = paste(action_categories_x, LF_row_x[,actions_column], sep=",")
      
    }
    
    # ---------------------- Steelhead ----------
    if( any(LF_steelhead$ReachName == reachname_x) ){
      
      # ------------- pull that reach --------------
      LF_row_x = LF_steelhead[which(LF_steelhead$ReachName == reachname_x),]
      # ---------------- pathway -------------------
      pathways_x = paste(pathways_x, "LF_steelhead", sep=",")
      # ----------------- habitat attributes ------------------
      habitat_attributes_x = paste(habitat_attributes_x, LF_row_x[,attributes_column], sep=",")
      # ----------------- action categories ------------------
      action_categories_x = paste(action_categories_x, LF_row_x[,actions_column], sep=",")
      
    }
    
    
    # ---------------------- Bull_Trout ----------
    if( any(LF_bull_trout$ReachName == reachname_x) ){
      
      # ------------- pull that reach --------------
      LF_row_x = LF_bull_trout[which(LF_bull_trout$ReachName == reachname_x),]
      # ---------------- pathway -------------------
      pathways_x = paste(pathways_x, "LF_bull_trout", sep=",")
      # ----------------- habitat attributes ------------------
      habitat_attributes_x = paste(habitat_attributes_x, LF_row_x[,attributes_column], sep=",")
      # ----------------- action categories ------------------
      action_categories_x = paste(action_categories_x, LF_row_x[,actions_column], sep=",")
      
    }
    
    # -------------------------- remove leading comma -----------------
    pathways_x = substr(pathways_x,2,nchar(pathways_x))
    habitat_attributes_x = substr(habitat_attributes_x,2,nchar(habitat_attributes_x))
    habitat_attributes_x = paste(habitat_attributes_x, collapse=",")
    action_categories_x = substr(action_categories_x,2,nchar(action_categories_x))
    action_categories_x = paste(action_categories_x, collapse=",")
    
    # ---------- prep the entire row ---------------
    number_of_pathways_x =  length( unlist(strsplit(pathways_x, ",")) )
    number_of_habitat_attributes_x =  length( unlist(strsplit(habitat_attributes_x, ",")) )
    number_of_action_categories_x =  length( unlist(strsplit(action_categories_x, ",")) )
    
    output_row_x = t( as.data.frame(c(pathways_x, number_of_pathways_x,
                                      habitat_attributes_x,number_of_habitat_attributes_x,
                                      action_categories_x, number_of_action_categories_x )) )
    colnames(output_row_x) = c("Pathways","Number_of_Pathways", "Impaired_Habitat_Attributes", "Number_Impaired_Habitat_Attributes",
                               "Action_Categories", "Number_Action_Categories")
    Pathway_Output_x = rbind(Pathway_Output_x, output_row_x)
  }
  
  # ----------------------- combine Reach Informatoin and Pathway processed info for output ----------
  Output_DF = cbind(Reach_Information_LF_Actions, Pathway_Output_x)
  rownames(Output_DF) = seq(1, nrow(Output_DF))
  
  return(Output_DF)
  
}




HQ_pathway_df = Habitat_Quality_Restoration_Unacceptable
LF_pathway_df = Limiting_Factor_Restoration_Unacceptable

FUNCTION_combine_across_pathways = function(HQ_pathway_df, LF_pathway_df){
  
  
  # ------------------------------------------------------------
  #       Initiate data frame
  # ------------------------------------------------------------
  reaches_unique = unique(c(HQ_pathway_df$ReachName, LF_pathway_df$ReachName ))
  
  # ------------ get Reach Information ------
  Reach_Information_ALL_Actions = Reach_Information_data %>%  
    filter(ReachName   %in% reaches_unique)
  # --------------- just get important reach information  ----------
  Reach_Information_ALL_Actions = Reach_Information_ALL_Actions[, c('ReachName','Basin',"Assessment.Unit",  "Spring.Chinook.Reach","Steelhead.Reach" ,"Bull.Trout.Reach" )]
  
  # ------------------------------------------------------------
  #       Loop Through reaches to get information
  # ------------------------------------------------------------
  Pathway_Output_x = c()
  for(reachname_x in Reach_Information_ALL_Actions$ReachName){
    
    # --------------------- initiate cells for each reach ---------------
    pathways_x  = c()
    habitat_attributes_x = c()
    action_categories_x = c()
    
    # ---------------------- Habitat Quality ----------
    if( any(HQ_pathway_df$ReachName == reachname_x) ){
      
      # ------------- pull that reach --------------
      HQ_row_x = HQ_pathway_df[which(HQ_pathway_df$ReachName == reachname_x),]
      # ---------------- pathway -------------------
      pathways_x = paste(pathways_x, HQ_row_x$Pathways, sep=",")
      # ----------------- habitat attributes ------------------
      habitat_attributes_x = paste(habitat_attributes_x, HQ_row_x$Impaired_Habitat_Attributes, sep=",")
      # ----------------- action categories ------------------
      action_categories_x = paste(action_categories_x, HQ_row_x$Action_Categories, sep=",")
      
    }
    
    # ---------------------- Limiting Factor ----------
    if( any(LF_pathway_df$ReachName == reachname_x) ){
      
      # ------------- pull that reach --------------
      LF_row_x = LF_pathway_df[which(LF_pathway_df$ReachName == reachname_x),]
      # ---------------- pathway -------------------
      pathways_x = paste(pathways_x, LF_row_x$Pathways, sep=",")
      # ----------------- habitat attributes ------------------
      habitat_attributes_x = paste(habitat_attributes_x, LF_row_x$Impaired_Habitat_Attributes, sep=",")
      # ----------------- action categories ------------------
      action_categories_x = paste(action_categories_x, LF_row_x$Action_Categories, sep=",")
      
    }
    
    
    # -------------------------- remove leading comma -----------------
    pathways_x = substr(pathways_x,2,nchar(pathways_x))
    habitat_attributes_x = substr(habitat_attributes_x,2,nchar(habitat_attributes_x))
    habitat_attributes_x = paste(habitat_attributes_x, collapse=",")
    action_categories_x = substr(action_categories_x,2,nchar(action_categories_x))
    action_categories_x = paste(action_categories_x, collapse=",")
    
    # ---------- prep the entire row ---------------
    number_of_pathways_x =  length( unlist(strsplit(pathways_x, ",")) )
    number_of_habitat_attributes_x =  length( unique(unlist(strsplit(habitat_attributes_x, ",")) ))
    number_of_action_categories_x =  length( unique(unlist(strsplit(action_categories_x, ",")) ))
    
    output_row_x = t( as.data.frame(c(pathways_x, number_of_pathways_x,
                                      habitat_attributes_x,number_of_habitat_attributes_x,
                                      action_categories_x, number_of_action_categories_x )) )
    colnames(output_row_x) = c("Pathways","Number_of_Pathways", "Impaired_Habitat_Attributes", "Number_Impaired_Habitat_Attributes",
                               "Action_Categories", "Number_Action_Categories")
    Pathway_Output_x = rbind(Pathway_Output_x, output_row_x)
  }
  
  # ----------------------- combine Reach Informatoin and Pathway processed info for output ----------
  Output_DF = cbind(Reach_Information_ALL_Actions, Pathway_Output_x)
  rownames(Output_DF) = seq(1, nrow(Output_DF))
  
  return(Output_DF)
  
  
}






