
# ---------------------------------------------------------------------------------------------------
#
#   Pull together AU layer data for WebMap Pop up
#
# ---------------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------
#              Establish AU layer
# ---------------------------------------------------------------------------------------------------

# ---------------- first, establish with existing reaches --------
pop_up_AU_information = Life_Stage_Priorities_AU_only_data_v2[,c("Assessment Unit","Basin")]

# -------------------second, add the additional Okanogan AU names ------------
i = 0
for(AU_x in Okanogan_AU_not_in_EDT_interface$`Assessment Unit` ){
  i = i + 1
  if(any(pop_up_AU_information$`Assessment Unit` == AU_x) == FALSE){
    pop_up_AU_information = rbind(pop_up_AU_information, Okanogan_AU_not_in_EDT_interface[i,])
  }
}


# ---------------------------------------------------------------------------------------------------
#             Pull in Scoring criteria for each species
# ---------------------------------------------------------------------------------------------------

scoring_Criteria_list = c("Intrinsic potential" ,"Spawning Areas", "Life Stages","Spawner abundance","Water Quality" ,           
              "Road Density" , "Canopy Cover","Perennial Flow","Barrier Density",          
               "Connectivity","Geomean (Habitat Quality)","Habitat Quality Score","Watershed Degradation",
              "Land stewardship","Future Stream Temperature","Hydrologic Regime Shift","Summer Low Flow",          
             "Flood Events","Flow Geomean","Climate change","Non-native fish species (BT Only)" ,"Weighted cumulative score")


# ---------------- Loop through the scoring criteria --------------
for(criteria_x in scoring_Criteria_list){
  print(criteria_x)
  criteria_column_x = c()

  for(AUx in pop_up_AU_information$`Assessment Unit`){
    #print(AUx)
    # -------------- if assessment unit in scoring criteria -----------
    if( any(Step1_Scores_Spteelhead$`Assessment unit` == AUx) ){
      # --------------------- Spring Chinook ---------------------
      spring_chinook_criteria_x = Step1_Scores_Spring_Chinook[which(Step1_Scores_Spring_Chinook$`Assessment unit` == AUx), criteria_x]
      if( !is.na(spring_chinook_criteria_x) ){
        if( spring_chinook_criteria_x%%1!=0 ){ spring_chinook_criteria_x = round(spring_chinook_criteria_x, digits=1) }
      }else{
        spring_chinook_criteria_x = "NA"
      }
      
      # --------------------- Steelhead ---------------------
      steelhead_criteria_x = Step1_Scores_Spteelhead[which(Step1_Scores_Spteelhead$`Assessment unit` == AUx), criteria_x]
      if(!is.na(steelhead_criteria_x)){
        if(steelhead_criteria_x%%1!=0){ steelhead_criteria_x = round(steelhead_criteria_x, digits=1) }
      }else{
        steelhead_criteria_x = "NA"
      }
      
      # --------------------- Bull Trout ---------------------
      bull_trout_criteria_x = Step1_Scores_Bull_Trout[which(Step1_Scores_Bull_Trout$`Assessment unit` == AUx), criteria_x]
      if(!is.na(bull_trout_criteria_x)){
        if(bull_trout_criteria_x%%1!=0){ bull_trout_criteria_x = round(bull_trout_criteria_x, digits=1) }
      }else{
        bull_trout_criteria_x = "NA"
      }
      
      # -------------- put together -------------
      if(criteria_x == "Non-native fish species (BT Only)"){
        criteria_cell_x = bull_trout_criteria_x
      }else if( spring_chinook_criteria_x == steelhead_criteria_x & spring_chinook_criteria_x == bull_trout_criteria_x ){
        criteria_cell_x = paste(spring_chinook_criteria_x, " (all species)", sep="")
      }else{
        criteria_cell_x = paste("SpCh:", spring_chinook_criteria_x, ",Stld:", steelhead_criteria_x, ",BuTr:", bull_trout_criteria_x, sep="" )
      }
      
    }else{
      if(criteria_x == "Non-native fish species (BT Only)"){
        criteria_cell_x = "NA" 
      }else{
        criteria_cell_x = "NA (all species)" 
      }
      
    }
    criteria_cell_x = as.data.frame(criteria_cell_x)
    if( is.null( colnames(criteria_column_x) ) == FALSE  ){
      colnames(criteria_cell_x) = colnames(criteria_column_x)
    }
    criteria_column_x = rbind(criteria_column_x, criteria_cell_x)
  }
  
  # -------------- add criteria column to data frame ----------
  pop_up_AU_information[criteria_x] = criteria_column_x
  
}

# ---------------------------------------------------------------------------------------------------
#             Pull in Life Stage prioritization for each species
# ---------------------------------------------------------------------------------------------------
life_stages_sprch_stld = c("Adult Migration", "Holding",  "Spawning", "Fry Colonization", 
                             "Summer Rearing",  "Winter Rearing", "Smolt Emigration")
life_stages_sprch_stld_priorites = c("Adult Migration" , "Holding and Maturation", "Spawning and Incubation", "Fry Colonization", 
                                     "Summer Rearing" ,"Winter Rearing","Smolt Outmigration"   )
life_stages_bull_trout = c("Adult Migration", "Holding",  "Spawning",
                             "Natal Rearing",  "Adult Non Spawning",  "Subadult Rearing" )
life_stages_bull_trout_priorities = c("Adult Migration" ,"Holding and Maturation","Spawning and Incubation",
                                      "BT Natal Rearing", "Adult Non-Spawning" ,"BT Subadult Rearing" ) 



# Life_Stage_Priorities_AU_and_Reach_data_AU_Only[[spring_chinook_life_stages[["Adult Migration"]]]]
  

# ---------------- Spring Chinook --------------
i = 0
for(life_stage_x in life_stages_sprch_stld){
  i = i + 1
  life_stage_column_x = c()
  
  for(AUx in pop_up_AU_information$`Assessment Unit`){
    
    # -------------- if assessment unit in scoring criteria -----------
    if( any(Life_Stage_Priorities_AU_and_Reach_data_AU_Only$`Assessment Unit` == AUx) ){
      # --------------------- Spring Chinook ---------------------
      spring_chinook_life_stage_x = Life_Stage_Priorities_AU_and_Reach_data_AU_Only[[spring_chinook_life_stages[[life_stages_sprch_stld_priorites[i]]]]][which(Life_Stage_Priorities_AU_and_Reach_data_AU_Only$`Assessment Unit` == AUx)]

    }else{
      spring_chinook_life_stage_x = "Life Stage Not Supported" 
    }
    spring_chinook_life_stage_x = as.data.frame(spring_chinook_life_stage_x)
    life_stage_column_x = rbind(life_stage_column_x, spring_chinook_life_stage_x)
  }
  
  # -------------- add criteria column to data frame ----------
  life_stage_column_name_x = c(paste("Spring_Chinook",life_stage_x,sep="_"))
  pop_up_AU_information[life_stage_column_name_x] = life_stage_column_x
}

# ---------------- Steelhead --------------
i = 0
for(life_stage_x in life_stages_sprch_stld){
  i = i + 1
  life_stage_column_x = c()
  
  for(AUx in pop_up_AU_information$`Assessment Unit`){
    
    # -------------- if assessment unit in scoring criteria -----------
    if( any(Life_Stage_Priorities_AU_and_Reach_data_AU_Only$`Assessment Unit` == AUx) ){
      # --------------------- Spring Chinook ---------------------
      steelhead_life_stage_x = Life_Stage_Priorities_AU_and_Reach_data_AU_Only[[steelhead_life_stages[[life_stages_sprch_stld_priorites[i]]]]][which(Life_Stage_Priorities_AU_and_Reach_data_AU_Only$`Assessment Unit` == AUx)]
      
    }else{
      steelhead_life_stage_x = "Life Stage Not Supported" 
    }
    steelhead_life_stage_x = as.data.frame(steelhead_life_stage_x)
    life_stage_column_x = rbind(life_stage_column_x, steelhead_life_stage_x)
  }
  
  # -------------- add criteria column to data frame ----------
  life_stage_column_name_x = c(paste("Steelhead",life_stage_x,sep="_"))
  pop_up_AU_information[life_stage_column_name_x] = life_stage_column_x
}

# ---------------- Bull Trout --------------
i = 0
for(life_stage_x in life_stages_bull_trout){
  i = i + 1
  life_stage_column_x = c()
  
  for(AUx in pop_up_AU_information$`Assessment Unit`){
    
    # -------------- if assessment unit in scoring criteria -----------
    if( any(Life_Stage_Priorities_AU_and_Reach_data_AU_Only$`Assessment Unit` == AUx) ){
      # --------------------- Spring Chinook ---------------------
      bull_trout_life_stage_x = Life_Stage_Priorities_AU_and_Reach_data_AU_Only[[bull_trout_life_stages[[life_stages_bull_trout_priorities[i]]]]][which(Life_Stage_Priorities_AU_and_Reach_data_AU_Only$`Assessment Unit` == AUx)]
      
    }else{
      bull_trout_life_stage_x = "Life Stage Not Supported" 
    }
    bull_trout_life_stage_x = as.data.frame(bull_trout_life_stage_x)
    life_stage_column_x = rbind(life_stage_column_x, bull_trout_life_stage_x)
  }
  
  # -------------- add criteria column to data frame ----------
  life_stage_column_name_x = c(paste("Bull_Trout",life_stage_x,sep="_"))
  pop_up_AU_information[life_stage_column_name_x] = life_stage_column_x
}


# --------------------------- Output ------------------
output_path_x =  paste(output_path,'AU_pop_up_information_for_WebMap.xlsx', sep="")
write_xlsx(pop_up_AU_information,output_path_x )


