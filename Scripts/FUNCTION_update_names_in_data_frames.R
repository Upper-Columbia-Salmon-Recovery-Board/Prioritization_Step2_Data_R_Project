
# -------------------------------------------------------------------
#
#   R Script to take old reach names and update with new Reach Names
#
# -------------------------------------------------------------------


# -------------------------------------------------------------------
#
#   Read in Crosswalk 
#
# -------------------------------------------------------------------

Okanogan_Reach_Crosswalk =  read_excel(  paste(Okanogan_EDT_path,'Okanogan_AU_Reach_Crosswalk.xlsx', sep="")  )

Crosswalk_to_use = Okanogan_Reach_Crosswalk

# ------------------------------------------------------------------------------------------------------------
#
#       Function to update reach names 
#
# ------------------------------------------------------------------------------------------------------------

# ------------- to test it out -------------
#updated_column_old = Okanogan_Crosswalk$ReachName_Old
#update_column_new = Okanogan_Crosswalk$ReachName_New
input_data_frame = habitat_raw_data
input_column = as.data.frame(habitat_raw_data[,"ReachName"])
#input_column = input_column[,1]
input_column_name = "ReachName"
new_and_old_data_frame = Crosswalk_to_use[,c("ReachName_Old", "ReachName_New")]
output_path_x = paste(Okanogan_EDT_path,'Habitat_Data_Raw.xlsx', sep="")

# -------------- the Function -----------------
FUNCTION_read_in_and_update_file_reach_names = function(input_data_frame, input_column, input_column_name, new_and_old_data_frame, output_path_x){
  
  # ------ which reaches have a single name for each --------------
  one_to_one_crossover = which(Okanogan_Crosswalk$identical_reaches_RTT_Okanogan == "yes" |
                                 Okanogan_Crosswalk$`Multiple_RTT_Reaches?` == "yes")
  # ---------------- which reaches have multiple Okanogan names for single RTT reach ----------
  duplicate_RTT_names =  which(Okanogan_Crosswalk$`Multiple_Okanogan_Reaches?` == "yes")
  
  # -----------------------------------------------
  #   Update names with direct 1:1 crossover
  # -----------------------------------------------
  
  # --------------- update names in column -----------------
  for(i in 1:nrow(new_and_old_data_frame)){
    if( any(one_to_one_crossover == i)  ){
      
      x_replace = which(input_column[,1] == as.character(new_and_old_data_frame[i,1]) )
      input_column[x_replace,1] = new_and_old_data_frame[i,2]
      
    }else if( any(duplicate_RTT_names == i) ){
      
      
    }

  }
  
  # ------------------ update column in data frame ------------
  input_data_frame[,input_column_name] = input_column
  
  # -----------------------------------------------
  #   Update names with 
  # -----------------------------------------------
  
  
  # ----------------- write excel ------------
  write_xlsx(input_data_frame,output_path_x )
  
}

# -------------------------------------------------------------------
#    Read and Write Data - FIRST time updating
# -------------------------------------------------------------------


# ---------------- ATLAS data ------------

path_x = paste('C:/Users/Ryan/Documents/2_Habitat_Prioritzation/Prioritization/Data/','ATLAS_flowIMPORT_Habitat_Data_Raw_032921.xlsx', sep="")
path_x2 = paste('C:/Users/Ryan/Documents/2_Habitat_Prioritzation/Prioritization/Data/','ATLAS_flowIMPORT_Habitat_Data_Raw_032921_updated_names.xlsx', sep="")
ATLAS_flow_data = read_excel( path_x )

FUNCTION_read_in_and_update_file_reach_names( ATLAS_flow_data, as.data.frame(ATLAS_flow_data[,"ReachName"]), "ReachName", 
                                              Crosswalk_to_use[,c("ReachName_Old", "ReachName_New")], 
                                              path_x2  )

# --------------- Habitat_Data_Raw ---------
FUNCTION_read_in_and_update_file_reach_names(habitat_raw_data, as.data.frame(habitat_raw_data[,"ReachName"]), "ReachName", 
                                             Crosswalk_to_use[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'Habitat_Data_Raw.xlsx', sep="") )

# --------------- Channel_Unit_Raw ---------
FUNCTION_read_in_and_update_file_reach_names(Channel_Unit_Raw, as.data.frame(Channel_Unit_Raw[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'Channel_Unit_Raw.xlsx', sep="")  )

# ----------------------- Confinement_Scores ----------------------
FUNCTION_read_in_and_update_file_reach_names(Confinement_Scores, 
                                             as.data.frame(Confinement_Scores[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'Confinement_Scores.xlsx', sep="")  )



# ----------------------- Habitat_Attribute_Notes_and_Professional_Judgement ----------------------
FUNCTION_read_in_and_update_file_reach_names(Habitat_Attribute_Notes_and_Professional_Judgement, 
                                             as.data.frame(Habitat_Attribute_Notes_and_Professional_Judgement[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'Habitat_Attribute_Notes_and_Professional_Judgement.xlsx', sep="")  )


# ----------------------- Life_Stage_Priorities_AU_and_Reach_data ----------------------
FUNCTION_read_in_and_update_file_reach_names(Life_Stage_Priorities_AU_and_Reach_data, 
                                             as.data.frame(Life_Stage_Priorities_AU_and_Reach_data[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(ranking_data_path,'LifeStagePriorities_AUandReach.xlsx', sep="")  )

# ------------------- Reach_Information_data ------------------
FUNCTION_read_in_and_update_file_reach_names(Reach_Information_data, 
                                             as.data.frame(Reach_Information_data[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'ReachInfo.xlsx', sep="")  )




# ------------------------------------------------------------------------------------------------------------
#
#        the Function to update data frames that have already been updated
#
# ------------------------------------------------------------------------------------------------------------



input_data_frame = Habitat_Attribute_Notes_and_Professional_Judgement
input_column = as.data.frame(Habitat_Attribute_Notes_and_Professional_Judgement[,"ReachName"])
input_column_name = "ReachName"
new_and_old_data_frame = Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")]
output_path_x = paste(habitat_data_path,'Habitat_Attribute_Notes_and_Professional_Judgement.xlsx', sep="") 


FUNCTION_read_in_and_update_file_reach_names_already_updated_once = function(input_data_frame, input_column, input_column_name, new_and_old_data_frame, output_path_x){
  
  # ------ which reaches have a single name for each --------------
  one_to_one_crossover = which(Crosswalk_to_use$identical_reaches_RTT_Okanogan == "yes" |
                                 Crosswalk_to_use$`Multiple_RTT_Reaches?` == "yes")
  # ---------------- which reaches have multiple Okanogan names for single RTT reach ----------
  duplicate_RTT_names =  which(Crosswalk_to_use$`Multiple_Okanogan_Reaches?` == "yes")
  
  # -----------------------------------------------
  #   Update names with direct 1:1 crossover
  # -----------------------------------------------
  
  # --------------- update names in column -----------------
  for(i in 1:nrow(Crosswalk_to_use)){
    
    # ------- if name is already been updated or is same ---------
    if( any(input_column$ReachName == Crosswalk_to_use$ReachName_New[i])   ){
      
      # NO need to replace anything, because no need
      
    # ----------- if new name for reach ----------
    }else{
      
    # ------------- IF one to one between RTT and Okanogan reach name (replace directly) -------
      if(  any(one_to_one_crossover == i)  ){
        
        # -------------- which reach to replace --------------
        x_replace = which(input_column$ReachName == Crosswalk_to_use$ReachName_Old[i])
        # ----------- replace it ------------
        input_column[x_replace,1] = Crosswalk_to_use$ReachName_New[i]
        
    # --------------- IF multiple Okanogan reaches for a single RTT reach ------------  
      }else if(   any(duplicate_RTT_names == i)    ){
        
        # ---------------  which RTT reach name is in the overlap -----
        reach_row_x = unlist(strsplit(Crosswalk_to_use$ReachName_New[i], ","))
        first_row_x = TRUE
        # ------ which row has duplicate Okanogan reaches -----------
        row_with_duplicate_i = which(input_data_frame$ReachName == Crosswalk_to_use$ReachName_Old[i])
        
        for(reach_x_i in reach_row_x){
          
          # ------ if first instance of reach ---------
          if(first_row_x){
            # -------------- which reach to replace --------------
            x_replace = which(input_column$ReachName == Crosswalk_to_use$ReachName_Old[i])
            # ----------- replace it ------------
            input_column[x_replace,1] = reach_x_i
            
            # ------ set as not first row --------
            first_row_x = FALSE
            
            
          # ---------- if NOT first instance of reach ----------
          }else{
            
            # -------------- which reach to replace --------------
            reach_x_i2 = as.data.frame(reach_x_i)
            colnames(reach_x_i2) = colnames(input_column)
            # ----------- add column at the bottom ------------
            input_column = rbind(input_column, reach_x_i2 )
            # ----------------- add new row --------
            new_row_x = input_data_frame[row_with_duplicate_i, ]
            new_row_x$ReachName = reach_x_i
            input_data_frame = rbind(input_data_frame , new_row_x)
          }
        } # end of multiple reaches loop
        
      } # end of if multiple reaches if/else
    } # end of if new name
  }
  
  # ------------------ update column in data frame ------------
  input_data_frame[,input_column_name] = input_column
  
  # ------------ redo order --------------
  input_data_frame  = input_data_frame[order(input_data_frame$ReachName),]
  
  # -----------------------------------------------
  #   Update names with 
  # -----------------------------------------------
  
  
  # ----------------- write excel ------------
  write_xlsx(input_data_frame,output_path_x )
  
}

# -------------------------------------------------------------------
#    Read and Write Data - FIRST time updating
# -------------------------------------------------------------------

# --------------- Habitat_Data_Raw ---------
FUNCTION_read_in_and_update_file_reach_names_already_updated_once(habitat_raw_data, as.data.frame(habitat_raw_data[,"ReachName"]), "ReachName", 
                                             Crosswalk_to_use[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'Habitat_Data_Raw.xlsx', sep="") )

# --------------- Channel_Unit_Raw ---------
FUNCTION_read_in_and_update_file_reach_names_already_updated_once(Channel_Unit_Raw, as.data.frame(Channel_Unit_Raw[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'Channel_Unit_Raw.xlsx', sep="")  )

# ----------------------- Confinement_Scores ----------------------
FUNCTION_read_in_and_update_file_reach_names_already_updated_once(Confinement_Scores, 
                                             as.data.frame(Confinement_Scores[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'Confinement_Scores.xlsx', sep="")  )



# ----------------------- Habitat_Attribute_Notes_and_Professional_Judgement ----------------------
FUNCTION_read_in_and_update_file_reach_names_already_updated_once(Habitat_Attribute_Notes_and_Professional_Judgement, 
                                             as.data.frame(Habitat_Attribute_Notes_and_Professional_Judgement[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'Habitat_Attribute_Notes_and_Professional_Judgement.xlsx', sep="")  )


# ----------------------- Life_Stage_Priorities_AU_and_Reach_data ----------------------
FUNCTION_read_in_and_update_file_reach_names_already_updated_once(Life_Stage_Priorities_AU_and_Reach_data, 
                                             as.data.frame(Life_Stage_Priorities_AU_and_Reach_data[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(ranking_data_path,'LifeStagePriorities_AUandReach.xlsx', sep="")  )

# ------------------- Reach_Information_data ------------------
FUNCTION_read_in_and_update_file_reach_names_already_updated_once(Reach_Information_data, 
                                             as.data.frame(Reach_Information_data[,"ReachName"]), "ReachName", 
                                             Okanogan_Crosswalk[,c("ReachName_Old", "ReachName_New")], 
                                             paste(habitat_data_path,'ReachInfo.xlsx', sep="")  )

# --------------- Protected_Percentage_Data ---------
FUNCTION_read_in_and_update_file_reach_names_already_updated_once(Protected_Percentage_Data, as.data.frame(Protected_Percentage_Data[,"ReachName"]), "ReachName", 
                                                                  Crosswalk_to_use[,c("ReachName_Old", "ReachName_New")], 
                                                                  paste(habitat_data_path,'Protected_Percentage_Data_new.xlsx', sep="") )




# -------------------------------------------------------------------
#
#   Read in new Life Stage Info - and update Life Stage Presence
#
# -------------------------------------------------------------------

life_stage_priority__path = paste(ranking_data_path,'LifeStagePriorities_AUandReach.xlsx', sep="")
 

file_path = "C:/Users/Ryan/Documents/2_Habitat_Prioritzation/Prioritization/Data/OkaLifeStages_May17.xlsx"
Life_Stage_Update_Names = read_excel(file_path) 


# -------------------------------------------------------
#    Update Life Stage in Crosswalk
# -------------------------------------------------------
Life_Stage_Update_Names$RTT_Life_Stage_Updated = NA
i = 0
for(life_stage_x in LifeStageCrosswalk_EDT$`EDT Life Stage`){
  i = i + 1
  # ------------------- find the life stage ---------------
  x = which(Life_Stage_Update_Names$`EDT Life Stage` == life_stage_x)
  # -------------------- update RTT names --------------------
  Life_Stage_Update_Names$RTT_Life_Stage_Updated[x] = LifeStageCrosswalk_EDT$`RTT Life Stage`[i]
}


# -------------------------------------------------------
#    Update Life Stage Presence and Sum
# -------------------------------------------------------

# -------------------- pull habitat attributes/life stages JUST for this species ---------
species = "Steelhead"
Attribute_LifeStage_Crosswalk_Life_Stage = Attribute_LifeStage_Crosswalk %>%
  filter(Species  %in% species  )

# -------------------------- get list of life stages --------------------
life_stages = unique(Attribute_LifeStage_Crosswalk_Life_Stage$'Life Stage')


# --------------- loop to update life stage presence data in Okanogan------------
for(reach_x in Life_Stage_Priorities_AU_and_Reach_data$ReachName){
  
  # ------------ pull prioritization life stage info in reach ------------
  Life_Stage_Priorities_AU_and_Reach_data_REACH_X = Life_Stage_Priorities_AU_and_Reach_data[which(Life_Stage_Priorities_AU_and_Reach_data$ReachName == reach_x),]
  
  # ------------------- only update if in Okanogan -------------
  if(Life_Stage_Priorities_AU_and_Reach_data_REACH_X$Basin == "Okanogan"){
    
    # ------------- pull all EDT data for this reach -----------
    life_stages_in_reach = Life_Stage_Update_Names[which(Life_Stage_Update_Names$Reach == reach_x),]
    
    # ---------------------- loop through life stages -----------
    for(life_stage_x in life_stages){
      
      # ----------------------- pull if life stage present in EDT -----------
      x = which(life_stages_in_reach$RTT_Life_Stage_Updated == life_stage_x)
      
      # ------------ if any ranks for this life stage - update ---------
      if(length(x) > 0){
        score_x = 1
      }else{
        score_x = 0
      }
      
      Life_Stage_Priorities_AU_and_Reach_data[which(Life_Stage_Priorities_AU_and_Reach_data$ReachName == reach_x),steelhead_life_stages_presence[[life_stage_x]]] = score_x
      
    }
    
  }

  
}

# View(Life_Stage_Priorities_AU_and_Reach_data[,c(1:3,18:31)])

life_stage_priority__path = paste(ranking_data_path,'LifeStagePriorities_AUandReach.xlsx', sep="")
write_xlsx(Life_Stage_Priorities_AU_and_Reach_data,life_stage_priority__path )


# ------------------------------------------------------------------------------------------------------------
#
#        NOTE - this code was not finished since I did not use
#        Function to update habitat attributes (when new data has old reach names)
#
# ------------------------------------------------------------------------------------------------------------

# ------------- to test ---------
#updated_column_old = Okanogan_Crosswalk$ReachName_Old
#update_column_new = Okanogan_Crosswalk$ReachName_New
data_frame_to_update = habitat_raw_data
reach_name_primary_df = "ReachName"
#input_column = input_column[,1]
data_frame_input = read_excel("C:/Users/Ryan/Documents/2_Habitat_Prioritzation/Prioritization/Data/ATLAS_flowIMPORT_Habitat_Data_Raw_032921_shared042121.xlsx", skip=1)
reach_name_input_df = "ReachName"
new_and_old_reach_name_data_frame = Okanogan_Reach_Crosswalk[,c("ReachName_Old", "ReachName_New")]


FUNCTION_update_habitat_attribute_data_with_old_reach_names = function(data_frame_to_update, reach_name_primary_df, input_column_name, new_and_old_data_frame, output_path_x){
  

  # ------------------------ loop through rows in new (input) data frame ----------
  for(i in 1:nrow(data_frame_input[,reach_name_input_df])){
    reach_x = as.character(data_frame_input[i,reach_name_input_df])
    # --------------------- update directly if reach names overlap ---------
    if( any( data_frame_to_update$ReachName == reach_x) ){
      
      # ----------------- identify which row -----------
      row_x = which(data_frame_to_update[,reach_name_primary_df] == reach_x)
      
    # ------------------ use crosswalk to identify ----------
    }else{
      
    }
    
    
  }
  # ------ which reaches have a single name for each --------------
  one_to_one_crossover = which(Crosswalk_to_use$identical_reaches_RTT_Okanogan == "yes" |
                                 Crosswalk_to_use$`Multiple_RTT_Reaches?` == "yes")
  # ---------------- which reaches have multiple Okanogan names for single RTT reach ----------
  duplicate_RTT_names =  which(Crosswalk_to_use$`Multiple_Okanogan_Reaches?` == "yes")


}
